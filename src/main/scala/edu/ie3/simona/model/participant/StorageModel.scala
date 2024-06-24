/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.StorageModel.{
  RefTargetSocParams,
  StorageRelevantData,
  StorageState,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Dimensionless, Each, Energy, Power, Seconds}

import java.time.ZonedDateTime
import java.util.UUID

final case class StorageModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    eStorage: Energy,
    pMax: Power,
    eta: Dimensionless,
    dod: Dimensionless,
    initialSoc: Double,
    targetSoc: Option[Double],
) extends SystemParticipant[StorageRelevantData, ApparentPower, StorageState](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    ) {

  private val minEnergy = zeroKWH

  // max Tolerance 1W till GWh storage
  private implicit val doubleTolerance: Power = eStorage / Seconds(1) * 3.6e-12

  /** In order to avoid faulty flexibility options, we want to avoid offering
    * charging/discharging that could last less than our smallest possible time
    * delta, which is one second.
    */
  private val toleranceMargin = pMax * Seconds(1d)

  /** Minimal allowed energy with tolerance margin added
    */
  private val minEnergyWithMargin = minEnergy + (toleranceMargin / eta.toEach)

  /** Maximum allowed energy with tolerance margin added
    */
  private val maxEnergyWithMargin = eStorage - (toleranceMargin * eta.toEach)

  private val refTargetSoc = targetSoc.map { target =>
    val targetEnergy = eStorage * target

    val targetWithPosMargin =
      targetEnergy + (toleranceMargin / eta.toEach)

    val targetWithNegMargin =
      targetEnergy - (toleranceMargin * eta.toEach)

    RefTargetSocParams(
      targetEnergy,
      targetWithPosMargin,
      targetWithNegMargin,
    )
  }

  /** Calculate the power behaviour based on the given data.
    *
    * @param tick
    *   Regarded instant in simulation
    * @param voltage
    *   Nodal voltage magnitude
    * @param modelState
    *   Current state of the model
    * @param data
    *   Further needed, secondary data
    * @return
    *   A tuple of active and reactive power
    */
  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      modelState: StorageState,
      data: StorageRelevantData,
  ): ApparentPower =
    throw new NotImplementedError(
      "Storage model cannot calculate power without flexibility control."
    )

  override protected def calculateActivePower(
      modelState: StorageState,
      data: StorageRelevantData,
  ): Power =
    throw new NotImplementedError(
      "Storage model cannot calculate power without flexibility control."
    )

  override def determineFlexOptions(
      data: StorageRelevantData,
      lastState: StorageState,
  ): ProvideFlexOptions = {
    val currentStoredEnergy =
      determineCurrentState(lastState, data.currentTick)

    val chargingPossible = !isFull(currentStoredEnergy)
    val dischargingPossible = !isEmpty(currentStoredEnergy)

    val refPower = refTargetSoc
      .map { targetParams =>
        if (currentStoredEnergy <= targetParams.targetWithPosMargin) {
          if (currentStoredEnergy >= targetParams.targetWithNegMargin) {
            // is within target +/- margin, no charging needed
            zeroKW
          } else {
            // below target - margin, charge up to target
            pMax
          }
        } else {
          // above target + margin, discharge to target
          pMax * (-1d)
        }
      }
      .getOrElse {
        // no target set
        zeroKW
      }

    ProvideMinMaxFlexOptions(
      uuid,
      refPower,
      if (dischargingPossible) pMax * (-1) else zeroKW,
      if (chargingPossible) pMax else zeroKW,
    )
  }

  private def calcNetPower(setPower: Power): Power =
    if (setPower > zeroKW) {
      // multiply eta if we're charging
      setPower * eta.toEach
    } else {
      // divide by eta if we're discharging
      // (draining the battery more than we get as output)
      setPower / eta.toEach
    }

  override def handleControlledPowerChange(
      data: StorageRelevantData,
      lastState: StorageState,
      setPower: Power,
  ): (StorageState, FlexChangeIndicator) = {
    val currentStoredEnergy =
      determineCurrentState(lastState, data.currentTick)

    val adaptedSetPower =
      if (
        // if power is close to zero, set it to zero
        (setPower ~= zeroKW)
        // do not keep charging if we're already full
        || (setPower > zeroKW && isFull(currentStoredEnergy))
        // do not keep discharging if we're already empty
        || (setPower < zeroKW && isEmpty(currentStoredEnergy))
      )
        zeroKW
      else
        setPower

    // net power after considering efficiency
    val netPower = calcNetPower(adaptedSetPower)

    val currentState =
      StorageState(
        currentStoredEnergy,
        adaptedSetPower,
        data.currentTick,
      )

    // if the storage is at minimum or maximum charged energy AND we are charging
    // or discharging, flex options will be different at the next activation
    val isEmptyOrFull =
      isEmpty(currentStoredEnergy) || isFull(currentStoredEnergy)
    // if target soc is enabled, we can also be at that exact point
    val isAtTarget = refTargetSoc.exists { targetParams =>
      currentStoredEnergy <= targetParams.targetWithPosMargin &&
      currentStoredEnergy >= targetParams.targetWithNegMargin
    }
    val isChargingOrDischarging = netPower != zeroKW
    // if we've been triggered just before we hit the minimum or maximum energy,
    // and we're still discharging or charging respectively (happens in edge cases),
    // we already set netPower to zero (see above) and also want to refresh flex options
    // at the next activation.
    // Similarly, if the ref target margin area is hit before hitting target SOC, we want
    // to refresh flex options.
    val hasObsoleteFlexOptions =
      (isFull(currentStoredEnergy) && setPower > zeroKW) ||
        (isEmpty(currentStoredEnergy) && setPower < zeroKW) ||
        (isAtTarget && setPower != zeroKW)

    val activateAtNextTick =
      ((isEmptyOrFull || isAtTarget) && isChargingOrDischarging) || hasObsoleteFlexOptions

    // calculate the time span until we're full or empty, if applicable
    val maybeTimeSpan =
      if (!isChargingOrDischarging) {
        // we're at 0 kW, do nothing
        None
      } else if (netPower > zeroKW) {
        // we're charging, calculate time until we're full or at target energy

        val closestEnergyTarget = refTargetSoc
          .flatMap { targetParams =>
            Option.when(
              currentStoredEnergy <= targetParams.targetWithNegMargin
            )(targetParams.targetSoc)
          }
          .getOrElse(eStorage)

        val energyToFull = closestEnergyTarget - currentStoredEnergy
        Some(energyToFull / netPower)
      } else {
        // we're discharging, calculate time until we're at lowest energy allowed or at target energy

        val closestEnergyTarget = refTargetSoc
          .flatMap { targetParams =>
            Option.when(
              currentStoredEnergy >= targetParams.targetWithPosMargin
            )(targetParams.targetSoc)
          }
          .getOrElse(minEnergy)

        val energyToEmpty = currentStoredEnergy - closestEnergyTarget
        Some(energyToEmpty / (netPower * (-1)))
      }

    // calculate the tick from time span
    val maybeNextTick = maybeTimeSpan.map { timeSpan =>
      val ticksToEmpty = Math.round(timeSpan.toSeconds)
      data.currentTick + ticksToEmpty
    }

    (currentState, FlexChangeIndicator(activateAtNextTick, maybeNextTick))
  }

  private def determineCurrentState(
      lastState: StorageState,
      currentTick: Long,
  ): Energy = {
    val timespan = currentTick - lastState.tick
    val netPower = calcNetPower(lastState.chargingPower)
    val energyChange = netPower * Seconds(timespan)

    val newEnergy = lastState.storedEnergy + energyChange

    // don't allow under- or overcharge e.g. due to tick rounding error
    minEnergy.max(eStorage.min(newEnergy))
  }

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is greater than the maximum charged
    *   energy allowed (minus a tolerance margin)
    */
  private def isFull(storedEnergy: Energy): Boolean =
    storedEnergy >= maxEnergyWithMargin

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is less than the minimal charged energy
    *   allowed (plus a tolerance margin)
    */
  private def isEmpty(storedEnergy: Energy): Boolean =
    storedEnergy <= minEnergyWithMargin
}

object StorageModel {

  final case class StorageRelevantData(
      currentTick: Long
  ) extends CalcRelevantData

  /** @param storedEnergy
    *   The amount of currently stored energy
    * @param chargingPower
    *   The power with which the storage is (dis-)charging, valid until the next
    *   state. Gross value that is valid outside the model, i.e. before
    *   considering efficiency etc.
    * @param tick
    *   The tick at which this state is valid
    */
  final case class StorageState(
      storedEnergy: Energy,
      chargingPower: Power,
      tick: Long,
  ) extends ModelState

  final case class RefTargetSocParams(
      targetSoc: Energy,
      targetWithPosMargin: Energy,
      targetWithNegMargin: Energy,
  )

  def apply(
      inputModel: StorageInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      initialSoc: Double,
      targetSoc: Option[Double],
  ): StorageModel = {

    val scaledInput = inputModel.copy().scale(scalingFactor).build()

    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        scaledInput.getOperationTime,
      )

    // build the fixed feed in model
    val model = StorageModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      QControl.apply(scaledInput.getqCharacteristics),
      Kilowatts(
        scaledInput.getType.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      scaledInput.getType.getCosPhiRated,
      KilowattHours(
        scaledInput.getType.geteStorage
          .to(PowerSystemUnits.KILOWATTHOUR)
          .getValue
          .doubleValue
      ),
      Kilowatts(
        scaledInput.getType.getpMax
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      Each(
        scaledInput.getType.getEta.to(PowerSystemUnits.PU).getValue.doubleValue
      ),
      Each(
        scaledInput.getType.getDod.to(PowerSystemUnits.PU).getValue.doubleValue
      ),
      initialSoc,
      targetSoc,
    )

    model.enable()
    model
  }

}
