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
import squants.Each
import squants.energy.{KilowattHours, Kilowatts, Watts}

import java.time.ZonedDateTime
import java.util.UUID

final case class StorageModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    override val scalingFactor: Double,
    qControl: QControl,
    sRated: squants.Power,
    cosPhiRated: Double,
    eStorage: squants.Energy,
    pMax: squants.Power,
    eta: squants.Dimensionless,
    dod: squants.Dimensionless,
    initialSoc: Double, // TODO this is ugly and should be solved in a different way, as this value is only used outside the model
    targetSoc: Option[Double], // TODO only needed for initializing fields
) extends SystemParticipant[StorageRelevantData, ApparentPower, StorageState](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated,
    ) {

  private val minEnergy = eStorage * dod.toEach

  // Tolerance fitting for capacities up to GWh
  // FIXME make dependent on capacity
  private implicit val doubleTolerance: squants.Power = Watts(1e-3)

  /** In order to avoid faulty flexibility options, we want to avoid offering
    * charging/discharging that could last less than one second.
    */
  private val toleranceMargin = pMax * squants.Seconds(1d)

  /** Minimal allowed energy with tolerance margin added
    */
  private val minEnergyWithMargin = minEnergy + (toleranceMargin / eta.toEach)

  /** Maximum allowed energy with tolerance margin added
    */
  private val maxEnergyWithMargin = eStorage - (toleranceMargin * eta.toEach)

  private val refTargetSoc = targetSoc.map { target =>
    val targetEnergy = eStorage * target

    val targetWithPosMargin = // FIXME this should be division
      targetEnergy + (toleranceMargin * eta.toEach)

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
      voltage: squants.Dimensionless,
      modelState: StorageState,
      data: StorageRelevantData,
  ): ApparentPower = ???

  override protected def calculateActivePower(
      modelState: StorageState,
      data: StorageRelevantData,
  ): squants.Power =
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
            Kilowatts(0d)
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
        Kilowatts(0d)
      }

    ProvideMinMaxFlexOptions(
      uuid,
      refPower,
      if (dischargingPossible) pMax * (-1) else Kilowatts(0d),
      if (chargingPossible) pMax else Kilowatts(0d),
    )
  }

  override def handleControlledPowerChange(
      data: StorageRelevantData,
      lastState: StorageState,
      setPower: squants.Power,
  ): (StorageState, FlexChangeIndicator) = {
    val currentStoredEnergy =
      determineCurrentState(lastState, data.currentTick)

    // net power after considering efficiency
    val netPower =
      if (setPower ~= Kilowatts(0d)) {
        // if power is close to zero, set it to zero
        Kilowatts(0d)
      } else if (setPower > Kilowatts(0d)) {
        if (isFull(currentStoredEnergy))
          Kilowatts(0d) // do not keep charging if we're already full
        else
          // multiply eta if we're charging
          setPower * eta.toEach
      } else {
        if (isEmpty(currentStoredEnergy))
          Kilowatts(0d) // do not keep discharging if we're already empty
        else
          // divide by eta if we're discharging
          // (draining the battery more than we get as output)
          setPower * eta.toEach // FIXME this should be division. Check as well with SoC Calculation.
      }

    val currentState =
      StorageState(
        currentStoredEnergy,
        netPower,
        data.currentTick,
      ) // FIXME this should be setPower instead of netPower ? Because the EM receives / sees Power after considering eta / output power. Internal "discharging" needs to be done with netpower! Check whether EM get's setpower or netPower.

    // if the storage is at minimum or maximum charged energy AND we are charging
    // or discharging, flex options will be different at the next activation
    val isEmptyOrFull =
      isEmpty(currentStoredEnergy) || isFull(currentStoredEnergy)
    // if target soc is enabled, we can also be at that exact point
    val isAtTarget = refTargetSoc.exists { targetParams =>
      currentStoredEnergy <= targetParams.targetWithPosMargin &&
      currentStoredEnergy >= targetParams.targetWithNegMargin
    }
    val isChargingOrDischarging = netPower != Kilowatts(0d)
    // if we've been triggered just before we hit the minimum or maximum energy,
    // and we're still discharging or charging respectively (happens in edge cases),
    // we already set netPower to zero (see above) and also want to refresh flex options
    // at the next activation.
    // Similarly, if the ref target margin area is hit before hitting target SOC, we want
    // to refresh flex options.
    val hasObsoleteFlexOptions =
      (isFull(currentStoredEnergy) && setPower > Kilowatts(0d)) ||
        (isEmpty(currentStoredEnergy) && setPower < Kilowatts(0d)) ||
        (isAtTarget && setPower != Kilowatts(0d))

    val activateAtNextTick =
      ((isEmptyOrFull || isAtTarget) && isChargingOrDischarging) || hasObsoleteFlexOptions

    // calculate the time span until we're full or empty, if applicable
    val maybeTimeSpan =
      if (!isChargingOrDischarging) {
        // we're at 0 kW, do nothing
        None
      } else if (netPower > Kilowatts(0d)) {
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
  ): squants.Energy = {
    val timespan = currentTick - lastState.tick
    val energyChange = lastState.chargingPower * squants.Seconds(timespan)

    val newEnergy = lastState.storedEnergy + energyChange

    // don't allow under- or overcharge e.g. due to tick rounding error
    // allow charges below dod though since batteries can start at 0 kWh
    // TODO don't allow SOCs below dod
    KilowattHours(0d).max(eStorage.min(newEnergy))
  }

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is greater than the maximum charged
    *   energy allowed (minus a tolerance margin)
    */
  private def isFull(storedEnergy: squants.Energy): Boolean =
    storedEnergy >= maxEnergyWithMargin

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is less than the minimal charged energy
    *   allowed (plus a tolerance margin)
    */
  private def isEmpty(storedEnergy: squants.Energy): Boolean =
    storedEnergy <= minEnergyWithMargin
}

object StorageModel {

  final case class StorageRelevantData(
      currentTick: Long
  ) extends CalcRelevantData

  final case class StorageState(
      storedEnergy: squants.Energy,
      chargingPower: squants.Power,
      tick: Long,
  ) extends ModelState

  final case class RefTargetSocParams(
      targetSoc: squants.Energy,
      targetWithPosMargin: squants.Energy,
      targetWithNegMargin: squants.Energy,
  )

  def apply(
      inputModel: StorageInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      initialSoc: Double,
      targetSoc: Option[Double],
  ): StorageModel = {
    {
      // TODO have this in some fail fast
      val dod =
        inputModel.getType.getDod.to(PowerSystemUnits.PU).getValue.doubleValue

      if (initialSoc > dod)
        throw new RuntimeException(
          s"Storage ${inputModel.getUuid}: Initial SOC can't be lower than DOD"
        )

      if (targetSoc.exists(_ < dod))
        throw new RuntimeException(
          s"Storage ${inputModel.getUuid}: Target SOC can't be lower than DOD"
        )
    }

    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime,
      )

    // build the fixed feed in model
    val model = StorageModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      scalingFactor,
      QControl.apply(inputModel.getqCharacteristics),
      Kilowatts(
        inputModel.getType.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      inputModel.getType.getCosPhiRated,
      KilowattHours(
        inputModel.getType.geteStorage
          .to(PowerSystemUnits.KILOWATTHOUR)
          .getValue
          .doubleValue
      ),
      Kilowatts(
        inputModel.getType.getpMax
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      Each(
        inputModel.getType.getEta.to(PowerSystemUnits.PU).getValue.doubleValue
      ),
      Each(
        inputModel.getType.getDod.to(PowerSystemUnits.PU).getValue.doubleValue
      ),
      initialSoc,
      targetSoc,
    )

    // TODO include activePowerGradient, lifeTime, lifeCycle ?

    model.enable()
    model
  }

}
