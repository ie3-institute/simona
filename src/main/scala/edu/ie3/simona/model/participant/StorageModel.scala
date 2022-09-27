/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.StorageModel.{
  StorageRelevantData,
  StorageState
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions
}
import edu.ie3.util.quantities.QuantityUtils.RichQuantity
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Energy, Power, Time}

final case class StorageModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    eStorage: ComparableQuantity[Energy],
    pMax: ComparableQuantity[Power],
    eta: ComparableQuantity[Dimensionless],
    dod: ComparableQuantity[Dimensionless]
) extends SystemParticipant[StorageRelevantData, StorageState](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    ) {

  private val lowestEnergy = eStorage.multiply(dod).asType(classOf[Energy])

  /** In order to avoid faulty flexibility options, we want to avoid offering
    * charging/discharging that could last less than one second.
    */
  private val toleranceMargin = pMax
    .multiply(Quantities.getQuantity(1, Units.SECOND))
    .asType(classOf[Energy])

  override protected def calculateActivePower(
      data: StorageRelevantData
  ): ComparableQuantity[Power] =
    throw new NotImplementedError(
      "Storage model cannot calculate power without flexibility control."
    )

  override def determineFlexOptions(
      data: StorageRelevantData,
      lastState: StorageState
  ): ProvideFlexOptions = {
    val currentStoredEnergy =
      determineCurrentState(lastState, data.currentTick)

    val chargingPossible = !isFull(currentStoredEnergy)
    val dischargingPossible = !isEmpty(currentStoredEnergy)

    ProvideMinMaxFlexOptions(
      uuid,
      zeroKW,
      if (dischargingPossible) pMax.multiply(-1) else zeroKW,
      if (chargingPossible) pMax else zeroKW
    )
  }

  override def handleControlledPowerChange(
      data: StorageRelevantData,
      lastState: StorageState,
      setPower: ComparableQuantity[Power]
  ): (StorageState, FlexChangeIndicator) = {
    val currentStoredEnergy =
      determineCurrentState(lastState, data.currentTick)

    // net power after considering efficiency
    val netPower = {
      val proposal = setPower
        .multiply(eta)
        .asType(classOf[Power])
        .to(PowerSystemUnits.KILOWATT)

      // if it's close to zero, set it to zero
      if (QuantityUtil.isEquivalentAbs(zeroKW, proposal, 1e-9))
        zeroKW
      else
        proposal
    }

    val currentState =
      StorageState(currentStoredEnergy, netPower, data.currentTick)

    // if the storage is at minimum or maximum charged energy AND we are charging
    // or discharging, flex options will be different at the next activation
    val isEmptyOrFull =
      isEmpty(currentStoredEnergy) || isFull(currentStoredEnergy)
    val isChargingOrDischarging =
      !QuantityUtil.isEquivalentAbs(zeroKW, netPower, 0)

    val activateAtNextTick = isEmptyOrFull && isChargingOrDischarging

    // calculate the time span until we're full or empty, if applicable
    val maybeTimeSpan =
      if (!isChargingOrDischarging) {
        // we're at 0 kW, do nothing
        None
      } else if (netPower.isGreaterThan(zeroKW)) {
        // we're charging, calculate time until we're full
        val energyToFull = eStorage.subtract(currentStoredEnergy)
        Some(energyToFull.divide(netPower).asType(classOf[Time]))
      } else {
        // we're discharging, calculate time until we're at lowest energy allowed
        val energyToEmpty = currentStoredEnergy.subtract(lowestEnergy)
        Some(energyToEmpty.divide(netPower.multiply(-1)).asType(classOf[Time]))
      }

    // calculate the tick from time span
    val maybeNextTick = maybeTimeSpan.map { timeSpan =>
      val ticksToEmpty =
        Math.round(timeSpan.to(Units.SECOND).getValue.doubleValue())
      data.currentTick + ticksToEmpty
    }

    (currentState, FlexChangeIndicator(activateAtNextTick, maybeNextTick))
  }

  private def determineCurrentState(
      lastState: StorageState,
      currentTick: Long
  ): ComparableQuantity[Energy] = {
    val timespan = currentTick - lastState.tick
    val energyChange = lastState.chargingPower
      .multiply(Quantities.getQuantity(timespan, Units.SECOND))
      .asType(classOf[Energy])

    val newEnergy = lastState.storedEnergy.add(energyChange)

    // don't allow under- or overcharge e.g. due to tick rounding error
    // allow charges below dod though since batteries can start at 0 kWh
    zeroKWH.max(eStorage.min(newEnergy))
  }

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is greater than the maximum charged
    *   energy allowed (minus a tolerance margin)
    */
  private def isFull(storedEnergy: ComparableQuantity[Energy]): Boolean =
    storedEnergy.isGreaterThanOrEqualTo(eStorage.subtract(toleranceMargin))

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is less than the minimal charged energy
    *   allowed (plus a tolerance margin)
    */
  private def isEmpty(storedEnergy: ComparableQuantity[Energy]): Boolean =
    storedEnergy.isLessThanOrEqualTo(lowestEnergy.add(toleranceMargin))

}

object StorageModel {

  final case class StorageRelevantData(
      currentTick: Long
  ) extends CalcRelevantData

  final case class StorageState(
      storedEnergy: ComparableQuantity[Energy],
      chargingPower: ComparableQuantity[Power],
      tick: Long
  ) extends ModelState

  def apply(
      inputModel: StorageInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): StorageModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    // build the fixed feed in model
    val model = StorageModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      scalingFactor,
      QControl.apply(inputModel.getqCharacteristics),
      inputModel.getType.getsRated,
      inputModel.getType.getCosPhiRated,
      inputModel.getType.geteStorage,
      inputModel.getType.getpMax,
      inputModel.getType.getEta,
      inputModel.getType.getDod
    )
    // TODO include activePowerGradient, lifeTime, lifeCycle ?

    model.enable()
    model
  }
}
