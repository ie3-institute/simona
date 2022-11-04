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
  StorageRelevantData,
  StorageState
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions
}
import edu.ie3.util.quantities.QuantityUtils.{RichQuantity, RichQuantityDouble}
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import tech.units.indriya.ComparableQuantity
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
    dod: ComparableQuantity[Dimensionless],
    initialSoc: Double // TODO this is ugly and should be solved in a different way, as this value is only used outside the model
) extends SystemParticipant[StorageRelevantData, ApparentPower, StorageState](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    ) {

  private val minEnergy = eStorage
    .multiply(dod)
    .asType(classOf[Energy])
    .to(PowerSystemUnits.KILOWATTHOUR)

  /** In order to avoid faulty flexibility options, we want to avoid offering
    * charging/discharging that could last less than one second.
    */
  private val toleranceMargin = pMax
    .multiply(1d.asSecond)
    .asType(classOf[Energy])
    .to(PowerSystemUnits.KILOWATTHOUR)

  /** Minimal allowed energy with tolerance margin added
    */
  private val minEnergyWithMargin = minEnergy
    .add(toleranceMargin.divide(eta).asType(classOf[Energy]))
    .to(PowerSystemUnits.KILOWATTHOUR)

  /** Maximum allowed energy with tolerance margin added
    */
  private val maxEnergyWithMargin =
    eStorage
      .subtract(toleranceMargin.multiply(eta).asType(classOf[Energy]))
      .to(PowerSystemUnits.KILOWATTHOUR)

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
      voltage: ComparableQuantity[Dimensionless],
      modelState: StorageState,
      data: StorageRelevantData
  ): ApparentPower = ???

  override protected def calculateActivePower(
      modelState: StorageState,
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
    val netPower =
      if (QuantityUtil.isEquivalentAbs(zeroKW, setPower, 1e-9)) {
        // if power is close to zero, set it to zero
        zeroKW
      } else if (setPower.isGreaterThan(zeroKW)) {
        if (isFull(currentStoredEnergy))
          zeroKW // do not keep charging if we're already full
        else
          // multiply eta if we're charging
          setPower
            .multiply(eta)
            .asType(classOf[Power])
            .to(PowerSystemUnits.KILOWATT)
      } else {
        if (isEmpty(currentStoredEnergy))
          zeroKW // do not keep discharging if we're already empty
        else
          // divide eta if we're discharging
          // (draining the battery more than we get as output)
          setPower
            .multiply(eta) // FIXME this should be division
            .asType(classOf[Power])
            .to(PowerSystemUnits.KILOWATT)
      }

    val currentState =
      StorageState(currentStoredEnergy, netPower, data.currentTick)

    // if the storage is at minimum or maximum charged energy AND we are charging
    // or discharging, flex options will be different at the next activation
    val isEmptyOrFull =
      isEmpty(currentStoredEnergy) || isFull(currentStoredEnergy)
    val isChargingOrDischarging =
      !QuantityUtil.isEquivalentAbs(zeroKW, netPower, 0)
    // if we've been triggered just before we hit the minimum or maximum energy,
    // and we're still discharging or charging respectively (happens in edge cases),
    // we already set netPower to zero (see above) and also want to refresh flex options
    // at the next activation
    val hasObsoleteFlexOptions =
      (isFull(currentStoredEnergy) && setPower.isGreaterThan(zeroKW)) ||
        (isEmpty(currentStoredEnergy) && setPower.isLessThan(zeroKW))

    val activateAtNextTick =
      (isEmptyOrFull && isChargingOrDischarging) || hasObsoleteFlexOptions

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
        val energyToEmpty = currentStoredEnergy.subtract(minEnergy)
        Some(energyToEmpty.divide(netPower.multiply(-1)).asType(classOf[Time]))
      }

    // calculate the tick from time span
    val maybeNextTick = maybeTimeSpan.map { timeSpan =>
      val ticksToEmpty =
        Math.round(timeSpan.to(Units.SECOND).getValue.doubleValue)
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
      .multiply(timespan.asSecond)
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
    storedEnergy.isGreaterThanOrEqualTo(maxEnergyWithMargin)

  /** @param storedEnergy
    *   the stored energy amount to check
    * @return
    *   whether the given stored energy is less than the minimal charged energy
    *   allowed (plus a tolerance margin)
    */
  private def isEmpty(storedEnergy: ComparableQuantity[Energy]): Boolean =
    storedEnergy.isLessThanOrEqualTo(minEnergyWithMargin)
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
      simulationEndDate: ZonedDateTime,
      initialSoc: Double
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
      inputModel.getType.getDod,
      initialSoc
    )
    // TODO include activePowerGradient, lifeTime, lifeCycle ?

    model.enable()
    model
  }
}
