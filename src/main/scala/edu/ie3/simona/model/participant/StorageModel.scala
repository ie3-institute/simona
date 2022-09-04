/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.simona.config.SimonaConfig
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
import edu.ie3.util.quantities.QuantityUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
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
) extends SystemParticipant[StorageRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    ) {

  private val lowestEnergy = eStorage.multiply(dod).asType(classOf[Energy])

  override protected def calculateActivePower(
      data: StorageRelevantData
  ): ComparableQuantity[Power] =
    throw new NotImplementedError(
      "Storage model cannot calculate power without flexibility control."
    )

  override def determineFlexOptions(
      data: StorageRelevantData
  ): ProvideFlexOptions = {
    val currentStoredEnergy =
      determineCurrentState(data.lastState, data.currentTick)

    val chargingPossible = currentStoredEnergy.isLessThan(eStorage)
    val dischargingPossible = currentStoredEnergy.isGreaterThan(lowestEnergy)

    val zeroKw = 0d.asKiloWatt

    ProvideMinMaxFlexOptions(
      uuid,
      zeroKw,
      if (dischargingPossible) pMax.multiply(-1) else zeroKw,
      if (chargingPossible) pMax else zeroKw
    )
  }

  override def handleControlledPowerChange(
      data: StorageRelevantData,
      setPower: ComparableQuantity[Power]
  ): (StorageRelevantData, Option[Long]) = {
    val currentStoredEnergy =
      determineCurrentState(data.lastState, data.currentTick)

    // net power after considering efficiency
    val netPower = setPower.multiply(eta).asType(classOf[Power])

    val currentState =
      StorageState(currentStoredEnergy, netPower, data.currentTick)
    val currentRelevantData =
      StorageRelevantData(currentState, data.currentTick)

    val maybeAdditionalTick =
      if (QuantityUtil.isEquivalentAbs(0d.asKiloWatt, netPower, 1e-9)) {
        // do nothing
        None
      } else if (netPower.isGreaterThan(0d.asKiloWatt)) {
        // charge
        val energyToFull = eStorage.subtract(currentStoredEnergy)
        val timeToFull = energyToFull.divide(netPower).asType(classOf[Time])
        val ticksToFull =
          Math.round(timeToFull.to(Units.SECOND).getValue.doubleValue())
        Some(data.currentTick + ticksToFull)
      } else {
        // discharge
        val energyToEmpty = currentStoredEnergy.subtract(lowestEnergy)
        val timeToEmpty =
          energyToEmpty.divide(netPower.multiply(-1)).asType(classOf[Time])
        val ticksToEmpty = timeToEmpty.to(Units.SECOND).getValue.intValue()
        Some(data.currentTick + ticksToEmpty)
      }

    (currentRelevantData, maybeAdditionalTick)
  }

  private def determineCurrentState(
      lastState: StorageState,
      currentTick: Long
  ): ComparableQuantity[Energy] = {
    val timespan = currentTick - lastState.tick
    val energyChange = lastState.chargingPower
      .multiply(Quantities.getQuantity(timespan, Units.SECOND))
      .asType(classOf[Energy])

    lastState.storedEnergy.add(energyChange)
    // TODO don't allow under- or overcharge due to rounding error
  }

}

object StorageModel {

  final case class StorageRelevantData(
      lastState: StorageState,
      currentTick: Long
  ) extends CalcRelevantData

  final case class StorageState(
      storedEnergy: ComparableQuantity[Energy],
      chargingPower: ComparableQuantity[Power],
      tick: Long
  )

  def apply(
      inputModel: StorageInput,
      modelConfiguration: SimonaConfig.StorageRuntimeConfig,
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
      modelConfiguration.scaling,
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
