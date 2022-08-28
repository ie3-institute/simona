/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.StorageModel.StorageRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  ProvideFlexOptions,
  ProvideMinMaxFlexOptions
}
import edu.ie3.util.quantities.QuantityUtil
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units

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
    pMax: ComparableQuantity[Power],
    eStorage: ComparableQuantity[Energy],
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
  ): ComparableQuantity[Power] = ???

  /** @param data
    * @return
    *   flex options and optionally the next tick at which flex options will
    *   change
    */
  def determineFlexOptions(
      data: StorageRelevantData
  ): ProvideFlexOptions = {

    val chargingPossible = data.storedEnergy.isLessThan(eStorage)
    val dischargingPossible = data.storedEnergy.isGreaterThan(lowestEnergy)

    val zeroKw = 0d.asKiloWatt

    ProvideMinMaxFlexOptions(
      uuid,
      zeroKw,
      if (dischargingPossible) pMax.multiply(-1) else zeroKw,
      if (chargingPossible) pMax else zeroKw
    )
  }

  /** @param setPower
    *   power that has been set by EmAgent
    * @return
    */
  def handleIssuePowerCtrl(
      data: StorageRelevantData,
      setPower: ComparableQuantity[Power]
  ): Option[Long] = {
    if (QuantityUtil.isEquivalentAbs(0d.asKiloWatt, setPower, 1e-9)) {
      // do nothing
      None
    } else if (setPower.isGreaterThan(0d.asKiloWatt)) {
      // charge
      val energyToFull = eStorage.subtract(data.storedEnergy)
      val timeToFull = energyToFull.divide(setPower).asType(classOf[Time])
      val secondsToFull = timeToFull.to(Units.SECOND).getValue.intValue()
      Some(data.currentTick + secondsToFull)
    } else {
      // discharge
      val energyToEmpty = data.storedEnergy.subtract(lowestEnergy)
      val timeToEmpty =
        energyToEmpty.divide(setPower.multiply(-1)).asType(classOf[Time])
      val secondsToEmpty = timeToEmpty.to(Units.SECOND).getValue.intValue()
      Some(data.currentTick + secondsToEmpty)
    }

  }

}

object StorageModel {

  final case class StorageRelevantData(
      storedEnergy: ComparableQuantity[Energy],
      currentTick: Long
  ) extends CalcRelevantData
}
