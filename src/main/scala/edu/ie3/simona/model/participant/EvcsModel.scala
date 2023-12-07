/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.EvcsModel.EvcsRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{DefaultQuantities, Megavars}
import squants.energy
import squants.energy.{KilowattHours, Kilowatts, Megawatts}
import squants.time.Seconds
import tech.units.indriya.quantity.Quantities.getQuantity

import java.time.ZonedDateTime
import java.util.UUID

/** EV charging station model
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param scalingFactor
  *   Scaling the output of the system
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  * @param chargingPoints
  *   Number of charging points available at this charging station
  * @param locationType
  *   The location type
  */
final case class EvcsModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    override protected val scalingFactor: Double,
    qControl: QControl,
    sRated: squants.Power,
    cosPhiRated: Double,
    chargingPoints: Int,
    locationType: EvcsLocationType
) extends SystemParticipant[EvcsRelevantData, ApparentPower](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    )
    with ApparentPowerParticipant[EvcsRelevantData]
    with LazyLogging {

  /** Calculate the power behaviour based on the given data.
    *
    * @param tick
    *   Regarded instant in simulation
    * @param voltage
    *   Nodal voltage magnitude
    * @param data
    *   Further needed, secondary data
    * @return
    *   A tuple of active and reactive power
    */
  def calculatePowerAndEvSoc(
      tick: Long,
      voltage: squants.Dimensionless,
      data: EvcsRelevantData
  ): (ApparentPower, Set[EvModel]) = {
    if (isInOperation(tick) && data.evMovementsDataFrameLength > 0) {
      val (activePower, evModels) = calculateActivePowerAndEvSoc(data)
      val reactivePower =
        calculateReactivePower(activePower, voltage)
      (
        ApparentPower(
          activePower,
          reactivePower
        ),
        evModels
      )
    } else {
      (
        ApparentPower(
          Megawatts(0d),
          Megavars(0d)
        ),
        data.currentEvs
      )
    }
  }

  /** Calculates active power based on given data. If sRated of this evcs is
    * exceeded, evs are dropped from charging at this time span.
    * @param data
    *   The needed data. evMovementsDataFrameLength > 0 is required.
    * @return
    *   Active power and ev models with updated stored energy
    */
  def calculateActivePowerAndEvSoc(
      data: EvcsRelevantData
  ): (squants.Power, Set[EvModel]) = {
    val (powerSum, models) = calculateActivePowerAndEvSoc(
      data.currentEvs,
      data.evMovementsDataFrameLength
    )
    if (powerSum <= sRated) {
      (powerSum, models)
    } else {
      // if we exceed sRated, we scale down charging power of all evs proportionally
      logger.warn(
        s"Set of charging evs is charging with $powerSum and thus exceeding evcs sRated $sRated."
      )

      val (calcEvs, noCalcEvs, _) =
        data.currentEvs.foldLeft(
          (
            Set.empty[EvModel],
            Set.empty[EvModel],
            DefaultQuantities.zeroKW
          )
        ) { case ((calcEvs, noCalcEvs, powerSum), ev) =>
          val newPower =
            powerSum + Kilowatts(ev.getSRatedAC.getValue.doubleValue())
          if (newPower <= sRated)
            (calcEvs + ev, noCalcEvs, newPower)
          else
            (calcEvs, noCalcEvs + ev, powerSum)
        }

      val (power, newCalcEvs) =
        calculateActivePowerAndEvSoc(
          calcEvs,
          data.evMovementsDataFrameLength
        )
      // include ignored evs
      (power, newCalcEvs ++ noCalcEvs)
    }
  }

  /** Calculates active power based on given set of evs
    * @param currentEvs
    *   The currently charging evs
    * @param dataFrameLength
    *   The duration that all evs are charging
    * @return
    *   Active power and ev models with updated stored energy
    */
  private def calculateActivePowerAndEvSoc(
      currentEvs: Set[EvModel],
      dataFrameLength: Long
  ): (squants.Power, Set[EvModel]) = {
    val tickDuration = Seconds(dataFrameLength)

    currentEvs.foldLeft(DefaultQuantities.zeroKW, Set.empty[EvModel]) {
      case ((powerSum, models), evModel) =>
        val (chargedEnergy, newEvModel) = charge(
          evModel,
          tickDuration
        )

        val chargingPower =
          chargedEnergy / tickDuration

        (
          powerSum + chargingPower,
          models + newEvModel
        )
    }
  }

  /** Charging given ev model (inside a copy) for given duration.
    * @param evModel
    *   The ev model to charge
    * @param duration
    *   The duration of charging
    * @return
    *   Charged energy and updated ev model as a copy
    */
  def charge(
      evModel: EvModel,
      duration: squants.Time
  ): (squants.Energy, EvModel) = {
    if (evModel.getStoredEnergy.isLessThan(evModel.getEStorage)) {
      val chargingPower =
        sRated.min(
          Kilowatts(
            evModel.getSRatedAC
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue()
          )
        )

      val chargeLeftToFull = KilowattHours(
        evModel.getEStorage
          .to(PowerSystemUnits.KILOWATTHOUR)
          .getValue
          .doubleValue()
      ) - KilowattHours(
        evModel.getStoredEnergy
          .to(PowerSystemUnits.KILOWATTHOUR)
          .getValue
          .doubleValue()
      )

      val potentialChargeDuringTick = chargingPower * duration

      val actualCharge = chargeLeftToFull.min(potentialChargeDuringTick)
      val newStoredEnergy = KilowattHours(
        evModel.getStoredEnergy
          .to(PowerSystemUnits.KILOWATTHOUR)
          .getValue
          .doubleValue()
      ) + actualCharge

      (
        actualCharge,
        evModel.copyWith(
          getQuantity(
            newStoredEnergy.value.doubleValue(),
            PowerSystemUnits.KILOWATTHOUR
          )
        )
      )
    } else
      (
        DefaultQuantities.zeroKWH,
        evModel
      )
  }

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      data: EvcsRelevantData
  ): energy.Power =
    throw new NotImplementedError("Use calculatePowerAndEvSoc() instead.")
}

object EvcsModel {

  /** Class that holds all relevant data for an Evcs model calculation
    *
    * @param evMovementsDataFrameLength
    *   the duration in ticks (= seconds) until next tick
    * @param currentEvs
    *   EVs that have been charging up until this tick. Can include EVs that are
    *   departing
    */
  final case class EvcsRelevantData(
      evMovementsDataFrameLength: Long,
      currentEvs: Set[EvModel]
  ) extends CalcRelevantData

  def apply(
      inputModel: EvcsInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EvcsModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    apply(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      scalingFactor,
      QControl(inputModel.getqCharacteristics),
      Kilowatts(
        inputModel.getType.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue()
      ),
      inputModel.getCosPhiRated,
      inputModel.getChargingPoints,
      inputModel.getLocationType
    )
  }

  /** Default factory method to create an EvcsModel instance.
    *
    * @param uuid
    *   the unique id of the model
    * @param id
    *   the human readable id
    * @param operationInterval
    *   the operation interval of the model
    * @param scalingFactor
    *   the scaling factor of the power output
    * @param qControl
    *   the q control this model is using
    * @param sRated
    *   the rated apparent power of the model
    * @param cosPhiRated
    *   the rated cosine phi of the model
    * @param chargingPoints
    *   Number of charging points available at this charging station
    * @param locationType
    *   The location type
    * @return
    *   the enabled EvcsModel
    */
  def apply(
      uuid: UUID,
      id: String,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      qControl: QControl,
      sRated: squants.Power,
      cosPhiRated: Double,
      chargingPoints: Int,
      locationType: EvcsLocationType
  ): EvcsModel = {
    val model = new EvcsModel(
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated,
      chargingPoints,
      locationType
    )

    model.enable()

    model
  }
}
