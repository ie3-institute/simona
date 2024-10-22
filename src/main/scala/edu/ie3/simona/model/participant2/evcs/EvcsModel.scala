/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.result.system.{
  EvcsResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.model.participant2.ParticipantModel
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
import edu.ie3.simona.model.participant2.evcs.EvcsModel.{
  ChargingStrategy,
  EvcsOperatingPoint,
  EvcsRelevantData,
  EvcsState,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EvMessage.ArrivingEvs
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.ReactivePower
import squants.energy.Watts
import squants.time.Seconds
import squants.{Dimensionless, Energy, Power}

import java.time.ZonedDateTime
import java.util.UUID

class EvcsModel private (
    override val uuid: UUID,
    override val sRated: Power,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    strategy: ChargingStrategy,
    override val currentType: ElectricCurrentType,
    override val lowestEvSoc: Double,
    vehicle2grid: Boolean,
) extends ParticipantModel[
      EvcsOperatingPoint,
      EvcsState,
      EvcsRelevantData,
    ]
    with EvcsChargingProperties {
  override def determineOperatingPoint(
      state: EvcsState,
      relevantData: EvcsRelevantData,
  ): (EvcsOperatingPoint, Option[Long]) = {
    val chargingPowers =
      strategy.determineChargingPowers(state.evs.values, state.tick, this)

    val nextEvent = chargingPowers.flatMap { case (uuid, power) =>
      val ev = state.evs.getOrElse(
        uuid,
        throw new CriticalFailureException(
          s"Charging strategy ${strategy.getClass.getSimpleName} returned a charging power for unknown UUID $uuid"
        ),
      )

      determineNextEvent(ev, power)
    }.minOption

    (EvcsOperatingPoint(chargingPowers), nextEvent)
  }

  private def determineNextEvent(
      ev: EvModelWrapper,
      chargingPower: Power,
  ): Option[Long] = {
    implicit val tolerance: Power = Watts(1e-3)
    if (chargingPower ~= zeroKW)
      None
    else {
      val timeUntilFullOrEmpty =
        if (chargingPower > zeroKW) {

          // if we're below lowest SOC, flex options will change at that point
          val targetEnergy =
            if (isEmpty(ev) && !isInLowerMargin(ev))
              ev.eStorage * lowestEvSoc
            else
              ev.eStorage

          (targetEnergy - ev.storedEnergy) / chargingPower
        } else
          (ev.storedEnergy - (ev.eStorage * lowestEvSoc)) / (chargingPower * (-1))

      Some(Math.round(timeUntilFullOrEmpty.toSeconds))
    }
  }

  override def determineState(
      lastState: EvcsState,
      operatingPoint: EvcsOperatingPoint,
      currentTick: Long,
  ): EvcsState = {

    val updatedEvs = lastState.evs.map { case (uuid, ev) =>
      uuid ->
        operatingPoint.evOperatingPoints
          .get(uuid)
          .map { chargingPower =>
            val newStoredEnergy = ev.storedEnergy +
              chargingPower * Seconds(
                currentTick - lastState.tick
              )
            ev.copy(storedEnergy = newStoredEnergy)
          }
          .getOrElse(ev)
    }

    EvcsState(updatedEvs, currentTick)
  }

  override def createResults(
      state: EvcsState,
      operatingPoint: EvcsOperatingPoint,
      complexPower: PrimaryData.ApparentPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] = ???

  override def createPrimaryDataResult(
      data: PrimaryData.PrimaryDataWithApparentPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new EvcsResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

  override def getRequiredServices: Iterable[ServiceType] =
    Iterable(
      ServiceType.EvMovementService
    )

  override def getInitialState(): EvcsState = EvcsState(Map.empty, -1)

  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
  ): EvcsRelevantData = {
    receivedData
      .collectFirst { case evData: ArrivingEvs =>
        EvcsRelevantData(tick, evData.arrivals)
      }
      .getOrElse {
        throw new CriticalFailureException(
          s"Expected ArrivingEvs, got $receivedData"
        )
      }
  }

  override def calcFlexOptions(
      state: EvcsState,
      relevantData: EvcsRelevantData,
  ): FlexibilityMessage.ProvideFlexOptions = {

    val preferredPowers =
      strategy.determineChargingPowers(state.evs.values, state.tick, this)

    val (maxCharging, preferredPower, forcedCharging, maxDischarging) =
      state.evs.foldLeft(
        (zeroKW, zeroKW, zeroKW, zeroKW)
      ) {
        case (
              (chargingSum, preferredSum, forcedSum, dischargingSum),
              (uuid, ev),
            ) =>
          val maxPower = getMaxAvailableChargingPower(ev)

          val preferredPower = preferredPowers.get(uuid)

          val maxCharging =
            if (!isFull(ev))
              maxPower
            else
              zeroKW

          val forced =
            if (isEmpty(ev) && !isInLowerMargin(ev))
              preferredPower.getOrElse(maxPower)
            else
              zeroKW

          val maxDischarging =
            if (!isEmpty(ev) && vehicle2grid)
              maxPower * -1
            else
              zeroKW

          (
            chargingSum + maxCharging,
            preferredSum + preferredPower.getOrElse(zeroKW),
            forcedSum + forced,
            dischargingSum + maxDischarging,
          )
      }

    // if we need to charge at least one EV, we cannot discharge any other
    val (adaptedMaxDischarging, adaptedPreferred) =
      if (forcedCharging > zeroKW)
        (forcedCharging, preferredPower.max(forcedCharging))
      else
        (maxDischarging, preferredPower)

    ProvideMinMaxFlexOptions(
      uuid,
      adaptedPreferred,
      adaptedMaxDischarging,
      maxCharging,
    )
  }

  override def handlePowerControl(
      state: EvcsState,
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (EvcsOperatingPoint, ParticipantModel.ModelChangeIndicator) = ???

  /** @param ev
    *   the ev whose stored energy is to be checked
    * @return
    *   whether the given ev's stored energy is greater than the maximum charged
    *   energy allowed (minus a tolerance margin)
    */
  private def isFull(ev: EvModelWrapper): Boolean =
    ev.storedEnergy >= (ev.eStorage - calcToleranceMargin(ev))

  /** @param ev
    *   the ev whose stored energy is to be checked
    * @return
    *   whether the given ev's stored energy is less than the minimal charged
    *   energy allowed (plus a tolerance margin)
    */
  private def isEmpty(ev: EvModelWrapper): Boolean =
    ev.storedEnergy <= (
      ev.eStorage * lowestEvSoc + calcToleranceMargin(ev)
    )

  /** @param ev
    *   the ev whose stored energy is to be checked
    * @return
    *   whether the given ev's stored energy is within +- tolerance of the
    *   minimal charged energy allowed
    */
  private def isInLowerMargin(ev: EvModelWrapper): Boolean = {
    val toleranceMargin = calcToleranceMargin(ev)
    val lowestSoc = ev.eStorage * lowestEvSoc

    ev.storedEnergy <= (
      lowestSoc + toleranceMargin
    ) && ev.storedEnergy >= (
      lowestSoc - toleranceMargin
    )
  }

  private def calcToleranceMargin(ev: EvModelWrapper): Energy =
    getMaxAvailableChargingPower(ev) * Seconds(1)

  /** Returns the maximum available charging power for an EV, which depends on
    * ev and charging station limits for AC and DC current
    *
    * @param ev
    *   ev for which the max charging power should be returned
    * @return
    *   maximum charging power for the EV at this charging station
    */
  override def getMaxAvailableChargingPower(
      ev: EvModelWrapper
  ): Power = {
    val evPower = currentType match {
      case ElectricCurrentType.AC =>
        ev.sRatedAc
      case ElectricCurrentType.DC =>
        ev.sRatedDc
    }
    /* Limit the charging power to the minimum of ev's and evcs' permissible power */
    evPower.min(sRated)
  }

}

object EvcsModel {

  final case class EvcsOperatingPoint(evOperatingPoints: Map[UUID, Power])
      extends OperatingPoint {

    override val activePower: Power =
      evOperatingPoints.values.reduceOption(_ + _).getOrElse(zeroKW)

    override val reactivePower: Option[ReactivePower] = ???
  }

  /** @param evs
    *   TODO also save starting tick, so that results are not cluttered
    * @param tick
    */
  final case class EvcsState(
      evs: Map[UUID, EvModelWrapper],
      override val tick: Long,
  ) extends ModelState

  final case class EvcsRelevantData(
      tick: Long,
      arrivals: Seq[EvModelWrapper],
  ) extends OperationRelevantData

  trait ChargingStrategy {
    def determineChargingPowers(
        evs: Iterable[EvModelWrapper],
        currentTick: Long,
        chargingProps: EvcsChargingProperties,
    ): Map[UUID, Power]
  }

}
