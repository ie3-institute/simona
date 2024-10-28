/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.result.system.{
  EvResult,
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
  ModelChangeIndicator,
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
import squants.energy.{Kilowatts, Watts}
import squants.time.Seconds
import squants.{Dimensionless, Energy, Power}
import tech.units.indriya.unit.Units.PERCENT

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
      strategy.determineChargingPowers(state.evs, state.tick, this)

    val nextEvent = state.evs
      .flatMap { ev =>
        chargingPowers.get(ev.uuid).map((ev, _))
      }
      .flatMap { case (ev, power) =>
        determineNextEvent(ev, power)
      }
      .minOption

    (EvcsOperatingPoint(chargingPowers), nextEvent)
  }

  override def zeroPowerOperatingPoint: EvcsOperatingPoint =
    EvcsOperatingPoint.zero

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

    val updatedEvs = lastState.evs.map { ev =>
      operatingPoint.evOperatingPoints
        .get(ev.uuid)
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
      lastOperatingPoint: Option[EvcsOperatingPoint],
      currentOperatingPoint: EvcsOperatingPoint,
      complexPower: PrimaryData.ApparentPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] = {
    val evResults = state.evs.flatMap { ev =>
      val lastOp = lastOperatingPoint.flatMap(_.evOperatingPoints.get(ev.uuid))
      val currentOp = currentOperatingPoint.evOperatingPoints.get(ev.uuid)

      val currentPower = currentOp.getOrElse(zeroKW)

      val resultPower =
        // only take results that are different from last time
        if (!lastOp.contains(currentPower))
          Some(currentPower)
        // create 0 kW results for EVs that are not charging anymore
        else if (lastOp.isDefined && currentOp.isEmpty)
          Some(zeroKW)
        else
          None

      resultPower.map { activePower =>
        // assume that reactive power is proportional to active power
        val reactivePower = complexPower.q * (activePower / complexPower.p)

        val soc = (ev.storedEnergy / ev.eStorage).asPu
          .to(PERCENT)

        new EvResult(
          dateTime,
          ev.uuid,
          activePower.toMegawatts.asMegaWatt,
          reactivePower.toMegavars.asMegaVar,
          soc,
        )
      }
    }

    val evcsResult = new EvcsResult(
      dateTime,
      uuid,
      complexPower.p.toMegawatts.asMegaWatt,
      complexPower.q.toMegavars.asMegaVar,
    )

    evResults ++ Iterable(evcsResult)
  }

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

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable(
      ServiceType.EvMovementService
    )

  override def createRelevantData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
      simulationTime: ZonedDateTime,
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
      strategy.determineChargingPowers(state.evs, state.tick, this)

    val (maxCharging, preferredPower, forcedCharging, maxDischarging) =
      state.evs.foldLeft(
        (zeroKW, zeroKW, zeroKW, zeroKW)
      ) {
        case (
              (chargingSum, preferredSum, forcedSum, dischargingSum),
              ev,
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
      relevantData: EvcsRelevantData,
      flexOptions: FlexibilityMessage.ProvideFlexOptions,
      setPower: Power,
  ): (EvcsOperatingPoint, ModelChangeIndicator) = {
    if (setPower == zeroKW)
      return (
        EvcsOperatingPoint(Map.empty),
        ModelChangeIndicator(),
      )

    // applicable evs can be charged/discharged, other evs cannot
    val applicableEvs = state.evs.filter { ev =>
      if (setPower > zeroKW)
        !isFull(ev)
      else
        !isEmpty(ev)
    }

    val (forcedChargingEvs, regularChargingEvs) =
      if (setPower > zeroKW)
        // lower margin is excluded since charging is not required here anymore
        applicableEvs.partition { ev =>
          isEmpty(ev) && !isInLowerMargin(ev)
        }
      else
        (Seq.empty, applicableEvs)

    val (forcedSchedules, remainingPower) =
      createScheduleWithSetPower(state.tick, forcedChargingEvs, setPower)

    val (regularSchedules, _) =
      createScheduleWithSetPower(state.tick, regularChargingEvs, remainingPower)

    val combinedSchedules = forcedSchedules ++ regularSchedules

    val allSchedules = combinedSchedules.map { case (ev, (power, _)) =>
      ev -> power
    }.toMap

    val aggregatedChangeIndicator = combinedSchedules
      .map { case (_, (_, indicator)) => indicator }
      .foldLeft(ModelChangeIndicator()) {
        case (aggregateIndicator, otherIndicator) =>
          aggregateIndicator | otherIndicator
      }

    (
      EvcsOperatingPoint(allSchedules),
      aggregatedChangeIndicator,
    )
  }

  /** Distributes some set power value across given EVs, taking into
    * consideration the maximum charging power of EVs and the charging station
    *
    * @param currentTick
    *   The current tick
    * @param evs
    *   The collection of EVs to assign charging power to
    * @param setPower
    *   The remaining power to assign to given EVs
    * @return
    *   A set of EV model and possibly charging schedule and activation
    *   indicators, as well as the remaining power that could not be assigned to
    *   given EVs
    */
  private def createScheduleWithSetPower(
      currentTick: Long,
      evs: Seq[EvModelWrapper],
      setPower: Power,
  ): (
      Seq[(UUID, (Power, ModelChangeIndicator))],
      Power,
  ) = {

    if (evs.isEmpty) return (Seq.empty, setPower)

    if (setPower.~=(zeroKW)(Kilowatts(1e-6))) {
      // No power left. Rest is not charging
      return (Seq.empty, zeroKW)
    }

    val proposedPower = setPower.divide(evs.size)

    val (exceedingPowerEvs, fittingPowerEvs) = evs.partition { ev =>
      if (setPower > zeroKW)
        proposedPower > getMaxAvailableChargingPower(ev)
      else
        proposedPower < (getMaxAvailableChargingPower(ev) * -1)
    }

    if (exceedingPowerEvs.isEmpty) {
      // end of recursion, rest of charging power fits to all

      val results = fittingPowerEvs.map { ev =>
        val chargingTicks = calcFlexOptionsChange(ev, proposedPower)
        val endTick = Math.min(currentTick + chargingTicks, ev.departureTick)

        (
          ev.uuid,
          (
            proposedPower,
            ModelChangeIndicator(
              changesAtNextActivation =
                isFull(ev) || isEmpty(ev) || isInLowerMargin(ev),
              changesAtTick = Some(endTick),
            ),
          ),
        )
      }

      (results, zeroKW)
    } else {
      // not all evs can be charged with proposed power

      // charge all exceeded evs with their respective maximum power
      val maxCharged = exceedingPowerEvs.map { ev =>
        val maxPower = getMaxAvailableChargingPower(ev)
        val power =
          if (setPower > zeroKW)
            maxPower
          else
            maxPower * (-1)

        val chargingTicks = calcFlexOptionsChange(ev, power)
        val endTick = Math.min(currentTick + chargingTicks, ev.departureTick)

        (ev, power, endTick)
      }

      val maxChargedResults = maxCharged.map { case (ev, power, endTick) =>
        (
          ev.uuid,
          (
            power,
            ModelChangeIndicator(
              changesAtNextActivation =
                isFull(ev) || isEmpty(ev) || isInLowerMargin(ev),
              changesAtTick = Some(endTick),
            ),
          ),
        )
      }

      // sum up allocated power
      val chargingPowerSum = maxCharged.foldLeft(zeroKW) {
        case (powerSum, (_, chargingPower, _)) =>
          powerSum + chargingPower
      }

      val remainingAfterAllocation = setPower - chargingPowerSum

      // go into the next recursion step with the remaining power
      val (nextIterationResults, remainingAfterRecursion) =
        createScheduleWithSetPower(
          currentTick,
          fittingPowerEvs,
          remainingAfterAllocation,
        )

      val combinedResults = maxChargedResults ++ nextIterationResults

      (combinedResults, remainingAfterRecursion)
    }

  }

  /** Calculates the duration (in ticks) until the flex options will change
    * next, which could be the battery being fully charged or discharged or the
    * minimum SOC requirement being reached
    *
    * @param ev
    *   The EV to charge/discharge
    * @param power
    *   The charging/discharging power
    * @return
    *   The tick at which flex options will change
    */
  private def calcFlexOptionsChange(
      ev: EvModelWrapper,
      power: Power,
  ): Long = {
    val timeUntilFullOrEmpty =
      if (power > zeroKW) {

        // if we're below lowest SOC, flex options will change at that point
        val targetEnergy =
          if (isEmpty(ev) && !isInLowerMargin(ev))
            ev.eStorage * lowestEvSoc
          else
            ev.eStorage

        (targetEnergy - ev.storedEnergy) / power
      } else
        (ev.storedEnergy - (ev.eStorage * lowestEvSoc)) / (power * (-1))

    Math.round(timeUntilFullOrEmpty.toSeconds)
  }

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

  def getInitialState: EvcsState = EvcsState(Seq.empty, -1)
}

object EvcsModel {

  final case class EvcsOperatingPoint(evOperatingPoints: Map[UUID, Power])
      extends OperatingPoint {

    override val activePower: Power =
      evOperatingPoints.values.reduceOption(_ + _).getOrElse(zeroKW)

    override val reactivePower: Option[ReactivePower] = None
  }

  object EvcsOperatingPoint {
    def zero: EvcsOperatingPoint = EvcsOperatingPoint(Map.empty)
  }

  final case class EvcsState(
      evs: Seq[EvModelWrapper],
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
