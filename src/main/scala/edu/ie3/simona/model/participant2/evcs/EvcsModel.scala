/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.evcs

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.result.system.{
  EvResult,
  EvcsResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvModelWrapper
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelInput,
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.model.participant2.evcs.EvcsModel.{
  EvcsOperatingPoint,
  EvcsState,
}
import edu.ie3.simona.model.participant2.{ChargingHelper, ParticipantModel}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits.KILOVOLTAMPERE
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  Kilovoltamperes,
  ReactivePower,
}
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.energy.{Kilowatts, Watts}
import squants.time.Seconds
import squants.{Energy, Power}
import tech.units.indriya.unit.Units.PERCENT

import java.time.ZonedDateTime
import java.util.UUID

class EvcsModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    strategy: EvcsChargingStrategy,
    override val currentType: ElectricCurrentType,
    override val lowestEvSoc: Double,
    chargingPoints: Int,
    vehicle2grid: Boolean,
) extends ParticipantModel[
      EvcsOperatingPoint,
      EvcsState,
    ]
    with EvcsChargingProperties {

  override val initialState: ModelInput => EvcsState = { input =>
    EvcsState(getArrivals(input.receivedData), input.currentTick)
  }

  override def determineState(
      lastState: EvcsState,
      operatingPoint: EvcsOperatingPoint,
      input: ModelInput,
  ): EvcsState = {

    val updatedEvs = lastState.evs.map { ev =>
      operatingPoint.evOperatingPoints
        .get(ev.uuid)
        .map { chargingPower =>
          val currentEnergy = ChargingHelper.calcEnergy(
            ev.storedEnergy,
            chargingPower,
            lastState.tick,
            input.currentTick,
            ev.eStorage,
          )

          ev.copy(storedEnergy = currentEnergy)
        }
        .getOrElse(ev)
    }

    val arrivals = getArrivals(input.receivedData)

    EvcsState(updatedEvs ++ arrivals, input.currentTick)
  }

  private def getArrivals(receivedData: Seq[Data]): Seq[EvModelWrapper] = {
    receivedData
      .collectFirst { case evData: ArrivingEvs =>
        evData.arrivals
      }
      .getOrElse(Seq.empty)
  }

  override def determineOperatingPoint(
      state: EvcsState
  ): (EvcsOperatingPoint, Option[Long]) = {
    val chargingPowers =
      strategy.determineChargingPowers(state.evs, state.tick, this)

    val nextEvent = state.evs
      .flatMap { ev =>
        chargingPowers.get(ev.uuid).map((ev, _))
      }
      .flatMap { case (ev, power) =>
        determineNextEvent(
          ev,
          power,
          state.tick,
        )
      }
      .minOption

    (EvcsOperatingPoint(chargingPowers), nextEvent)
  }

  override def zeroPowerOperatingPoint: EvcsOperatingPoint =
    EvcsOperatingPoint.zero

  override def createResults(
      state: EvcsState,
      lastOperatingPoint: Option[EvcsOperatingPoint],
      currentOperatingPoint: EvcsOperatingPoint,
      complexPower: ComplexPower,
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

    val powerDifferent = lastOperatingPoint.forall(
      _.activePower != complexPower.p
    )

    val evcsResult =
      if (powerDifferent)
        Iterable(
          new EvcsResult(
            dateTime,
            uuid,
            complexPower.p.toMegawatts.asMegaWatt,
            complexPower.q.toMegavars.asMegaVar,
          )
        )
      else
        Iterable.empty

    evResults ++ evcsResult
  }

  override def createPrimaryDataResult(
      data: PrimaryData.PrimaryDataWithComplexPower[_],
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

  override def determineFlexOptions(
      state: EvcsState
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

          val preferredPower = preferredPowers.get(ev.uuid)

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

  override def determineOperatingPoint(
      state: EvcsState,
      setPower: Power,
  ): (EvcsOperatingPoint, OperationChangeIndicator) = {
    if (setPower == zeroKW)
      return (
        EvcsOperatingPoint(Map.empty),
        OperationChangeIndicator(),
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
      distributeChargingPower(state.tick, forcedChargingEvs, setPower)

    val (regularSchedules, _) =
      distributeChargingPower(state.tick, regularChargingEvs, remainingPower)

    val combinedSchedules = forcedSchedules ++ regularSchedules

    // preparing results
    val allSchedules = combinedSchedules.map { case (ev, power) =>
      ev.uuid -> power
    }.toMap

    val aggregateIndicator = combinedSchedules
      .map { case (ev, chargingPower) =>
        val endTick = determineNextEvent(ev, chargingPower, state.tick)
          .map(math.min(_, ev.departureTick))
          .getOrElse(ev.departureTick)

        OperationChangeIndicator(
          changesAtNextActivation =
            isFull(ev) || isEmpty(ev) || isInLowerMargin(ev),
          changesAtTick = Some(endTick),
        )
      }
      .foldLeft(OperationChangeIndicator()) {
        case (aggregate, otherIndicator) =>
          aggregate | otherIndicator
      }

    (
      EvcsOperatingPoint(allSchedules),
      aggregateIndicator,
    )
  }

  /** Distributes some set power across given EVs, taking into consideration the
    * maximum charging power of EVs and charging station
    *
    * @param currentTick
    *   The current tick
    * @param evs
    *   The collection of EVs to assign charging power to
    * @param setPower
    *   The remaining power to assign to given EVs
    * @return
    *   A sequence of EV model and their charging power, as well as the
    *   remaining power that could not be assigned to given EVs
    */
  private def distributeChargingPower(
      currentTick: Long,
      evs: Seq[EvModelWrapper],
      setPower: Power,
  ): (Seq[(EvModelWrapper, Power)], Power) = {

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
        (ev, proposedPower)
      }

      (results, zeroKW)
    } else {
      // not all evs can be charged with proposed power

      // charge all exceeded evs with their respective maximum power
      val maxChargedResults = exceedingPowerEvs.map { ev =>
        val maxPower = getMaxAvailableChargingPower(ev)
        val power =
          if (setPower > zeroKW)
            maxPower
          else
            maxPower * -1

        (ev, power)
      }

      // sum up allocated power
      val chargingPowerSum = maxChargedResults.foldLeft(zeroKW) {
        case (powerSum, (_, chargingPower)) =>
          powerSum + chargingPower
      }

      val remainingAfterAllocation = setPower - chargingPowerSum

      // go into the next recursion step with the remaining power
      val (nextIterationResults, remainingAfterRecursion) =
        distributeChargingPower(
          currentTick,
          fittingPowerEvs,
          remainingAfterAllocation,
        )

      val combinedResults = maxChargedResults ++ nextIterationResults

      (combinedResults, remainingAfterRecursion)
    }
  }

  /** Calculates the tick at which the target energy (e.g. full on charging or
    * empty on discharging) is reached.
    *
    * @param ev
    *   The EV to charge/discharge
    * @param power
    *   The charging/discharging power
    * @param currentTick
    *   The current simulation tick
    * @return
    *   The tick wat which the target is reached
    */
  private def determineNextEvent(
      ev: EvModelWrapper,
      power: Power,
      currentTick: Long,
  ): Option[Long] = {
    // TODO adapt like in StorageModel: dependent tolerance
    implicit val tolerance: Power = Watts(1e-3)

    val chargingEnergyTarget = () =>
      if (isEmpty(ev) && !isInLowerMargin(ev))
        ev.eStorage * lowestEvSoc
      else
        ev.eStorage

    val dischargingEnergyTarget = () => ev.eStorage * lowestEvSoc

    ChargingHelper.calcNextEventTick(
      ev.storedEnergy,
      power,
      currentTick,
      chargingEnergyTarget,
      dischargingEnergyTarget,
    )
  }

  /** Handling requests that are not part of the standard participant protocol
    *
    * @param state
    *   The current state
    * @param ctx
    *   The actor context that can be used to send replies
    * @param msg
    *   The received request
    * @return
    *   An updated state, or the same state provided as parameter
    */
  override def handleRequest(
      state: EvcsState,
      ctx: ActorContext[ParticipantAgent.Request],
      msg: ParticipantRequest,
  ): EvcsState = msg match {
    case freeLotsRequest: EvFreeLotsRequest =>
      val stayingEvsCount =
        state.evs.count(_.departureTick > freeLotsRequest.tick)

      freeLotsRequest.replyTo ! FreeLotsResponse(
        uuid,
        chargingPoints - stayingEvsCount,
      )

      state

    case departingEvsRequest: DepartingEvsRequest =>
      // create a set for faster containment checking
      val requestedEvs = departingEvsRequest.departingEvs.toSet

      val (departingEvs, stayingEvs) = state.evs.partition { ev =>
        requestedEvs.contains(ev.uuid)
      }

      if (departingEvs.size != requestedEvs.size) {
        requestedEvs.foreach { requestedUuid =>
          if (!departingEvs.exists(_.uuid == requestedUuid))
            ctx.log.warn(
              s"EV $requestedUuid should depart from this station (according to external simulation), but has not been parked here."
            )
        }
      }

      departingEvsRequest.replyTo ! DepartingEvsResponse(uuid, departingEvs)

      state.copy(evs = stayingEvs)

  }

  /* HELPER METHODS */

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

  def apply(
      input: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
  ): EvcsModel =
    new EvcsModel(
      input.getUuid,
      input.getId,
      Kilovoltamperes(
        input.getType.getsRated.to(KILOVOLTAMPERE).getValue.doubleValue
      ),
      input.getCosPhiRated,
      QControl(input.getqCharacteristics),
      EvcsChargingStrategy(modelConfig.chargingStrategy),
      input.getType.getElectricCurrentType,
      modelConfig.lowestEvSoc,
      input.getChargingPoints,
      input.getV2gSupport,
    )

}
