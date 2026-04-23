/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.result.system.{
  EvResult,
  EvcsResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.config.RuntimeConfig.EvcsRuntimeConfig
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsOperatingPoint,
  EvcsState,
}
import edu.ie3.simona.model.participant.flex.ParticipantFlexModel
import edu.ie3.simona.model.participant.{ChargingHelper, ParticipantModel}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.service.Data.PrimaryData
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.Data.SecondaryData.*
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.quantities.QuantityUtils.{asMegaVar, asMegaWatt, asPu}
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toApparent
import edu.ie3.util.scala.quantities.{ApparentPower, ReactivePower}
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.energy.PowerConversions.PowerNumeric
import squants.energy.{Kilowatts, Watts}
import squants.time.Seconds
import squants.{Dimensionless, Energy, Power}
import tech.units.indriya.unit.Units.PERCENT

import java.time.ZonedDateTime
import java.util.UUID

class EvcsModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    val strategy: EvcsChargingStrategy,
    override val currentType: ElectricCurrentType,
    override val departureTargetSoc: Double,
    val chargingPoints: Int,
    val vehicle2grid: Boolean,
) extends ParticipantModel[
      EvcsOperatingPoint,
      EvcsState,
    ]
    with EvcsChargingProperties {

  override val flexModels
      : Map[FlexType, ParticipantFlexModel[EvcsOperatingPoint, EvcsState]] =
    Map(
      FlexType.PowerLimit -> EvcsPowerLimitFlexModel(this),
      FlexType.EnergyBoundaries -> EvcsEnergyBoundariesFlexModel(this),
    )

  override def determineState(
      lastState: EvcsState,
      operatingPoint: EvcsOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): EvcsState = {

    val updatedEvs = lastState.evs.map { ev =>
      operatingPoint.evOperatingPoints
        .get(ev.uuid)
        .map { chargingPower =>
          val currentEnergy = ChargingHelper.calcEnergy(
            ev.storedEnergy,
            chargingPower,
            lastState.tick,
            tick,
            ev.eStorage,
          )

          ev.copy(storedEnergy = currentEnergy)
        }
        .getOrElse(ev)
    }

    EvcsState(updatedEvs, tick)
  }

  override def handleInput(
      state: EvcsState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): EvcsState = {
    receivedData
      .collectFirst { case evData: ArrivingEvs =>
        evData
      }
      .map(newData =>
        state.copy(
          state.evs ++ newData.arrivals
        )
      )
      .getOrElse(state)
  }

  override def determineOperatingPoint(
      state: EvcsState
  ): (EvcsOperatingPoint, Option[Long]) = {
    // applicable evs can be charged, other evs cannot
    // since V2G only applies when Em-controlled we don't have to consider empty batteries
    val applicableEvs = state.evs.filter(!isFull(_))

    val chargingPowers =
      strategy.determineChargingPowers(applicableEvs, state.tick, this)

    val nextEvent = applicableEvs
      .flatMap { ev =>
        chargingPowers.get(ev.uuid).map((ev, _))
      }
      .flatMap { case (ev, power) =>
        determineChargingLimitEvent(
          ev,
          power,
          state.tick,
        )
      }
      .minOption

    (
      EvcsOperatingPoint(addMissingZeroPowerEntries(state.evs, chargingPowers)),
      nextEvent,
    )
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
        if !lastOp.contains(currentPower) then Some(currentPower)
        // create 0 kW results for EVs that are not charging anymore
        else if lastOp.isDefined && currentOp.isEmpty then Some(zeroKW)
        else None

      resultPower.map { activePower =>
        // EVs are assumed to have no reactive power
        val reactivePower = zeroKVAr

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
      if powerDifferent then
        Iterable(
          new EvcsResult(
            dateTime,
            uuid,
            complexPower.p.toMegawatts.asMegaWatt,
            complexPower.q.toMegavars.asMegaVar,
          )
        )
      else Iterable.empty

    evResults ++ evcsResult
  }

  override def createPrimaryDataResult(
      data: PrimaryData.PrimaryDataWithComplexPower[?],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new EvcsResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

  override def determineOperatingPoint(
      state: EvcsState,
      setPower: Power,
  ): EvcsOperatingPoint = {

    // applicable evs can be charged/discharged, other evs cannot
    val (applicableEvs, idleEvs) = state.evs.partition { ev =>
      if setPower == zeroKW then false
      else if setPower > zeroKW then !isFull(ev)
      else !isEmpty(ev)
    }

    val (forcedChargingEvs, regularChargingEvs) =
      if setPower > zeroKW then {
        applicableEvs.partition { ev =>
          requiresMaxCharging(ev, state.tick)
        }
      } else (Seq.empty, applicableEvs)

    val idleSchedules = idleEvs.map(_ -> zeroKW)

    // first, distribute power amongst EVs that
    // require charging to hit their target SOC
    val (forcedSchedules, remainingPower) =
      distributeChargingPower(state.tick, forcedChargingEvs, setPower)

    val (regularSchedules, _) =
      distributeChargingPower(state.tick, regularChargingEvs, remainingPower)

    val chargingPowers =
      (idleSchedules ++ forcedSchedules ++ regularSchedules).map {
        case (ev, power) =>
          ev.uuid -> power
      }.toMap

    EvcsOperatingPoint(
      addMissingZeroPowerEntries(state.evs, chargingPowers)
    )
  }

  /** Distributes some set power across given EVs, taking into consideration the
    * maximum charging power of EVs and charging station.
    *
    * @param currentTick
    *   The current tick.
    * @param evs
    *   The collection of EVs to assign charging power to.
    * @param setPower
    *   The remaining power to assign to given EVs.
    * @return
    *   A sequence of EV model and their charging power, as well as the
    *   remaining power that could not be assigned to given EVs.
    */
  private def distributeChargingPower(
      currentTick: Long,
      evs: Seq[EvModelWrapper],
      setPower: Power,
  ): (Seq[(EvModelWrapper, Power)], Power) = {

    if evs.isEmpty then return (evs.map(_ -> zeroKW), setPower)

    val tolerance =
      if evs.nonEmpty then calcPowerTolerance(evs.head)
      else Kilowatts(1e-9)

    if setPower.~=(zeroKW)(using tolerance) then {
      // No power left. Rest is not charging
      return (evs.map(_ -> zeroKW), zeroKW)
    }

    val proposedPower = setPower.divide(evs.size)

    val (exceedingPowerEvs, fittingPowerEvs) = evs.partition { ev =>
      if setPower > zeroKW then proposedPower > getMaxAvailableChargingPower(ev)
      else proposedPower < (getMaxAvailableChargingPower(ev) * -1)
    }

    if exceedingPowerEvs.isEmpty then {
      // end of recursion, rest of charging power fits to all

      val results = fittingPowerEvs.map(_ -> proposedPower)

      (results, zeroKW)
    } else {
      // not all evs can be charged with proposed power

      // charge all exceeded evs with their respective maximum power
      val maxChargedResults = exceedingPowerEvs.map { ev =>
        val maxPower = getMaxAvailableChargingPower(ev)
        val power =
          if setPower > zeroKW then maxPower
          else maxPower * -1

        ev -> power
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
    *   The EV to charge/discharge.
    * @param power
    *   The charging/discharging power.
    * @param currentTick
    *   The current simulation tick.
    * @return
    *   The tick at which the target is reached.
    */
  def determineChargingLimitEvent(
      ev: EvModelWrapper,
      power: Power,
      currentTick: Long,
  ): Option[Long] = {
    implicit val tolerance: Power = calcPowerTolerance(ev)

    val chargingEnergyTarget = () => ev.eStorage

    val dischargingEnergyTarget = () => zeroKWh

    ChargingHelper.calcNextEventTick(
      ev.storedEnergy,
      power,
      currentTick,
      chargingEnergyTarget,
      dischargingEnergyTarget,
    )
  }

  override def handleRequest(
      state: EvcsState,
      ctx: ActorContext[ParticipantAgent.Message],
      msg: DirectAgentRequest,
  ): EvcsState = msg match {
    case freeLotsRequest: EvFreeLotsRequest =>
      val stayingEvsCount =
        // freeLotsRequest.tick is the current tick
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

      if departingEvs.size != requestedEvs.size then {
        requestedEvs.foreach { requestedUuid =>
          if !departingEvs.exists(_.uuid == requestedUuid) then
            ctx.log.warn(
              s"EV $requestedUuid should depart from this station (according to external simulation), but has not been parked here."
            )
        }
      }

      departingEvsRequest.replyTo ! DepartingEvsResponse(uuid, departingEvs)

      state.copy(evs = stayingEvs)

  }

  /* HELPER METHODS */

  /** Adds zero power values for EVs that have not been assigned any charging
    * power yet.
    *
    * @param evs
    *   The complete set of EVs currently connected to the charging station.
    * @param chargingPowers
    *   The charging powers that have been determined, which might not contain
    *   values for all EVs.
    * @return
    *   A complete map of charging powers for all connected EVs.
    */
  private def addMissingZeroPowerEntries(
      evs: Seq[EvModelWrapper],
      chargingPowers: Map[UUID, Power],
  ): Map[UUID, Power] =
    evs.map { ev =>
      ev.uuid -> chargingPowers.getOrElse(ev.uuid, zeroKW)
    }.toMap

  /** @param ev
    *   the EV whose stored energy is to be checked
    * @return
    *   whether the given ev's stored energy is greater than the maximum charged
    *   energy allowed (minus a tolerance margin)
    */
  def isFull(ev: EvModelWrapper): Boolean =
    ev.storedEnergy >= (ev.eStorage - calcToleranceMargin(ev))

  /** @param ev
    *   The EV whose stored energy is to be checked.
    * @return
    *   Whether the given ev's energy storage is empty (plus a tolerance
    *   margin).
    */
  def isEmpty(ev: EvModelWrapper): Boolean =
    ev.storedEnergy <= calcToleranceMargin(ev)

  /** Determines whether given EV requires charging with maximum power to reach
    * (or come as close as possible to) the target SOC at departure. Returns
    * false if we can charge with anything else than maximum power and still
    * reach the target.
    *
    * @param ev
    *   The EV whose stored energy is to be checked.
    * @param tick
    *   The current tick.
    * @return
    *   Whether the EV is required to charge with maximum power.
    */
  def requiresMaxCharging(ev: EvModelWrapper, tick: Long): Boolean = {
    val maxPower = getMaxAvailableChargingPower(ev)
    val maxCharged =
      ev.storedEnergy + maxPower * ev.timeToDeparture(tick)

    val targetEnergy = ev.eStorage * departureTargetSoc
    val tolerance = calcToleranceMargin(ev)

    maxCharged < (targetEnergy + tolerance)
  }

  private def calcToleranceMargin(ev: EvModelWrapper): Energy =
    // since ticks are floored, there could be a difference
    // of a second worth of charging
    getMaxAvailableChargingPower(ev) * Seconds(1)

  /** Calculates a tolerance for power comparisons for a specific EV.
    *
    * Very small charging or discharging powers relative to the EV's battery
    * capacity can result in extremely large charging durations when computing
    * the next activation tick (e.g. time until fully charged or empty).
    *
    * Since simulation time is represented as Long values (ticks in seconds),
    * this can lead to overflows and therefore undefined behavior.
    *
    * To prevent this, powers below this tolerance are treated as zero. The
    * tolerance scales with the EV's battery capacity and corresponds roughly
    * to:
    *
    * 1 W per 1 GWh battery capacity
    *
    * @param ev
    *   The EV whose battery capacity is used to derive the tolerance.
    * @return
    *   A power threshold below which values are treated as zero.
    */
  private[evcs] def calcPowerTolerance(ev: EvModelWrapper): Power =
    ev.eStorage / Seconds(1) / 3.6e12
}

object EvcsModel {

  final case class EvcsOperatingPoint(evOperatingPoints: Map[UUID, Power])
      extends OperatingPoint {

    override val activePower: Power =
      evOperatingPoints.values.sum

    override val reactivePower: Option[ReactivePower] = None
  }

  object EvcsOperatingPoint {
    def zero: EvcsOperatingPoint = EvcsOperatingPoint(Map.empty)
  }

  final case class EvcsState(
      evs: Seq[EvModelWrapper],
      override val tick: Long,
  ) extends ModelState

  final case class Factory(
      input: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
  ) extends ParticipantModelFactory[EvcsState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.EvMovementService)

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): EvcsState = EvcsState(Seq.empty, tick)

    override def create(): EvcsModel =
      new EvcsModel(
        input.getUuid,
        input.getId,
        input.getType.getsRated.toApparent,
        input.getCosPhiRated,
        QControl(input.getqCharacteristics),
        EvcsChargingStrategy(modelConfig.chargingStrategy),
        input.getType.getElectricCurrentType,
        modelConfig.departureTargetSoc,
        input.getChargingPoints,
        input.getV2gSupport,
      )

  }

}
