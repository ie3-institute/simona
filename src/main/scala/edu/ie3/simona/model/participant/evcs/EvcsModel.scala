/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.result.system.{EvResult, EvcsResult}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvcsModel._
import edu.ie3.simona.model.participant.evcs.uncontrolled.{
  ConstantPowerCharging,
  MaximumPowerCharging,
}
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  FlexChangeIndicator,
  ModelState,
  SystemParticipant,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.Kilowatts
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.energy.Kilowatts
import squants.time.Seconds
import squants.{Dimensionless, Energy, Power}
import tech.units.indriya.unit.Units.PERCENT

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedMap
import scala.collection.immutable.SortedSet

/** EV charging station model
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param simulationStartDate
  *   The start date of the simulation
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power per charging point
  * @param cosPhiRated
  *   Rated power factor
  * @param chargingPoints
  *   Number of charging points available at this charging station
  * @param locationType
  *   The location type
  * @param strategy
  *   Strategy to follow in oder to determine the charging habits
  * @param lowestEvSoc
  *   The lowest SOC possible for EV batteries (inverse of max dod)
  */
final case class EvcsModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    simulationStartDate: ZonedDateTime,
    qControl: QControl,
    sRated: ApparentPower,
    currentType: ElectricCurrentType,
    cosPhiRated: Double,
    chargingPoints: Int,
    locationType: EvcsLocationType,
    vehicle2grid: Boolean,
    strategy: ChargingStrategy.Value,
    lowestEvSoc: Double,
) extends SystemParticipant[EvcsRelevantData, ComplexPower, EvcsState](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated * chargingPoints,
      cosPhiRated,
    )
    with LazyLogging
    with MaximumPowerCharging
    with ConstantPowerCharging {

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. The scheduling depends on the chosen
    * strategy.
    *
    * @param data
    *   data including the current EVs parked at the charging station
    * @return
    *   Charging schedule for charging given EVs
    */
  def calculateNewScheduling(
      data: EvcsRelevantData,
      evs: Seq[EvModelWrapper],
  ): ScheduleMap = {
    if (
      locationType == EvcsLocationType.CHARGING_HUB_TOWN || locationType == EvcsLocationType.CHARGING_HUB_HIGHWAY
    ) {
      /* Cars at charging hubs always charge eagerly */
      chargeWithMaximumPower(
        data.tick,
        evs,
      )
    } else
      scheduleByStrategy(
        strategy,
        data.tick,
        evs,
      )
  }

  /** Determine the schedule by defined charging strategy
    *
    * @param strategy
    *   Chosen charging strategy
    * @param currentTick
    *   Current simulation time
    * @param evs
    *   Collection of currently parked evs
    * @return
    *   Charging schedule for charging given EVs
    */
  private def scheduleByStrategy(
      strategy: ChargingStrategy.Value,
      currentTick: Long,
      evs: Seq[EvModelWrapper],
  ): ScheduleMap = strategy match {
    case ChargingStrategy.MAX_POWER =>
      chargeWithMaximumPower(
        currentTick,
        evs,
      )
    case ChargingStrategy.CONSTANT_POWER =>
      chargeWithConstantPower(
        currentTick,
        evs,
      )
    case ChargingStrategy.GRID_ORIENTED =>
      throw new NotImplementedError(
        "Grid oriented strategy currently not implemented"
      )
    case ChargingStrategy.MARKET_ORIENTED =>
      throw new NotImplementedError(
        "Market oriented strategy currently not implemented"
      )

  }

  /** Update EVs for the timeframe since the last scheduling
    *
    * @param state
    *   the last state
    * @param currentTick
    *   current tick
    * @return
    *   updated EVs
    */
  def applySchedule(
      state: EvcsState,
      currentTick: Long,
  ): Seq[EvModelWrapper] = if (state.schedule.nonEmpty) {
    state.evs
      .map(ev =>
        state.schedule
          .get(ev.uuid)
          .map {
            chargeEv(
              ev,
              _,
              state.tick,
              currentTick,
            )
          }
          .getOrElse(ev)
      )
  } else {
    logger.debug(
      "There are EVs parked at this charging station, but there was no scheduling since the last" +
        "update. Probably the EVs already finished charging."
    )
    state.evs
  }

  /** Charge the given EV under consideration a applicable schedule
    *
    * @param ev
    *   Electric vehicle to charge
    * @param schedule
    *   Schedule for this car
    * @param lastSchedulingTick
    *   Last tick, that a schedule has been processed
    * @param currentTick
    *   Current time in Simulation
    * @return
    *   The charged EV
    */
  private def chargeEv(
      ev: EvModelWrapper,
      schedule: ChargingSchedule,
      lastSchedulingTick: Long,
      currentTick: Long,
  ): EvModelWrapper = {
    /* Determine charged energy in the charging interval */
    val chargedEnergySinceLastScheduling =
      schedule.toSeq
        .filter { case ScheduleEntry(tickStart, tickStop, _) =>
          /* Filter for entries, that end after the last schedule application (that slice is not yet fully applied)
           * and that start before the current tick */
          tickStop > lastSchedulingTick && tickStart < currentTick
        }
        .sortBy(_.tickStart)
        .foldLeft(zeroKWH) { case (accumulatedEnergy, scheduleEntry) =>
          /* Only the timeframe from the start of last scheduling update and current tick must be considered */
          val trimmedEntry = trimScheduleEntry(
            scheduleEntry,
            lastSchedulingTick,
            currentTick,
          )

          /* Determine the energy charged within this slice of the schedule and accumulate it */
          accumulatedEnergy + calcChargedEnergy(trimmedEntry)
        }
    /* Update EV with the charged energy during the charging interval */
    ev.copy(
      storedEnergy = ev.storedEnergy + chargedEnergySinceLastScheduling
    )
  }

  /** Create [[EvResult]]s and [[EvcsResult]]s for all EVs that have been
    * connected to this charging station for some time in the time interval from
    * (and including) the last tick to (but excluding) the current tick.
    * Schedule entries that start at current tick are excluded, but will be
    * considered in the next interval.
    *
    * As an exception to the rule, for EVs that are departing at current tick,
    * an [[EvResult]] with 0 kW starting at current tick is created.
    * @param lastState
    *   The last EVCS state
    * @param currentTick
    *   The current tick
    * @param voltageMagnitude
    *   The voltage magnitude used for reactive power calculation
    * @return
    *   EV and EVCS results
    */
  def createResults(
      lastState: EvcsState,
      currentTick: Long,
      voltageMagnitude: Dimensionless,
  ): (Iterable[EvResult], Iterable[EvcsResult]) = {

    val lastTick = lastState.tick

    val lastEvMap = lastState.evs.map(ev => ev.uuid -> ev).toMap

    val prefilteredSchedules = lastState.schedule.view.mapValues {
      _.filter { case ScheduleEntry(tickStart, tickStop, _) =>
        /* Filter for entries, that end after the last schedule application
               and that start before the current tick.
               Entries that end at lastTick are not included because schedule
               intervals are open at the right hand side.
               Entries that start at currentTick are not included because these
               will be calculated with the next state.
         */
        tickStop > lastTick && tickStart < currentTick
      }
    }

    val entriesByStartTick = prefilteredSchedules.toSeq
      .flatMap { case (evUuid, entries) =>
        // unsorted for speedier execution
        entries.unsorted.map { entry =>
          // trim down entries to the currently
          // considered window of the charging schedule
          evUuid -> trimScheduleEntry(
            entry,
            lastTick,
            currentTick,
          )
        }
      }
      .groupBy { case (_, entry) =>
        entry.tickStart
      }
      .to(SortedMap)

    val startAndStopTicks = prefilteredSchedules.values
      .flatMap {
        _.unsorted.flatMap { case ScheduleEntry(start, stop, _) =>
          Iterable(start, stop)
        }
      }
      .filter(tick => tick >= lastTick && tick < currentTick)
      .to(SortedSet)
      // the last tick needs to be present
      .incl(lastTick)

    // in order to create 0kW entries for EVs that do not
    // start charging right away at lastTick, create mock
    // schedule entries that end before lastTick
    val startingSchedules = lastEvMap.keys.map {
      _ -> ScheduleEntry(lastTick, lastTick, zeroKW)
    }

    val (currentEvs, currentSchedules, evResults, evcsResults) =
      startAndStopTicks.foldLeft(
        lastEvMap,
        startingSchedules,
        Seq.empty[EvResult],
        Seq.empty[EvcsResult],
      ) { case ((evMap, lastActiveEntries, evResults, evcsResults), tick) =>
        val time = tick.toDateTime(simulationStartDate)

        // separate into those entries that are still active
        // and those that have ended before or at tick
        val (stillActive, endedEntries) = lastActiveEntries.partition {
          case (_, entry) =>
            entry.tickStop > tick
        }
        // entries that become active with tick
        val newActiveEntries =
          entriesByStartTick.getOrElse(tick, Iterable.empty).toMap

        // for those entries that ended with tick and that
        // do not have a directly connected entry after that,
        // add 0 kW entries
        val noChargingEvResults =
          endedEntries
            .filterNot { case evUuid -> _ =>
              newActiveEntries.contains(evUuid)
            }
            .map { case evUuid -> _ =>
              val ev = evMap(evUuid)

              createEvResult(
                ev,
                tick,
                zeroKW,
                voltageMagnitude,
              )
            }

        // create result and update EVs with the
        // newly active entries
        val (updatedEvMap, chargingEvResults) =
          newActiveEntries.foldLeft(evMap, Seq.empty[EvResult]) {
            case ((evMap, results), evUuid -> entry) =>
              val ev = evMap(evUuid)

              val result = createEvResult(
                ev,
                entry.tickStart,
                entry.chargingPower,
                voltageMagnitude,
              )

              // update EV
              val newEvStoredEnergy = ev.storedEnergy +
                calcChargedEnergy(entry)
              val newEv = ev.copy(storedEnergy = newEvStoredEnergy)

              (
                evMap.updated(evUuid, newEv),
                results.appended(result),
              )
          }

        val currentActiveEntries = stillActive ++ newActiveEntries

        // create the EVCS result with all currently active entries
        val evcsP = currentActiveEntries.foldLeft(zeroKW) {
          case (powerSum, _ -> entry) =>
            powerSum + entry.chargingPower
        }
        val evcsQ = calculateReactivePower(
          evcsP,
          voltageMagnitude,
        )
        val evcsResult = new EvcsResult(
          time,
          uuid,
          evcsP.toMegawatts.asMegaWatt,
          evcsQ.toMegavars.asMegaVar,
        )

        (
          updatedEvMap,
          currentActiveEntries,
          evResults ++ chargingEvResults ++ noChargingEvResults,
          evcsResults :+ evcsResult,
        )
      }

    // special case: also add EVs that are departing at current tick
    // because they won't be included when the next results are created
    val departingEvResults = currentSchedules
      .map { case evUuid -> _ =>
        currentEvs(evUuid)
      }
      .filter {
        // only take those that are departing now
        _.departureTick == currentTick
      }
      .map {
        createEvResult(
          _,
          currentTick,
          zeroKW,
          voltageMagnitude,
        )
      }

    (evResults ++ departingEvResults, evcsResults)
  }

  private def createEvResult(
      ev: EvModelWrapper,
      tick: Long,
      p: Power,
      voltageMagnitude: Dimensionless,
  ) = {
    val q = calculateReactivePower(
      p,
      voltageMagnitude,
    )
    val soc = (ev.storedEnergy / ev.eStorage).asPu
      .to(PERCENT)

    new EvResult(
      tick.toDateTime(simulationStartDate),
      ev.uuid,
      p.toMegawatts.asMegaWatt,
      q.toMegavars.asMegaVar,
      soc,
    )
  }

  /** Limit the actual charging window. The beginning is determined by the
    * latest tick of either the schedule start or the last scheduled tick. The
    * end is determined by the earlier tick of either the schedule end or the
    * current tick.
    *
    * @param scheduleEntry
    *   Section of a scheduled
    * @param lastSchedulingTick
    *   The last scheduling tick
    * @param currentTick
    *   The current tick
    * @return
    *   A trimmed version of given scheduleEntry
    */
  private def trimScheduleEntry(
      scheduleEntry: ScheduleEntry,
      lastSchedulingTick: Long,
      currentTick: Long,
  ): ScheduleEntry =
    scheduleEntry.copy(
      tickStart = math.max(scheduleEntry.tickStart, lastSchedulingTick),
      tickStop = math.min(scheduleEntry.tickStop, currentTick),
    )

  /** Determine the energy, that has been charged during the schedule entry time
    * interval.
    *
    * @param scheduleEntry
    *   The schedule entry
    * @return
    *   The energy charged during the time interval of the schedule entry
    */
  private def calcChargedEnergy(
      scheduleEntry: ScheduleEntry
  ): Energy =
    scheduleEntry.chargingPower * Seconds(
      scheduleEntry.tickStop - scheduleEntry.tickStart
    )

  /** Returns the maximum available charging power for an EV, which depends on
    * ev and charging station limits for AC and DC current
    *
    * @param ev
    *   ev for which the max charging power should be returned
    * @return
    *   maximum charging power for the EV at this charging station
    */
  def getMaxAvailableChargingPower(
      ev: EvModelWrapper
  ): Power = {
    val evPower = currentType match {
      case ElectricCurrentType.AC =>
        ev.sRatedAc.toPower(1.0)
      case ElectricCurrentType.DC =>
        ev.sRatedDc
    }
    /* Limit the charging power to the minimum of ev's and evcs' permissible power */
    evPower.min(sRated.toPower(1.0))
  }

  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      modelState: EvcsState,
      data: EvcsRelevantData,
  ): ComplexPower =
    throw new NotImplementedError(
      "Use calculateNewScheduling() or chargeEv() instead."
    )

  override protected def calculateActivePower(
      modelState: EvcsState,
      data: EvcsRelevantData,
  ): Power =
    throw new NotImplementedError(
      "Use calculateNewScheduling() or chargeEv() instead."
    )

  override def determineFlexOptions(
      data: EvcsRelevantData,
      lastState: EvcsState,
  ): FlexibilityMessage.ProvideFlexOptions = {

    val currentEvs = determineCurrentEvs(data, lastState)

    val preferredScheduling = calculateNewScheduling(data, currentEvs)

    val (maxCharging, preferredPower, forcedCharging, maxDischarging) =
      currentEvs.foldLeft(
        (zeroKW, zeroKW, zeroKW, zeroKW)
      ) {
        case (
              (chargingSum, preferredSum, forcedSum, dischargingSum),
              ev,
            ) =>
          val maxPower = getMaxAvailableChargingPower(ev)

          val preferred = preferredScheduling
            .get(ev.uuid)
            .flatMap {
              _.find { case ScheduleEntry(tickStart, tickStop, _) =>
                tickStart <= data.tick && tickStop > data.tick
              }.map(_.chargingPower)
            }

          val maxCharging =
            if (!isFull(ev))
              maxPower
            else
              zeroKW

          val forced =
            if (isEmpty(ev) && !isInLowerMargin(ev))
              preferred.getOrElse(maxPower)
            else
              zeroKW

          val maxDischarging =
            if (!isEmpty(ev) && vehicle2grid)
              maxPower * -1
            else
              zeroKW

          (
            chargingSum + maxCharging,
            preferredSum + preferred.getOrElse(zeroKW),
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

  // TODO less activations could be possible if after departure of vehicles, the additional power might be added to remaining non-full vehicles
  // (minor) TODO? if IssueNoControl is sent, there might be a different result than anticipated when calculating flex options (strat is not used)
  override def handleControlledPowerChange(
      data: EvcsRelevantData,
      lastState: EvcsState,
      setPower: Power,
  ): (EvcsState, FlexChangeIndicator) = {
    val currentEvs = determineCurrentEvs(data, lastState)

    if (setPower == zeroKW)
      return (
        EvcsState(
          evs = currentEvs,
          schedule = Map.empty,
          tick = data.tick,
        ),
        FlexChangeIndicator(),
      )

    // applicable evs can be charged/discharged, other evs cannot
    val applicableEvs = currentEvs.filter { ev =>
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
      createScheduleWithSetPower(data.tick, forcedChargingEvs, setPower)

    val (regularSchedules, _) =
      createScheduleWithSetPower(data.tick, regularChargingEvs, remainingPower)

    val combinedSchedules = forcedSchedules ++ regularSchedules

    val allSchedules = combinedSchedules.map { case (ev, (schedule, _, _)) =>
      ev -> schedule
    }.toMap

    (
      EvcsState(
        evs = currentEvs,
        schedule = allSchedules,
        tick = data.tick,
      ),
      aggregateFlexChange(combinedSchedules),
    )
  }

  /** Aggregates a flex change indicator from controlled schedule calcuation
    * result
    *
    * @param combinedSchedules
    *   The schedule calculation results
    * @return
    *   The aggregated flex change indicator
    */
  private def aggregateFlexChange(
      combinedSchedules: Seq[(UUID, (ChargingSchedule, Long, Boolean))]
  ): FlexChangeIndicator = {
    val schedulesOnly =
      combinedSchedules.map { case (_, schedule) => schedule }

    val scheduleAtNextActivation = schedulesOnly
      .map { case (_, _, scheduleAtNext) => scheduleAtNext }
      .reduceOption(_ || _)
      .getOrElse(false)

    val nextScheduledTick = schedulesOnly.map { case (_, endTick, _) =>
      endTick
    }.minOption

    FlexChangeIndicator(
      scheduleAtNextActivation,
      nextScheduledTick,
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
      Seq[(UUID, (ChargingSchedule, Long, Boolean))],
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
            SortedSet(
              ScheduleEntry(currentTick, endTick, proposedPower)
            ),
            endTick,
            isFull(ev) || isEmpty(ev) || isInLowerMargin(ev),
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
            maxPower * -1

        val chargingTicks = calcFlexOptionsChange(ev, power)
        val endTick = Math.min(currentTick + chargingTicks, ev.departureTick)

        (ev, power, endTick)
      }

      val maxChargedResults = maxCharged.map { case (ev, power, endTick) =>
        (
          ev.uuid,
          (
            SortedSet(ScheduleEntry(currentTick, endTick, power)),
            endTick,
            isFull(ev) || isEmpty(ev) || isInLowerMargin(ev),
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
        (ev.storedEnergy - (ev.eStorage * lowestEvSoc)) / (power * -1)

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

  /** Determines the current state of staying and arriving EVs.
    *
    * @param data
    *   the EvcsRelevantData containing arriving EVs, the current tick etc.
    * @param lastState
    *   the last known state of the EVCS. Could be the state at the current
    *   tick.
    * @return
    *   The EVs currently parked at the EVCS, including the arriving EVs
    */
  def determineCurrentEvs(
      data: EvcsRelevantData,
      lastState: EvcsState,
  ): Seq[EvModelWrapper] = {

    // If last state is outdated, determine
    // current state for already parked EVs
    val currentEVs =
      if (lastState.tick < data.tick)
        applySchedule(lastState, data.tick)
      else
        lastState.evs

    validateArrivals(
      lastState.evs,
      data.arrivals,
      chargingPoints,
    )

    currentEVs ++ data.arrivals
  }

  /** Checks whether requested departing EVs are consistent with currently
    * connected EVs. Only logs warnings, does not throw exceptions.
    *
    * @param lastEvs
    *   EVs of the last tick
    * @param departures
    *   Departing EVs at the current tick
    */
  def validateDepartures(
      lastEvs: Seq[EvModelWrapper],
      departures: Seq[UUID],
  ): Unit = {
    departures.foreach { ev =>
      if (!lastEvs.exists(_.uuid == ev))
        logger.warn(
          s"EV $ev should depart from this station (according to external simulation), but has not been parked here."
        )
    }

  }

  /** Checks whether provided arriving EVs are consistent with charging station
    * specifications and currently connected EVs. Only logs warnings, does not
    * throw exceptions.
    *
    * @param lastEvs
    *   EVs of the last tick
    * @param arrivals
    *   Arriving EVs at the current tick
    * @param chargingPoints
    *   max number of charging points available at this CS
    */
  def validateArrivals(
      lastEvs: Seq[EvModelWrapper],
      arrivals: Seq[EvModelWrapper],
      chargingPoints: Int,
  ): Unit = {

    arrivals.foreach { ev =>
      if (lastEvs.exists(_.uuid == ev.uuid))
        logger.warn(
          s"EV ${ev.id} should arrive at this station (according to external simulation), but is already parked here."
        )
    }

    val newCount = lastEvs.size +
      arrivals.count { ev =>
        !lastEvs.exists(_.uuid == ev.uuid)
      }

    if (newCount > chargingPoints)
      logger.warn(
        "More EVs are parking at this station than physically possible."
      )
  }
}

object EvcsModel {

  /** A charging schedule for a single EV, consisting of multiple schedule
    * entries that are (primarily) sorted by start tick
    */
  private type ChargingSchedule = SortedSet[ScheduleEntry]

  /** A schedule map consisting of charging schedules for multiple EVs,
    * referenced by their model UUID
    */
  type ScheduleMap = Map[UUID, ChargingSchedule]

  /** Class that holds all relevant data for an EVCS model calculation
    *
    * @param tick
    *   The current tick
    * @param arrivals
    *   The evs arriving at the current tick
    */
  final case class EvcsRelevantData(
      tick: Long,
      arrivals: Seq[EvModelWrapper],
  ) extends CalcRelevantData

  /** Class that represents the state of the charging station (including
    * schedules for future charging) at a given point in time
    *
    * @param evs
    *   EVs that are parked at the charging station
    * @param schedule
    *   The schedule determining when to load which EVs with which power, as a
    *   map EV model UUID -> charging schedule
    * @param tick
    *   The tick that the data has been calculated for
    */
  final case class EvcsState(
      evs: Seq[EvModelWrapper],
      schedule: ScheduleMap,
      tick: Long,
  ) extends ModelState

  /** Schedule entry specifying a time interval in which the EV should be
    * charged/discharged with some given power
    *
    * @param tickStart
    *   start of charging interval
    * @param tickStop
    *   end of charging interval
    * @param chargingPower
    *   charging power for the charging interval
    */
  final case class ScheduleEntry(
      tickStart: Long,
      tickStop: Long,
      chargingPower: squants.Power,
  ) extends Ordered[ScheduleEntry] {
    override def compare(that: ScheduleEntry): Int = {
      val startComp = tickStart.compare(that.tickStart)
      if (startComp != 0)
        startComp
      else {
        // important for checking equality: consider other fields as well
        val stopComp = tickStop.compare(that.tickStop)
        if (stopComp != 0)
          stopComp
        else
          chargingPower.compareTo(that.chargingPower)
      }
    }
  }

  /** Default factory method to create an EvcsModel instance.
    *
    * @param inputModel
    *   The EVCS input model providing parameters
    * @param scalingFactor
    *   The scaling factor of the power output
    * @param simulationStartDate
    *   The start date of the simulation
    * @param simulationEndDate
    *   The end date of the simulation
    * @param chargingStrategy
    *   The charging strategy to use
    * @param lowestEvSoc
    *   The lowest SOC possible for EV batteries (inverse of max dod)
    * @return
    *   The enabled EvcsModel
    */
  def apply(
      inputModel: EvcsInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      chargingStrategy: String,
      lowestEvSoc: Double,
  ): EvcsModel = {

    val scaledInput = inputModel.copy().scale(scalingFactor).build()

    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        scaledInput.getOperationTime,
      )

    val model = EvcsModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      simulationStartDate,
      QControl(scaledInput.getqCharacteristics),
      Kilovoltamperes(
        scaledInput.getType.getsRated.to(KILOVOLTAMPERE).getValue.doubleValue
      ),
      scaledInput.getType.getElectricCurrentType,
      scaledInput.getCosPhiRated,
      scaledInput.getChargingPoints,
      scaledInput.getLocationType,
      scaledInput.getV2gSupport,
      ChargingStrategy(chargingStrategy),
      lowestEvSoc,
    )

    model.enable()

    model
  }

}
