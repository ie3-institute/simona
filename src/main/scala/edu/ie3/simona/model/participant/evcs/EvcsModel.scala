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
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState
}
import edu.ie3.simona.model.participant.evcs.uncontrolled.{
  ConstantPowerCharging,
  MaximumPowerCharging
}
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  FlexChangeIndicator,
  ModelState,
  SystemParticipant
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import squants.energy
import squants.energy.{KilowattHours, Kilowatts}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units.PERCENT

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Power
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
  * @param scalingFactor
  *   Scaling the output of the system
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
  */
final case class EvcsModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    simulationStartDate: ZonedDateTime,
    qControl: QControl,
    sRated: energy.Power,
    currentType: ElectricCurrentType,
    cosPhiRated: Double,
    chargingPoints: Int,
    locationType: EvcsLocationType,
    vehicle2grid: Boolean,
    strategy: ChargingStrategy.Value,
    lowestEvSoc: Double
) extends SystemParticipant[EvcsRelevantData, ApparentPower, EvcsState](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      (sRated * chargingPoints),
      cosPhiRated
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
    *   scheduling for charging the EVs
    */
  def calculateNewScheduling(
      data: EvcsRelevantData,
      evs: Set[EvModelWrapper]
  ): Map[EvModelWrapper, Option[ChargingSchedule]] = {
    if (
      locationType == EvcsLocationType.CHARGING_HUB_TOWN || locationType == EvcsLocationType.CHARGING_HUB_HIGHWAY
    ) {
      /* Cars at charging hubs always charge eagerly */
      chargeWithMaximumPower(
        data.tick,
        evs
      )
    } else
      scheduleByStrategy(
        strategy,
        data.tick,
        evs
      )
  }

  /** Determine the schedule by defined charging strategy
    *
    * @param strategy
    *   Chosen charging strategy
    * @param currentTick
    *   Current simulation time
    * @param evs
    *   Collection of currently apparent evs
    * @return
    *   A set of [[ChargingSchedule]]s
    */
  private def scheduleByStrategy(
      strategy: ChargingStrategy.Value,
      currentTick: Long,
      evs: Set[EvModelWrapper]
  ): Map[EvModelWrapper, Option[ChargingSchedule]] = strategy match {
    case ChargingStrategy.MAX_POWER =>
      chargeWithMaximumPower(
        currentTick,
        evs
      )
    case ChargingStrategy.CONSTANT_POWER =>
      chargeWithConstantPower(
        currentTick,
        evs
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
      currentTick: Long
  ): Set[EvModelWrapper] = if (state.schedule.nonEmpty) {
    state.evs
      .map(ev =>
        state
          .getSchedule(ev)
          .map {
            chargeEv(
              ev,
              _,
              state.tick,
              currentTick
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
      currentTick: Long
  ): EvModelWrapper = {
    /* Determine charged energy in the charging interval */
    val chargedEnergySinceLastScheduling =
      schedule.schedule.toSeq
        .filter { case ChargingSchedule.Entry(tickStart, tickStop, _) =>
          /* Filter for entries, that end after the last schedule application (that slice is not yet fully applied)
           * and that start before the current tick */
          tickStop > lastSchedulingTick && tickStart < currentTick
        }
        .sortBy(_.tickStart)
        .foldLeft(KilowattHours(0d)) {
          case (accumulatedEnergy, scheduleEntry) =>
            /* Only the timeframe from the start of last scheduling update and current tick must be considered */
            val trimmedEntry = trimScheduleEntry(
              scheduleEntry,
              lastSchedulingTick,
              currentTick
            )

            /* Determine the energy charged within this slice of the schedule and accumulate it */
            accumulatedEnergy + chargedEnergyInScheduleEntry(trimmedEntry)
        }
    /* Update EV with the charged energy during the charging interval */
    ev.copy(
      storedEnergy = ev.storedEnergy + chargedEnergySinceLastScheduling
    )
  }

  def createResults(
      lastState: EvcsState,
      currentTick: Long,
      voltageMagnitude: squants.Dimensionless
  ): (Iterable[EvResult], Iterable[EvcsResult]) = {

    val lastTick = lastState.tick

    val lastEvMap = lastState.evs.map(ev => ev.uuid -> ev).toMap

    val prefilteredSchedules = lastState.schedule.values.flatten
      .map { case schedule @ ChargingSchedule(_, entries) =>
        val filteredEntries = entries
          .filter { case ChargingSchedule.Entry(tickStart, tickStop, _) =>
            /* Filter for entries, that end after the last schedule application
               and that start before the current tick.
               Entries that end at lastTick are not included because schedule
               intervals are open at the right hand side.
               Entries that start at currentTick are not included because these
               will be calculated with the next state.
             */
            tickStop > lastTick && tickStart < currentTick
          }

        schedule.copy(
          schedule = filteredEntries
        )
      }

    val entriesByStartTick = prefilteredSchedules
      .flatMap { case ChargingSchedule(evUuid, schedule) =>
        schedule.unsorted
          .map { entry =>
            // trim down entries to the currently considered window of the charging schedule
            evUuid -> trimScheduleEntry(
              entry,
              lastTick,
              currentTick
            )
          }
      }
      .groupBy { case _ -> entry =>
        entry.tickStart
      }
      .to(SortedMap)

    val startAndStopTicks = prefilteredSchedules
      .flatMap { case ChargingSchedule(_, schedule) =>
        schedule.unsorted.flatMap {
          case ChargingSchedule.Entry(start, stop, _) =>
            Iterable(start, stop)
        }
      }
      .filter(tick => tick >= lastTick && tick < currentTick)
      .to(SortedSet)
      // the last tick needs to be included,
      // the current tick excluded
      .incl(lastTick)
      .excl(currentTick)

    // in order to create 0kW entries for EVs that do not
    // start charging right away at lastTick, create mock
    // schedule entries that end before lastTick
    val startingSchedules = lastEvMap.keys.map {
      _ -> ChargingSchedule.Entry(lastTick, lastTick, Kilowatts(0d))
    }

    val (currentEvs, currentSchedules, evResults, evcsResults) =
      startAndStopTicks.foldLeft(
        lastEvMap,
        startingSchedules,
        Seq.empty[EvResult],
        Seq.empty[EvcsResult]
      ) { case ((evMap, lastActiveEntries, evResults, evcsResults), tick) =>
        val time = tick.toDateTime(simulationStartDate)

        val (stillActive, endedEntries) = lastActiveEntries.partition {
          case (_, entry) =>
            entry.tickStop > tick
        }

        val newActiveEntries =
          entriesByStartTick.getOrElse(tick, Iterable.empty).toMap

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
                Kilowatts(0d),
                voltageMagnitude
              )
            }

        val (updatedEvMap, chargingEvResults) =
          newActiveEntries.foldLeft(evMap, Seq.empty[EvResult]) {
            case ((evMap, results), evUuid -> entry) =>
              val ev = evMap(evUuid)

              val result = createEvResult(
                ev,
                entry.tickStart,
                entry.chargingPower,
                voltageMagnitude
              )

              // update EV
              val newEvStoredEnergy = ev.storedEnergy +
                chargedEnergyInScheduleEntry(entry)
              val newEv = ev.copy(storedEnergy = newEvStoredEnergy)

              (
                evMap.updated(evUuid, newEv),
                results.appended(result)
              )
          }

        val currentActiveEntries = stillActive ++ newActiveEntries

        val evcsP = currentActiveEntries.foldLeft(Kilowatts(0d)) {
          case (powerSum, _ -> entry) =>
            powerSum + entry.chargingPower
        }

        val evcsQ = calculateReactivePower(
          evcsP,
          voltageMagnitude
        )

        val evcsResult = new EvcsResult(
          time,
          uuid,
          evcsP.toMegawatts.asMegaWatt,
          evcsQ.toMegavars.asMegaVar
        )

        (
          updatedEvMap,
          currentActiveEntries,
          evResults ++ chargingEvResults ++ noChargingEvResults,
          evcsResults :+ evcsResult
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
          Kilowatts(0d),
          voltageMagnitude
        )
      }

    (evResults ++ departingEvResults, evcsResults)
  }

  private def createEvResult(
      ev: EvModelWrapper,
      tick: Long,
      p: squants.Power,
      voltageMagnitude: squants.Dimensionless
  ) = {
    val q = calculateReactivePower(
      p,
      voltageMagnitude
    )
    val soc = (ev.storedEnergy / ev.eStorage).asPu
      .to(PERCENT)

    new EvResult(
      tick.toDateTime(simulationStartDate),
      ev.uuid,
      p.toMegawatts.asMegaWatt,
      q.toMegavars.asMegaVar,
      soc
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
      scheduleEntry: ChargingSchedule.Entry,
      lastSchedulingTick: Long,
      currentTick: Long
  ): ChargingSchedule.Entry =
    scheduleEntry.copy(
      tickStart = math.max(scheduleEntry.tickStart, lastSchedulingTick),
      tickStop = math.min(scheduleEntry.tickStop, currentTick)
    )

  /** Determine the energy, that has been charged during the schedule entry time
    * interval.
    *
    * @param scheduleEntry
    *   The schedule entry
    * @return
    *   The energy charged during the time interval of the schedule entry
    */
  private def chargedEnergyInScheduleEntry(
      scheduleEntry: ChargingSchedule.Entry
  ): squants.Energy =
    scheduleEntry.chargingPower * squants.time.Seconds(
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
  ): squants.Power = {
    val evPower = currentType match {
      case ElectricCurrentType.AC =>
        ev.sRatedAc
      case ElectricCurrentType.DC =>
        ev.sRatedDc
    }
    /* Limit the charging power to the minimum of ev's and evcs' permissible power */
    evPower.min(sRated)
  }

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
      voltage: squants.Dimensionless,
      modelState: EvcsState,
      data: EvcsRelevantData
  ): ApparentPower = ???

  /** Calculate the active power behaviour of the model
    *
    * @param modelState
    *   Current state of the model
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      modelState: EvcsState,
      data: EvcsRelevantData
  ): squants.Power =
    throw new NotImplementedError("Use calculatePowerAndEvSoc() instead.")

  override def determineFlexOptions(
      data: EvcsRelevantData,
      lastState: EvcsState
  ): FlexibilityMessage.ProvideFlexOptions = {

    val currentEvs = determineCurrentState(data, lastState)

    val preferredScheduling = calculateNewScheduling(data, currentEvs)

    val preferredPower =
      preferredScheduling.values.flatten.foldLeft(Kilowatts(0d)) {
        case (sum, ChargingSchedule(_, schedule)) =>
          val power =
            schedule
              .find { case ChargingSchedule.Entry(tickStart, tickStop, _) =>
                tickStart <= data.tick && tickStop > data.tick
              }
              .map(_.chargingPower)
              .getOrElse(Kilowatts(0d))
          sum + power
      }

    val (maxCharging, forcedCharging, maxDischarging) =
      preferredScheduling.foldLeft(
        (Kilowatts(0d), Kilowatts(0d), Kilowatts(0d))
      ) { case ((chargingSum, forcedSum, dischargingSum), (ev, _)) =>
        val maxPower = getMaxAvailableChargingPower(ev)

        val maxCharging =
          if (!isFull(ev))
            maxPower
          else
            Kilowatts(0d)

        val forcedCharging =
          if (isEmpty(ev) && !isInLowerMargin(ev))
            maxPower // TODO maybe use preferred power instead
          else
            Kilowatts(0d)

        val maxDischarging =
          if (!isEmpty(ev) && vehicle2grid)
            maxPower * -1
          else
            Kilowatts(0d)

        (
          chargingSum + maxCharging,
          forcedSum + forcedCharging,
          dischargingSum + maxDischarging
        )
      }

    // if we need to charge at least one EV, we cannot discharge any other
    val (adaptedMin, adaptedPreferred) =
      if (forcedCharging > Kilowatts(0d))
        (forcedCharging, preferredPower.max(forcedCharging))
      else
        (maxDischarging, preferredPower)

    ProvideMinMaxFlexOptions(
      uuid,
      adaptedPreferred,
      adaptedMin,
      maxCharging
    )
  }

  // TODO sometimes we issue too early nextTicks, since remaining power might be added to remaining non-full vehicles
  // (minor) TODO? if IssueNoControl is sent, there might be a different result than anticipated when calculating flex options (strat is not used)
  override def handleControlledPowerChange(
      data: EvcsRelevantData,
      lastState: EvcsState,
      setPower: squants.Power
  ): (EvcsState, FlexChangeIndicator) = {
    val currentEvs = determineCurrentState(data, lastState)

    if (setPower == Kilowatts(0d))
      return (
        EvcsState(
          evs = currentEvs,
          schedule = currentEvs.map(_ -> None).toMap,
          tick = data.tick
        ),
        FlexChangeIndicator()
      )

    // applicable evs can be charged/discharged, other evs cannot
    val (applicableEvs, otherEvs) = currentEvs.partition { ev =>
      if (setPower > Kilowatts(0d))
        !isFull(ev)
      else
        !isEmpty(ev)
    }

    val (forcedChargingEvs, regularChargingEvs) =
      if (setPower > Kilowatts(0d))
        // lower margin is excluded since charging is not required here anymore
        applicableEvs.partition { ev =>
          isEmpty(ev) && !isInLowerMargin(ev)
        }
      else
        (Set.empty[EvModelWrapper], applicableEvs)

    val (forcedSchedules, remainingPower) =
      createScheduleWithSetPower(data.tick, forcedChargingEvs, setPower)

    val (regularSchedules, _) =
      createScheduleWithSetPower(data.tick, regularChargingEvs, remainingPower)

    val combinedSchedules = forcedSchedules ++ regularSchedules

    val schedulesOnly = combinedSchedules.flatMap { case (_, scheduleOpt) =>
      scheduleOpt
    }

    val scheduleAtNextActivation = schedulesOnly
      .map { case (_, _, scheduleAtNext) => scheduleAtNext }
      .reduceOption(_ || _)
      .getOrElse(false)

    val nextScheduledTick = schedulesOnly.map { case (_, endTick, _) =>
      endTick
    }.minOption

    val allSchedules = combinedSchedules.map {
      case (ev, Some((schedule, _, _))) =>
        ev -> Some(schedule)
      case (ev, None) => ev -> None
    }.toMap ++ otherEvs.map(_ -> None).toMap

    (
      EvcsState(
        evs = allSchedules.keys.toSet,
        schedule = allSchedules,
        tick = data.tick
      ),
      FlexChangeIndicator(
        scheduleAtNextActivation,
        nextScheduledTick
      )
    )
  }

  /** @param currentTick
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
      evs: Set[EvModelWrapper],
      setPower: squants.Power
  ): (
      Set[(EvModelWrapper, Option[(ChargingSchedule, Long, Boolean)])],
      squants.Power
  ) = {

    if (evs.isEmpty) return (Set.empty, setPower)

    if (setPower.~=(Kilowatts(0d))(Kilowatts(1e-6))) {
      // No power left. Rest is not charging
      return (evs.map { _ -> None }, Kilowatts(0d))
    }

    val proposedPower = setPower.divide(evs.size)

    val (exceedingPowerEvs, fittingPowerEvs) = evs.partition { ev =>
      if (setPower > Kilowatts(0d))
        proposedPower > getMaxAvailableChargingPower(ev)
      else
        proposedPower < (getMaxAvailableChargingPower(ev) * -1)
    }

    if (exceedingPowerEvs.isEmpty) {
      // end of recursion, rest of charging power fits to all

      val results = fittingPowerEvs.map { ev =>
        val chargingTicks = calculateChargingDuration(ev, proposedPower)
        val endTick = Math.min(currentTick + chargingTicks, ev.departureTick)

        (
          ev,
          Some(
            ChargingSchedule(
              ev,
              Seq(ChargingSchedule.Entry(currentTick, endTick, proposedPower))
            ),
            endTick,
            isFull(ev) || isEmpty(ev) || isInLowerMargin(ev)
          )
        )
      }: Set[(EvModelWrapper, Option[(ChargingSchedule, Long, Boolean)])]

      (results, Kilowatts(0d))
    } else {
      // not all evs can be charged with proposed power

      // charge all exceeded evs with their respective maximum power
      val maxCharged = exceedingPowerEvs.map { ev =>
        val maxPower = getMaxAvailableChargingPower(ev)
        val power =
          if (setPower > Kilowatts(0d))
            maxPower
          else
            maxPower * (-1)

        val chargingTicks = calculateChargingDuration(ev, power)
        val endTick = Math.min(currentTick + chargingTicks, ev.departureTick)

        (ev, power, endTick)
      }

      // sum up allocated power
      val chargingPowerSum = maxCharged.foldLeft(Kilowatts(0d)) {
        case (powerSum, (_, chargingPower, _)) =>
          powerSum + chargingPower
      }

      val remainingAfterAllocation = setPower - chargingPowerSum

      // go into the next recursion step with the remaining power
      val (nextIterationResults, remainingAfterRecursion) =
        createScheduleWithSetPower(
          currentTick,
          fittingPowerEvs,
          remainingAfterAllocation
        )

      val combinedResults = maxCharged.map { case (ev, power, endTick) =>
        (
          ev,
          Some(
            ChargingSchedule(
              ev,
              Seq(ChargingSchedule.Entry(currentTick, endTick, power))
            ),
            endTick,
            isFull(ev) || isEmpty(ev) || isInLowerMargin(ev)
          )
        )
      } ++ nextIterationResults

      (combinedResults, remainingAfterRecursion)
    }

  }

  private def calculateChargingDuration(
      ev: EvModelWrapper,
      power: squants.Power
  ): Long = {
    val timeUntilFullOrEmpty =
      if (power > Kilowatts(0d)) {

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

  private def calcToleranceMargin(ev: EvModelWrapper): squants.Energy =
    getMaxAvailableChargingPower(ev) * squants.time.Seconds(1)

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
  def determineCurrentState(
      data: EvcsRelevantData,
      lastState: EvcsState
  ): Set[EvModelWrapper] = {
    // TODO use Seq instead of Set as return value

    // if last state is from before current tick, determine current state
    val currentEVs =
      if (lastState.tick < data.tick)
        applySchedule(lastState, data.tick)
      else
        lastState.evs

    validateArrivals(
      lastState.evs,
      data.arrivals,
      chargingPoints
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
      lastEvs: Set[EvModelWrapper],
      departures: Seq[UUID]
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
      lastEvs: Set[EvModelWrapper],
      arrivals: Seq[EvModelWrapper],
      chargingPoints: Int
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

  /** Class that holds all relevant data for an Evcs model calculation
    *
    * @param tick
    *   The current tick
    * @param arrivals
    *   The evs arriving at the current tick
    * @param voltages
    *   Nodal voltage per known time instant
    */
  final case class EvcsRelevantData(
      tick: Long,
      arrivals: Seq[EvModelWrapper]
  ) extends CalcRelevantData

  /** Class that represents the state of the charging station at a given point
    * in time
    *
    * @param evs
    *   EVs that are staying at the charging station
    * @param schedule
    *   the schedule determining when to load which EVs with which power
    * @param tick
    *   The tick that the data has been calculated for
    */
  final case class EvcsState(
      evs: Set[EvModelWrapper],
      schedule: Map[EvModelWrapper, Option[ChargingSchedule]],
      tick: Long
  ) extends ModelState {
    def getSchedule(ev: EvModelWrapper): Option[ChargingSchedule] =
      schedule.getOrElse(ev, None)
  }

  def apply(
      inputModel: EvcsInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      chargingStrategy: String,
      lowestEvSoc: Double
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
      simulationStartDate,
      QControl(inputModel.getqCharacteristics),
      inputModel.getType.getsRated,
      inputModel.getType.getElectricCurrentType,
      inputModel.getCosPhiRated,
      inputModel.getChargingPoints,
      inputModel.getLocationType,
      inputModel.getV2gSupport,
      ChargingStrategy(chargingStrategy),
      lowestEvSoc
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
    * @param simulationStartDate
    *   The start date of the simulation
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
    * @param chargingStrategy
    *   The charging strategy to use
    * @return
    *   the enabled EvcsModel
    */
  def apply(
      uuid: UUID,
      id: String,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      qControl: QControl,
      sRated: ComparableQuantity[Power],
      currentType: ElectricCurrentType,
      cosPhiRated: Double,
      chargingPoints: Int,
      locationType: EvcsLocationType,
      vehicle2grid: Boolean,
      chargingStrategy: ChargingStrategy.Value,
      lowestEvSoc: Double
  ): EvcsModel = {
    val model = new EvcsModel(
      uuid,
      id,
      operationInterval,
      scalingFactor,
      simulationStartDate,
      qControl,
      energy.Kilowatts(sRated.to(KILOWATT).getValue.doubleValue),
      currentType,
      cosPhiRated,
      chargingPoints,
      locationType,
      vehicle2grid,
      chargingStrategy,
      lowestEvSoc
    )

    model.enable()

    model
  }
}
