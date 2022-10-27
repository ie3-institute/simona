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
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState
}
import edu.ie3.simona.model.participant.evcs.gridoriented.GridOrientedCharging
import edu.ie3.simona.model.participant.evcs.gridoriented.GridOrientedCurrentPrice.calculateCurrentPriceGridOriented
import edu.ie3.simona.model.participant.evcs.marketoriented.MarketOrientedCharging
import edu.ie3.simona.model.participant.evcs.marketoriented.MarketOrientedCurrentPrice.calculateCurrentPriceMarketOriented
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
import edu.ie3.simona.ontology.messages.FlexibilityMessage
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.service.market.StaticMarketSource
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.{RichQuantity, RichQuantityDouble}
import edu.ie3.util.quantities.interfaces.Currency
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{PERCENT, SECOND}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Energy, Power, Time}
import scala.annotation.tailrec
import scala.collection.SortedMap
import scala.collection.immutable.SortedSet
import scala.collection.parallel.CollectionConverters._
import scala.util.{Failure, Success}

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
    sRated: ComparableQuantity[Power],
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
      sRated.multiply(chargingPoints),
      cosPhiRated
    )
    with LazyLogging
    with MaximumPowerCharging
    with ConstantPowerCharging
    with GridOrientedCharging
    with MarketOrientedCharging {

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
      evs: Set[EvModel]
  ): Map[EvModel, Option[ChargingSchedule]] = {
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
        simulationStartDate,
        evs,
        data.voltages
      )
  }

  /** Determine the schedule by defined charging strategy
    *
    * @param strategy
    *   Chosen charging strategy
    * @param currentTick
    *   Current simulation time
    * @param simulationStartDate
    *   The simulation start time
    * @param evs
    *   Collection of currently apparent evs
    * @param voltages
    *   Mapping from simulation time to nodal voltage
    * @return
    *   A set of [[ChargingSchedule]]s
    */
  private def scheduleByStrategy(
      strategy: ChargingStrategy.Value,
      currentTick: Long,
      simulationStartDate: ZonedDateTime,
      evs: Set[EvModel],
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]]
  ): Map[EvModel, Option[ChargingSchedule]] = strategy match {
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
      chargeGridOriented(
        currentTick,
        simulationStartDate,
        evs,
        voltages
      )
    case ChargingStrategy.MARKET_ORIENTED =>
      chargeMarketOriented(
        currentTick,
        simulationStartDate,
        evs
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
  ): Set[EvModel] = if (state.schedule.nonEmpty) {
    /* Apply schedules asynchronously */
    state.evs.par
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
      .seq
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
      ev: EvModel,
      schedule: ChargingSchedule,
      lastSchedulingTick: Long,
      currentTick: Long
  ): EvModel = {
    /* Determine charged energy in the charging interval */
    val chargedEnergySinceLastScheduling =
      schedule.schedule.toSeq
        .filter { case ChargingSchedule.Entry(tickStart, tickStop, _) =>
          /* Filter for entries, that end after the last schedule application (that slice is not yet fully applied)
           * and that start before the current tick */
          tickStop > lastSchedulingTick && tickStart < currentTick
        }
        .sortBy(_.tickStart)
        .foldLeft(0d.asKiloWattHour) {
          case (accumulatedEnergy, scheduleEntry) =>
            /* Only the timeframe from the start of last scheduling update and current tick must be considered */
            val trimmedEntry = trimScheduleEntry(
              scheduleEntry,
              lastSchedulingTick,
              currentTick
            )

            /* Determine the energy charged within this slice of the schedule and accumulate it */
            accumulatedEnergy.add(
              chargedEnergyInScheduleSlice(trimmedEntry)
            )
        }
    /* Update EV with the charged energy during the charging interval */
    ev.copyWith(
      ev.getStoredEnergy.add(chargedEnergySinceLastScheduling)
    )
  }

  def createResults(
      lastState: EvcsState,
      currentTick: Long,
      voltageMagnitude: ComparableQuantity[Dimensionless]
  ): (Iterable[EvResult], Iterable[EvcsResult]) = {

    val lastTick = lastState.tick

    val lastEvMap = lastState.evs.map(ev => ev.getUuid -> ev).toMap

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
      _ -> ChargingSchedule.Entry(lastTick, lastTick, 0d.asKiloWatt)
    }

    val (_, _, evResults, evcsResults) =
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
                0d.asKiloWatt,
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
              val newEvStoredEnergy = ev.getStoredEnergy.add(
                chargedEnergyInScheduleSlice(entry)
              )
              val newEv = ev.copyWith(newEvStoredEnergy)

              (
                evMap.updated(evUuid, newEv),
                results.appended(result)
              )
          }

        val currentActiveEntries = stillActive ++ newActiveEntries

        val evcsP = currentActiveEntries.foldLeft(0d.asKiloWatt) {
          case (powerSum, _ -> entry) =>
            powerSum.add(entry.chargingPower)
        }

        val evcsQ = calculateReactivePower(
          evcsP,
          voltageMagnitude
        )

        val evcsResult = new EvcsResult(
          time,
          uuid,
          evcsP,
          evcsQ
        )

        (
          updatedEvMap,
          currentActiveEntries,
          evResults ++ chargingEvResults ++ noChargingEvResults,
          evcsResults :+ evcsResult
        )
      }

    (evResults, evcsResults)
  }

  private def createEvResult(
      ev: EvModel,
      tick: Long,
      p: ComparableQuantity[Power],
      voltageMagnitude: ComparableQuantity[Dimensionless]
  ) = {
    val q = calculateReactivePower(
      p,
      voltageMagnitude
    )
    val soc = ev.getStoredEnergy
      .divide(ev.getEStorage)
      .asType(classOf[Dimensionless])
      .to(PERCENT)

    new EvResult(
      tick.toDateTime(simulationStartDate),
      ev.getUuid,
      p,
      q,
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
    *   Definition of the schedule slice
    * @return
    *   The energy charged during this slice
    */
  private def chargedEnergyInScheduleSlice(
      scheduleEntry: ChargingSchedule.Entry
  ): ComparableQuantity[Energy] =
    scheduleEntry.chargingPower
      .multiply(
        Quantities.getQuantity(
          (scheduleEntry.tickStop - scheduleEntry.tickStart).toDouble,
          SECOND
        )
      )
      .asType(classOf[Energy])
      .to(KILOWATTHOUR)

  /** Calculate the charging costs for a schedule entry. The schedule entry is
    * defined by its start and end time and the charging power. The energy price
    * might change within this time window. A recursive function is used to
    * calculate the total costs from start to end considering price changes.
    * @param startTime
    *   the start time of the schedule entry
    * @param stopTime
    *   the end time of the schedule entry
    * @param power
    *   the charging power for the schedule entry
    * @param costs
    *   the costs for the schedule entry, which are summed up while recursion
    * @return
    *   the total costs for the schedule entry
    */
  @tailrec
  private def getChargingCostsForScheduleEntry(
      startTime: ZonedDateTime,
      stopTime: ZonedDateTime,
      power: ComparableQuantity[Power],
      costs: ComparableQuantity[Currency]
  ): ComparableQuantity[Currency] = {

    // FIXME: This is super hacky! The price table only has entries for 2025!!!
    val marketTimeStart = startTime.withYear(2025)
    val marketTimeEnd = stopTime.withYear(2025)

    val nextFullHour =
      startTime.withMinute(0).withSecond(0).withNano(0).plusHours(1L)

    if (nextFullHour.isBefore(marketTimeEnd)) {

      val updatedCosts = costs.add(
        Quantities
          .getQuantity(
            marketTimeStart.until(nextFullHour, ChronoUnit.SECONDS),
            SECOND
          )
          .multiply(power)
          .asType(classOf[Energy])
          .to(KILOWATTHOUR)
          .multiply(
            StaticMarketSource
              .price(marketTimeStart)
              .map(_.to(EURO_PER_KILOWATTHOUR)) match {
              case Success(value)     => value
              case Failure(exception) => throw exception
            }
          )
          .asType(classOf[Currency])
          .to(EURO)
      )

      getChargingCostsForScheduleEntry(
        nextFullHour,
        marketTimeEnd,
        power,
        updatedCosts
      )

    } else {
      val duration = Quantities
        .getQuantity(
          marketTimeStart.until(marketTimeEnd, ChronoUnit.SECONDS),
          SECOND
        )
      val chargedEnergy =
        duration.multiply(power).asType(classOf[Energy]).to(KILOWATTHOUR)
      val marketPrice =
        StaticMarketSource
          .price(marketTimeStart)
          .map(_.to(EURO_PER_KILOWATTHOUR)) match {
          case Success(value)     => value
          case Failure(exception) => throw exception
        }
      val chargingPrice = chargedEnergy
        .multiply(marketPrice)
        .asType(classOf[Currency])
        .to(EURO)
      costs.add(chargingPrice)
    }

  }

  /** Calculate a current price signal to communicate to the evs deciding on a
    * charging session. A high price could influence evs to charge somewhere
    * else or not charge at all, dependent on the implementation in the mobility
    * simulation. The price can be determined using different mechanisms. In a
    * grid-oriented mechanism with the goal to limit the grid impact, the
    * knowledge of the currently parked evs and their scheduling and the
    * predicted node voltages for the future are used. In a market-oriented
    * mechanism, the price is determined using the energy price prediction. Only
    * public charging stations can have a dynamic price signal. The price signal
    * is currently designed to be a value between 0 and 1 to allow comparison
    * also for different mechanism in combination.
    *
    * @param currentTick
    *   current tick of the request
    * @param state
    *   the evcs state data including the current parked evs and scheduling
    * @return
    *   the current price signal of the evcs
    */
  def calculateCurrentPriceSignalToCommunicateToEvs(
      currentTick: Long,
      state: EvcsState
  ): Option[Double] =
    // Only EvcsLocationType != Home and Work have prices
    this.locationType match {
      case EvcsLocationType.HOME | EvcsLocationType.WORK =>
        /* only public charging stations have dynamic charging prices */
        None

      case publicLocationType =>
        val lengthOfRelevantIntervalInSeconds: Int =
          if (
            publicLocationType == EvcsLocationType.CUSTOMER_PARKING || publicLocationType == EvcsLocationType.STREET
          )
            7200 // 2 hours
          else 1800 // 30 minutes

        strategy match {
          case ChargingStrategy.GRID_ORIENTED =>
            calculateCurrentPriceGridOriented(
              this,
              currentTick,
              lengthOfRelevantIntervalInSeconds,
              state
            )
          case ChargingStrategy.MARKET_ORIENTED =>
            calculateCurrentPriceMarketOriented(
              currentTick,
              simulationStartDate,
              lengthOfRelevantIntervalInSeconds
            )
          case _ => None
        }
    }

  /** Return all ticks included in a schedule
    * @param schedule
    *   schedule including schedule entries with start and stop ticks
    * @return
    *   all ticks in the schedule
    */
  def getAllTicksOfSchedule(
      schedule: Set[ChargingSchedule.Entry]
  ): Set[Long] = schedule.flatMap(entry => Seq(entry.tickStart, entry.tickStop))

  /** Returns the maximum available charging power for an EV, which depends on
    * ev and charging station limits for AC and DC current
    *
    * @param ev
    *   ev for which the max charging power should be returned
    * @return
    *   maximum charging power for the EV at this charging station
    */
  def getMaxAvailableChargingPower(
      ev: EvModel
  ): ComparableQuantity[Power] = {
    val evPower = currentType match {
      case ElectricCurrentType.AC =>
        ev.getSRatedAC
      case ElectricCurrentType.DC =>
        ev.getSRatedDC
    }
    /* Limit the charging power to the minimum of ev's and evcs' permissible power */
    evPower.min(sRated).to(PowerSystemUnits.KILOWATT)
  }

  /** Calculate the power behaviour based on the given data.
    *
    * @param tick
    *   Regarded instant in simulation
    * @param voltage
    *   Nodal voltage magnitude
    * @param maybeModelState
    *   Current state of the model
    * @param data
    *   Further needed, secondary data
    * @return
    *   A tuple of active and reactive power
    */
  override def calculatePower(
      tick: Long,
      voltage: ComparableQuantity[Dimensionless],
      modelState: EvcsState,
      data: EvcsRelevantData
  ): ApparentPower = ???

  /** Calculate the active power behaviour of the model
    *
    * @param maybeModelState
    *   Optional model state
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      modelState: EvcsState,
      data: EvcsRelevantData
  ): ComparableQuantity[Power] =
    throw new NotImplementedError("Use calculatePowerAndEvSoc() instead.")

  override def determineFlexOptions(
      data: EvcsRelevantData,
      lastState: EvcsState
  ): FlexibilityMessage.ProvideFlexOptions = {

    val currentEvs = determineCurrentState(data, lastState)

    val preferredScheduling = calculateNewScheduling(data, currentEvs)

    val preferredPower = preferredScheduling.values.flatten.foldLeft(zeroKW) {
      case (sum, ChargingSchedule(_, schedule)) =>
        val power =
          schedule
            .find { case ChargingSchedule.Entry(tickStart, tickStop, _) =>
              tickStart <= data.tick && tickStop > data.tick
            }
            .map(_.chargingPower)
            .getOrElse(0d.asKiloWatt)
        sum.add(power)
    }

    val (maxCharging, maxDischarging) =
      preferredScheduling.foldLeft((zeroKW, zeroKW)) {
        case ((chargingSum, dischargingSum), (ev, _)) =>
          val maxPower = getMaxAvailableChargingPower(ev)

          val maxCharging =
            if (!isFull(ev))
              maxPower
            else
              zeroKW

          val maxDischarging =
            if (!isEmpty(ev) && vehicle2grid)
              maxPower.multiply(-1)
            else
              zeroKW

          (chargingSum.add(maxCharging), dischargingSum.add(maxDischarging))
      }

    ProvideMinMaxFlexOptions(
      uuid,
      preferredPower,
      maxDischarging,
      maxCharging
    )
  }

  // TODO sometimes we issue too early nextTicks, since remaining power might be added to remaining non-full vehicles
  // (minor) TODO? if IssueNoControl is sent, there might be a different result than anticipated when calculating flex options (strat is not used)
  override def handleControlledPowerChange(
      data: EvcsRelevantData,
      lastState: EvcsState,
      setPower: ComparableQuantity[Power]
  ): (EvcsState, FlexChangeIndicator) = {
    val currentEvs = determineCurrentState(data, lastState)

    if (QuantityUtil.isEquivalentAbs(zeroKW, setPower, 0))
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
      if (setPower.isGreaterThan(zeroKW))
        !isFull(ev)
      else
        !isEmpty(ev)
    }

    val chargingSchedule =
      createScheduleWithSetPower(data.tick, applicableEvs, setPower)

    val scheduleAtNextActivation = chargingSchedule
      .map { case (_, _, _, scheduleAtNext) =>
        scheduleAtNext
      }
      .reduceOption(_ || _)
      .getOrElse(false)

    val nextScheduledTick = chargingSchedule.map { case (_, _, endTick, _) =>
      endTick
    }.minOption

    val allSchedules = chargingSchedule.map { case (ev, schedule, _, _) =>
      ev -> Some(schedule)
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

  private def createScheduleWithSetPower(
      currentTick: Long,
      evs: Set[EvModel],
      setPower: ComparableQuantity[Power]
  ): Set[(EvModel, ChargingSchedule, Long, Boolean)] = {

    if (evs.isEmpty) return Set.empty

    val proposedPower = setPower.divide(evs.size)

    val (exceedingPowerEvs, fittingPowerEvs) = evs.partition { ev =>
      if (setPower.isGreaterThan(zeroKW))
        proposedPower.isGreaterThan(getMaxAvailableChargingPower(ev))
      else
        proposedPower.isLessThan(getMaxAvailableChargingPower(ev).multiply(-1))
    }

    if (exceedingPowerEvs.isEmpty) {
      // end of recursion, rest of charging power fits to all

      fittingPowerEvs.map { ev =>
        val chargingTicks = calculateChargingDuration(ev, proposedPower)
        val endTick = Math.min(currentTick + chargingTicks, ev.getDepartureTick)

        (
          ev,
          ChargingSchedule(
            ev,
            Seq(ChargingSchedule.Entry(currentTick, endTick, proposedPower))
          ),
          endTick,
          isFull(ev) || isEmpty(ev)
        )
      }
    } else {
      // not all evs can be charged with proposed power

      // charge all exceeded evs with their respective maximum power
      val maxCharged = exceedingPowerEvs.map { ev =>
        val maxPower = getMaxAvailableChargingPower(ev)
        val power =
          if (setPower.isGreaterThan(zeroKW))
            maxPower
          else
            maxPower.multiply(-1)

        val chargingTicks = calculateChargingDuration(ev, power)
        val endTick = Math.min(currentTick + chargingTicks, ev.getDepartureTick)

        (ev, power, endTick)
      }

      // if there's evs left whose max power has not been exceeded, go on with the recursion
      val nextIterationResults = if (fittingPowerEvs.nonEmpty) {

        // sum up allocated power
        val chargingPowerSum = maxCharged.foldLeft(zeroKW) {
          case (powerSum, (_, chargingPower, _)) =>
            powerSum.add(chargingPower)
        }

        // go into the next recursion step with the remaining power
        createScheduleWithSetPower(
          currentTick,
          fittingPowerEvs,
          setPower.subtract(chargingPowerSum)
        )
      } else Set.empty

      maxCharged.map { case (ev, power, endTick) =>
        (
          ev,
          ChargingSchedule(
            ev,
            Seq(ChargingSchedule.Entry(currentTick, endTick, power))
          ),
          endTick,
          isFull(ev) || isEmpty(ev)
        )
      } ++ nextIterationResults
    }

  }

  private def calculateChargingDuration(
      ev: EvModel,
      power: ComparableQuantity[Power]
  ): Long = {
    val timeUntilFullOrEmpty =
      if (power.isGreaterThan(zeroKW))
        ev.getEStorage
          .subtract(ev.getStoredEnergy)
          .divide(power)
          .asType(classOf[Time])
      else
        ev.getStoredEnergy
          .subtract(ev.getEStorage.multiply(lowestEvSoc))
          .divide(power.multiply(-1))
          .asType(classOf[Time])

    Math.round(
      timeUntilFullOrEmpty.to(SECOND).getValue.doubleValue
    )
  }

  /** @param ev
    *   the ev whose stored energy is to be checked
    * @return
    *   whether the given ev's stored energy is greater than the maximum charged
    *   energy allowed (minus a tolerance margin)
    */
  private def isFull(ev: EvModel): Boolean =
    ev.getStoredEnergy.isGreaterThanOrEqualTo(
      ev.getEStorage.subtract(calcToleranceMargin(ev))
    )

  /** @param ev
    *   the ev whose stored energy is to be checked
    * @return
    *   whether the given ev's stored energy is less than the minimal charged
    *   energy allowed (plus a tolerance margin)
    */
  private def isEmpty(ev: EvModel): Boolean =
    ev.getStoredEnergy.isLessThanOrEqualTo(
      ev.getEStorage.multiply(lowestEvSoc).add(calcToleranceMargin(ev))
    )

  private def calcToleranceMargin(ev: EvModel): ComparableQuantity[Energy] =
    getMaxAvailableChargingPower(ev)
      .multiply(Quantities.getQuantity(1, SECOND))
      .asType(classOf[Energy])

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
  ): Set[EvModel] = {
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
  def validateDepartures(lastEvs: Set[EvModel], departures: Seq[UUID]): Unit = {
    departures.foreach { ev =>
      if (!lastEvs.exists(_.getUuid == ev))
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
      lastEvs: Set[EvModel],
      arrivals: Seq[EvModel],
      chargingPoints: Int
  ): Unit = {

    arrivals.foreach { ev =>
      if (lastEvs.exists(_.getUuid == ev.getUuid))
        logger.warn(
          s"EV ${ev.getId} should arrive at this station (according to external simulation), but is already parked here."
        )
    }

    val newCount = lastEvs.size +
      arrivals.count { ev =>
        !lastEvs.exists(_.getUuid == ev.getUuid)
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
      arrivals: Seq[EvModel],
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]]
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
      evs: Set[EvModel],
      schedule: Map[EvModel, Option[ChargingSchedule]],
      tick: Long
  ) extends ModelState {
    def getSchedule(ev: EvModel): Option[ChargingSchedule] =
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
      sRated,
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
