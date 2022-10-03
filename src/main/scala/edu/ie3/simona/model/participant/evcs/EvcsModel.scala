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
import edu.ie3.datamodel.models.result.system.EvResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState,
  PowerEntry
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
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.{RichQuantity, RichQuantityDouble}
import edu.ie3.util.quantities.interfaces.Currency
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
    strategy: ChargingStrategy.Value
) extends SystemParticipant[EvcsRelevantData, EvcsState](
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

  private val lowestEvSoc = 0.2 // TODO config param

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

  /** Update EVs for the timeframe since the last scheduling and calculate
    * equivalent results
    *
    * @param currentTick
    *   current tick
    * @param voltage
    *   the current voltage
    * @return
    *   updated EVs and EvResults including charging prices
    */
  def applySchedule(
      currentTick: Long,
      voltage: ComparableQuantity[Dimensionless],
      state: EvcsState
  ): Set[(EvModel, Vector[EvResult], Vector[PowerEntry])] = if (
    state.schedule.nonEmpty
  ) {
    /* Apply schedules asynchronously */
    state.evs.par
      .map(
        applyScheduleToEv(
          _,
          state,
          state.tick,
          currentTick,
          simulationStartDate,
          voltage
        )
      )
      .seq
  } else {
    logger.debug(
      "There are EVs parked at this charging station, but there was no scheduling since the last" +
        "update. Probably the EVs already finished charging."
    )
    state.evs.map { ev => (ev, Vector.empty, Vector.empty) }
  }

  /** Fish for the schedule, that applies for the concrete ev and apply it.
    *
    * @param ev
    *   Car model
    * @param state
    *   The state data containing the schedule
    * @param lastSchedulingTick
    *   Simulation time, when the last schedule has been applied
    * @param currentTick
    *   Current simulation time
    * @param simulationStartDate
    *   Simulation start time
    * @param voltage
    *   Voltage magnitude of the connection node
    * @return
    *   A future onto the combination of updated ev model and achieved results
    */
  private def applyScheduleToEv(
      ev: EvModel,
      state: EvcsState,
      lastSchedulingTick: Long,
      currentTick: Long,
      simulationStartDate: ZonedDateTime,
      voltage: ComparableQuantity[Dimensionless]
  ): (EvModel, Vector[EvResult], Vector[PowerEntry]) = state
    .getSchedule(ev)
    .map {
      chargeEv(
        ev,
        _,
        lastSchedulingTick,
        currentTick,
        simulationStartDate,
        voltage
      )
    }
    .getOrElse(ev, Vector.empty, Vector.empty)

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
    * @param simulationStartDate
    *   Simulation start time
    * @param voltageMagnitude
    *   Current voltage magnitude at the connection node
    * @return
    *   The charged energy and intermediate charging results for a given
    *   electric vehicle
    */
  private def chargeEv(
      ev: EvModel,
      schedule: ChargingSchedule,
      lastSchedulingTick: Long,
      currentTick: Long,
      simulationStartDate: ZonedDateTime,
      voltageMagnitude: ComparableQuantity[Dimensionless]
  ): (EvModel, Vector[EvResult], Vector[PowerEntry]) = {
    /* Determine charged energy in the charging interval */
    val (chargedEnergySinceLastScheduling, evResults, powerEntries) =
      chargedEnergy(
        ev,
        schedule,
        lastSchedulingTick,
        currentTick,
        simulationStartDate,
        voltageMagnitude
      )

    /* Update EV with the charged energy during the charging interval */
    (
      ev.copyWith(
        ev.getStoredEnergy.add(chargedEnergySinceLastScheduling)
      ),
      evResults,
      powerEntries
    )
  }

  /** Determine the charged energy and the intermediate charging results for the
    * given ev and schedule
    *
    * @param ev
    *   Electric vehicle to charge
    * @param schedule
    *   Schedule for the charging
    * @param lastSchedulingTick
    *   Last tick, when a schedule ended
    * @param currentTick
    *   Current time in simulation
    * @param simulationStartDate
    *   Start date time of the overall simulation
    * @param voltageMagnitude
    *   Current nodal voltage magnitude
    * @return
    *   The charged energy and the intermediate charging results
    */
  private def chargedEnergy(
      ev: EvModel,
      schedule: ChargingSchedule,
      lastSchedulingTick: Long,
      currentTick: Long,
      simulationStartDate: ZonedDateTime,
      voltageMagnitude: ComparableQuantity[Dimensionless]
  ): (ComparableQuantity[Energy], Vector[EvResult], Vector[PowerEntry]) =
    schedule.schedule.toSeq
      .filter { case ChargingSchedule.Entry(tickStart, tickStop, _) =>
        /* Filter for entries, that end after the last schedule application (that slice is not yet fully applied)
         * and that start before the current tick */
        tickStop >= lastSchedulingTick && tickStart < currentTick
      }
      .sortBy(_.tickStart)
      .foldLeft(
        (
          Quantities.getQuantity(0, KILOWATTHOUR),
          Vector.empty[EvResult],
          Vector.empty[PowerEntry]
        )
      ) { case ((accumulatedEnergy, results, powerEntries), scheduleEntry) =>
        /* Only the timeframe from the start of last scheduling update and current tick must be considered */
        val (chargingWindowStart, chargingWindowEnd) =
          actualChargingWindow(
            scheduleEntry,
            lastSchedulingTick,
            currentTick
          )

        /* Determine the energy charged within this slice of the schedule and accumulate it */
        val chargedEnergyInThisScheduleEntry =
          chargedEnergyInScheduleSlice(
            scheduleEntry,
            chargingWindowStart,
            chargingWindowEnd
          )
        val totalChargedEnergy = accumulatedEnergy.add(
          chargedEnergyInThisScheduleEntry
        )

        val p = scheduleEntry.chargingPower
        val q = calculateReactivePower(
          scheduleEntry.chargingPower,
          voltageMagnitude
        )
        val power = PowerEntry(
          chargingWindowStart,
          chargingWindowEnd,
          ApparentPower(p, q)
        )
        val soc = ev.getStoredEnergy
          .add(accumulatedEnergy)
          .divide(ev.getEStorage)
          .asType(classOf[Dimensionless])
          .to(PERCENT)

        val result = new EvResult(
          chargingWindowStart.toDateTime(simulationStartDate),
          ev.getUuid,
          p,
          q,
          soc
        )
        (
          totalChargedEnergy,
          results :+ result,
          powerEntries :+ power
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
    *   The actual start and end of the charging window
    */
  private def actualChargingWindow(
      scheduleEntry: ChargingSchedule.Entry,
      lastSchedulingTick: Long,
      currentTick: Long
  ): (Long, Long) = {
    (
      math.max(scheduleEntry.tickStart, lastSchedulingTick),
      math.min(scheduleEntry.tickStop, currentTick)
    )
  }

  /** Determine the energy, that has been charged during one slice of a
    * schedule.
    *
    * @param scheduleEntry
    *   Definition of the schedule slice
    * @param chargingWindowStart
    *   Start of the charging window
    * @param chargingWindowEnd
    *   End of the charging window
    * @return
    *   The energy charged during this slice
    */
  private def chargedEnergyInScheduleSlice(
      scheduleEntry: ChargingSchedule.Entry,
      chargingWindowStart: Long,
      chargingWindowEnd: Long
  ): ComparableQuantity[Energy] =
    scheduleEntry.chargingPower
      .multiply(
        Quantities.getQuantity(
          (chargingWindowEnd - chargingWindowStart).toDouble,
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

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
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
          schedule.headOption.map(_.chargingPower).getOrElse(0d.asKiloWatt)
        sum.add(power)
    }

    val (maxCharging, maxDischarging) =
      preferredScheduling.foldLeft((zeroKW, zeroKW)) {
        case ((chargingSum, dischargingSum), (ev, _)) =>
          val maxPower = getMaxAvailableChargingPower(ev)

          val maxCharging =
            if (isChargeable(ev))
              maxPower
            else
              0d.asKiloWatt

          val maxDischarging =
            if (isDischargeable(ev))
              maxPower.multiply(-1)
            else
              0d.asKiloWatt

          (chargingSum.add(maxCharging), dischargingSum.add(maxDischarging))
      }

    ProvideMinMaxFlexOptions(
      uuid,
      preferredPower,
      maxCharging,
      maxDischarging
    )
  }

  override def handleControlledPowerChange(
      data: EvcsRelevantData,
      lastState: EvcsState,
      setPower: ComparableQuantity[Power]
  ): (EvcsState, FlexChangeIndicator) = {
    val currentEvs = determineCurrentState(data, lastState)

    if (QuantityUtil.isEquivalentAbs(zeroKW, setPower, 0))
      return (
        lastState.copy(
          evs = currentEvs,
          schedule = currentEvs.map(_ -> None).toMap,
          tick = data.tick
        ),
        FlexChangeIndicator()
      )

    val (applicableEvs, otherEvs) = currentEvs.partition { ev =>
      if (setPower.isGreaterThan(zeroKW))
        isChargeable(ev)
      else
        isDischargeable(ev)
    }

    val chargingSchedule = applicableEvs.map { ev =>
      val power = setPower.divide(applicableEvs.size)

      val timeUntilFullOrEmpty =
        if (setPower.isGreaterThan(zeroKW)) {
          ev.getEStorage
            .subtract(ev.getStoredEnergy)
            .divide(power)
            .asType(classOf[Time])
        } else {
          ev.getStoredEnergy
            .subtract(ev.getEStorage.multiply(lowestEvSoc))
            .divide(power.multiply(-1))
            .asType(classOf[Time])
        }

      val ticksUntilFullOrEmpty = Math.round(
        timeUntilFullOrEmpty.to(SECOND).getValue.doubleValue
      )

      val endTick =
        Math.min(data.tick + ticksUntilFullOrEmpty, ev.getDepartureTick)

      (
        ev,
        ChargingSchedule(
          ev,
          Seq(ChargingSchedule.Entry(data.tick, endTick, power))
        ),
        endTick,
        isFull(ev) || isEmpty(ev)
      )
    }

    val nextScheduledTick = chargingSchedule.map { case (_, _, endTick, _) =>
      endTick
    }.minOption

    val scheduleAtNextActivation = chargingSchedule
      .map { case (_, _, _, scheduleAtNext) =>
        scheduleAtNext
      }
      .reduceOption(_ || _)
      .getOrElse(false)

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

  private def determineCurrentState(
      data: EvcsRelevantData,
      lastState: EvcsState
  ): Set[EvModel] = {
    // determine current state
    val currentTime = data.tick.toDateTime(simulationStartDate)
    val voltage = data.voltages
      .filter { case (time, _) =>
        time.isBefore(currentTime) || time.isEqual(currentTime)
      }
      .maxByOption { case (date, _) =>
        date
      }
      .map { case (_, voltage) =>
        voltage
      }
      .getOrElse(Quantities.getQuantity(1d, PU))

    applySchedule(data.tick, voltage, lastState).map { case (ev, _, _) =>
      ev
    }
  }

  private def isChargeable(
      ev: EvModel
  ): Boolean =
    ev.getStoredEnergy.isLessThan(
      ev.getEStorage.subtract(calcToleranceMargin(ev))
    )

  private def isDischargeable(
      ev: EvModel
  ): Boolean =
    ev.getStoredEnergy.isGreaterThan(
      ev.getEStorage.multiply(lowestEvSoc).add(calcToleranceMargin(ev))
    )

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
      ev.getEStorage.add(calcToleranceMargin(ev))
    )

  private def calcToleranceMargin(ev: EvModel): ComparableQuantity[Energy] =
    getMaxAvailableChargingPower(ev)
      .multiply(Quantities.getQuantity(1, SECOND))
      .asType(classOf[Energy])

}

object EvcsModel {

  /** Class that holds all relevant data for an Evcs model calculation
    *
    * @param tick
    *   The current tick
    * @param voltages
    *   Nodal voltage per known time instant
    */
  final case class EvcsRelevantData(
      tick: Long,
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

  /** Container class for apparent power
    *
    * @param start
    *   Begin of the entry
    * @param end
    *   End of the entry
    * @param power
    *   apparent power
    */
  final case class PowerEntry(start: Long, end: Long, power: ApparentPower)

  def apply(
      inputModel: EvcsInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      chargingStrategy: String
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
      ChargingStrategy(chargingStrategy)
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
      chargingStrategy: ChargingStrategy.Value
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
      chargingStrategy
    )

    model.enable()

    model
  }
}