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
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsRelevantData
import edu.ie3.simona.model.participant.evcs.gridoriented.GridOrientedCurrentPrice.calculateCurrentPriceGridOriented
import edu.ie3.simona.model.participant.evcs.gridoriented.GridOrientedScheduling.calculateNewGridOrientedScheduling
import edu.ie3.simona.model.participant.evcs.marketoriented.MarketOrientedCurrentPrice.calculateCurrentPriceMarketOriented
import edu.ie3.simona.model.participant.evcs.uncontrolled.SchedulingWithConstantPower.calculateNewSchedulingWithConstantPower
import edu.ie3.simona.model.participant.evcs.uncontrolled.SchedulingWithMaximumPower.calculateNewSchedulingWithMaximumChargingPower
import edu.ie3.simona.model.participant.evcs.marketoriented.MarketOrientedScheduling.calculateNewMarketOrientedScheduling
import edu.ie3.simona.model.participant.{CalcRelevantData, SystemParticipant}
import edu.ie3.simona.service.market.StaticMarketSource
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits.{
  EURO,
  EURO_PER_KILOWATTHOUR,
  KILOWATT,
  KILOWATTHOUR,
  MEGAVAR,
  MEGAWATT
}
import edu.ie3.util.quantities.interfaces.Currency
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKWH
import edu.ie3.util.scala.quantities.QuantityUtil.RichQuantity
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{PERCENT, SECOND}

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Energy, Power}
import scala.annotation.tailrec
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
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    currentType: ElectricCurrentType,
    cosPhiRated: Double,
    chargingPoints: Int,
    locationType: EvcsLocationType
) extends SystemParticipant[EvcsRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    )
    with LazyLogging {

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. The scheduling depends on the chosen
    * strategy.
    * @param currentTick
    *   current tick
    * @param data
    *   data including the current EVs parked at the charging station
    * @return
    *   scheduling for charging the EVs
    */
  def calculateNewScheduling(
      currentTick: Long,
      startTime: ZonedDateTime,
      data: EvcsRelevantData
  ): Set[EvcsChargingScheduleEntry] = {

    val chargingStrategy = "maxPower"

    this.locationType match {

      case EvcsLocationType.CHARGING_HUB_TOWN |
          EvcsLocationType.CHARGING_HUB_HIGHWAY =>
        calculateNewSchedulingWithMaximumChargingPower(
          this,
          currentTick,
          data.currentEvs
        )

      case _ =>
        chargingStrategy match {
          case "maxPower" =>
            calculateNewSchedulingWithMaximumChargingPower(
              this,
              currentTick,
              data.currentEvs
            )
          case "constantPower" =>
            calculateNewSchedulingWithConstantPower(
              this,
              currentTick,
              data.currentEvs
            )
          case "gridOrientedScheduling" =>
            calculateNewGridOrientedScheduling(
              this,
              currentTick,
              startTime,
              data.currentEvs,
              data.voltages
            )
          case "marketOrientedScheduling" =>
            calculateNewMarketOrientedScheduling(
              this,
              currentTick,
              startTime,
              data.currentEvs
            )
          case _ =>
            throw new InvalidParameterException("Charging Strategy unknown.")
        }

    }

  }

  /** Calculate the apparent power changes in the given time interval.
    * @param schedule
    *   The charging schedule realized in the given time interval
    * @param voltage
    *   The last known voltage
    * @param requestTick
    *   The tick of the request, representing the end of the time interval
    * @param lastUpdateTick
    *   The tick of the last update, representing the start of the time interval
    * @return
    *   Set of tuples of tick and apparent power
    */
  def calculatePowerForLastInterval(
      schedule: Set[EvcsChargingScheduleEntry],
      voltage: ComparableQuantity[Dimensionless],
      requestTick: Long,
      lastUpdateTick: Long
  ): Set[(Long, ApparentPower)] = {

    /* Filter schedule for updates relevant for this interval */
    val filteredSchedule: Set[EvcsChargingScheduleEntry] = schedule
      .filter(_.tickStop >= lastUpdateTick)
      .filter(_.tickStart <= requestTick)

    /* Get all ticks with changes in a sorted vector */
    val allTicks: Set[Long] = getAllTicksOfSchedule(filteredSchedule)

    val results: Set[(Long, ApparentPower)] = allTicks.map { tick =>
      val activePower = filteredSchedule
        .foldLeft(
          Quantities.getQuantity(0, KILOWATT)
        )((p, entry) => {
          if (entry.tickStart <= tick && entry.tickStop > tick)
            p.add(entry.chargingPower)
          else p
        })
        .to(MEGAWATT)

      val reactivePower =
        calculateReactivePower(activePower, voltage).to(MEGAVAR)

      (tick, ApparentPower(activePower, reactivePower))

    }

    /* INCLUDE startTick of interval, EXCLUDE endTick of interval,
     * because in this endTick new EVs could arrive in the next step if this function was called while processing
     * an EvMovements update. */
    results.filter(_._1 >= lastUpdateTick).filter(_._1 < requestTick)
  }

  /** Update EVs for the timeframe since the last scheduling.
    * @param currentTick
    *   current tick
    * @param data
    *   data containing the scheduling
    * @return
    *   updated EVs
    */
  def updateEvsAccordingToScheduling(
      currentTick: Long,
      lastSchedulingTick: Long,
      data: EvcsRelevantData
  ): Set[EvModel] = {

    if (data.currentEvs.isEmpty) {
      Set.empty
    } else {

      if (data.schedule.nonEmpty) {
        data.currentEvs.map { ev =>
          val schedulingForThisEv = data.schedule.filter(
            _.ev == ev
          )

          /* Determine charged energy in the charging interval */
          val chargedEnergySinceLastScheduling: ComparableQuantity[Energy] =
            schedulingForThisEv.foldLeft(
              Quantities.getQuantity(0, KILOWATTHOUR)
            )(
              (
                  chargedEnergy: ComparableQuantity[Energy],
                  scheduleEntry: EvcsChargingScheduleEntry
              ) => {
                /* FIXME: Duplicate to this#updateEvsAccordingToSchedulingAndCalculateChargingPricesAsEvResults */
                /* Only the timeframe from the start of last scheduling update and current tick must be considered */
                val limitedScheduleEntryStart = {
                  math.max(scheduleEntry.tickStart, lastSchedulingTick)
                }
                val limitedScheduleEntryStop =
                  math.min(scheduleEntry.tickStop, currentTick)

                val chargedEnergyInThisScheduleEntry
                    : ComparableQuantity[Energy] = {
                  /* if schedule entry is not in the future and not in the past (past shouldn't happen anyway) */
                  if (
                    scheduleEntry.tickStart < currentTick
                    && scheduleEntry.tickStop >= lastSchedulingTick
                  ) {
                    scheduleEntry.chargingPower
                      .multiply(
                        Quantities.getQuantity(
                          (limitedScheduleEntryStop - limitedScheduleEntryStart).toDouble,
                          SECOND
                        )
                      )
                      .asType(classOf[Energy])
                      .to(KILOWATTHOUR)
                  } else {
                    zeroKWH
                  }
                }

                chargedEnergy.add(
                  chargedEnergyInThisScheduleEntry
                )
              }
            )

          /* Update EV with the charged energy during the charging interval */
          ev.copyWith(
            ev.getStoredEnergy.add(chargedEnergySinceLastScheduling)
          )
        }
      } else {
        logger.info(
          "There are EVs parked at this charging station, but there was no scheduling since the last" +
            "update. Probably the EVs already finished charging."
        )
        data.currentEvs
      }
    }
  }

  /** Update EVs for the timeframe since the last scheduling and calculate the
    * charging prices. !!!!!!!!!!!!!! NO PERMANENT SOLUTION !!!!!!!!!!!!!!!
    * (just for master thesis)
    * @param currentTick
    *   current tick
    * @param data
    *   data containing the scheduling
    * @return
    *   updated EVs and EvResults including charging prices
    */
  def updateEvsAccordingToSchedulingAndCalculateChargingPricesAsEvResults(
      currentTick: Long,
      startTime: ZonedDateTime,
      lastSchedulingTick: Long,
      data: EvcsRelevantData
  ): Set[(EvModel, Vector[EvResult])] = if (data.schedule.nonEmpty)
    data.currentEvs.map(ev =>
      chargeEv(ev, data.schedule, lastSchedulingTick, currentTick, startTime)
    )
  else {
    logger.debug(
      "There are EVs parked at this charging station, but there was no scheduling since the last" +
        "update. Probably the EVs already finished charging."
    )
    data.currentEvs.map { ev => (ev, Vector.empty) }
  }

  /** Charge the given EV under consideration a applicable schedule
    *
    * @param ev
    *   Electric vehicle to charge
    * @param schedules
    *   Entirety of all schedules
    * @param lastSchedulingTick
    *   Last tick, that a schedule has been processed
    * @param currentTick
    *   Current time in Simulation
    * @param simulationStart
    *   Wall clock time of the simulation start
    * @return
    *   The charged energy and intermediate charging results for a given
    *   electric vehicle
    */
  private def chargeEv(
      ev: EvModel,
      schedules: Set[EvcsChargingScheduleEntry],
      lastSchedulingTick: Long,
      currentTick: Long,
      simulationStart: ZonedDateTime
  ): (EvModel, Vector[EvResult]) = {
    val schedule = schedules.filter(
      _.ev == ev
    )

    /* Determine charged energy in the charging interval */
    val (chargedEnergySinceLastScheduling, evResults) =
      determineChargedEnergySinceLastCalculation(
        ev,
        schedule,
        lastSchedulingTick,
        currentTick,
        simulationStart
      )

    /* Update EV with the charged energy during the charging interval */
    (
      ev.copyWith(
        ev.getStoredEnergy.add(chargedEnergySinceLastScheduling)
      ),
      evResults
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
    * @param simulationStart
    *   Start date time of the overall simulation
    * @return
    *   The charged energy and the intermediate charging results
    */
  private def determineChargedEnergySinceLastCalculation(
      ev: EvModel,
      schedule: Set[EvcsChargingScheduleEntry],
      lastSchedulingTick: Long,
      currentTick: Long,
      simulationStart: ZonedDateTime
  ): (ComparableQuantity[Energy], Vector[EvResult]) = schedule.toSeq
    .sortBy(x => x.tickStart)
    .foldLeft(
      (
        Quantities.getQuantity(0, KILOWATTHOUR),
        Vector.empty[EvResult]
      )
    ) { case ((accumulatedEnergy, results), scheduleEntry) =>
      /* Only the timeframe from the start of last scheduling update and current tick must be considered */
      val (chargingWindowStart, chargingWindowEnd) =
        actualChargingWindow(
          scheduleEntry,
          lastSchedulingTick,
          currentTick
        )

      if (
        scheduleEntry.tickStart < currentTick && scheduleEntry.tickStop >= lastSchedulingTick
      ) {
        /* There is actual charging happening */

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

        val chargingCosts =
          getChargingCostsForScheduleEntry(
            chargingWindowStart.toDateTime(simulationStart),
            chargingWindowEnd.toDateTime(simulationStart),
            scheduleEntry.chargingPower,
            Quantities.getQuantity(0, EURO)
          )

        /* TODO: NSteffan: This is a non-permanent solution to write out SoC and charging prices for EVs for
                verification purposes in my master thesis.
         */
        val result = new EvResult(
          this.getUuid,
          chargingWindowEnd.toDateTime(simulationStart),
          ev.getUuid,
          Quantities.getQuantity(
            chargingCosts.getValue.doubleValue(),
            MEGAWATT
          ), // TODO: from EURO to MEGAWATT for correct type -> actually p should be written in EvResult
          Quantities.getQuantity(
            chargedEnergyInThisScheduleEntry.getValue
              .doubleValue(),
            MEGAWATT
          ), // TODO: from KILOWATTHOUR to MEGAWATT for correct type -> actually q should be written in EvResult
          ev.getStoredEnergy
            .add(totalChargedEnergy)
            .divide(ev.getEStorage)
            .asType(classOf[Dimensionless])
            .to(PERCENT)
        )
        (
          totalChargedEnergy,
          results :+ result
        )
      } else {
        /* There is no charging happening -> Provide dummy values */
        (
          zeroKWH,
          results
        )
      }
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
      scheduleEntry: EvcsChargingScheduleEntry,
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
      scheduleEntry: EvcsChargingScheduleEntry,
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
    * @param startTime
    *   start time of the simulation to convert ticks to real time
    * @param data
    *   the evcs relevant data including the current parked ev, scheduling, and
    *   voltages
    * @return
    *   the current price signal of the evcs
    */
  def calculateCurrentPriceSignalToCommunicateToEvs(
      currentTick: Long,
      startTime: ZonedDateTime,
      data: EvcsRelevantData
  ): Option[Double] = {

    // Only EvcsLocationType != Home and Work have prices
    this.locationType match {

      case EvcsLocationType.HOME | EvcsLocationType.WORK =>
        /* only public charging stations have dynamic charging prices */
        None

      case publicLocationType =>
        val strategy = "none"
        val lengthOfRelevantIntervalInSeconds: Int =
          publicLocationType match {
            case EvcsLocationType.CUSTOMER_PARKING | EvcsLocationType.STREET =>
              7200 // 2 hours
            case _ => 1800 // 30 minutes
          }

        strategy match {
          case "gridOriented" =>
            calculateCurrentPriceGridOriented(
              this,
              currentTick,
              lengthOfRelevantIntervalInSeconds,
              data
            )
          case "marketOriented" =>
            calculateCurrentPriceMarketOriented(
              currentTick,
              startTime,
              lengthOfRelevantIntervalInSeconds
            )
          case _ =>
            None

        }

    }
  }

  /** Return all ticks included in a schedule
    * @param schedule
    *   schedule including schedule entries with start and stop ticks
    * @return
    *   all ticks in the schedule
    */
  def getAllTicksOfSchedule(
      schedule: Set[EvcsChargingScheduleEntry]
  ): Set[Long] = {

    schedule.foldLeft(Set.empty[Long])((setOfTicks, entry) => {
      val updateSet: Set[Long] =
        setOfTicks ++ Set(entry.tickStart, entry.tickStop)
      updateSet
    })
  }

  /** Returns the maximum available charging power for an EV, which depends on
    * ev and charging station limits for AC and DC current
    * @param ev
    *   ev for which the max charging power should be returned
    * @return
    *   maximum charging power for the EV at this charging station
    */
  def getMaxAvailableChargingPower(
      ev: EvModel
  ): ComparableQuantity[Power] = {
    currentType match {
      case ElectricCurrentType.AC =>
        sRated.min(ev.getSRatedAC).to(KILOWATT)
      case ElectricCurrentType.DC =>
        sRated.min(ev.getSRatedDC).to(KILOWATT)
    }
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
}

object EvcsModel {

  /** Class that holds all relevant data for an Evcs model calculation
    *
    * @param currentEvs
    *   EVs that have been charging up until this tick. Can include EVs that are
    *   departing
    * @param schedule
    *   the schedule determining when to load which EVs with which power
    */
  final case class EvcsRelevantData(
      currentEvs: Set[EvModel],
      schedule: Set[EvcsChargingScheduleEntry],
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]]
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
      inputModel.getType.getsRated,
      inputModel.getType.getElectricCurrentType,
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
      sRated: ComparableQuantity[Power],
      currentType: ElectricCurrentType,
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
      currentType,
      cosPhiRated,
      chargingPoints,
      locationType
    )

    model.enable()

    model
  }
}
