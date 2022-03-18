/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.marketoriented

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.model.participant.evcs
import edu.ie3.simona.model.participant.evcs.SchedulingTimeWindows.SchedulingTimeWindowWithPrice
import edu.ie3.simona.model.participant.evcs.PredictionAndSchedulingUtils.{
  calculateRemainingEnergyToBeChargedAfterThisUpdate,
  findDispatchableEvs,
  getDepartureTimesAndRequiredEnergyOfAllEvs,
  getEvsStillParkedAtThisTime
}
import edu.ie3.simona.model.participant.evcs.marketoriented.MarketPricePrediction.{
  PredictedPrice,
  getPredictedPricesForRelevantTimeWindowBasedOnReferencePrices,
  priceTimeTable
}
import edu.ie3.simona.model.participant.evcs.{
  ChargingSchedule,
  EvcsChargingScheduleEntry,
  EvcsModel
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATT
import edu.ie3.util.scala.quantities.DefaultQuantities
import edu.ie3.util.scala.quantities.QuantityUtil.RichQuantity
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.SECOND

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import javax.measure.quantity.{Energy, Power}
import scala.annotation.tailrec

trait MarketOrientedCharging {
  this: EvcsModel =>

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. In this case, the scheduling is supposed to
    * be grid conducive, trying to minimize the charging costs based on energy
    * price predictions.
    *
    * @param currentTick
    *   current tick
    * @param startTime
    *   start time of simulation
    * @param evs
    *   currently parked evs at the charging station
    * @return
    *   scheduling for charging the EVs
    */
  def chargeMarketOriented(
      currentTick: Long,
      startTime: ZonedDateTime,
      evs: Set[EvModel]
  ): Map[EvModel, Option[ChargingSchedule]] = {
    val currentTime = currentTick.toDateTime(startTime)

    /* Find evs that cannot be scheduled an exclude those from scheduling, charge with max power */
    val (nonDispatchableEvs, dispatchableEvs) =
      findDispatchableEvs(
        this,
        evs,
        currentTime,
        startTime
      )

    /* Create scheduling for evs that need ot charge with maximum power */
    val scheduleForEvsThatChargeWithMaxPower =
      chargeWithMaximumPower(
        currentTick,
        nonDispatchableEvs
      )

    /* Create scheduling for evs that can be scheduled */
    val scheduleForSchedulableEvs =
      if (dispatchableEvs.nonEmpty) {
        scheduleDispatchableEvs(
          this,
          currentTick,
          currentTime,
          startTime,
          dispatchableEvs
        )
      } else Map.empty[EvModel, Option[ChargingSchedule]]

    scheduleForEvsThatChargeWithMaxPower ++ scheduleForSchedulableEvs
  }

  /** Determine a market oriented scheduling for evs that don't need to be
    * charged with maximum power until their departure. The scheduling for
    * charging of these evs is based on predicted market prices based on
    * historic prices.
    *
    * @param evcsModel
    *   evcs model to calculate for / with
    * @param currentTick
    *   current tick
    * @param currentTime
    *   current time at start of scheduling
    * @param startTime
    *   start time of simulation
    * @param evs
    *   currently parked evs at the charging station
    * @return
    *   scheduling for charging the schedulable evs
    */
  private def scheduleDispatchableEvs(
      evcsModel: EvcsModel,
      currentTick: Long,
      currentTime: ZonedDateTime,
      startTime: ZonedDateTime,
      evs: Set[EvModel]
  ): Map[EvModel, Option[ChargingSchedule]] = {
    /* Get all departure times and required energies of the currently parked and schedulable evs in separate lists */
    val (departureTimes, _) =
      getDepartureTimesAndRequiredEnergyOfAllEvs(evs, startTime)

    /* Find latest departure time for filtering later */
    val lastDepartureTime = departureTimes.maxOption match {
      case Some(time) => time
      case None =>
        throw new InvalidParameterException(
          "This shouldn't happen, there must be at least one EV here."
        )
    }

    /* Get time windows with predicted energy prices for the relevant time until departure of the last EV */
    val predictedPrices =
      getPredictedPricesForRelevantTimeWindowBasedOnReferencePrices(
        currentTime,
        lastDepartureTime,
        priceTimeTable
      )

    /* Get scheduling time windows. The time windows are separated by price changes and departing evs */
    val schedulingTimeWindows =
      getSchedulingTimeWindows(
        predictedPrices,
        departureTimes,
        startTime,
        evs
      )

    /* Start with ev departing first and distribute charging energy */
    evs.toVector
      .sortBy(ev => ev.getDepartureTick)
      .map { ev =>
        {
          val scheduleForEv =
            createScheduleForThisEv(
              schedulingTimeWindows,
              ev,
              evcsModel,
              startTime
            )

          ev -> Some(ChargingSchedule(ev, scheduleForEv))
        }
      }
      .toMap
  }

  /** Calculate the charging schedule for this ev based on the current
    * scheduling time window information.
    *
    * @param schedulingTimeWindows
    *   the scheduling time windows
    * @param ev
    *   the ev for which the charging schedule should be calculated
    * @param evcsModel
    *   the evcs model
    * @param startTime
    *   the start time of the simulation to convert ticks to real times
    * @return
    *   charging schedule for this ev
    */
  private def createScheduleForThisEv(
      schedulingTimeWindows: Vector[SchedulingTimeWindowWithPrice],
      ev: EvModel,
      evcsModel: EvcsModel,
      startTime: ZonedDateTime
  ): Seq[ChargingSchedule.Entry] = {
    /* Energy that needs to be distributed on the time windows to charge the ev to full SoC */
    val energyToChargeForEv = ev.getEStorage.subtract(ev.getStoredEnergy)

    /* Filter relevant time windows for this ev and add information if already used with max power */
    val windowsForEv =
      getRelevantScheduleWindowsForThisEvWithBlockedInformation(
        ev,
        schedulingTimeWindows
      )

    recursiveCalculationOfSchedulingForThisEv(
      Seq.empty[ChargingSchedule.Entry],
      energyToChargeForEv,
      windowsForEv,
      ev,
      evcsModel,
      startTime
    )
  }

  /** Calculate the charging schedule for an ev. The energy to charge is
    * distributed on the available time windows in a recursive manner, until all
    * energy is to reach a SoC of 100% is distributed or the ev charges with
    * maximum power in all time windows (however, this case should have been
    * excluded through pre-filtering of the schedulable evs). The distribution
    * of charging energy and power is oriented on market prices with the goal to
    * charge at low predicted market prices to minimize energy costs.
    *
    * @param currentScheduleForEv
    *   the current schedule for the ev. At start, this is empty.
    * @param currentRemainingEnergyToChargeForEv
    *   the remaining energy the ev needs to charge to reach 100% SoC
    * @param currentWindowsForEv
    *   the current time windows relevant for the ev with price information
    * @param ev
    *   the ev to schedule
    * @param evcsModel
    *   the evcs model
    * @param startTime
    *   the simulation start time to convert ticks to real time
    * @return
    *   the updated charging schedule for the ev
    */
  @tailrec
  private def recursiveCalculationOfSchedulingForThisEv(
      currentScheduleForEv: Seq[ChargingSchedule.Entry],
      currentRemainingEnergyToChargeForEv: ComparableQuantity[Energy],
      currentWindowsForEv: Vector[
        (SchedulingTimeWindowWithPrice, Boolean)
      ],
      ev: EvModel,
      evcsModel: EvcsModel,
      startTime: ZonedDateTime
  ): Seq[ChargingSchedule.Entry] = {
    /* Filter for time windows not already used with max power */
    val availableWindows: Vector[SchedulingTimeWindowWithPrice] =
      currentWindowsForEv.filter(_._2 == false).map(x => x._1)

    val (
      updatedScheduleForEv,
      updatedRemainingEnergyToChargeForEv,
      updatedWindowsForEv
    ) =
      /* Find time window of still available time windows with minimum price */
      availableWindows.minByOption(_.price) match {

        case Some(window) =>
          val powerForEvForWindow =
            currentRemainingEnergyToChargeForEv
              .divide(
                Quantities.getQuantity(
                  window.length,
                  SECOND
                )
              )
              .asType(classOf[Power])
              .min(evcsModel.getMaxAvailableChargingPower(ev))
              .to(KILOWATT)

          /* Block window if EV charges with max possible power in this window */
          val windowBlockedForEv =
            powerForEvForWindow.isGreaterThanOrEqualTo(
              evcsModel.getMaxAvailableChargingPower(ev)
            )

          /* Replace the time window with the updated, blocked version */
          val updatedWindowsForEv = currentWindowsForEv.filterNot(
            _._1 == window
          ) :+ (window, windowBlockedForEv)

          /* Calculate remaining energy the ev needs to charge */
          val updatedRemainingEnergyToChargeForEv: ComparableQuantity[Energy] =
            calculateRemainingEnergyToBeChargedAfterThisUpdate(
              powerForEvForWindow,
              window,
              currentRemainingEnergyToChargeForEv
            )

          /* Add schedule entry for ev and this time window */
          val updatedScheduleForEv =
            currentScheduleForEv :+ ChargingSchedule.Entry(
              startTime
                .until(
                  window.start,
                  ChronoUnit.SECONDS
                ),
              startTime
                .until(window.end, ChronoUnit.SECONDS),
              powerForEvForWindow
            )

          (
            updatedScheduleForEv,
            updatedRemainingEnergyToChargeForEv,
            updatedWindowsForEv
          )

        case None =>
          throw new InvalidParameterException(
            "This shouldn't happen. There must be at least one time window still available, because EVs that " +
              "need to charge with full power and aren't schedulable were excluded earlier."
          )

      }

    /* While not all energy required is distributed on the time windows (and as long as there are still available
     * time windows the ev can charge more energy in, hence where it is not yet charging with max charging power)
     * distribute more energy on the time windows. If all energy is distributed, return the results.
     */
    if (
      updatedRemainingEnergyToChargeForEv
        .isGreaterThan(DefaultQuantities.zeroKWH)
      && !updatedWindowsForEv.forall(x => x._2)
    ) {
      recursiveCalculationOfSchedulingForThisEv(
        updatedScheduleForEv,
        updatedRemainingEnergyToChargeForEv,
        updatedWindowsForEv,
        ev,
        evcsModel,
        startTime
      )
    } else
      updatedScheduleForEv

  }

  /** Get relevant scheduling time windows for a specific ev
    *
    * @param ev
    *   the ev to get the relevant time windows for
    * @param schedulingTimeWindows
    *   the current version of the scheduling time windows
    * @return
    *   the relevant schedule time windows
    */
  private def getRelevantScheduleWindowsForThisEvWithBlockedInformation(
      ev: EvModel,
      schedulingTimeWindows: Vector[SchedulingTimeWindowWithPrice]
  ): Vector[(SchedulingTimeWindowWithPrice, Boolean)] = {

    schedulingTimeWindows
      .foldLeft(Vector.empty[SchedulingTimeWindowWithPrice])(
        (
            relevantWindows: Vector[SchedulingTimeWindowWithPrice],
            window: SchedulingTimeWindowWithPrice
        ) => {
          if (window.parkedEvs.contains(ev)) {
            relevantWindows :+ window
          } else {
            relevantWindows
          }
        }
      )
      .map(x => (x, false))

  }

  /** Create the scheduling time windows required for the scheduling of the evs.
    * The list of scheduling time windows is created based on the predicted
    * prices and their time windows and the departure times of the evs. For each
    * time window, a SchedulingTimeWindow is created with according information.
    *
    * @param predictedPrices
    *   the predicted price time windows
    * @param departureTimes
    *   the departure times of all evs to schedule
    * @param startTime
    *   the start time of the simulation to obtain real times from ticks
    * @param evs
    *   the evs to schedule
    * @return
    *   list of scheduling time windows
    */
  private def getSchedulingTimeWindows(
      predictedPrices: Vector[PredictedPrice],
      departureTimes: Vector[ZonedDateTime],
      startTime: ZonedDateTime,
      evs: Set[EvModel]
  ): Vector[SchedulingTimeWindowWithPrice] = {
    predictedPrices
      .foldLeft(
        Vector.empty[SchedulingTimeWindowWithPrice]
      )(
        (
            timeWindows: Vector[SchedulingTimeWindowWithPrice],
            entry: PredictedPrice
        ) => {

          val departureTimesInThisTimeWindow: Vector[ZonedDateTime] =
            departureTimes
              .filter(_.isAfter(entry.start))
              .filter(_.isBefore(entry.end))
              .sorted

          if (departureTimesInThisTimeWindow.nonEmpty) {

            var x = Vector.empty[SchedulingTimeWindowWithPrice]
            val size: Int = departureTimesInThisTimeWindow.size

            x = x :+ SchedulingTimeWindowWithPrice(
              entry.start,
              departureTimesInThisTimeWindow(0),
              entry.price,
              entry.start.until(
                departureTimesInThisTimeWindow(0),
                ChronoUnit.SECONDS
              ),
              getEvsStillParkedAtThisTime(evs, entry.start, startTime)
            )
            for (i <- 0 until size) {
              if (i < size - 1) {
                x = x :+ SchedulingTimeWindowWithPrice(
                  departureTimesInThisTimeWindow(i),
                  departureTimesInThisTimeWindow(i + 1),
                  entry.price,
                  departureTimesInThisTimeWindow(i).until(
                    departureTimesInThisTimeWindow(i + 1),
                    ChronoUnit.SECONDS
                  ),
                  getEvsStillParkedAtThisTime(
                    evs,
                    departureTimesInThisTimeWindow(i),
                    startTime
                  )
                )
              } else {
                x = x :+ SchedulingTimeWindowWithPrice(
                  departureTimesInThisTimeWindow(i),
                  entry.end,
                  entry.price,
                  departureTimesInThisTimeWindow(i).until(
                    entry.end,
                    ChronoUnit.SECONDS
                  ),
                  getEvsStillParkedAtThisTime(
                    evs,
                    departureTimesInThisTimeWindow(i),
                    startTime
                  )
                )
              }
            }

            timeWindows :++ x

          } else {

            timeWindows :+ SchedulingTimeWindowWithPrice(
              entry.start,
              entry.end,
              entry.price,
              entry.start.until(entry.end, ChronoUnit.SECONDS),
              getEvsStillParkedAtThisTime(evs, entry.start, startTime)
            )

          }

        }
      )
      .filter(_.parkedEvs.nonEmpty)
      .filterNot(x => x.start.isEqual(x.end))
      .sortBy { case SchedulingTimeWindowWithPrice(start, _, _, _, _) =>
        start
      }
  }

}
