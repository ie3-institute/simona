/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.gridoriented

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.model.participant.evcs.SchedulingTimeWindows.SchedulingTimeWindowWithVoltage
import edu.ie3.simona.model.participant.evcs.PredictionAndSchedulingUtils.{
  calculateRemainingEnergyToBeChargedAfterThisUpdate,
  calculateSumOfEnergies,
  findDispatchableEvs,
  getDepartureTimesAndRequiredEnergyOfAllEvs,
  getEvsStillParkedAtThisTime
}
import edu.ie3.simona.model.participant.evcs.gridoriented.VoltagePrediction.{
  PredictedVoltage,
  getPredictedVoltagesForRelevantTimeWindowBasedOnReferenceVoltages
}
import edu.ie3.simona.model.participant.evcs.{ChargingSchedule, EvcsModel}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, PU}
import edu.ie3.util.scala.quantities.DefaultQuantities
import edu.ie3.util.scala.quantities.QuantityUtil.RichQuantity
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.SECOND

import java.time.temporal.ChronoUnit
import java.time.ZonedDateTime
import javax.measure.quantity.{Dimensionless, Energy, Power}
import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

trait GridOrientedCharging {
  this: EvcsModel =>

  /* Dispersion factor to increasingly avoid that single EVs fully occupy one time slot but charge more evenly
   * distributed over the time windows */
  val dispersionFactor: Double = 0.5
  val voltageTolerance: Double = 0.0000000001

  /** Determine scheduling for charging the EVs currently parked at the charging
    * station until their departure. In this case, the scheduling is supposed to
    * be grid conducive, trying to minimize grid impact based on node voltage
    * predictions.
    *
    * @param currentTick
    *   current tick
    * @param startTime
    *   start time of simulation
    * @param evs
    *   currently parked evs at the charging station
    * @param voltages
    *   voltage memory of the charging station
    * @return
    *   scheduling for charging the EVs
    */
  def chargeGridOriented(
      currentTick: Long,
      startTime: ZonedDateTime,
      evs: Set[EvModel],
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]]
  ): Map[EvModel, Option[ChargingSchedule]] = {
    val currentTime = currentTick.toDateTime(startTime)

    /* Find evs that cannot be scheduled and exclude those from scheduling, charge with max power */
    val (nonDispatchableEvs, dispatchableEvs) =
      findDispatchableEvs(
        this,
        evs,
        currentTime,
        startTime
      )

    Await.result(
      scheduleNonDispatchableEvs(currentTick, nonDispatchableEvs)
        .zip(
          scheduleDispatchableEvs(
            currentTick,
            currentTime,
            startTime,
            dispatchableEvs,
            voltages
          )
        )
        .map {
          case (scheduleForNonDispatchableEvs, scheduleForDispatchableEvs) =>
            scheduleForNonDispatchableEvs ++ scheduleForDispatchableEvs
        },
      Duration("1h")
    )
  }

  /** Determine the schedule for all non-dispatchable evs
    *
    * @param tick
    *   Current simulation time
    * @param evs
    *   Collection of non-dispatchable evs
    * @return
    *   Future onto mapping from ev to it's schedule
    */
  private def scheduleNonDispatchableEvs(
      tick: Long,
      evs: Set[EvModel]
  ): Future[Map[EvModel, Option[ChargingSchedule]]] = Future {
    chargeWithMaximumPower(
      tick,
      evs
    )
  }

  /** Determine a grid conducive scheduling for evs that don't need to be
    * charged with maximum power until their departure. The scheduling for
    * charging of these evs is based on predicted grid utilization based on
    * historic node voltages.
    *
    * @param currentTick
    *   current tick
    * @param currentTime
    *   current time at start of scheduling
    * @param startTime
    *   start time of simulation
    * @param evs
    *   currently parked evs at the charging station
    * @param voltages
    *   voltage memory of the charging station
    * @return
    *   scheduling for charging the dispatchable evs
    */
  private def scheduleDispatchableEvs(
      currentTick: Long,
      currentTime: ZonedDateTime,
      startTime: ZonedDateTime,
      evs: Set[EvModel],
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]]
  ): Future[Map[EvModel, Option[ChargingSchedule]]] = Future {
    if (evs.nonEmpty) {

      /* Get all departure times and required energies of the currently parked and dispatchable evs in separate lists */
      val (departureTimes, requiredEnergies) =
        getDepartureTimesAndRequiredEnergyOfAllEvs(evs, startTime)

      /* Find latest departure time for filtering later */
      val lastDepartureTime = departureTimes.maxOption match {
        case Some(time) => time
        case None =>
          throw new InvalidParameterException(
            "This shouldn't happen, there must be at least one EV here."
          )
      }

      /* Calculate total energy to be charged by dispatchable evs */
      val totalEnergyToBeChargedBySchedulableEvs = calculateSumOfEnergies(
        requiredEnergies
      )

      /* Get time windows with predicted voltage values for the relevant time until departure of the last EV */
      getPredictedVoltagesForRelevantTimeWindowBasedOnReferenceVoltages(
        currentTime,
        lastDepartureTime,
        voltages
      ).map { predictedVoltages =>
        val minVoltageInTimeWindow = getMinimumVoltageInPredictedVoltages(
          predictedVoltages
        )

        /* Get scheduling time windows. The time windows are separated by voltage changes and departing evs */
        val schedulingTimeWindows =
          getSchedulingTimeWindows(
            predictedVoltages,
            departureTimes,
            minVoltageInTimeWindow.subtract(
              Quantities.getQuantity(0.0000001, PU)
            ), // To avoid dividing by zero
            startTime,
            evs
          )

        /* Calculate sum of box sizes (voltage deviation * length) and length of all time windows */
        val (totalTimeWindowLength, totalTimeWindowSize) =
          calculateTotalTimeWindowLengthAndSize(schedulingTimeWindows)

        /* Calculate power per voltage deviation */
        val powerPerVoltageDeviation =
          calculatePowerPerVoltageDeviation(
            totalTimeWindowSize,
            totalTimeWindowLength,
            totalEnergyToBeChargedBySchedulableEvs
          )

        /* Start with ev departing first and distribute charging energy */
        evs.toVector
          .sortBy(_.getDepartureTick)
          .foldLeft(
            (
              Map.empty[EvModel, Option[ChargingSchedule]],
              schedulingTimeWindows
            )
          ) { case ((evToSchedule, schedulingTimeWindows), ev) =>
            val (updatedWindowsForEv, scheduleForEv) =
              createScheduleForThisEvAndUpdateSchedulingTimeWindows(
                schedulingTimeWindows,
                ev,
                this,
                startTime,
                powerPerVoltageDeviation
              )

            val updatedSchedulingTimeWindows =
              updateSchedulingTimeWindows(
                schedulingTimeWindows,
                updatedWindowsForEv
              )

            /* Update the overall scheduling of the evcs with the schedule for this ev and the updated time windows. */
            (
              evToSchedule ++ Map(
                ev -> Some(ChargingSchedule(ev, scheduleForEv))
              ),
              updatedSchedulingTimeWindows
            )
          }
          ._1
      } match {
        case Failure(exception) =>
          logger.warn(
            s"Voltage prediction for tick $currentTick failed. Charge evs with maximum power. Reason: '${exception.getMessage}'"
          )
          chargeWithMaximumPower(currentTick, evs)
        case Success(evToSchedule) => evToSchedule
      }
    } else Map.empty[EvModel, Option[ChargingSchedule]]
  }

  /** Calculate the charging schedule for this ev based on the current
    * scheduling time window information.
    *
    * @param currentSchedulingTimeWindows
    *   the current scheduling time windows
    * @param ev
    *   the ev for which the charging schedule should be calculated
    * @param evcsModel
    *   the evcs model
    * @param startTime
    *   the start time of the simulation to convert ticks to real times
    * @param powerPerVoltageDeviation
    *   the power per voltage deviation factor
    * @return
    *   updated scheduling time windows and charging schedule for this ev
    */
  private def createScheduleForThisEvAndUpdateSchedulingTimeWindows(
      currentSchedulingTimeWindows: Vector[SchedulingTimeWindowWithVoltage],
      ev: EvModel,
      evcsModel: EvcsModel,
      startTime: ZonedDateTime,
      powerPerVoltageDeviation: ComparableQuantity[Power]
  ): (
      Vector[SchedulingTimeWindowWithVoltage],
      Seq[ChargingSchedule.Entry]
  ) = {
    /* Energy that needs to be distributed on the time windows to charge the ev to full SoC */
    val energyToChargeForEv = ev.getEStorage.subtract(ev.getStoredEnergy)

    /* Filter relevant time windows for this ev and add information if already used with max power */
    val windowsForEv =
      getRelevantScheduleWindowsForThisEvWithBlockedInformation(
        ev,
        currentSchedulingTimeWindows
      )

    val (finalUpdatedWindowsForEv, finalScheduleForEv) =
      recursiveCalculationOfSchedulingForThisEv(
        Seq.empty[ChargingSchedule.Entry],
        energyToChargeForEv,
        windowsForEv,
        ev,
        evcsModel,
        startTime,
        powerPerVoltageDeviation
      )

    (finalUpdatedWindowsForEv.map(_._1), finalScheduleForEv)
  }

  /** Calculate the charging schedule for an ev. The energy to charge is
    * distributed on the available time windows in a recursive manner, until all
    * energy is to reach a SoC of 100% is distributed or the ev charges with
    * maximum power in all time windows (however, this case should have been
    * excluded through pre-filtering of the schedulable evs). The distribution
    * of charging energy and power is oriented on voltage level predictions and
    * an estimation of the influence of a power increase on the voltage level,
    * expressed by a power per voltage deviation.
    *
    * @param currentScheduleForEv
    *   the current schedule for the ev. At start, this is empty.
    * @param currentRemainingEnergyToChargeForEv
    *   the remaining energy the ev needs to charge to reach 100% SoC
    * @param currentWindowsForEv
    *   the current time windows relevant for the ev with voltage information
    * @param ev
    *   the ev to schedule
    * @param evcsModel
    *   the evcs model
    * @param startTime
    *   the simulation start time to convert ticks to real time
    * @param powerPerVoltageDeviation
    *   the power per voltage deviation estimation
    * @return
    *   the updated scheduling time windows with voltage level information and
    *   the charging schedule for the ev
    */
  @tailrec
  private def recursiveCalculationOfSchedulingForThisEv(
      currentScheduleForEv: Seq[ChargingSchedule.Entry],
      currentRemainingEnergyToChargeForEv: ComparableQuantity[Energy],
      currentWindowsForEv: Vector[
        (SchedulingTimeWindowWithVoltage, Boolean)
      ],
      ev: EvModel,
      evcsModel: EvcsModel,
      startTime: ZonedDateTime,
      powerPerVoltageDeviation: ComparableQuantity[Power]
  ): (
      Vector[(SchedulingTimeWindowWithVoltage, Boolean)],
      Seq[ChargingSchedule.Entry]
  ) = {
    /* Filter for time windows not already used with max power */
    val availableWindows = currentWindowsForEv
      .filterNot { case (_, alreadyUsedWithMaxPower) =>
        alreadyUsedWithMaxPower
      }
      .map(_._1)

    val (
      updatedScheduleForEv,
      updatedRemainingEnergyToChargeForEv,
      updatedWindowsForEv
    ) =
      /* Find time window of still available time windows with max voltage deviation */
      availableWindows.maxByOption(_.voltageDeviation) match {
        case Some(window) =>
          /* See if the best remaining time windows has already negative or zero voltage deviation.
           * If this is the case, look if there are multiple with the same value
           * to distribute the remaining energy equally on them.
           * If the found window has positive voltage deviation, continue with that window.
           */
          if (
            window.voltageDeviation
              .isGreaterThan(Quantities.getQuantity(0, PU))
          ) {
            /* Check for existing schedule entry and power to charge this ev with in this time window.
             * If such entry exists, it needs to be replaced.
             */
            val (maybeReducedScheduleForEv, powerForEvForWindowBeforeUpdate) =
              checkForEarlierScheduleEntryForEvInThisWindowAndReturnPreviousPowerAndScheduleWithoutThisEntry(
                currentScheduleForEv,
                startTime,
                window
              )

            val additionalPowerForEvForWindow =
              calculateAdditionalPowerForEvForWindow(
                ev,
                evcsModel,
                powerPerVoltageDeviation,
                window,
                currentRemainingEnergyToChargeForEv,
                powerForEvForWindowBeforeUpdate,
                availableWindows
              )

            calculateUpdatedScheduleAndUpdatedRemainingEnergyToChargeAndUpdatedWindowsForEv(
              ev,
              powerForEvForWindowBeforeUpdate,
              additionalPowerForEvForWindow,
              window,
              powerPerVoltageDeviation,
              evcsModel,
              currentWindowsForEv,
              currentRemainingEnergyToChargeForEv,
              maybeReducedScheduleForEv,
              startTime
            )
          } else {
            /* -> The best time window has negative or zero voltage deviation. */

            /* Collect all windows with the same voltage deviation to distribute the remaining energy equally on them. */
            val allWindowsWithEqualVoltageDeviation =
              availableWindows.filter(x =>
                edu.ie3.util.quantities.QuantityUtil.isEquivalentAbs(
                  x.voltageDeviation,
                  window.voltageDeviation,
                  voltageTolerance
                )
              )

            val maxPossibleAdditionalChargingPowerForWindows =
              getMaxPossibleEqualAdditionalChargingPowerForTimeWindows(
                allWindowsWithEqualVoltageDeviation,
                currentScheduleForEv,
                startTime,
                ev,
                evcsModel
              )

            val additionalPowerForEvForEachWindow =
              calculateAdditionalPowerForEvForEachWindow(
                allWindowsWithEqualVoltageDeviation,
                availableWindows,
                window,
                powerPerVoltageDeviation,
                maxPossibleAdditionalChargingPowerForWindows,
                currentRemainingEnergyToChargeForEv
              )

            allWindowsWithEqualVoltageDeviation.foldLeft(
              (
                currentScheduleForEv,
                currentRemainingEnergyToChargeForEv,
                currentWindowsForEv
              )
            ) {
              case (
                    (
                      currentScheduleForEv,
                      currentRemainingEnergyToChargeForEv,
                      currentWindowsForEv
                    ),
                    window
                  ) =>
                /* Check for existing schedule entry and power to charge this ev with in this time window.
                 * If such entry exists, it needs to be replaced.
                 */
                val (
                  maybeReducedScheduleForEv,
                  powerForEvForWindowBeforeUpdate
                ) =
                  checkForEarlierScheduleEntryForEvInThisWindowAndReturnPreviousPowerAndScheduleWithoutThisEntry(
                    currentScheduleForEv,
                    startTime,
                    window
                  )

                calculateUpdatedScheduleAndUpdatedRemainingEnergyToChargeAndUpdatedWindowsForEv(
                  ev,
                  powerForEvForWindowBeforeUpdate,
                  additionalPowerForEvForEachWindow,
                  window,
                  powerPerVoltageDeviation,
                  evcsModel,
                  currentWindowsForEv,
                  currentRemainingEnergyToChargeForEv,
                  maybeReducedScheduleForEv,
                  startTime
                )
            }
          }

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
        startTime,
        powerPerVoltageDeviation
      )
    } else
      (updatedWindowsForEv, updatedScheduleForEv)

  }

  /** Check if there is already a charging schedule entry for an ev for the
    * given time window. If there is an entry, is is removed from the schedule
    * to be replaced later and return the reduced scheduling and power scheduled
    * in this time window. Otherwise return the original schedule and zero
    * power.
    *
    * @param schedule
    *   the scheduling for an ev
    * @param startTime
    *   start time to convert ticks to real time
    * @param window
    *   the time window for which it should be checked whether there is an entry
    * @return
    *   the reduced or original scheduling and the found power value or zero
    *   power
    */
  private def checkForEarlierScheduleEntryForEvInThisWindowAndReturnPreviousPowerAndScheduleWithoutThisEntry(
      schedule: Seq[ChargingSchedule.Entry],
      startTime: ZonedDateTime,
      window: SchedulingTimeWindowWithVoltage
  ): (Seq[ChargingSchedule.Entry], ComparableQuantity[Power]) = {

    schedule.find(
      _.tickStart == window.start
    ) match {
      case Some(existingSchedule) =>
        /* We need to update this schedule entry with higher power. Delete the entry to add it again later. */
        val reducedScheduleForEv =
          schedule.filterNot(_ == existingSchedule)
        /* New charging power for the schedule entry is sum of previous value and additional power. */
        (reducedScheduleForEv, existingSchedule.chargingPower)
      case None =>
        /* No schedule entry exists yet for this time window. */
        (schedule, DefaultQuantities.zeroKW)
    }

  }

  /** Calculate the additional power an ev is schedule to charge with in the
    * given time window. The power comes from the power per voltage deviation
    * factor, maybe limited by a maximum value from the ev or evcs or the by
    * remaining required energy to be charged. If there are still multiple
    * windows available to distribute the charging energy for this ev on, the
    * energy is dispersed by assigning less than the maximum possible energy to
    * this time window.
    *
    * @param ev
    *   the ev
    * @param evcsModel
    *   the evcs model
    * @param powerPerVoltageDeviation
    *   the power per voltage deviation
    * @param window
    *   the window for which the additional power should be calculated
    * @param remainingEnergyToChargeForEv
    *   the remaining energy the ev needs to be charged
    * @param previousPowerForEvForWindow
    *   the previous power that was scheduled fot this window
    * @param availableWindows
    *   the still available windows for the ev
    * @return
    *   the additional power to be scheduled for the ev in this time window
    */
  private def calculateAdditionalPowerForEvForWindow(
      ev: EvModel,
      evcsModel: EvcsModel,
      powerPerVoltageDeviation: ComparableQuantity[Power],
      window: SchedulingTimeWindowWithVoltage,
      remainingEnergyToChargeForEv: ComparableQuantity[Energy],
      previousPowerForEvForWindow: ComparableQuantity[Power],
      availableWindows: Vector[SchedulingTimeWindowWithVoltage]
  ): ComparableQuantity[Power] = {

    /* If still multiple windows are available to distribute the charging energy for this ev on,
     * disperse the energy by assigning less than the maximum possible energy to the current time window.
     * Like this, the energy for all evs are distributed more smoothly instead of single evs filling
     * complete time windows.
     */
    val weighting =
      if (
        availableWindows.count(
          _.voltageDeviation.isGreaterThan(Quantities.getQuantity(0, PU))
        ) <= 1
      ) 1
      else dispersionFactor

    powerPerVoltageDeviation
      .multiply(
        window.voltageDeviation.getValue
          .doubleValue() * weighting
      )
      .min(
        evcsModel
          .getMaxAvailableChargingPower(ev)
          .subtract(previousPowerForEvForWindow)
      )
      .min(
        remainingEnergyToChargeForEv
          .divide(
            Quantities
              .getQuantity(
                window.length,
                SECOND
              )
          )
          .asType(classOf[Power])
      )
      .to(KILOWATT)
  }

  /** Calculate the remaining voltage deviation for a time window. Through the
    * connection of power and voltage deviation, from the previous voltage level
    * and the charging power, the new value can be estimated. The voltage
    * deviation is rounded to zero if it is very small. It can also become
    * negative.
    *
    * @param window
    *   the time window
    * @param additionalPowerForEvForWindow
    *   the additional power in the time window that changes the voltage level
    * @param powerPerVoltageDeviation
    *   the power per voltage deviation estimation
    * @return
    *   the new remaining voltage deviation.
    */
  private def calculateRemainingVoltageDeviation(
      window: SchedulingTimeWindowWithVoltage,
      additionalPowerForEvForWindow: ComparableQuantity[Power],
      powerPerVoltageDeviation: ComparableQuantity[Power]
  ): ComparableQuantity[Dimensionless] = {

    /* Calculate remaining voltage deviation with the determined additional charging power */
    val exactRemainingVoltageDeviation: ComparableQuantity[Dimensionless] =
      window.voltageDeviation.subtract(
        Quantities.getQuantity(
          additionalPowerForEvForWindow
            .divide(powerPerVoltageDeviation)
            .getValue
            .doubleValue(),
          PU
        )
      )
    val roundedVoltageDeviation: ComparableQuantity[Dimensionless] =
      if (
        exactRemainingVoltageDeviation
          .isLessThan(Quantities.getQuantity(voltageTolerance, PU))
        && exactRemainingVoltageDeviation
          .isGreaterThan(Quantities.getQuantity(-voltageTolerance, PU))
      )
        DefaultQuantities.zeroPU
      else exactRemainingVoltageDeviation

    roundedVoltageDeviation
  }

  /** From a list of time windows with equal voltage deviation, find the maximum
    * power the ev can charge with in all time windows without exceeding the
    * charging power limits of any window. E.g. if the max charging power of the
    * ev is 11 kW and the ev is already charging with 5 kW in one time window,
    * the max addititional power for all windows is 6 kW.
    *
    * @param allWindowsWithEqualVoltageDeviation
    *   the list of windows
    * @param currentScheduleForEv
    *   the current schedule possibly including the current charging powers for
    *   the windows
    * @param startTime
    *   start time of simulation to convert ticks to real time
    * @param ev
    *   the ev
    * @param evcsModel
    *   the evcs model
    * @return
    *   the maximum additional power to charge the ev with in all time windows
    */
  private def getMaxPossibleEqualAdditionalChargingPowerForTimeWindows(
      allWindowsWithEqualVoltageDeviation: Vector[
        SchedulingTimeWindowWithVoltage
      ],
      currentScheduleForEv: Seq[ChargingSchedule.Entry],
      startTime: ZonedDateTime,
      ev: EvModel,
      evcsModel: EvcsModel
  ): ComparableQuantity[Power] = {

    /* Get current total power to charge this ev with in this time window. This is necessary, as maybe
     * previously a charging power for this window was scheduled that needs to be increased now.
     */
    val maxPreviousPowerForEvForWindowsBeforeUpdate: ComparableQuantity[Power] =
      allWindowsWithEqualVoltageDeviation
        .foldLeft(DefaultQuantities.zeroKW)(
          (
              maxPower: ComparableQuantity[Power],
              window: SchedulingTimeWindowWithVoltage
          ) => {
            currentScheduleForEv.find(
              _.tickStart == window.start
            ) match {
              case Some(existingSchedule) =>
                if (existingSchedule.chargingPower.isGreaterThan(maxPower)) {
                  existingSchedule.chargingPower
                } else maxPower
              case None =>
                maxPower
            }
          }
        )

    evcsModel
      .getMaxAvailableChargingPower(ev)
      .subtract(maxPreviousPowerForEvForWindowsBeforeUpdate)

  }

  /** Calculate the desired additional power to charge an ev with in all windows
    * of a set. If there is time window with a lower voltage deviation value,
    * the charging is power is limited to let the windows reach the same voltage
    * value but not fall below that. The additional power is limited by the
    * maximum possible power constrained by maximum charging powers in all
    * windows and the remaining required energy of the ev and.
    *
    * @param allWindowsWithEqualVoltageDeviation
    *   the windows to set the charging power for
    * @param availableWindows
    *   the still available windows for the ev which might include a window with
    *   worse voltage
    * @param window
    *   one of the windows with equal voltage levels
    * @param powerPerVoltageDeviation
    *   the power per voltage deviation estimation
    * @param maxPossibleAdditionalChargingPowerForWindows
    *   the maximum possible additional charging power for the windows
    * @param currentRemainingEnergyToChargeForEv
    *   the remaining energy the ev needs to charge
    * @return
    *   the additional charging power for all windows of the set
    */
  private def calculateAdditionalPowerForEvForEachWindow(
      allWindowsWithEqualVoltageDeviation: Vector[
        SchedulingTimeWindowWithVoltage
      ],
      availableWindows: Vector[SchedulingTimeWindowWithVoltage],
      window: SchedulingTimeWindowWithVoltage,
      powerPerVoltageDeviation: ComparableQuantity[Power],
      maxPossibleAdditionalChargingPowerForWindows: ComparableQuantity[Power],
      currentRemainingEnergyToChargeForEv: ComparableQuantity[Energy]
  ): ComparableQuantity[Power] = {

    val totalLengthOfAllWindowsWithEqualVoltageDeviation =
      allWindowsWithEqualVoltageDeviation.foldLeft(0d)(
        (sumLength: Double, window: SchedulingTimeWindowWithVoltage) => {
          sumLength + window.length
        }
      )

    /* Find optional window with second max deviation */
    val windowWithSecondMaxNegativeDeviation
        : Option[SchedulingTimeWindowWithVoltage] =
      availableWindows
        .filter(
          _.voltageDeviation
            .isLessThan(
              window.voltageDeviation
                .subtract(Quantities.getQuantity(voltageTolerance, PU))
            )
          // make sure not to take window with minimal less voltage deviation which is included in allWindowsWithEqualVoltageDeviation (ugly solution)
        )
        .maxByOption(_.voltageDeviation)

    /* If there is a window with second max deviation (= bigger negative deviation), fill all windows
      with max deviation (=lower negative) to same deviation as the second max deviation.
      If there in so such window, this means that out best option is also the worst and we have no orientation
      or comparison. Therefore, split the remaining energy that needs to be charged equally on all windows.
     */
    windowWithSecondMaxNegativeDeviation match {

      case Some(windowWithSecondMaxDeviation) =>
        val voltageDeviationDifference: ComparableQuantity[Dimensionless] =
          window.voltageDeviation
            .subtract(
              windowWithSecondMaxDeviation.voltageDeviation
            )

        /* Calculate charging power for ev for this time window. This comes from the power per voltage
         * deviation factor, maybe limited by a maximum value from the ev or evcs or the by remaining
         * required energy to be charged
         */
        powerPerVoltageDeviation
          .multiply(
            voltageDeviationDifference.getValue.doubleValue()
          )
          .min(
            currentRemainingEnergyToChargeForEv
              .divide(
                Quantities.getQuantity(
                  totalLengthOfAllWindowsWithEqualVoltageDeviation,
                  SECOND
                )
              )
              .asType(classOf[Power])
          )
          .min(maxPossibleAdditionalChargingPowerForWindows)
          .to(KILOWATT)

      case None =>
        /* Calculate charging power for ev for this time window. This comes from the remaining
         * required energy to be charged, maybe limited by a maximum value from the ev or evcs or the by
         */
        currentRemainingEnergyToChargeForEv
          .divide(
            Quantities.getQuantity(
              totalLengthOfAllWindowsWithEqualVoltageDeviation,
              SECOND
            )
          )
          .asType(classOf[Power])
          .min(maxPossibleAdditionalChargingPowerForWindows)
          .to(KILOWATT)

    }

  }

  /** Update the charging schedule of the ev for a given time window, update the
    * remaining energy the ev needs to charge and update the time window with
    * the estimated new voltage deviation due to higher charging power.
    *
    * @param ev
    *   the ev
    * @param powerForEvForWindowBeforeUpdate
    *   the power scheduled for the ev and window before this update
    * @param additionalPowerForEvForWindow
    *   the additional charging power for the ev in this window
    * @param window
    *   the time window
    * @param powerPerVoltageDeviation
    *   the power per voltage deviation estimation
    * @param evcsModel
    *   the evcs model
    * @param currentWindowsForEv
    *   the time windows with information on estimated voltage deviations before
    *   this update
    * @param currentRemainingEnergyToChargeForEv
    *   the remaining energy the ev needs to charge before this update
    * @param maybeReducedScheduleForEv
    *   the previous charging schedule of the ev
    * @param startTime
    *   the simulation start time to convert ticks to real times
    * @return
    *   the updated time windows with voltage information, the remaining energy
    *   to charge, and the updated charging schedule for the ev
    */
  private def calculateUpdatedScheduleAndUpdatedRemainingEnergyToChargeAndUpdatedWindowsForEv(
      ev: EvModel,
      powerForEvForWindowBeforeUpdate: ComparableQuantity[Power],
      additionalPowerForEvForWindow: ComparableQuantity[Power],
      window: SchedulingTimeWindowWithVoltage,
      powerPerVoltageDeviation: ComparableQuantity[Power],
      evcsModel: EvcsModel,
      currentWindowsForEv: Vector[
        (SchedulingTimeWindowWithVoltage, Boolean)
      ],
      currentRemainingEnergyToChargeForEv: ComparableQuantity[Energy],
      maybeReducedScheduleForEv: Seq[ChargingSchedule.Entry],
      startTime: ZonedDateTime
  ) = {

    /* Calculate new total power to charge this ev with in this time window */
    val updatedPowerForEvForWindow: ComparableQuantity[Power] =
      powerForEvForWindowBeforeUpdate
        .add(additionalPowerForEvForWindow)

    val remainingVoltageDeviation =
      calculateRemainingVoltageDeviation(
        window,
        additionalPowerForEvForWindow,
        powerPerVoltageDeviation
      )

    /* Update this window with updated remaining voltage deviation */
    val updatedWindow = window.copy(
      voltageDeviation = remainingVoltageDeviation
    )

    /* Block window if EV charges with max possible power in this window */
    val windowBlockedForEv =
      updatedPowerForEvForWindow.isGreaterThanOrEqualTo(
        evcsModel.getMaxAvailableChargingPower(ev)
      )

    /* Replace the time window with the updated version */
    val updatedWindowsForEv = currentWindowsForEv.filterNot(
      _._1 == window
    ) :+ (updatedWindow, windowBlockedForEv)

    val updatedRemainingEnergyToChargeForEv: ComparableQuantity[Energy] =
      calculateRemainingEnergyToBeChargedAfterThisUpdate(
        additionalPowerForEvForWindow,
        window,
        currentRemainingEnergyToChargeForEv
      )

    /* Add schedule entry for ev and this time window */
    val updatedScheduleForEv =
      maybeReducedScheduleForEv :+ ChargingSchedule.Entry(
        window.start,
        window.end,
        updatedPowerForEvForWindow
      )

    (
      updatedScheduleForEv,
      updatedRemainingEnergyToChargeForEv,
      updatedWindowsForEv
    )

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
      schedulingTimeWindows: Vector[SchedulingTimeWindowWithVoltage]
  ): Vector[(SchedulingTimeWindowWithVoltage, Boolean)] = {

    schedulingTimeWindows
      .foldLeft(Vector.empty[SchedulingTimeWindowWithVoltage])(
        (
            relevantWindows: Vector[SchedulingTimeWindowWithVoltage],
            window: SchedulingTimeWindowWithVoltage
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

  /** Finds and returns the minimum voltage value in a list of voltages.
    *
    * @param predictedVoltages
    *   the list of voltage values
    * @return
    *   the minimum value
    */
  private def getMinimumVoltageInPredictedVoltages(
      predictedVoltages: Vector[PredictedVoltage]
  ): ComparableQuantity[Dimensionless] = {
    predictedVoltages.foldLeft(Quantities.getQuantity(10, PU))(
      (
          minVoltage: ComparableQuantity[Dimensionless],
          voltage: PredictedVoltage
      ) => {
        if (voltage.voltage.isLessThan(minVoltage)) voltage.voltage
        else minVoltage
      }
    )
  }

  /** Create the scheduling time windows required for the scheduling of the evs.
    * The list of scheduling time windows is created based on the predicted
    * voltages and their time windows and the departure times of the evs. For
    * each time window, a SchedulingTimeWindow is created with according
    * information.
    *
    * @param predictedVoltages
    *   the predicted voltage time windows
    * @param departureTimes
    *   the departure times of all evs to schedule
    * @param minVoltageOfAllTimeWindows
    *   the minimum voltage of all voltages as a reference value to calculate
    *   deviations
    * @param startTime
    *   the start time of the simulation to obtain real times from ticks
    * @param evs
    *   the evs to schedule
    * @return
    *   list of scheduling time windows
    */
  private def getSchedulingTimeWindows(
      predictedVoltages: Vector[PredictedVoltage],
      departureTimes: Vector[ZonedDateTime],
      minVoltageOfAllTimeWindows: ComparableQuantity[Dimensionless],
      startTime: ZonedDateTime,
      evs: Set[EvModel]
  ): Vector[SchedulingTimeWindowWithVoltage] = {
    predictedVoltages
      .foldLeft(
        Vector.empty[SchedulingTimeWindowWithVoltage]
      ) {
        (
            timeWindows: Vector[SchedulingTimeWindowWithVoltage],
            entry: PredictedVoltage
        ) =>
          {

            val departureTimesInThisTimeWindow: Vector[ZonedDateTime] =
              departureTimes
                .filter(_.isAfter(entry.start))
                .filter(_.isBefore(entry.end))
                .sorted

            if (departureTimesInThisTimeWindow.nonEmpty) {

              var x = Vector.empty[SchedulingTimeWindowWithVoltage]
              val size: Int = departureTimesInThisTimeWindow.size

              x = x :+ SchedulingTimeWindowWithVoltage(
                startTime.until(entry.start, ChronoUnit.SECONDS),
                startTime
                  .until(departureTimesInThisTimeWindow(0), ChronoUnit.SECONDS),
                entry.voltage,
                entry.voltage.subtract(minVoltageOfAllTimeWindows),
                entry.start.until(
                  departureTimesInThisTimeWindow(0),
                  ChronoUnit.SECONDS
                ) * entry.voltage
                  .subtract(minVoltageOfAllTimeWindows)
                  .getValue
                  .doubleValue(),
                getEvsStillParkedAtThisTime(evs, entry.start, startTime)
              )
              for (i <- 0 until size) {
                if (i < size - 1) {
                  x = x :+ SchedulingTimeWindowWithVoltage(
                    startTime.until(
                      departureTimesInThisTimeWindow(i),
                      ChronoUnit.SECONDS
                    ),
                    startTime.until(
                      departureTimesInThisTimeWindow(i + 1),
                      ChronoUnit.SECONDS
                    ),
                    entry.voltage,
                    entry.voltage.subtract(minVoltageOfAllTimeWindows),
                    departureTimesInThisTimeWindow(i).until(
                      departureTimesInThisTimeWindow(i + 1),
                      ChronoUnit.SECONDS
                    ) * entry.voltage
                      .subtract(minVoltageOfAllTimeWindows)
                      .getValue
                      .doubleValue(),
                    getEvsStillParkedAtThisTime(
                      evs,
                      departureTimesInThisTimeWindow(i),
                      startTime
                    )
                  )
                } else {
                  x = x :+ SchedulingTimeWindowWithVoltage(
                    startTime.until(
                      departureTimesInThisTimeWindow(i),
                      ChronoUnit.SECONDS
                    ),
                    startTime.until(entry.end, ChronoUnit.SECONDS),
                    entry.voltage,
                    entry.voltage.subtract(minVoltageOfAllTimeWindows),
                    departureTimesInThisTimeWindow(i).until(
                      entry.end,
                      ChronoUnit.SECONDS
                    ) * entry.voltage
                      .subtract(minVoltageOfAllTimeWindows)
                      .getValue
                      .doubleValue(),
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

              timeWindows :+ SchedulingTimeWindowWithVoltage(
                startTime.until(entry.start, ChronoUnit.SECONDS),
                startTime.until(entry.end, ChronoUnit.SECONDS),
                entry.voltage,
                entry.voltage.subtract(minVoltageOfAllTimeWindows),
                entry.start.until(entry.end, ChronoUnit.SECONDS) * entry.voltage
                  .subtract(minVoltageOfAllTimeWindows)
                  .getValue
                  .doubleValue(),
                getEvsStillParkedAtThisTime(evs, entry.start, startTime)
              )

            }

          }
      }
      .filter(_.parkedEvs.nonEmpty)
      .filterNot(x => x.start == x.end)
      .sortBy { case SchedulingTimeWindowWithVoltage(start, _, _, _, _, _) =>
        start
      }
  }

  /** Calculate the total length and size from a list of scheduling time
    * windows. The size is calculated as length * height, where the height means
    * the voltage deviation to the reference value, which is hold in the
    * scheduling time windows.
    *
    * @param schedulingTimeWindows
    *   the list of scheduling time windows
    * @return
    *   the total (time) length and size of the time windows
    */
  private def calculateTotalTimeWindowLengthAndSize(
      schedulingTimeWindows: Vector[SchedulingTimeWindowWithVoltage]
  ): (Long, Double) = {
    schedulingTimeWindows.foldLeft(0L, 0d)(
      (sums: (Long, Double), timeWindow: SchedulingTimeWindowWithVoltage) => {
        val length = sums._1 + timeWindow.length
        val size = if (timeWindow.size > 0) {
          sums._2 + timeWindow.size
        } else {
          sums._2
        }
        (length, size)
      }
    )
  }

  /** From the total size and length of the time windows and the total required
    * energy to be charged by the EVs, a ratio power per voltage deviation can
    * be obtained. The idea is to build a connection between a charging power
    * and a voltage change. The total size (time * voltage deviation) is
    * equivalent to the total energy. Then the medium voltage deviation is
    * equivalent to the medium charging power to charge the total energy.
    *
    * @param totalTimeWindowSize
    *   the total time window size
    * @param totalTimeWindowLength
    *   the total time window length
    * @param totalEnergyToBeChargedBySchedulableEvs
    *   the total energy to be charged by the evs
    * @return
    *   the power per voltage deviation
    */
  private def calculatePowerPerVoltageDeviation(
      totalTimeWindowSize: Double,
      totalTimeWindowLength: Long,
      totalEnergyToBeChargedBySchedulableEvs: ComparableQuantity[Energy]
  ): ComparableQuantity[Power] = {

    val averageVoltageDeviation: Double =
      totalTimeWindowSize / totalTimeWindowLength
    val averageChargingPower: ComparableQuantity[Power] =
      totalEnergyToBeChargedBySchedulableEvs
        .divide(Quantities.getQuantity(totalTimeWindowLength, SECOND))
        .asType(classOf[Power])
        .to(KILOWATT)
    val powerPerVoltageDeviation: ComparableQuantity[Power] =
      averageChargingPower.divide(averageVoltageDeviation)
    powerPerVoltageDeviation
  }

  /** Update the list of scheduling time windows with a subset of updated time
    * windows.
    * @param previousSchedulingTimeWindows
    *   previous list
    * @param subSetOfUpdatedSchedulingTimeWindows
    *   subset of scheduling window with newer information
    * @return
    *   updated and ordered list of scheduling time windows
    */
  private def updateSchedulingTimeWindows(
      previousSchedulingTimeWindows: Vector[SchedulingTimeWindowWithVoltage],
      subSetOfUpdatedSchedulingTimeWindows: Vector[
        SchedulingTimeWindowWithVoltage
      ]
  ): Vector[SchedulingTimeWindowWithVoltage] = {

    /* Update the scheduling time windows with the updated time windows from this ev scheduling */
    (previousSchedulingTimeWindows.foldLeft(previousSchedulingTimeWindows)(
      (
          filteredWindows: Vector[SchedulingTimeWindowWithVoltage],
          entry: SchedulingTimeWindowWithVoltage
      ) => {
        subSetOfUpdatedSchedulingTimeWindows
          .find(_.start == entry.start) match {
          case Some(_) => filteredWindows.filterNot(_ == entry)
          case None    => filteredWindows
        }
      }
    ) ++ subSetOfUpdatedSchedulingTimeWindows).sortBy {
      case SchedulingTimeWindowWithVoltage(start, _, _, _, _, _) => start
    }
  }

}
