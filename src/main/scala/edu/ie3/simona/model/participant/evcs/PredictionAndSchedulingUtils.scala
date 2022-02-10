/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.participant.evcs.SchedulingTimeWindows.SchedulingTimeWindow
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import edu.ie3.util.scala.quantities.DefaultQuantities
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.SECOND

import java.time.{DayOfWeek, ZonedDateTime}
import java.time.temporal.ChronoUnit
import javax.measure.quantity.{Energy, Power}

object PredictionAndSchedulingUtils {

  /** Time stamp including a day, hour, and minute
    *
    * @param dayOfWeek
    *   day of week
    * @param hour
    *   hour of day
    * @param minute
    *   minute oh hour
    */
  case class TimeStamp(
      dayOfWeek: DayOfWeek,
      hour: Int,
      minute: Int
  ) extends Ordered[TimeStamp] {
    def getMinuteOfWeek: Int = {
      this.dayOfWeek.getValue * 24 * 60 + this.hour * 60 + this.minute
    }

    def isBetween(start: TimeStamp, end: TimeStamp): Boolean = {
      if (start.getMinuteOfWeek <= end.getMinuteOfWeek) {
        (start.getMinuteOfWeek <= this.getMinuteOfWeek
        && this.getMinuteOfWeek < end.getMinuteOfWeek)
      } else { // start is after end -> e.g. from Sunday to Monday
        (start.getMinuteOfWeek <= this.getMinuteOfWeek
        || this.getMinuteOfWeek < end.getMinuteOfWeek)
      }
    }

    def minutesUntil(timeStamp: TimeStamp): Int = {
      if (this.getMinuteOfWeek <= timeStamp.getMinuteOfWeek) {
        val timeInMinutes = timeStamp.getMinuteOfWeek - this.getMinuteOfWeek
        timeInMinutes
      } else {
        val timeInMinutes =
          7 * 24 * 60 - (this.getMinuteOfWeek - timeStamp.getMinuteOfWeek)
        timeInMinutes
      }
    }

    override def compare(that: TimeStamp): Int = {
      val dayCompare = dayOfWeek.compareTo(that.dayOfWeek)
      if (dayCompare != 0)
        dayCompare
      else {
        val hourCompare = hour.compare(that.hour)
        if (hourCompare != 0)
          hourCompare
        else minute.compare(that.minute)
      }
    }
  }

  /** Find the evs currently charging at the charging station that need to be
    * charged with maximum power until departure due to a low SoC.
    *
    * @param evcsModel
    *   evcs model to calculate for / with
    * @param evs
    *   currently parked evs at the charging station
    * @param currentTime
    *   current tick
    * @param startTime
    *   start time of simulation
    * @return
    *   set of evs that need ot be charged with maximum power until departure
    */
  def findEvsThatNeedToBeChargedWithMaximumPower(
      evcsModel: EvcsModel,
      evs: Set[EvModel],
      currentTime: ZonedDateTime,
      startTime: ZonedDateTime
  ): Set[EvModel] = {
    evs.foldLeft(Set.empty[EvModel])((evs: Set[EvModel], ev) => {
      val remainingParkingTime = currentTime.until(
        ev.getDepartureTick.toLong.toDateTime(startTime),
        ChronoUnit.SECONDS
      )
      if (
        ev.getEStorage
          .subtract(ev.getStoredEnergy)
          .to(KILOWATTHOUR)
          .divide(Quantities.getQuantity(remainingParkingTime, SECOND))
          .asType(classOf[Power])
          .to(KILOWATT)
          .isGreaterThanOrEqualTo(evcsModel.getMaxAvailableChargingPower(ev))
      ) {
        evs + ev
      } else evs
    })
  }

  /** Collect and return all departure times and required energies of the
    * schedulable evs in separate lists.
    *
    * @param evs
    *   evs that are schedulable
    * @param startTime
    *   the start time of the simulation to obtain real times from ticks
    * @return
    *   tuple of lists of departure times and required energies
    */
  def getDepartureTimesAndRequiredEnergyOfAllEvs(
      evs: Set[EvModel],
      startTime: ZonedDateTime
  ): (Vector[ZonedDateTime], Vector[ComparableQuantity[Energy]]) = {
    val departureTimesAndRequiredEnergies = evs
      .map { ev =>
        {
          (
            ev.getDepartureTick.toLong.toDateTime(startTime),
            ev.getEStorage.subtract(ev.getStoredEnergy)
          )
        }
      }
      .toVector
      .sortBy { case (time, _) =>
        time
      }

    val departureTimes: Vector[ZonedDateTime] =
      departureTimesAndRequiredEnergies.map(x => x._1)
    val requiredEnergies: Vector[ComparableQuantity[Energy]] =
      departureTimesAndRequiredEnergies.map(x => x._2)

    (departureTimes, requiredEnergies)
  }

  /** Collect and return all evs that are still parked to charge at a specific
    * time.
    *
    * @param evs
    *   the evs
    * @param time
    *   the time for which the parking evs shall be collected
    * @param startTime
    *   the start time to obtain real times from ticks
    * @return
    *   the list of evs that are still parking at the specific time
    */
  def getEvsStillParkedAtThisTime(
      evs: Set[EvModel],
      time: ZonedDateTime,
      startTime: ZonedDateTime
  ): Set[EvModel] = {
    evs.filter(_.getDepartureTick.toLong.toDateTime(startTime).isAfter(time))
  }

  /** Calculate the sum of all energies.
    *
    * @param energies
    *   list of energy values
    * @return
    *   sum of values
    */
  def calculateSumOfEnergies(
      energies: Vector[ComparableQuantity[Energy]]
  ): ComparableQuantity[Energy] = {
    energies.foldLeft(Quantities.getQuantity(0, KILOWATTHOUR))(
      (
          sumEnergy: ComparableQuantity[Energy],
          energy: ComparableQuantity[Energy]
      ) => {
        sumEnergy.add(energy)
      }
    )
  }

  /** Calculate the remaining energy the ev needs to charge after the increase
    * of power for this window. Therefore, the additional energy charged in this
    * time window is calculated an subtracted from the previous remaining energy
    * left to charge. If the remaining energy is very small, it is rounded to
    * zero.
    *
    * @param additionalPowerForEvForWindow
    *   the additional charging power for the given window.
    * @param window
    *   the time window the power change is for
    * @param previousRemainingEnergyToCharge
    *   the previous remaining energy the ev needs to charge
    * @return
    *   the remaining energy to charge the ev after this power update
    */
  def calculateRemainingEnergyToBeChargedAfterThisUpdate(
      additionalPowerForEvForWindow: ComparableQuantity[Power],
      window: SchedulingTimeWindow,
      previousRemainingEnergyToCharge: ComparableQuantity[Energy]
  ): ComparableQuantity[Energy] = {

    /* Calculate the charged energy for the ev in this time window */
    val additionalChargedEnergyForEvInThisWindow =
      additionalPowerForEvForWindow
        .multiply(
          Quantities
            .getQuantity(window.length, SECOND)
        )
        .asType(classOf[Energy])
        .to(KILOWATTHOUR)
    // logger.info(
    //  s"Additional energy charged in this window: $additionalChargedEnergyForEvInThisWindow"
    // )

    /* Update remaining energy that needs to be distributed on the time windows to fully charge the ev.
     * Round if value is close to zero to avoid to many iterations. */
    val exactRemainingEnergyToChargeForEv =
      previousRemainingEnergyToCharge
        .subtract(additionalChargedEnergyForEvInThisWindow)
    val updatedRemainingEnergyToChargeForEv =
      if (
        exactRemainingEnergyToChargeForEv
          .isLessThan(Quantities.getQuantity(0.001, KILOWATTHOUR))
        && exactRemainingEnergyToChargeForEv
          .isGreaterThan(
            Quantities.getQuantity(-0.001, KILOWATTHOUR)
          )
      )
        DefaultQuantities.zeroKWH
      else exactRemainingEnergyToChargeForEv

    updatedRemainingEnergyToChargeForEv
  }

}
