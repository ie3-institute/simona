/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.gridoriented

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.model.participant.evcs.EvcsModel
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKWH
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.SECOND

import javax.measure.quantity.{Dimensionless, Energy, Power}
import scala.annotation.tailrec

object GridOrientedCurrentPrice extends LazyLogging {

  // TODO: Another option for grid-oriented price signal generation would be to consider the
  //  predicted voltages in the next hours. Not implemented as part of my thesis due to lack of time.

  /** Calculate price signal between 0 and 1 based on the average charging power
    * of the evcs scheduled from current time in a specified time window length.
    * The higher the average charging power for the currently parking evs is in
    * this time interval, the higher is the price signal.
    * @param evcsModel
    *   the evcs model
    * @param currentTick
    *   the current tick the price signal is requested for
    * @param timeLength
    *   the time length of the considered window to calculate the average power
    *   for
    * @param state
    *   the evcs state
    * @return
    *   optional price signal between 0 and 1
    */
  def calculateCurrentPriceGridOriented(
      evcsModel: EvcsModel,
      currentTick: Long,
      timeLength: Int,
      state: EvcsModel.EvcsState
  ): Option[Double] = {

    if (state.evs.isEmpty) {
      Some(0d)
    } else {
      val scheduleEntries =
        state.schedule.flatMap(_._2).flatMap(_.schedule).toSet

      /* Filter schedule for relevant interval */
      val filteredSchedule = scheduleEntries
        .filter(_.tickStop > currentTick)
        .filter(_.tickStart < currentTick + timeLength)

      val allTicks = evcsModel.getAllTicksOfSchedule(filteredSchedule)

      /* Filter for relevant ticks, including the last tick before the interval and all ticks in the interval */
      val relevantTicksForInterval = {
        allTicks
          .filter(_ < currentTick)
          .maxOption match {
          case Some(firstTickReachingIntoWindow) =>
            allTicks
              .filter(_ >= currentTick)
              .filter(
                _ < currentTick + timeLength
              ) + firstTickReachingIntoWindow
          case None =>
            allTicks
              .filter(_ >= currentTick)
              .filter(_ < currentTick + timeLength)
        }

      }

      /* Calculate power for relevant ticks in sorted vector */
      val ticksAndPower: Vector[(Long, ComparableQuantity[Power])] =
        relevantTicksForInterval
          .map { tick =>
            val activePower = filteredSchedule
              .foldLeft(
                Quantities.getQuantity(0, KILOWATT)
              )((p, entry) => {
                if (entry.tickStart <= tick && entry.tickStop > tick)
                  p.add(entry.chargingPower)
                else p
              })
              .to(KILOWATT)

            (tick, activePower)

          }
          .toVector
          .sortBy(x => x._1)

      if (ticksAndPower.nonEmpty) {

        val averagePowerInRelevantInterval = getAveragePower(
          ticksAndPower,
          currentTick,
          currentTick + timeLength,
          0,
          zeroKWH
        )

        val currentPriceSignal = averagePowerInRelevantInterval
          .to(KILOWATT)
          .divide(
            evcsModel.sRated.multiply(evcsModel.chargingPoints).to(KILOWATT)
          )
          .asType(classOf[Dimensionless])
          .getValue
          .doubleValue()

        Some(currentPriceSignal)

      } else {
        Some(0d)
      }

    }

  }

  /** Calculate the average charging power in the specified time window based on
    * the evcs charging schedule. The calculation happens recursively.
    * @param ticksAndPower
    *   the ticks and corresponding total charging power of the evcs
    * @param startTick
    *   the start tick of the time interval to calculate the average power for
    * @param endTick
    *   the end tick of the time interval to calculate the average power for
    * @param numberOfEntry
    *   the current number of the entry of the vector of ticks and powers,
    *   increased recursively
    * @param energy
    *   the current total energy charged in the time interval, updated
    *   recursively
    * @return
    *   the average charging power in the time interval from the first entry
    *   start and stated length
    */
  @tailrec
  private def getAveragePower(
      ticksAndPower: Vector[(Long, ComparableQuantity[Power])],
      startTick: Long,
      endTick: Long,
      numberOfEntry: Int,
      energy: ComparableQuantity[Energy]
  ): ComparableQuantity[Power] = {

    val updatedEnergy: ComparableQuantity[Energy] = {

      if (numberOfEntry + 1 < ticksAndPower.size) {
        energy.add(
          ticksAndPower(numberOfEntry)._2
            .multiply(
              Quantities.getQuantity(
                math.min(ticksAndPower(numberOfEntry + 1)._1, endTick) - math
                  .max(
                    ticksAndPower(
                      numberOfEntry
                    )._1,
                    startTick
                  ),
                SECOND
              )
            )
            .asType(classOf[Energy])
            .to(KILOWATTHOUR)
        )
      } else {
        energy.add(
          ticksAndPower(numberOfEntry)._2
            .multiply(
              Quantities.getQuantity(
                endTick - math.max(ticksAndPower(numberOfEntry)._1, startTick),
                SECOND
              )
            )
            .asType(classOf[Energy])
            .to(KILOWATTHOUR)
        )
      }
    }

    if (numberOfEntry + 1 < ticksAndPower.size) {
      getAveragePower(
        ticksAndPower,
        startTick,
        endTick,
        numberOfEntry + 1,
        updatedEnergy
      )
    } else {
      updatedEnergy
        .divide(Quantities.getQuantity(endTick - startTick, SECOND))
        .asType(classOf[Power])
        .to(KILOWATT)
    }

  }

}
