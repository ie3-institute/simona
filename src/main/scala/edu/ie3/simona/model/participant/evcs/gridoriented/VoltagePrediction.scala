/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.gridoriented

import edu.ie3.simona.exceptions.{
  InvalidParameterException,
  PredictionException
}
import edu.ie3.simona.model.participant.evcs.PredictionAndSchedulingUtils.TimeStamp
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.{DayOfWeek, ZonedDateTime}
import javax.measure.Quantity
import javax.measure.quantity.Dimensionless

object VoltagePrediction {

  /* Set resolution of voltage windows in minutes.
   * Maximum value: 60 (?) */
  val voltageResolution: Int = 10

  /** Entry in the voltage time table, which includes the history of voltage
    * values for a whole week.
    *
    * @param fromTimeStamp
    *   start of time frame
    * @param untilTimeStamp
    *   end of time frame
    * @param voltage
    *   voltage for this time frame
    */
  case class VoltageTimeTableEntry(
      fromTimeStamp: TimeStamp,
      untilTimeStamp: TimeStamp,
      voltage: ComparableQuantity[Dimensionless]
  )

  /** Predicted voltage for a specific time in the future.
    *
    * @param start
    *   start of time window
    * @param end
    *   end of time window
    * @param voltage
    *   predicted voltage in this time window
    */
  case class PredictedVoltage(
      start: ZonedDateTime,
      end: ZonedDateTime,
      voltage: ComparableQuantity[Dimensionless]
  )

  private type VoltageReference =
    (TimeStamp, (ComparableQuantity[Dimensionless], Int))

  /** Calculate a time table for a reference week based on the known voltages
    * from memory. The reference voltages are calculated on a defined minute
    * basis (max 60min), if the resolution of voltage updates is higher. Each
    * known voltage value for a time window is weighted equally.
    *
    * @param voltages
    *   known voltages from history
    * @return
    *   vector of time table entries including the voltage value and the
    *   timeframe
    */
  def calculateReferenceVoltageTimeTable(
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]]
  ): Vector[VoltageTimeTableEntry] = {

    val voltageReferenceMap: Map[
      TimeStamp,
      (ComparableQuantity[Dimensionless], Int)
    ] = voltages.foldLeft(
      Map.empty: Map[
        TimeStamp,
        (ComparableQuantity[Dimensionless], Int)
      ]
    )(
      (
          referenceVoltages: Map[
            TimeStamp,
            (ComparableQuantity[Dimensionless], Int)
          ],
          voltage
      ) => {

        val day: DayOfWeek = voltage._1.getDayOfWeek
        val hour: Int = voltage._1.getHour
        val minute: Int =
          voltage._1.getMinute - (voltage._1.getMinute % voltageResolution)

        referenceVoltages.get(TimeStamp(day, hour, minute)) match {

          case Some(entry) =>
            /* There already is an entry. Update the entry with equal weighting */
            val updatedNumberOfValues = entry._2 + 1
            val updatedPrediction = entry._1
              .multiply(entry._2)
              .add(voltage._2)
              .divide(updatedNumberOfValues)

            referenceVoltages + (TimeStamp(
              day,
              hour,
              minute
            ) -> (updatedPrediction, updatedNumberOfValues))

          case None =>
            /* There is no entry yet. Add new entry */
            referenceVoltages + (TimeStamp(
              day,
              hour,
              minute
            ) -> (voltage._2, 1))
        }
      }
    )

    /* Order voltage entries based on time stamp */
    val orderedVoltageReferenceMap = voltageReferenceMap.toVector.sortBy(_._1)
    blurVoltages(orderedVoltageReferenceMap, 3).toVector
  }

  /** Blur voltages and build equivalent [[VoltageTimeTableEntry]]s
    *
    * @param orderedVoltageReferences
    *   Ordered voltage references
    * @param windowLength
    *   Length of the blurring window
    * @return
    *   A sequence of [[VoltageTimeTableEntry]]s
    */
  private def blurVoltages(
      orderedVoltageReferences: Seq[VoltageReference],
      windowLength: Int
  ): Seq[VoltageTimeTableEntry] =
    enhanceVoltageReferences(orderedVoltageReferences, windowLength)
      .sliding(windowLength)
      .map { slidingWindow =>
        val lastTwo = slidingWindow.slice(windowLength - 2, windowLength)
        lastTwo.headOption
          .flatMap(head => lastTwo.lastOption.map((head, _)))
          .map { case ((startTime, _), (endTime, _)) =>
            (startTime, endTime)
          }
          .zip(mean(slidingWindow.map { case (_, (quantity, _)) =>
            quantity
          })) match {
          case Some(((startTime, endTime), mean)) =>
            VoltageTimeTableEntry(startTime, endTime, mean)
          case None =>
            throw PredictionException(
              s"Unable to build blurred values for sliding window '$slidingWindow'."
            )
        }
      }
      .toSeq

  /** Pre- and append entries to the voltage references to ensure proper
    * blurring afterwards
    *
    * @param orderedVoltageReferences
    *   Ordered sequence of voltage references
    * @param windowLength
    *   Length of the blurring window
    * @return
    *   Voltage references with pre- and appended values
    */
  private def enhanceVoltageReferences(
      orderedVoltageReferences: Seq[VoltageReference],
      windowLength: Int
  ): Seq[VoltageReference] = {
    val neededAmount =
      windowLength - 2 // The value always apply for the time stamp and the successor (2 time stamps)
    val tail = orderedVoltageReferences.slice(
      orderedVoltageReferences.length - neededAmount,
      orderedVoltageReferences.length
    )
    if (tail.length < neededAmount)
      throw PredictionException(
        "Unable to determine sliding window. Window length too long."
      )
    val head = orderedVoltageReferences.slice(0, neededAmount)
    if (head.length < neededAmount)
      throw PredictionException(
        "Unable to determine sliding window. Window length too long."
      )
    tail ++ orderedVoltageReferences ++ head
  }

  /** Determine the mean of a given sequence of quantities
    *
    * @param quantities
    *   The quantities to average
    * @tparam Q
    *   The type of quantity
    * @return
    *   The mean of all quantities
    */
  private def mean[Q <: Quantity[Q]](
      quantities: Seq[ComparableQuantity[Q]]
  ): Option[ComparableQuantity[Q]] =
    quantities.headOption.map { head =>
      quantities
        .slice(1, quantities.length)
        .foldLeft(head)((sum, summand) => sum.add(summand))
        .divide(quantities.length)
    }

  /** Get time windows with predicted voltage values for the relevant time until
    * departure of the last EV based on the reference voltages in the voltage
    * time table.
    *
    * @param currentTime
    *   current time to know where to start in the voltage time table
    * @param endTime
    *   the end time (e.g. departure time of the last ev) until which time the
    *   voltages are required (can be max 7 days ahead of current time)
    * @param voltageTimeTable
    *   the voltage time table with reference values for a whole week
    * @return
    *   vector of predicted voltages with value and timeframe, ordered in time
    */
  def getPredictedVoltagesForRelevantTimeWindowBasedOnReferenceVoltages(
      currentTime: ZonedDateTime,
      endTime: ZonedDateTime,
      voltageTimeTable: Vector[VoltageTimeTableEntry]
  ): Vector[PredictedVoltage] = {

    if (endTime.isAfter(currentTime.plusDays(7)))
      throw new InvalidParameterException(
        "Predicted voltages can currently only be created up to one week ahead of the current time"
      )

    val currentTimeStamp =
      TimeStamp(
        currentTime.getDayOfWeek,
        currentTime.getHour,
        currentTime.getMinute
      )

    val currentVoltage = getVoltageTimeTableEntryThisTimeStampBelongsTo(
      currentTimeStamp,
      voltageTimeTable
    )

    /* The voltage prediction for the current time frame the current time belongs to has to be added first,
     * afterwards the predictions until one week form current time are created based on the voltage time table.
     */

    PredictedVoltage(
      currentTime, // .plusMinutes(currentTimeStamp.minutesUntil(currentVoltage.fromTimeStamp)),
      currentTime
        .plusMinutes(
          currentTimeStamp.minutesUntil(currentVoltage.untilTimeStamp)
        )
        .minusSeconds(currentTime.getSecond),
      currentVoltage.voltage
    ) +: voltageTimeTable
      /* entry that belongs to current time is added manually before, must be excluded here */
      .filter(currentVoltage.fromTimeStamp != _.fromTimeStamp)
      .foldLeft(Vector.empty[PredictedVoltage])(
        (timeTable: Vector[PredictedVoltage], entry: VoltageTimeTableEntry) => {
          timeTable :+ PredictedVoltage(
            currentTime
              .plusMinutes(
                currentTimeStamp.minutesUntil(entry.fromTimeStamp)
              )
              .minusSeconds(currentTime.getSecond),
            currentTime
              .plusMinutes(
                currentTimeStamp.minutesUntil(entry.untilTimeStamp)
              )
              .minusSeconds(currentTime.getSecond),
            entry.voltage
          )
        }
      )
      .filter(_.start.isBefore(endTime))
      .sortBy { case PredictedVoltage(start, _, _) =>
        start
      }
  }

  /** Find and return the voltage time table entry that belongs to a specific
    * time stamp. The time stamp must therefore be between the start and end
    * time stamp of the voltage time table entry.
    *
    * @param timeStamp
    *   the time stamp the time table entry should be returned
    * @param voltageTimeTable
    *   the voltage time table
    * @return
    *   the voltage time table entry the time stamp belongs to
    */
  private def getVoltageTimeTableEntryThisTimeStampBelongsTo(
      timeStamp: TimeStamp,
      voltageTimeTable: Vector[VoltageTimeTableEntry]
  ): VoltageTimeTableEntry = {

    val it = voltageTimeTable.iterator
    while (it.hasNext) {
      val entry: VoltageTimeTableEntry = it.next()
      if (timeStamp.isBetween(entry.fromTimeStamp, entry.untilTimeStamp))
        return entry
    }
    throw new InvalidParameterException(
      "This shouldn't happen, the voltage time table must " +
        "cover all possible time stamps."
    )

    /*
    voltageTimeTable.foldLeft(None: Option[VoltageTimeTableEntry])(
      (result: Option[VoltageTimeTableEntry], entry: VoltageTimeTableEntry) => {
        if (timeStamp.isBetween(entry.fromTimeStamp, entry.toTimeStamp)) {
          Some(entry)
        } else result
      }
    )
     */
  }

}
