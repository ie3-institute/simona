/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs.gridoriented

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.exceptions.{
  InvalidParameterException,
  PredictionException
}
import edu.ie3.simona.model.participant.evcs.PredictionAndSchedulingUtils.TimeStamp
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import javax.measure.Quantity
import javax.measure.quantity.Dimensionless
import scala.util.{Failure, Try}

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
  final case class VoltageTimeTableEntry(
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
  final case class PredictedVoltage(
      start: ZonedDateTime,
      end: ZonedDateTime,
      voltage: ComparableQuantity[Dimensionless]
  )

  private type VoltageReference = (TimeStamp, ComparableQuantity[Dimensionless])

  /** Get time windows with predicted voltage values for the relevant time until
    * departure of the last EV based on the reference voltages in the voltage
    * time table.
    *
    * @param currentTime
    *   current time to know where to start in the voltage time table
    * @param endTime
    *   the end time (e.g. departure time of the last ev) until which time the
    *   voltages are required (can be max 7 days ahead of current time)
    * @param voltages
    *   Map from wall-clock time to known voltages
    * @param blurringTimeWindowLength
    *   Length of the sliding window to use for blurring voltages
    * @param voltageTimeBinResolution
    *   Resolution to determine voltages per time bin in minutes
    * @return
    *   vector of predicted voltages with value and timeframe, ordered in time
    */
  def getPredictedVoltagesForRelevantTimeWindowBasedOnReferenceVoltages(
      currentTime: ZonedDateTime,
      endTime: ZonedDateTime,
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]],
      blurringTimeWindowLength: Int = 3,
      voltageTimeBinResolution: Int = voltageResolution
  ): Try[Vector[PredictedVoltage]] = {
    if (endTime.isAfter(currentTime.plusDays(7)))
      throw new InvalidParameterException(
        "Predicted voltages can currently only be created up to one week ahead of the current time"
      )

    calculateReferenceVoltageTimeTable(
      voltages,
      blurringTimeWindowLength,
      voltageTimeBinResolution
    ).map { voltageTimeTable =>
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
          (
              timeTable: Vector[PredictedVoltage],
              entry: VoltageTimeTableEntry
          ) => {
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
  }

  /** Calculate a time table for a reference week based on the known voltages
    * from memory. The reference voltages are calculated on a defined minute
    * basis (max 60min), if the resolution of voltage updates is higher. Each
    * known voltage value for a time window is weighted equally.
    *
    * @param voltages
    *   known voltages from history
    * @param blurringWindowLength
    *   Length of the sliding window to use for blurring voltages
    * @param voltageTimeBinResolution
    *   Resolution to determine voltages per time bin in minutes
    * @return
    *   vector of time table entries including the voltage value and the
    *   timeframe
    */
  private def calculateReferenceVoltageTimeTable(
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]],
      blurringWindowLength: Int = 3,
      voltageTimeBinResolution: Int = voltageResolution
  ): Try[Vector[VoltageTimeTableEntry]] = {
    val voltageReferenceMap =
      averageVoltagesPerTimeStamp(voltages, voltageTimeBinResolution)

    /* Order voltage entries based on time stamp */
    val orderedVoltageReferenceMap = voltageReferenceMap.toVector.sortBy(_._1)
    blurVoltages(orderedVoltageReferenceMap, blurringWindowLength).map(
      _.toVector
    )
  }

  /** Determine the average voltage per time stamp / time bin. The time stamp,
    * the voltages are averaged for, is determined by the voltage resolution in
    * minutes.
    *
    * @param voltages
    *   Mapping from zoned date time to voltage value
    * @param timeBinResolution
    *   Resolution of averaging time bin
    * @return
    *   A mapping from time bin to average voltage magnitude
    */
  private def averageVoltagesPerTimeStamp(
      voltages: Map[ZonedDateTime, ComparableQuantity[Dimensionless]],
      timeBinResolution: Int
  ): Map[TimeStamp, ComparableQuantity[Dimensionless]] = voltages
    .groupBy { case (zdt, _) =>
      val day = zdt.getDayOfWeek
      val hour = zdt.getHour
      val minute =
        zdt.getMinute - (zdt.getMinute % timeBinResolution)
      TimeStamp(day, hour, minute)
    }
    .map { case (timeStamp, zdtToVoltage) =>
      /* Determine the average per time stamp */
      timeStamp -> mean(zdtToVoltage.values.toSeq)
        .getOrElse(Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE))
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
  ): Try[Seq[VoltageTimeTableEntry]] =
    enhanceVoltageReferences(orderedVoltageReferences, windowLength).map {
      _.sliding(windowLength)
        .map { slidingWindow =>
          val lastTwo = slidingWindow.slice(windowLength - 2, windowLength)
          lastTwo.headOption
            .flatMap(head => lastTwo.lastOption.map((head, _)))
            .map { case ((startTime, _), (endTime, _)) =>
              (startTime, endTime)
            }
            .zip(mean(slidingWindow.map { case (_, quantity) =>
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
    }

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
  ): Try[Seq[VoltageReference]] = {
    val neededAmount =
      windowLength - 2 // The value always apply for the time stamp and the successor (2 time stamps)
    if (orderedVoltageReferences.length < neededAmount) {
      Failure(
        PredictionException(
          s"Cannot enhance the voltage references, as they are too short in general. Needed length: $neededAmount, actual length: ${orderedVoltageReferences.size}"
        )
      )
    } else
      Try {
        val tail = orderedVoltageReferences.slice(
          orderedVoltageReferences.length - neededAmount,
          orderedVoltageReferences.length
        )
        val head = orderedVoltageReferences.slice(0, neededAmount)
        tail ++ orderedVoltageReferences ++ head
      }
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
  ): VoltageTimeTableEntry = voltageTimeTable
    .find { case VoltageTimeTableEntry(fromTimeStamp, untilTimeStamp, _) =>
      timeStamp.isBetween(fromTimeStamp, untilTimeStamp)
    }
    .getOrElse {
      throw new InvalidParameterException(
        "This shouldn't happen, the voltage time table must cover all possible time stamps."
      )
    }
}
