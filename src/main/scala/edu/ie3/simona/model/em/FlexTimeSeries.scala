/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.timeseries.individual.IndividualTimeSeries
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.io.FlexSignalFromExcel
import squants.Power
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.util.{Failure, Success}
import edu.ie3.util.quantities.PowerSystemUnits

import scala.jdk.OptionConverters.RichOptional

final case class FlexTimeSeries(
    timeSeries: IndividualTimeSeries[PValue],
    startDateTime: ZonedDateTime,
    resolutionHours: Int,
    minValue: Power,
    maxValue: Power,
    threshold: Double
) {
  def get(tick: Long): Power = {
    // round current time to flexTimeSeries.resolutionHours hrs
    val currentDateTime = tick.toDateTime(startDateTime)
    val currentHour = currentDateTime.getHour
    val roundedHour =
      currentHour - currentHour % resolutionHours
    val roundedDateTime = currentDateTime
      .withHour(roundedHour)
      .withMinute(0)
      .withSecond(0)
      .withNano(0)

    timeSeries
      .getTimeBasedValue(roundedDateTime)
      .toScala
      .getOrElse(
        throw new RuntimeException(
          s"Could not retrieve value for $roundedDateTime"
        )
      )
      .getValue
      .getP
      .toScala
      .map(p => Kilowatts(p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue))
      .getOrElse(
        throw new RuntimeException(
          s"No value set for $roundedDateTime"
        )
      )
  }
}

object FlexTimeSeries {

  def apply(
      config: SimonaConfig.Simona.Runtime.RootEm
  )(implicit startDateTime: ZonedDateTime): FlexTimeSeries = {
    val timeSeriesType =
      FlexSignalFromExcel.TimeSeriesType(config.timeSeriesType)
    val timeSeries = FlexSignalFromExcel
      .flexSignals(config.filePath, config.nodeId, timeSeriesType) match {
      case Success(timeSeries) => timeSeries
      case Failure(exception)  => throw exception
    }

    val resolutionHours =
      if (timeSeries.getEntries.size() < 2)
        throw new RuntimeException(
          s"Less than two entries for flex time series ${config.nodeId}"
        )
      else {
        val valueIt = timeSeries.getEntries.iterator()
        val entry1 = valueIt.next().getTime
        val entry2 = valueIt.next().getTime

        ChronoUnit.HOURS.between(entry1, entry2).intValue
      }

    // in case of resLoad we use totalResload (considering Simona participants) for min max setting
    val (minValue, maxValue) =
      FlexSignalFromExcel.getCorrespondingMinMaxValues(
        timeSeriesType,
        timeSeries,
        config
      )

    FlexTimeSeries(
      timeSeries,
      startDateTime,
      resolutionHours,
      minValue,
      maxValue,
      config.threshold
    )
  }
}
