/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.InputConfig.{Grid, Primary, Weather}
import edu.ie3.simona.config.ConfigParams._

case class InputConfig(
    grid: Grid,
    primary: Primary = Primary(),
    weather: Weather = Weather(),
)

object InputConfig {
  final case class Grid(
      datasource: Grid.Datasource
  )
  object Grid {
    final case class Datasource(
        csvParams: Option[BaseCsvParams] = None,
        id: String,
    )
  }

  final case class Primary(
      couchbaseParams: scala.Option[CouchbaseParams] = None,
      csvParams: Option[TimeStampedCsvParams] = None,
      influxDb1xParams: Option[TimeStampedInfluxDb1xParams] = None,
      sqlParams: Option[TimeStampedSqlParams] = None,
  )

  final case class Weather(
      datasource: Weather.Datasource = Weather.Datasource()
  )
  object Weather {
    final case class Datasource(
        coordinateSource: Datasource.CoordinateSource =
          Datasource.CoordinateSource(),
        couchbaseParams: Option[CouchbaseParams] = None,
        csvParams: Option[BaseCsvParams] = None,
        influxDb1xParams: Option[BaseInfluxDb1xParams] = None,
        maxCoordinateDistance: Double = 50000,
        resolution: Option[Long] = None,
        sampleParams: Option[SampleParams] = None,
        scheme: String = "icon",
        sqlParams: Option[BaseSqlParams] = None,
        timestampPattern: Option[String] = None,
    )
    object Datasource {
      final case class CoordinateSource(
          csvParams: Option[BaseCsvParams] = None,
          gridModel: String = "icon",
          sampleParams: Option[SampleParams] = None,
          sqlParams: Option[BaseSqlParams] = None,
      )
    }
  }
}
