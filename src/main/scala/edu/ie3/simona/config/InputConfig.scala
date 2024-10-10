/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.InputConfig._
import edu.ie3.simona.config.IoConfigUtils._

case class InputConfig(
    grid: GridConfig,
    primary: PrimaryConfig = PrimaryConfig.empty,
    weather: WeatherConfig = WeatherConfig.sample
)

object InputConfig {

  case class GridConfig(
      datasource: GridDataSource
  )

  case class GridDataSource(
      id: String,
      csvParams: Option[PsdmCsvParams] = None
  )

  case class PrimaryConfig(
      // TODO: csvParams probably need to be optionally hierarchic
      csvParams: Option[TimeStampedDataCsvParams] = None,
      influxDb1xParams: Option[InfluxDb1xParams] = None,
      // TODO: primary service does not need table in params
      sqlParams: Option[TimeStampedSqlParams] = None,
      couchbaseParams: Option[CouchbaseParams] = None
  )

  object PrimaryConfig {
    def empty: PrimaryConfig = PrimaryConfig()
  }

  case class WeatherConfig(
      datasource: WeatherDataSourceConfig
  )

  object WeatherConfig {
    def sample: WeatherConfig = WeatherConfig(WeatherDataSourceConfig.sample)
  }

  case class WeatherDataSourceConfig(
      scheme: String,
      sampleParams: Option[WeatherSampleParams],
      timeStampPattern: Option[String],
      resolution: Option[Long],
      maxCoordinateDistance: Double = 50000d,
      csvParams: Option[BaseCsvParams] = None,
      influxDb1xParams: Option[InfluxDb1xParams],
      sqlParams: Option[BaseSqlParams],
      couchbaseParams: Option[CouchbaseParams],
      coordinateSource: CoordinateSourceConfig
  )

  object WeatherDataSourceConfig {
    def sample: WeatherDataSourceConfig = WeatherDataSourceConfig(
      scheme = "",
      sampleParams = Some(WeatherSampleParams(use = true)),
      timeStampPattern = None,
      resolution = None,
      csvParams = None,
      influxDb1xParams = None,
      sqlParams = None,
      couchbaseParams = None,
      coordinateSource = CoordinateSourceConfig.sample
    )
  }

  final case class CoordinateSourceConfig(
      gridModel: String = "icon",
      csvParams: Option[BaseCsvParams],
      sampleParams: Option[WeatherSampleParams],
      sqlParams: Option[BaseSqlParams]
  )

  object CoordinateSourceConfig {
    def sample: CoordinateSourceConfig = {
      CoordinateSourceConfig(
        csvParams = None,
        sampleParams = Some(WeatherSampleParams(use = true)),
        sqlParams = None
      )
    }
  }

  // TODO: this class is useless
  case class WeatherSampleParams(
      use: Boolean
  )
}
