package edu.ie3.simona.config

import edu.ie3.simona.config.InputConfig._
import edu.ie3.simona.config.IoConfigUtils._

case class InputConfig(
  grid: GridConfig,
  primary: PrimaryConfig,
  weather: WeatherConfig,
)

object InputConfig {

  case class GridConfig(
    datasource: GridDataSource

  )

  case class GridDataSource(
    id: String,
    csvParams: Option[PsdmCsvParams] = None,
  )

  case class PrimaryConfig(
    csvParams: Option[TimeStampedDataCsvParams] = None,
    influxDb1xParams: Option[InfluxDb1xParams] = None,
    sqlParams: Option[SQLParams] = None,
    couchbaseParams: Option[CouchbaseParams] = None
  )

  case class WeatherConfig(
    datasource: WeatherDataSourceConfig,
  )

  case class WeatherDataSourceConfig(
    scheme: String,
    sampleParams: Option[WeatherSampleParams],
    timeStampPattern: Option[String],
    resolution: Option[Long],
    maxCoordinateDistance: Option[Double] = Some(50000),
    csvParams: Option[BaseCsvParams] = None,
    influxDb1xParams: Option[InfluxDb1xParams],
    sqlParams: Option[SQLParams],
    couchbaseParams: Option[CouchbaseParams],
    coordinateSource: CoordinateSourceConfig
  )

  final case class CoordinateSourceConfig(
    csvParams: Option[BaseCsvParams],
    sampleParams: Option[WeatherSampleParams],
    sqlParams: Option[SQLParams]
  )

  case class WeatherSampleParams(
    use: Boolean
  )

}
