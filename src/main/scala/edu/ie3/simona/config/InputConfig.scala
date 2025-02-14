/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.InputConfig.{Grid, Primary, Weather}
import edu.ie3.simona.config.ConfigParams._

/** Input configuration for simona.
  * @param grid
  *   mainly the source for grid data
  * @param primary
  *   source for primary data (default: empty)
  * @param weather
  *   source for weather data (default: empty)
  */
final case class InputConfig(
    grid: Grid,
    primary: Primary = Primary.empty,
    weather: Weather = Weather.empty,
)

object InputConfig {

  /** Configuration for grid input.
    * @param datasource
    *   with grid data
    */
  final case class Grid(
      datasource: GridDatasource
  )

  /** Case class with options for primary data source parameters
    * @param couchbaseParams
    *   used for [[edu.ie3.datamodel.io.connectors.CouchbaseConnector]]
    *   (default: None)
    * @param csvParams
    *   used for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]] (default:
    *   None)
    * @param influxDb1xParams
    *   used for [[edu.ie3.datamodel.io.connectors.InfluxDbConnector]] (default:
    *   None)
    * @param sqlParams
    *   used for [[edu.ie3.datamodel.io.source.sql.SqlDataSource]] (default:
    *   None)
    */
  final case class Primary(
      couchbaseParams: Option[CouchbaseParams] = None,
      csvParams: Option[TimeStampedCsvParams] = None,
      influxDb1xParams: Option[TimeStampedInfluxDb1xParams] = None,
      sqlParams: Option[TimeStampedSqlParams] = None,
  )
  object Primary {

    /** Returns an empty [[Primary]] with default params.
      */
    def empty: Primary = Primary()
  }

  final case class Weather(
      datasource: WeatherDatasource = WeatherDatasource.empty
  )
  object Weather {

    /** Returns an empty [[Weather]] with default params.
      */
    def empty: Weather = Weather()
  }

  /** Source containing the grid data
    * @param csvParams
    *   parameters for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]]
    * @param id
    *   of the datasource
    */
  final case class GridDatasource(
      csvParams: Option[BaseCsvParams] = None,
      id: String,
  )

  /** Case class with parameters for a weather source.
    * @param coordinateSource
    *   source for the used coordinates
    * @param couchbaseParams
    *   used for [[edu.ie3.datamodel.io.connectors.CouchbaseConnector]]
    *   (default: None)
    * @param csvParams
    *   used for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]] (default:
    *   None)
    * @param influxDb1xParams
    *   used for [[edu.ie3.datamodel.io.connectors.InfluxDbConnector]] (default:
    *   None)
    * @param maxCoordinateDistance
    *   maximal distance in meter to consider for data points (default: 50 km)
    * @param resolution
    *   option for the time in seconds between data points (default: 3600
    *   seconds)
    * @param sampleParams
    *   sample parameters (default: None)
    * @param scheme
    *   for the weather data (default: icon)
    * @param sqlParams
    *   used for [[edu.ie3.datamodel.io.source.sql.SqlDataSource]] (default:
    *   None)
    * @param timestampPattern
    *   option for overriding the time pattern used for the source (default:
    *   None)
    */
  final case class WeatherDatasource(
      coordinateSource: CoordinateSource = CoordinateSource.empty,
      couchbaseParams: Option[CouchbaseParams] = None,
      csvParams: Option[BaseCsvParams] = None,
      influxDb1xParams: Option[BaseInfluxDb1xParams] = None,
      maxCoordinateDistance: Double = 50000,
      resolution: Long = 3600L,
      sampleParams: Option[SampleParams] = None,
      scheme: String = "icon",
      sqlParams: Option[BaseSqlParams] = None,
      timestampPattern: Option[String] = None,
  )
  object WeatherDatasource {

    /** Returns an empty [[WeatherDatasource]] with default params.
      */
    def empty: WeatherDatasource = WeatherDatasource()
  }

  /** Case class with options for coordinate source parameters.
    * @param csvParams
    *   used for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]] (default:
    *   None)
    * @param gridModel
    *   the model of the coordinate grid (default: icon)
    * @param sampleParams
    *   sample parameters (default: None)
    * @param sqlParams
    *   used for [[edu.ie3.datamodel.io.source.sql.SqlDataSource]] (default:
    *   None)
    */
  final case class CoordinateSource(
      csvParams: Option[BaseCsvParams] = None,
      gridModel: String = "icon",
      sampleParams: Option[SampleParams] = None,
      sqlParams: Option[BaseSqlParams] = None,
  )
  object CoordinateSource {

    /** Returns an empty [[CoordinateSource]] with default params.
      */
    def empty: CoordinateSource = CoordinateSource()
  }

}
