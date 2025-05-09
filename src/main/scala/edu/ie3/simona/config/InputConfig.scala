/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.simona.config.InputConfig.{Grid, LoadProfile, Primary, Weather}
import edu.ie3.simona.config.ConfigParams._

/** Input configuration for simona.
  * @param extSimDir
  *   Option for the directory, where external simulation are placed in.
  * @param grid
  *   Mainly the source for grid data.
  * @param loadProfile
  *   Source for load profile data (default: empty).
  * @param primary
  *   Source for primary data (default: empty).
  * @param weather
  *   Source for weather data (default: empty).
  */
final case class InputConfig(
    extSimDir: Option[String],
    grid: Grid,
    loadProfile: LoadProfile = LoadProfile.empty,
    primary: Primary = Primary(),
    weather: Weather = Weather(),
)

object InputConfig {

  /** Configuration for grid input.
    * @param datasource
    *   With grid data.
    */
  final case class Grid(
      datasource: GridDatasource
  )

  /** Case class with option for load profile data source.
    *
    * @param datasource
    *   Containing load profiles.
    */
  final case class LoadProfile(
      datasource: LoadProfile.Datasource = LoadProfile.Datasource()
  )
  object LoadProfile {

    /** Returns an empty [[LoadProfile]] with default params.
      */
    def empty: LoadProfile = LoadProfile()

    /** Case class with options for load profile data source parameters.
      *
      * @param csvParams
      *   Used for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]] (default:
      *   None).
      * @param sqlParams
      *   Used for [[edu.ie3.datamodel.io.source.sql.SqlDataSource]] (default:
      *   None).
      */
    final case class Datasource(
        csvParams: Option[BaseCsvParams] = None,
        sqlParams: Option[SqlParams] = None,
    )
  }

  /** Case class with options for primary data source parameters.
    * @param couchbaseParams
    *   Used for [[edu.ie3.datamodel.io.connectors.CouchbaseConnector]]
    *   (default: None).
    * @param csvParams
    *   Used for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]] (default:
    *   None).
    * @param influxDb1xParams
    *   Used for [[edu.ie3.datamodel.io.connectors.InfluxDbConnector]] (default:
    *   None).
    * @param sqlParams
    *   Used for [[edu.ie3.datamodel.io.source.sql.SqlDataSource]] (default:
    *   None).
    */
  final case class Primary(
      couchbaseParams: Option[CouchbaseParams] = None,
      csvParams: Option[TimeStampedCsvParams] = None,
      influxDb1xParams: Option[TimeStampedInfluxDb1xParams] = None,
      sqlParams: Option[TimeStampedSqlParams] = None,
  )

  final case class Weather(
      datasource: WeatherDatasource = WeatherDatasource()
  )

  /** Source containing the grid data.
    * @param csvParams
    *   Parameters for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]].
    * @param id
    *   Of the datasource.
    */
  final case class GridDatasource(
      csvParams: Option[BaseCsvParams] = None,
      id: String,
  )

  /** Case class with parameters for a weather source.
    * @param coordinateSource
    *   Source for the used coordinates.
    * @param couchbaseParams
    *   Used for [[edu.ie3.datamodel.io.connectors.CouchbaseConnector]]
    *   (default: None).
    * @param csvParams
    *   Used for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]] (default:
    *   None).
    * @param influxDb1xParams
    *   Used for [[edu.ie3.datamodel.io.connectors.InfluxDbConnector]] (default:
    *   None).
    * @param maxCoordinateDistance
    *   Maximal distance in meter to consider for data points (default: 50 km).
    * @param resolution
    *   Option for the time in seconds between data points (default: 3600
    *   seconds).
    * @param sampleParams
    *   Sample parameters (default: None).
    * @param scheme
    *   For the weather data (default: icon).
    * @param sqlParams
    *   Used for [[edu.ie3.datamodel.io.source.sql.SqlDataSource]] (default:
    *   None).
    * @param timestampPattern
    *   Option for overriding the time pattern used for the source (default:
    *   None).
    */
  final case class WeatherDatasource(
      coordinateSource: CoordinateSource = CoordinateSource(),
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

  /** Case class with options for coordinate source parameters.
    * @param csvParams
    *   Used for [[edu.ie3.datamodel.io.source.csv.CsvDataSource]] (default:
    *   None).
    * @param gridModel
    *   The model of the coordinate grid (default: icon).
    * @param sampleParams
    *   Sample parameters (default: None).
    * @param sqlParams
    *   Used for [[edu.ie3.datamodel.io.source.sql.SqlDataSource]] (default:
    *   None).
    */
  final case class CoordinateSource(
      csvParams: Option[BaseCsvParams] = None,
      gridModel: String = "icon",
      sampleParams: Option[SampleParams] = None,
      sqlParams: Option[BaseSqlParams] = None,
  )

}
