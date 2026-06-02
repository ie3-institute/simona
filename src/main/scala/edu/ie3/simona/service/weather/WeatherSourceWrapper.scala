/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.connectors.{
  CouchbaseConnector,
  InfluxDbConnector,
  SqlConnector,
}
import edu.ie3.datamodel.io.factory.timeseries.{
  CosmoTimeBasedWeatherValueFactory,
  IconTimeBasedWeatherValueFactory,
}
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.couchbase.CouchbaseWeatherSource
import edu.ie3.datamodel.io.source.csv.CsvWeatherSource
import edu.ie3.datamodel.io.source.influxdb.InfluxDbWeatherSource
import edu.ie3.datamodel.io.source.sql.SqlWeatherSource
import edu.ie3.datamodel.io.source.{
  IdCoordinateSource,
  WeatherSource as PsdmWeatherSource,
}
import edu.ie3.simona.config.ConfigParams.{
  BaseCsvParams,
  BaseInfluxDb1xParams,
  CouchbaseParams,
  SqlParams,
}
import edu.ie3.simona.config.InputConfig
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.service.weather.WeatherSource as SimonaWeatherSource
import edu.ie3.simona.service.weather.WeatherSource.{
  WeatherScheme,
  toWeatherData,
}
import edu.ie3.simona.service.weather.WeatherSourceWrapper.{
  WeightSum,
  ZERO_WEATHER_DATA,
}
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import edu.ie3.util.DoubleUtils.!~=
import edu.ie3.util.interval.ClosedInterval
import org.locationtech.jts.geom.Point
import squants.motion.MetersPerSecond
import squants.radio.WattsPerSquareMeter
import squants.thermal.Kelvin
import tech.units.indriya.ComparableQuantity

import java.nio.file.Paths
import java.time.ZonedDateTime
import javax.measure.quantity.Length
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.{
  CollectionHasAsScala,
  IterableHasAsJava,
  MapHasAsScala,
}
import scala.util.{Failure, Success, Try}

/** This class provides an implementation of the SIMONA trait
  * [[edu.ie3.simona.service.weather.WeatherSource]], by wrapping the trait
  * around the [[edu.ie3.datamodel.io.source.WeatherSource]] interface from the
  * PowerSystemDataModel project. This enables the user to use any
  * implementation of the PowerSystemDataModel WeatherSource as a weather data
  * source in SIMONA.
  * @param source
  *   any implementation of the PowerSystemDataModel weather source to pull data
  *   from
  * @param idCoordinateSource
  *   a source for coordinate data
  * @param simulationStart
  *   start of the simulation
  */
private[weather] final case class WeatherSourceWrapper private (
    source: PsdmWeatherSource,
    override val idCoordinateSource: IdCoordinateSource,
    resolution: Long,
    maxCoordinateDistance: ComparableQuantity[Length],
)(
    private implicit val simulationStart: ZonedDateTime
) extends SimonaWeatherSource
    with LazyLogging {

  override def getWeather(
      startTick: Long,
      endTick: Long,
      weightedCoordinates: WeatherSource.WeightedCoordinates,
  ): SortedMap[ZonedDateTime, WeatherData] = {
    val interval = new ClosedInterval(startTick.toDateTime, endTick.toDateTime)
    val coordinates = weightedCoordinates.weighting.keys.toList.asJavaCollection

    source
      .getWeather(
        interval,
        coordinates,
      )
      .asScala
      .map { case (coordinate, timeSeries) =>
        timeSeries.getEntries.asScala.map { weatherValue =>
          (weatherValue.getTime, coordinate) -> weatherValue.getValue
        }.toMap
      }
      .flatten
      .groupMap { case ((time, _), _) =>
        time
      } { case ((_, location), weatherValue) =>
        val weight = weightedCoordinates.weighting.getOrElse(
          location, {
            logger.warn(s"Received an unexpected point: $location")
            0d
          },
        )
        val weatherData = toWeatherData(weatherValue)
        (location, weatherData, weight)
      }
      .map { case (time, weather) =>
        time -> spatialDataInterpolation(weather)
      }
      .to(SortedMap)
  }

  private def spatialDataInterpolation(
      weatherData: Iterable[(Point, WeatherData, Double)]
  ): WeatherData =
    weatherData.foldLeft((ZERO_WEATHER_DATA, WeightSum.ZERO_WEIGHT_SUM)) {
      case ((averagedWeather, weightSum), (point, weather, weight)) =>
        /* Calculate the contribution of a single coordinate value to the
         * averaged weather information.
         *
         * If we got an empty quantity (Double.Nan, which can be the case,
         * as this particular value might be missing in the weather data),
         * we do let it out and also return the "effective"  weight of 0d.
         *
         * Careful: Use Double.isNaN because Double.NaN != Double.NaN
         */

        /* Determine actual weights and contributions */
        val (diffIrradiance, diffIrrWeight) =
          if weather.diffIrr.value.isNaN then {
            // Some data sets do not provide diffuse irradiance, so we do not
            // warn here
            logger.debug(s"Diffuse solar irradiance not available at $point.")
            (averagedWeather.diffIrr, 0d)
          } else {
            (averagedWeather.diffIrr + weather.diffIrr * weight, weight)
          }

        val (dirIrradiance, dirIrrWeight) =
          if weather.dirIrr.value.isNaN then {
            logger.warn(s"Direct solar irradiance not available at $point.")
            (averagedWeather.dirIrr, 0d)
          } else {
            (averagedWeather.dirIrr + weather.dirIrr * weight, weight)
          }

        val (temperature, tempWeight) =
          if weather.temp.value.isNaN then {
            logger.warn(s"Temperature not available at $point.")
            (averagedWeather.temp, 0d)
          } else {
            // Important: squants temperature addition is bugged.
            // Conversion to Kelvin necessary.
            (averagedWeather.temp + weather.temp.in(Kelvin) * weight, weight)
          }

        val (windVelocity, windVelWeight) =
          if weather.windVel.value.isNaN then {
            logger.warn(s"Wind velocity not available at $point.")
            (averagedWeather.windVel, 0d)
          } else {
            (averagedWeather.windVel + weather.windVel * weight, weight)
          }

        val (groundTempLvl3, groundTempLvl3Weight) =
          weather.groundTempLvl3 match {
            case None =>
              (averagedWeather.groundTempLvl3, 0d)
            case Some(temp) =>
              if temp.value.isNaN then {
                logger.warn(
                  s"Ground temperature at level 3 is NaN at $point."
                )
                (averagedWeather.groundTempLvl3, 0d)
              } else {
                // Important: squants temperature addition is bugged.
                // Conversion to Kelvin necessary.
                (
                  Some(
                    averagedWeather.groundTempLvl3
                      .getOrElse(Kelvin(0d)) + temp.in(Kelvin) * weight
                  ),
                  weight,
                )
              }
          }

        val (groundTempLvl4, groundTempLvl4Weight) =
          weather.groundTempLvl4 match {
            case None =>
              (averagedWeather.groundTempLvl4, 0d)
            case Some(temp) =>
              if temp.value.isNaN then {
                logger.warn(
                  s"Ground temperature at level 4 is NaN at $point."
                )
                (averagedWeather.groundTempLvl4, 0d)
              } else {
                // Important: squants temperature addition is bugged.
                // Conversion to Kelvin necessary.
                (
                  Some(
                    averagedWeather.groundTempLvl4
                      .getOrElse(Kelvin(0d)) + temp.in(Kelvin) * weight
                  ),
                  weight,
                )
              }
          }

        (
          WeatherData(
            diffIrradiance,
            dirIrradiance,
            temperature,
            windVelocity,
            groundTempLvl3,
            groundTempLvl4,
          ),
          weightSum.add(
            diffIrrWeight,
            dirIrrWeight,
            tempWeight,
            windVelWeight,
            groundTempLvl3Weight,
            groundTempLvl4Weight,
          ),
        )
    } match {
      case (weatherData: WeatherData, weightSum: WeightSum) =>
        weightSum.scale(weatherData)
    }

  override def getDataTicks(
      requestFrameStart: Long,
      requestFrameEnd: Long,
  ): Array[Long] = {
    // Note: because we want data for the start tick as well, we need to use any tick before the start tick
    val intervalStart = requestFrameStart.toDateTime.minusSeconds(1)

    source
      .getTimeKeysAfter(intervalStart)
      .asScala
      .flatMap { case (_, timeKeys) =>
        timeKeys.asScala
      }
      .map(_.toTick)
      .filter(_ <= requestFrameEnd)
      .toArray
  }
}

private[weather] object WeatherSourceWrapper extends LazyLogging {

  def apply(
      source: PsdmWeatherSource
  )(implicit
      simulationStart: ZonedDateTime,
      idCoordinateSource: IdCoordinateSource,
      resolution: Long,
      distance: ComparableQuantity[Length],
  ): WeatherSourceWrapper = {
    WeatherSourceWrapper(
      source,
      idCoordinateSource,
      resolution,
      distance,
    )
  }

  private[weather] def buildPSDMSource(
      cfgParams: InputConfig.WeatherDatasource,
      definedWeatherSource: Option[Any],
  )(implicit
      idCoordinateSource: IdCoordinateSource
  ): Option[PsdmWeatherSource] = {
    implicit val scheme: String = cfgParams.scheme

    val factory = buildFactory(scheme)

    val source = definedWeatherSource.flatMap {
      case BaseCsvParams(csvSep, directoryPath, _) =>
        // initializing a csv weather source
        Some(
          new CsvWeatherSource(
            csvSep,
            Paths.get(directoryPath),
            new FileNamingStrategy(),
            idCoordinateSource,
            factory,
          )
        )
      case couchbaseParams: CouchbaseParams =>
        // initializing a couchbase weather source
        val couchbaseConnector = new CouchbaseConnector(
          couchbaseParams.url,
          couchbaseParams.bucketName,
          couchbaseParams.userName,
          couchbaseParams.password,
        )
        Some(
          new CouchbaseWeatherSource(
            couchbaseConnector,
            idCoordinateSource,
            couchbaseParams.coordinateColumnName,
            couchbaseParams.keyPrefix,
            factory,
          )
        )
      case BaseInfluxDb1xParams(database, _, url) =>
        // initializing an influxDb weather source
        val influxDb1xConnector =
          new InfluxDbConnector(url, database)
        Some(
          new InfluxDbWeatherSource(
            influxDb1xConnector,
            idCoordinateSource,
            factory,
          )
        )
      case sqlParams: SqlParams =>
        // initializing a sql weather source
        val sqlConnector = new SqlConnector(
          sqlParams.jdbcUrl,
          sqlParams.userName,
          sqlParams.password,
        )
        Some(
          new SqlWeatherSource(
            sqlConnector,
            idCoordinateSource,
            sqlParams.schemaName,
            sqlParams.tableName,
            factory,
          )
        )
      case _ =>
        // no weather source is initialized
        None
    }

    source.foreach { src =>
      logger.info(
        s"Successfully initialized ${src.getClass.getSimpleName} as source for WeatherSourceWrapper."
      )
    }

    source
  }

  private def buildFactory(scheme: String) =
    Try(WeatherScheme(scheme)) match {
      case Failure(exception) =>
        throw new InitializationException(
          s"Error while initializing WeatherFactory for weather source wrapper: '$scheme' is not a weather scheme. Supported schemes:\n\t${WeatherScheme.values
              .mkString("\n\t")}'",
          exception,
        )
      case Success(WeatherScheme.ICON) =>
        new IconTimeBasedWeatherValueFactory()
      case Success(WeatherScheme.COSMO) =>
        new CosmoTimeBasedWeatherValueFactory()
      case Success(unknownScheme) =>
        throw new InitializationException(
          s"Error while initializing WeatherFactory for weather source wrapper: weather scheme '$unknownScheme' is not an expected input."
        )
    }

  /** Simple container class to allow for accumulating determination of the sum
    * of weights for different weather properties for different locations
    * surrounding a given coordinate of interest
    *
    * @param diffIrr
    *   Sum of weight for diffuse irradiance
    * @param dirIrr
    *   Sum of weight for direct irradiance
    * @param temp
    *   Sum of weight for temperature
    * @param windVel
    *   Sum of weight for wind velocity
    * @param groundTempLvl3
    *   Sum of weight for ground temperature level 3 (28-100 cm) measured at 64
    *   cm
    * @param groundTempLvl4
    *   Sum of weight for ground temperature level 4 (100-289 cm) measured at
    *   195 cm
    */
  final case class WeightSum(
      diffIrr: Double,
      dirIrr: Double,
      temp: Double,
      windVel: Double,
      groundTempLvl3: Double,
      groundTempLvl4: Double,
  ) {
    def add(
        addedDiffIrr: Double,
        addedDirIrr: Double,
        addedTemp: Double,
        addedWindVel: Double,
        addedGroundTempLvl3: Double,
        addedGroundTempLvl4: Double,
    ): WeightSum =
      WeightSum(
        this.diffIrr + addedDiffIrr,
        this.dirIrr + addedDirIrr,
        this.temp + addedTemp,
        this.windVel + addedWindVel,
        this.groundTempLvl3 + addedGroundTempLvl3,
        this.groundTempLvl4 + addedGroundTempLvl4,
      )

    /** Scale the given [[WeatherData]] by dividing by the sum of weights per
      * attribute of the weather data. If one of the weight sums is empty (and
      * thus a division by zero would happen) the defined "empty" information
      * for this attribute is returned.
      *
      * @param weatherData
      *   Weighted and accumulated weather information
      * @return
      *   Weighted weather information, which are divided by the sum of weights
      */
    def scale(weatherData: WeatherData): WeatherData = weatherData match {
      case WeatherData(
            diffIrr,
            dirIrr,
            temp,
            windVel,
            groundTempLvl3,
            groundTempLvl4,
          ) =>
        implicit val precision: Double = 1e-3
        WeatherData(
          if this.diffIrr !~= 0d then diffIrr.divide(this.diffIrr)
          else ZERO_WEATHER_DATA.diffIrr,
          if this.dirIrr !~= 0d then dirIrr.divide(this.dirIrr)
          else ZERO_WEATHER_DATA.dirIrr,
          if this.temp !~= 0d then temp.divide(this.temp)
          else ZERO_WEATHER_DATA.temp,
          if this.windVel !~= 0d then windVel.divide(this.windVel)
          else ZERO_WEATHER_DATA.windVel,
          if this.groundTempLvl3 !~= 0d then
            groundTempLvl3.map(_.divide(this.groundTempLvl3))
          else ZERO_WEATHER_DATA.groundTempLvl3,
          if this.groundTempLvl4 !~= 0d then
            groundTempLvl4.map(_.divide(this.groundTempLvl4))
          else ZERO_WEATHER_DATA.groundTempLvl4,
        )
    }
  }

  object WeightSum {
    val ZERO_WEIGHT_SUM: WeightSum = WeightSum(0d, 0d, 0d, 0d, 0d, 0d)
  }

  /** Weather data with all values set to zero.
    *
    * For temperature to represent a quantity with zero value, we need to
    * explicitly set temperature to absolute zero, so 0 K. When temperature
    * measures the movement of atoms, absolute zero means no movement, which
    * represents the concept best.
    */
  val ZERO_WEATHER_DATA: WeatherData = WeatherData(
    WattsPerSquareMeter(0d),
    WattsPerSquareMeter(0d),
    Kelvin(0d),
    MetersPerSecond(0d),
    Some(Kelvin(0d)),
    Some(Kelvin(0d)),
  )

}
