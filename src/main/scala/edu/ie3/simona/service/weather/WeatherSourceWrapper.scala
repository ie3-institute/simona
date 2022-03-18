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
  SqlConnector
}
import edu.ie3.datamodel.io.factory.timeseries.{
  IconTimeBasedWeatherValueFactory,
  CosmoTimeBasedWeatherValueFactory
}
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.couchbase.CouchbaseWeatherSource
import edu.ie3.datamodel.io.source.csv.CsvWeatherSource
import edu.ie3.datamodel.io.source.influxdb.InfluxDbWeatherSource
import edu.ie3.datamodel.io.source.sql.SqlWeatherSource
import edu.ie3.datamodel.io.source.{
  IdCoordinateSource,
  WeatherSource => PsdmWeatherSource
}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Weather.Datasource.{
  CouchbaseParams,
  InfluxDb1xParams,
  SqlParams
}
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.weather.WeatherSource.{
  EMPTY_WEATHER_DATA,
  WeatherScheme,
  toWeatherData
}
import edu.ie3.simona.service.weather.WeatherSourceWrapper.WeightSum
import edu.ie3.simona.service.weather.{WeatherSource => SimonaWeatherSource}
import edu.ie3.simona.util.TickUtil
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.exceptions.EmptyQuantityException
import edu.ie3.util.interval.ClosedInterval
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import javax.measure.Quantity
import scala.jdk.CollectionConverters.{IterableHasAsJava, MapHasAsScala}
import scala.jdk.OptionConverters.RichOptional
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
    resolution: Long
)(
    private implicit val simulationStart: ZonedDateTime
) extends SimonaWeatherSource
    with LazyLogging {

  /** Get the weather data for the given tick as a weighted average taking into
    * account the given weighting of weather coordinates.
    *
    * @param tick
    *   Simulation date in question
    * @param weightedCoordinates
    *   The coordinate in question
    * @return
    *   Matching weather data
    */
  override def getWeather(
      tick: Long,
      weightedCoordinates: WeatherSource.WeightedCoordinates
  ): WeatherMessage.WeatherData = {
    val dateTime = tick.toDateTime
    val interval = new ClosedInterval(dateTime, dateTime)
    val coordinates = weightedCoordinates.weighting.keys.toList.asJavaCollection
    val results = source
      .getWeather(
        interval,
        coordinates
      )
      .asScala
      .toMap
    val weatherDataMap = results.flatMap { case (point, timeSeries) =>
      // change temperature scale for the upcoming calculations
      timeSeries
        .getValue(dateTime)
        .toScala
        .map(weatherValue => point -> toWeatherData(weatherValue))
    }

    weatherDataMap.foldLeft((EMPTY_WEATHER_DATA, WeightSum.EMPTY_WEIGHT_SUM)) {
      case ((averagedWeather, currentWeightSum), (point, currentWeather)) =>
        /** Calculate the contribution of a single coordinate value to the
          * averaged weather information. If we got an empty quantity (which can
          * be the case, as this particular value might be missing in the
          * weather data), we do let it out and also return the "effective"
          * weight of 0d.
          */
        def calculateContrib[Q <: Quantity[Q]](
            quantity: Quantity[Q],
            weight: Double,
            defaultUnit: javax.measure.Unit[Q],
            warningString: String
        ): (Quantity[Q], Double) = {
          try {
            (quantity.multiply(weight), weight)
          } catch {
            case e: EmptyQuantityException =>
              logger.warn(warningString, e)
              (Quantities.getQuantity(0d, defaultUnit), 0d)
          }
        }

        /* Get pre-calculated weight for this coordinate */
        val weight = weightedCoordinates.weighting.getOrElse(
          point, {
            logger.warn(s"Received an unexpected point: $point")
            0d
          }
        )

        /* Determine actual weights and contributions */
        val (diffRadContrib, diffRadWeight) = currentWeather.diffRad match {
          case EMPTY_WEATHER_DATA.diffRad => (EMPTY_WEATHER_DATA.diffRad, 0d)
          case nonEmptyDiffRad =>
            calculateContrib(
              nonEmptyDiffRad,
              weight,
              StandardUnits.SOLAR_IRRADIANCE,
              s"Diffuse solar irradiance not available at $point."
            )
        }
        val (dirRadContrib, dirRadWeight) = currentWeather.dirRad match {
          case EMPTY_WEATHER_DATA.dirRad => (EMPTY_WEATHER_DATA.dirRad, 0d)
          case nonEmptyDirRad =>
            calculateContrib(
              nonEmptyDirRad,
              weight,
              StandardUnits.SOLAR_IRRADIANCE,
              s"Direct solar irradiance not available at $point."
            )
        }
        val (tempContrib, tempWeight) = currentWeather.temp match {
          case EMPTY_WEATHER_DATA.temp => (EMPTY_WEATHER_DATA.temp, 0d)
          case nonEmptyTemp =>
            calculateContrib(
              nonEmptyTemp,
              weight,
              StandardUnits.TEMPERATURE,
              s"Temperature not available at $point."
            )
        }
        val (windVelContrib, windVelWeight) = currentWeather.windVel match {
          case EMPTY_WEATHER_DATA.windVel => (EMPTY_WEATHER_DATA.windVel, 0d)
          case nonEmptyWindVel =>
            calculateContrib(
              nonEmptyWindVel,
              weight,
              StandardUnits.WIND_VELOCITY,
              s"Wind velocity not available at $point."
            )
        }

        /* Sum up weight and contributions */
        (
          WeatherData(
            averagedWeather.diffRad.add(diffRadContrib),
            averagedWeather.dirRad.add(dirRadContrib),
            averagedWeather.temp.add(tempContrib),
            averagedWeather.windVel.add(windVelContrib)
          ),
          currentWeightSum.add(
            diffRadWeight,
            dirRadWeight,
            tempWeight,
            windVelWeight
          )
        )
    } match {
      case (weatherData: WeatherData, weightSum: WeightSum) =>
        /* Divide by weight sum to correctly account for missing data. Change temperature scale back to absolute*/
        WeatherData(
          weatherData.diffRad.divide(weightSum.diffRad),
          weatherData.dirRad.divide(weightSum.dirRad),
          weatherData.temp.divide(weightSum.temp),
          weatherData.windVel.divide(weightSum.windVel)
        )
    }

  }

  /** Determine an Array with all ticks between the request frame's start and
    * end on which new data is available
    *
    * @param requestFrameStart
    *   Beginning of the announced request frame
    * @param requestFrameEnd
    *   End of the announced request frame
    * @return
    *   Array with data ticks
    */
  override def getDataTicks(
      requestFrameStart: Long,
      requestFrameEnd: Long
  ): Array[Long] =
    TickUtil.getTicksInBetween(requestFrameStart, requestFrameEnd, resolution)
}

private[weather] object WeatherSourceWrapper extends LazyLogging {
  private val DEFAULT_RESOLUTION = 360L

  def apply(
      csvSep: String,
      folderPath: String,
      idCoordinateSourceFunction: () => IdCoordinateSource,
      timestampPattern: Option[String],
      scheme: String,
      resolution: Option[Long]
  )(implicit simulationStart: ZonedDateTime): WeatherSourceWrapper = {
    val idCoordinateSource = idCoordinateSourceFunction()
    val source = new CsvWeatherSource(
      csvSep,
      folderPath,
      new FileNamingStrategy(),
      idCoordinateSource,
      buildFactory(timestampPattern, scheme)
    )
    logger.info(
      "Successfully initiated CsvWeatherSource as source for WeatherSourceWrapper."
    )
    WeatherSourceWrapper(
      source,
      idCoordinateSource,
      resolution.getOrElse(DEFAULT_RESOLUTION)
    )
  }

  def apply(
      couchbaseParams: CouchbaseParams,
      idCoordinateSourceFunction: () => IdCoordinateSource,
      timestampPattern: Option[String],
      scheme: String,
      resolution: Option[Long]
  )(implicit simulationStart: ZonedDateTime): WeatherSourceWrapper = {
    val couchbaseConnector = new CouchbaseConnector(
      couchbaseParams.url,
      couchbaseParams.bucketName,
      couchbaseParams.userName,
      couchbaseParams.password
    )
    val idCoordinateSource = idCoordinateSourceFunction()
    val source = new CouchbaseWeatherSource(
      couchbaseConnector,
      idCoordinateSourceFunction(),
      couchbaseParams.coordinateColumnName,
      couchbaseParams.keyPrefix,
      buildFactory(timestampPattern, scheme)
    )
    logger.info(
      "Successfully initiated CouchbaseWeatherSource as source for WeatherSourceWrapper."
    )
    WeatherSourceWrapper(
      source,
      idCoordinateSource,
      resolution.getOrElse(DEFAULT_RESOLUTION)
    )
  }

  def apply(
      influxDbParams: InfluxDb1xParams,
      idCoordinateSourceFunction: () => IdCoordinateSource,
      timestampPattern: Option[String],
      scheme: String,
      resolution: Option[Long]
  )(implicit simulationStart: ZonedDateTime): WeatherSourceWrapper = {
    val influxDb1xConnector =
      new InfluxDbConnector(influxDbParams.url, influxDbParams.database)
    val idCoordinateSource = idCoordinateSourceFunction()
    val source = new InfluxDbWeatherSource(
      influxDb1xConnector,
      idCoordinateSource,
      buildFactory(timestampPattern, scheme)
    )
    logger.info(
      "Successfully initiated InfluxDbWeatherSource as source for WeatherSourceWrapper."
    )
    WeatherSourceWrapper(
      source,
      idCoordinateSource,
      resolution.getOrElse(DEFAULT_RESOLUTION)
    )
  }

  def apply(
      sqlParams: SqlParams,
      idCoordinateSourceFunction: () => IdCoordinateSource,
      timestampPattern: Option[String],
      scheme: String,
      resolution: Option[Long]
  )(implicit simulationStart: ZonedDateTime): WeatherSourceWrapper = {
    val sqlConnector = new SqlConnector(
      sqlParams.jdbcUrl,
      sqlParams.userName,
      sqlParams.password
    )
    val idCoordinateSource = idCoordinateSourceFunction()
    val source = new SqlWeatherSource(
      sqlConnector,
      idCoordinateSource,
      sqlParams.schemaName,
      sqlParams.tableName,
      buildFactory(timestampPattern, scheme)
    )
    logger.info(
      "Successfully initiated SqlWeatherSource as source for WeatherSourceWrapper."
    )
    WeatherSourceWrapper(
      source,
      idCoordinateSource,
      resolution.getOrElse(DEFAULT_RESOLUTION)
    )
  }

  private def buildFactory(timestampPattern: Option[String], scheme: String) = {
    timestampPattern match {
      case None => initWeatherFactory(scheme)
      case Some(timeStampPattern) =>
        initWeatherFactory(scheme, timeStampPattern)
    }
  }

  private def initWeatherFactory(scheme: String) =
    Try(WeatherScheme(scheme)) match {
      case Failure(_) =>
        throw new InitializationException(
          s"Error while initializing WeatherFactory for weather source wrapper: '$scheme' is not a weather scheme. Supported schemes:\n\t${WeatherScheme.values
            .mkString("\n\t")}'"
        )
      case Success(WeatherScheme.ICON) => new IconTimeBasedWeatherValueFactory()
      case Success(WeatherScheme.COSMO) =>
        new CosmoTimeBasedWeatherValueFactory()
      case Success(unknownScheme) =>
        throw new InitializationException(
          s"Error while initializing WeatherFactory for weather source wrapper: weather scheme '$unknownScheme' is not an expected input."
        )
    }

  private def initWeatherFactory(scheme: String, timeStampPattern: String) =
    Try(WeatherScheme(scheme)) match {
      case Failure(_) =>
        throw new InitializationException(
          s"Error while initializing WeatherFactory for weather source wrapper: '$scheme' is not a weather scheme. Supported schemes:\n\t${WeatherScheme.values
            .mkString("\n\t")}'"
        )
      case Success(WeatherScheme.ICON) =>
        new IconTimeBasedWeatherValueFactory(timeStampPattern)
      case Success(WeatherScheme.COSMO) =>
        new CosmoTimeBasedWeatherValueFactory(timeStampPattern)
      case Success(unknownScheme) =>
        throw new InitializationException(
          s"Error while initializing WeatherFactory for weather source wrapper: weather scheme '$unknownScheme' is not an expected input."
        )
    }

  final case class WeightSum(
      diffRad: Double,
      dirRad: Double,
      temp: Double,
      windVel: Double
  ) {
    def add(
        diffRad: Double,
        dirRad: Double,
        temp: Double,
        windVel: Double
    ): WeightSum =
      WeightSum(
        this.diffRad + diffRad,
        this.dirRad + dirRad,
        this.temp + temp,
        this.windVel + windVel
      )
  }
  case object WeightSum {
    val EMPTY_WEIGHT_SUM: WeightSum = WeightSum(0d, 0d, 0d, 0d)
  }

}
