/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.io.factory.timeseries.{CosmoIdCoordinateFactory, IconIdCoordinateFactory, IdCoordinateFactory}
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.IdCoordinateSource
import edu.ie3.datamodel.io.source.csv.CsvIdCoordinateSource
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.timeseries.individual.{IndividualTimeSeries, TimeBasedValue}
import edu.ie3.datamodel.models.value.{SolarIrradianceValue, WeatherValue}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.BaseCsvParams
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Weather.Datasource._
import edu.ie3.simona.exceptions.{InvalidConfigParameterException, ServiceException}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.weather.WeatherSource.{AgentCoordinates, WeightedCoordinates}
import edu.ie3.simona.util.ConfigUtil.CsvConfigUtil.checkBaseCsvParams
import edu.ie3.simona.util.ConfigUtil.DatabaseConfigUtil.{checkCouchbaseParams, checkInfluxDb1xParams, checkSqlParams}
import edu.ie3.simona.util.ParsableEnumeration
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.geo.{CoordinateDistance, GeoUtils}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.interfaces.Irradiance
import org.locationtech.jts.geom.{Coordinate, Point}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import javax.measure.Quantity
import javax.measure.quantity.{Dimensionless, Length, Speed, Temperature}
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

trait WeatherSource {
  protected val idCoordinateSource: IdCoordinateSource

  /** Determine the relevant coordinates around the queried one together with
    * their weighting factors in averaging
    *
    * @param coordinate
    *   Coordinates of requesting Agent
    * @param amountOfInterpolationCoords
    *   The minimum required amount of coordinates with weather data surrounding
    *   the given coordinate that will be used for interpolation
    * @param maxInterpolationCoordinateDistance
    *   The allowed max distance of agent coordinates to the surrounding weather
    *   coordinates
    * @return
    *   The result of the attempt to determine the closest coordinates with
    *   their weighting
    */
  def getWeightedCoordinates(
      coordinate: WeatherSource.AgentCoordinates,
      amountOfInterpolationCoords: Int,
      maxInterpolationCoordinateDistance: Quantity[Length]
  ): Try[WeatherSource.WeightedCoordinates] = {
    getNearestCoordinatesWithDistances(
      coordinate,
      amountOfInterpolationCoords,
      maxInterpolationCoordinateDistance
    ) match {
      case Success(nearestCoordinates) =>
        determineWeights(nearestCoordinates)
      case Failure(exception) =>
        Failure(
          new ServiceException(
            "Determination of coordinate weights failed.",
            exception
          )
        )
    }
  }

  /** Tries to find the nearest coordinates around the queried coordinate. If
    * the queried coordinate hits a weather coordinate directly, only this one
    * is returned. Otherwise, check if the queried coordinate is surrounded in
    * all four quadrants.
    *
    * @param coordinate
    *   Coordinates of requesting Agent
    * @param amountOfInterpolationCoords
    *   The minimum required amount of coordinates with weather data surrounding
    *   the given coordinate that will be used for interpolation
    * @param maxInterpolationCoordinateDistance
    *   The allowed max distance of agent coordinates to the surrounding weather
    *   coordinates
    * @return
    */
  def getNearestCoordinatesWithDistances(
      coordinate: WeatherSource.AgentCoordinates,
      amountOfInterpolationCoords: Int,
      maxInterpolationCoordinateDistance: Quantity[Length]
  ): Try[Iterable[CoordinateDistance]] = {
    val queryPoint = coordinate.toPoint

    /* Go and get the nearest coordinates, that are known to the weather source */
    val nearestCoords = idCoordinateSource
      .getNearestCoordinates(queryPoint, amountOfInterpolationCoords)
      .asScala

    nearestCoords.find(coordinateDistance =>
      coordinateDistance.getCoordinateB.equalsExact(queryPoint, 1e-6)
    ) match {
      case Some(exactHit) =>
        /* The queried coordinate hit one of the weather coordinates. Don't average and take it directly */
        Success(Vector(exactHit))
      case None if nearestCoords.size < amountOfInterpolationCoords =>
        Failure(
          ServiceException(
            s"There are not enough coordinates for averaging. Found ${nearestCoords.size} but need $amountOfInterpolationCoords."
          )
        )
      case None =>
        /* Check if enough coordinates are within the coordinate distance limit */
        val nearestCoordsInMaxDistance = nearestCoords.filter(coordDistance =>
          coordDistance.getDistance
            .isLessThan(maxInterpolationCoordinateDistance)
        )
        if (nearestCoordsInMaxDistance.size < amountOfInterpolationCoords) {
          Failure(
            ServiceException(
              s"There are not enough coordinates within the max coordinate distance of $maxInterpolationCoordinateDistance. Found ${nearestCoordsInMaxDistance.size} but need $amountOfInterpolationCoords."
            )
          )
        } else {
          /* Check, if the queried coordinate is surrounded at each quadrant */
          val (topLeft, topRight, bottomLeft, bottomRight) = nearestCoords
            .map(_.getCoordinateB)
            .foldLeft((false, false, false, false)) {
              case ((tl, tr, bl, br), point) =>
                (
                  tl || (point.getX < queryPoint.getX && point.getY > queryPoint.getY),
                  tr || (point.getX > queryPoint.getX && point.getY > queryPoint.getY),
                  bl || (point.getX < queryPoint.getX && point.getY < queryPoint.getY),
                  br || (point.getX > queryPoint.getX && point.getY < queryPoint.getY)
                )
            }

          /* There has to be a coordinate in each quadrant */
          if (topLeft && topRight && bottomLeft && bottomRight)
            Success(nearestCoords)
          else
            Failure(
              ServiceException(
                s"The queried point shall be surrounded by $amountOfInterpolationCoords weather coordinates, which are in each quadrant. This is not the case."
              )
            )
        }
    }
  }

  /** Determine the weights of each coordinate. It is ensured, that the entirety
    * of weights sum up to 1.0
    *
    * @param nearestCoordinates
    *   Collection of nearest coordinates with their distances
    * @return
    *   An attempt to calculate the average
    */
  def determineWeights(
      nearestCoordinates: Iterable[CoordinateDistance]
  ): Try[WeightedCoordinates] = {
    nearestCoordinates.headOption match {
      case Some(dist) if nearestCoordinates.size == 1 =>
        /* There is only one coordinate -> weight this with 1 */
        Success(
          WeightedCoordinates(Map(dist.getCoordinateB -> 1d))
        )
      case _ =>
        /* There is more than one coordinate or none existent */
        val totalDistanceToSurroundingCoordinates =
          nearestCoordinates.foldLeft(Quantities.getQuantity(0d, Units.METRE)) {
            case (cumulativeDistance, coordinateDistance) =>
              cumulativeDistance.add(coordinateDistance.getDistance)
          }

        /* Partial function, that transfers a distance to proximity */
        val toProximity = (coordinateDistance: CoordinateDistance) =>
          1 - coordinateDistance.getDistance
            .divide(totalDistanceToSurroundingCoordinates)
            .asType(classOf[Dimensionless])
            .to(PowerSystemUnits.PU)
            .getValue
            .doubleValue()

        if (
          totalDistanceToSurroundingCoordinates.isGreaterThan(
            Quantities.getQuantity(0d, Units.METRE)
          )
        ) {
          val weightMap = nearestCoordinates
            .map(coordinateDistance => {
              /* Maybe some words on the calculus of the weight here: We intend to have a weight, that linear increases
               * from zero to one, the closer the coordinate is to the coordinate in question. Therefore we calculate the
               * proximity of each node as a linear function between 1 at 0m distance to the questioned coordinate to zero
               * at the sum of all coordinates' distances (1 - d / d_sum). However, summing up this proximity over all
               * n coordinates brings n*1 from the left part of the sum and -1 as the sum of all distances shares.
               * Thereby all weights sum up to n-1. Therefore, we divide by this to scale the sum of weights to one. */
              val weight =
                toProximity(coordinateDistance) / (nearestCoordinates.size - 1)
              coordinateDistance.getCoordinateB -> weight
            })
            .toMap

          val weightSum = weightMap.values.sum
          if (weightSum > 0.99 && weightSum < 1.01)
            Success(WeightedCoordinates(weightMap))
          else
            Failure(
              ServiceException(
                "The sum of weights differs more than 1 % from 100 %."
              )
            )
        } else
          Failure(
            ServiceException(
              "The total sum of distances to surrounding coordinates is 0 m or less. Therefore averaging would lead to numeric errors."
            )
          )
    }
  }

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
  def getWeather(
      tick: Long,
      weightedCoordinates: WeightedCoordinates
  ): WeatherData

  /** Get the weather data for the given tick and agent coordinates having a
    * weighted average of weather values.
    *
    * @param tick
    *   Simulation date in question
    * @param agentToWeightedCoordinates
    *   The coordinates in question
    * @return
    *   Matching weather data
    */
  def getWeather(
      tick: Long,
      agentToWeightedCoordinates: Map[AgentCoordinates, WeightedCoordinates]
  ): Map[AgentCoordinates, WeatherData] = agentToWeightedCoordinates.map {
    case (agentCoordinates, weightedCoordinates) =>
      agentCoordinates -> getWeather(tick, weightedCoordinates)
  }

  /** Determine an Array with all ticks between the request frame's start and
    * end on which new data is available. Both the request frame's start and end
    * are INCLUDED.
    *
    * @param requestFrameStart
    *   Beginning of the announced request frame
    * @param requestFrameEnd
    *   End of the announced request frame
    * @return
    *   Array with data ticks
    */
  def getDataTicks(
      requestFrameStart: Long,
      requestFrameEnd: Long
  ): Array[Long]
}

object WeatherSource {

  def apply(
      dataSourceConfig: SimonaConfig.Simona.Input.Weather.Datasource,
      simulationStart: ZonedDateTime
  ): WeatherSource =
    checkConfig(dataSourceConfig)(simulationStart)

  /** Check the provided weather data source configuration to ensure its
    * validity. If the configuration is valid, a function to build the
    * corresponding [[WeatherSource]] instance is returned. For any invalid
    * configuration parameters exceptions are thrown.
    *
    * @param weatherDataSourceCfg
    *   the config to be checked
    * @return
    *   a function that can be used to actually build the configured weather
    *   data source
    */
  def checkConfig(
      weatherDataSourceCfg: SimonaConfig.Simona.Input.Weather.Datasource
  ): ZonedDateTime => WeatherSource = {

    // check and get coordinate source
    val coordinateSourceFunction: () => IdCoordinateSource =
      checkCoordinateSource(
        weatherDataSourceCfg.coordinateSource
      )

    /* Check, if the column scheme is supported */
    if (!WeatherScheme.isEligibleInput(weatherDataSourceCfg.scheme))
      throw new InvalidConfigParameterException(
        s"The weather data scheme '${weatherDataSourceCfg.scheme}' is not supported. Supported schemes:\n\t${WeatherScheme.values
            .mkString("\n\t")}"
      )

    // check weather source parameters
    val supportedWeatherSources =
      Set("influxdb1x", "csv", "sql", "couchbase", "sample")
    val definedWeatherSources = Vector(
      weatherDataSourceCfg.sampleParams,
      weatherDataSourceCfg.csvParams,
      weatherDataSourceCfg.influxDb1xParams,
      weatherDataSourceCfg.couchbaseParams,
      weatherDataSourceCfg.sqlParams
    ).filter(_.isDefined)

    val timestampPattern: Option[String] = weatherDataSourceCfg.timestampPattern
    val scheme: String = weatherDataSourceCfg.scheme
    val resolution: Option[Long] = weatherDataSourceCfg.resolution

    // check that only one source is defined
    if (definedWeatherSources.size > 1)
      throw new InvalidConfigParameterException(
        s"Multiple weather sources defined: '${definedWeatherSources.map(_.getClass.getSimpleName).mkString("\n\t")}'." +
          s"Please define only one source!\nAvailable sources:\n\t${supportedWeatherSources.mkString("\n\t")}"
      )
    val weatherSourceFunction: ZonedDateTime => WeatherSource =
      definedWeatherSources.headOption match {
        case Some(
              Some(baseCsvParams @ BaseCsvParams(csvSep, directoryPath, _))
            ) =>
          checkBaseCsvParams(baseCsvParams, "WeatherSource")
          (simulationStart: ZonedDateTime) =>
            WeatherSourceWrapper(
              csvSep,
              directoryPath,
              coordinateSourceFunction,
              timestampPattern,
              scheme,
              resolution
            )(simulationStart)
        case Some(Some(params: CouchbaseParams)) =>
          checkCouchbaseParams(params)
          (simulationStart: ZonedDateTime) =>
            WeatherSourceWrapper(
              params,
              coordinateSourceFunction,
              timestampPattern,
              scheme,
              resolution
            )(simulationStart)
        case Some(Some(params @ InfluxDb1xParams(database, _, url))) =>
          checkInfluxDb1xParams("WeatherSource", url, database)
          (simulationStart: ZonedDateTime) =>
            WeatherSourceWrapper(
              params,
              coordinateSourceFunction,
              timestampPattern,
              scheme,
              resolution
            )(simulationStart)
        case Some(Some(params: SqlParams)) =>
          checkSqlParams(params)
          (simulationStart: ZonedDateTime) =>
            WeatherSourceWrapper(
              params,
              coordinateSourceFunction,
              timestampPattern,
              scheme,
              resolution
            )(simulationStart)
        case Some(Some(_: SampleParams)) =>
          // sample weather, no check required
          // coordinate source must be sample coordinate source
          // calling the function here is not an issue as the sample coordinate source is already
          // an object (= no overhead costs)
          coordinateSourceFunction() match {
            case _: SampleWeatherSource.SampleIdCoordinateSource.type =>
              // all fine
              (simulationStart: ZonedDateTime) =>
                new SampleWeatherSource()(simulationStart)
            case coordinateSource =>
              // cannot use sample weather source with other combination of weather source than sample weather source
              throw new InvalidConfigParameterException(
                s"Invalid coordinate source " +
                  s"'${coordinateSource.getClass.getSimpleName}' defined for SampleWeatherSource. " +
                  "Please adapt the configuration to use sample coordinate source for weather data!"
              )
          }
        case None | Some(_) =>
          throw new InvalidConfigParameterException(
            s"No weather source defined! This is currently not supported! Please provide the config parameters for one " +
              s"of the following weather sources:\n\t${supportedWeatherSources.mkString("\n\t")}"
          )
      }

    weatherSourceFunction
  }

  /** Check the provided coordinate id data source configuration to ensure its
    * validity. If the configuration is valid, a function to build the
    * corresponding [[IdCoordinateSource]] instance is returned. For any invalid
    * configuration parameters exceptions are thrown.
    *
    * @param coordinateSourceConfig
    *   the config to be checked
    * @return
    *   a function that can be used to actually build the configured coordinate
    *   id data source
    */
  private def checkCoordinateSource(
      coordinateSourceConfig: SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource
  ): () => IdCoordinateSource = {
    val supportedCoordinateSources = Set("csv", "sample")
    val definedCoordSources = Vector(
      coordinateSourceConfig.sampleParams,
      coordinateSourceConfig.csvParams
    ).filter(_.isDefined)

    // check that only one source is defined
    if (definedCoordSources.size > 1)
      throw new InvalidConfigParameterException(
        s"Multiple coordinate sources defined: '${definedCoordSources.map(_.getClass.getSimpleName).mkString("\n\t")}'." +
          s"Please define only one source!\nAvailable sources:\n\t${supportedCoordinateSources.mkString("\n\t")}"
      )

    // check source parameters
    definedCoordSources.headOption match {
      case Some(
            Some(baseCsvParams @ BaseCsvParams(csvSep, directoryPath, _))
          ) =>
        checkBaseCsvParams(baseCsvParams, "CoordinateSource")
        val idCoordinateFactory = checkCoordinateFactory(
          coordinateSourceConfig.gridModel
        )
        () =>
          new CsvIdCoordinateSource(
            csvSep,
            directoryPath,
            new FileNamingStrategy(),
            idCoordinateFactory
          )
      case Some(
            Some(
              _: SimonaConfig.Simona.Input.Weather.Datasource.CoordinateSource.SampleParams
            )
          ) =>
        // sample coordinates, no check required
        () => SampleWeatherSource.SampleIdCoordinateSource
      case None | Some(_) =>
        throw new InvalidConfigParameterException(
          s"No coordinate source defined! This is currently not supported! Please provide the config parameters for one " +
            s"of the following coordinate sources:\n\t${supportedCoordinateSources.mkString("\n\t")}"
        )
    }
  }

  /** Check the provided coordinate grid model configuration to ensure its
    * validity. If the configuration is valid, the corresponding
    * IdCoordinateSource is returned. For any invalid configuration parameters
    * exceptions are thrown.
    *
    * @param gridModel
    *   the grid model string to be checked
    * @return
    *   a function that can be used to actually build the id coordinate factory
    *   for the grid model
    */
  private def checkCoordinateFactory(
      gridModel: String
  ): IdCoordinateFactory = {
    if (gridModel.isEmpty)
      throw new InvalidConfigParameterException("No grid model defined!")
    gridModel.toLowerCase() match {
      case "icon"  => new IconIdCoordinateFactory()
      case "cosmo" => new CosmoIdCoordinateFactory()
      case _ =>
        throw new InvalidConfigParameterException(
          s"Grid model '$gridModel' is not supported!"
        )
    }
  }

  /** Represents an empty weather data object
    *
    * For temperature to represent an "empty" quantity, we need to explicitly
    * set temperature to absolute zero, so 0°K. When temperature measures the
    * movement of atoms, absolute zero means no movement, which represents the
    * "empty" concept best.
    */
  val EMPTY_WEATHER_DATA: WeatherData = WeatherData(
    Quantities.getQuantity(0d, StandardUnits.SOLAR_IRRADIANCE),
    Quantities.getQuantity(0d, StandardUnits.SOLAR_IRRADIANCE),
    Quantities.getQuantity(288.15d, Units.KELVIN).to(StandardUnits.TEMPERATURE),
    Quantities.getQuantity(0d, StandardUnits.WIND_VELOCITY)
  )

  /** Methode to get weather data from a time series. This method automatically
    * interpolates missing values.
    * @param timeSeries
    *   with weather values
    * @param dateTime
    *   timestamp in question
    * @return
    *   weather data object
    */
  def getWeatherData(
      timeSeries: IndividualTimeSeries[WeatherValue],
      dateTime: ZonedDateTime
  ): WeatherData = {

    // gets a value option
    val valueOption: Option[WeatherValue] =
      timeSeries.getValue(dateTime).toScala

    valueOption match {
      case Some(value) =>
        // found values are used to create a weather data object
        // missing values are interpolated

        WeatherData(
          value.getSolarIrradiance.getDiffuseIrradiance.toScala
            .getOrElse(interpolateValue(timeSeries, dateTime, "diffIrr")),
          value.getSolarIrradiance.getDirectIrradiance.toScala
            .getOrElse(interpolateValue(timeSeries, dateTime, "dirIrr")),
          value.getTemperature.getTemperature.toScala
            .getOrElse(interpolateValue(timeSeries, dateTime, "temp")),
          value.getWind.getVelocity.toScala
            .getOrElse(interpolateValue(timeSeries, dateTime, "windVel"))
        )
      case None =>
        // if no values are found all values are interpolated

        WeatherData(
          Quantities.getQuantity(
            interpolateValue(timeSeries, dateTime, "diffIrr").getValue,
            StandardUnits.SOLAR_IRRADIANCE
          ),
          Quantities.getQuantity(
            interpolateValue(timeSeries, dateTime, "dirIrr").getValue,
            StandardUnits.SOLAR_IRRADIANCE
          ),
          Quantities.getQuantity(
            interpolateValue(timeSeries, dateTime, "temp").getValue,
            StandardUnits.TEMPERATURE
          ),
          Quantities.getQuantity(
            interpolateValue(timeSeries, dateTime, "windVel").getValue,
            StandardUnits.WIND_VELOCITY
          )
        )
    }
  }

  /** Method for interpolation of weather values.
    * @param timeSeries
    *   with weather data
    * @param dateTime
    *   timestamp for which an interpolation is needed
    * @param get
    *   string defining the searched value
    * @return
    *   new quantity
    */
  def interpolateValue(
      timeSeries: IndividualTimeSeries[WeatherValue],
      dateTime: ZonedDateTime,
      get: String
  ): ComparableQuantity[_] = {
    // gets two options for values
    val previousOption: Option[(ComparableQuantity[_], ZonedDateTime)] =
      getNewValue(
        timeSeries,
        dateTime,
        dateTime.minusHours(2),
        get,
        backwards = true
      )
    val nextOption: Option[(ComparableQuantity[_], ZonedDateTime)] =
      getNewValue(
        timeSeries,
        dateTime,
        dateTime.plusHours(2),
        get,
        backwards = false
      )

    (previousOption, nextOption) match {
      case (Some(preVal), Some(nextVal)) =>
        // only if a previous and a next value are found
        // found values are weighted with their time difference and used to calculate a new value

        val diffToPreviousValue: Long =
          ChronoUnit.SECONDS.between(preVal._2, dateTime)
        val diffToNextValue: Long =
          ChronoUnit.SECONDS.between(dateTime, nextVal._2)

        preVal._1
          .multiply(diffToPreviousValue)
          .add(nextVal._1.multiply(diffToNextValue))
          .divide(diffToPreviousValue + diffToNextValue)
      case (_, _) =>
        // if at least one value could not be found, the value found in the EMPTY_WEATHER_DATA object is returned
        get match {
          case "diffIrr" =>
            EMPTY_WEATHER_DATA.diffIrr
          case "dirIrr" =>
            EMPTY_WEATHER_DATA.dirIrr
          case "temp" =>
            EMPTY_WEATHER_DATA.temp
          case "windVel" =>
            EMPTY_WEATHER_DATA.windVel
        }
    }
  }

  /** Recursive method to find a new value for a specific weather data.
    *
    * @param timeSeries
    *   with weather values
    * @param lastTime
    *   last timestamp
    * @param maxDateTime
    *   after with the recursion ends
    * @param get
    *   string defining the searched value
    * @param backwards
    *   defines if the recursion is backwards or forwards
    * @return
    *   an option for a quantity and a time
    */
  @tailrec
  def getNewValue(
      timeSeries: IndividualTimeSeries[WeatherValue],
      lastTime: ZonedDateTime,
      maxDateTime: ZonedDateTime,
      get: String,
      backwards: Boolean
  ): Option[(ComparableQuantity[_], ZonedDateTime)] = {
    // if one is true, then the recursion ends
    if (backwards && lastTime.isBefore(maxDateTime)) {
      return None
    } else if (!backwards && lastTime.isAfter(maxDateTime)) {
      return None
    }

    // gets the new value option
    val timeBasedValue: Option[TimeBasedValue[WeatherValue]] =
      if (backwards) {
        timeSeries.getPreviousTimeBasedValue(lastTime).toScala
      } else {
        timeSeries.getNextTimeBasedValue(lastTime).toScala
      }

    timeBasedValue match {
      case Some(value) =>
        val weatherValue: WeatherValue = value.getValue

        // gets the searched value as an option
        val quantity: Option[ComparableQuantity[_]] = get match {
          case "diffIrr" =>
            weatherValue.getSolarIrradiance.getDiffuseIrradiance.toScala
          case "dirIrr" =>
            weatherValue.getSolarIrradiance.getDirectIrradiance.toScala
          case "temp" =>
            weatherValue.getTemperature.getTemperature.toScala
          case "windVel" =>
            weatherValue.getWind.getVelocity.toScala
        }

        quantity match {
          case Some(data) =>
            // returning the found value
            Some(data, value.getTime)
          case None =>
            // recursion with found time as a new timestamp
            getNewValue(
              timeSeries,
              value.getTime,
              maxDateTime,
              get,
              backwards
            )
        }
      case None =>
        val newTime: ZonedDateTime = if (backwards) {
          lastTime.minusMinutes(15)
        } else {
          lastTime.plusMinutes(15)
        }

        getNewValue(
          timeSeries,
          newTime,
          maxDateTime,
          get,
          backwards
        )
    }
  }

  /** Weather package private case class to combine the provided agent
    * coordinates into one single entity
    */
  private[weather] final case class AgentCoordinates(
      latitude: Double,
      longitude: Double
  ) {
    def toPoint: Point =
      GeoUtils.DEFAULT_GEOMETRY_FACTORY.createPoint(
        new Coordinate(longitude, latitude)
      )
  }

  /** Package private class to aid the averaging of weather values at
    * coordinates
    *
    * @param weighting
    *   Mapping from weather coordinate to it's weight in averaging
    */
  private[weather] final case class WeightedCoordinates(
      weighting: Map[Point, Double]
  )

  /** Enumeration of all supported weather "column" schemes including
    * permissible config values
    */
  object WeatherScheme extends ParsableEnumeration {
    val ICON: Value = Value("icon")
    val COSMO: Value = Value("cosmo")
  }

}
