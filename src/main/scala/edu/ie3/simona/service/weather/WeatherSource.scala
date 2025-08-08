/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.io.connectors.SqlConnector
import edu.ie3.datamodel.io.factory.timeseries.{
  CosmoIdCoordinateFactory,
  IconIdCoordinateFactory,
  SqlIdCoordinateFactory,
}
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.IdCoordinateSource
import edu.ie3.datamodel.io.source.csv.{CsvDataSource, CsvIdCoordinateSource}
import edu.ie3.datamodel.io.source.sql.SqlIdCoordinateSource
import edu.ie3.datamodel.models.value.WeatherValue
import edu.ie3.simona.config.InputConfig
import edu.ie3.simona.config.ConfigParams.{
  BaseCsvParams,
  BaseSqlParams,
  SampleParams,
}
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.service.weather.WeatherSource.{
  AgentCoordinates,
  WeightedCoordinates,
}
import edu.ie3.simona.service.weather.WeatherSourceWrapper.buildPSDMSource
import edu.ie3.simona.util.ParsableEnumeration
import edu.ie3.util.geo.{CoordinateDistance, GeoUtils}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.WattsPerSquareMeter
import org.locationtech.jts.geom.{Coordinate, Point}
import squants.motion.MetersPerSecond
import squants.thermal.Kelvin
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.nio.file.Paths
import java.time.ZonedDateTime
import javax.measure.quantity.{Dimensionless, Length}
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

trait WeatherSource {
  protected val idCoordinateSource: IdCoordinateSource
  protected val maxCoordinateDistance: ComparableQuantity[Length]

  /** Determine the relevant coordinates around the queried one together with
    * their weighting factors in averaging
    *
    * @param coordinate
    *   Coordinates of requesting Agent
    * @param amountOfInterpolationCoords
    *   The minimum required amount of coordinates with weather data surrounding
    *   the given coordinate that will be used for interpolation
    * @return
    *   The result of the attempt to determine the closest coordinates with
    *   their weighting
    */
  def getWeightedCoordinates(
      coordinate: WeatherSource.AgentCoordinates,
      amountOfInterpolationCoords: Int,
  ): Try[WeatherSource.WeightedCoordinates] = {
    getNearestCoordinatesWithDistances(
      coordinate,
      amountOfInterpolationCoords,
    ) match {
      case Success(nearestCoordinates) =>
        determineWeights(nearestCoordinates)
      case Failure(exception) =>
        Failure(
          new ServiceException(
            "Determination of coordinate weights failed.",
            exception,
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
    * @return
    */
  def getNearestCoordinatesWithDistances(
      coordinate: WeatherSource.AgentCoordinates,
      amountOfInterpolationCoords: Int,
  ): Try[Iterable[CoordinateDistance]] = {
    val queryPoint = coordinate.toPoint

    /* Go and get the corner coordinates, that are within a given distance */
    val possibleCornerPoints = idCoordinateSource.findCornerPoints(
      queryPoint,
      maxCoordinateDistance,
    )

    possibleCornerPoints.size() match {
      case 1 =>
        // found one exact match
        Success(possibleCornerPoints.asScala)
      case nr if nr == amountOfInterpolationCoords =>
        // found enough points for interpolating
        Success(possibleCornerPoints.asScala)
      case invalidNo =>
        Failure(
          ServiceException(
            s"There are not enough coordinates for averaging. Found $invalidNo within the given distance of " +
              s"$maxCoordinateDistance but need $amountOfInterpolationCoords. Please make sure that there are enough coordinates within the given distance."
          )
        )
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
               * from zero to one, the closer the coordinate is to the coordinate in question. Therefore, we calculate the
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
      weightedCoordinates: WeightedCoordinates,
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
      agentToWeightedCoordinates: Map[AgentCoordinates, WeightedCoordinates],
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
      requestFrameEnd: Long,
  ): Array[Long]
}

object WeatherSource {

  def apply(
      weatherDataSourceCfg: InputConfig.WeatherDatasource
  )(implicit simulationStart: ZonedDateTime): WeatherSource = {
    // get coordinate source
    implicit val coordinateSourceFunction: IdCoordinateSource =
      buildCoordinateSource(weatherDataSourceCfg.coordinateSource)

    val definedWeatherSources = Vector(
      weatherDataSourceCfg.sampleParams,
      weatherDataSourceCfg.csvParams,
      weatherDataSourceCfg.influxDb1xParams,
      weatherDataSourceCfg.couchbaseParams,
      weatherDataSourceCfg.sqlParams,
    ).find(_.isDefined).flatten

    if (definedWeatherSources.isEmpty) {
      // should not happen, due to the config fail fast check
      throw new SourceException(
        s"Expected a WeatherSource, but no source where defined in $weatherDataSourceCfg."
      )
    }

    implicit val resolution: Long = weatherDataSourceCfg.resolution
    implicit val distance: ComparableQuantity[Length] =
      Quantities.getQuantity(
        weatherDataSourceCfg.maxCoordinateDistance,
        Units.METRE,
      )

    buildPSDMSource(weatherDataSourceCfg, definedWeatherSources)
      .map(WeatherSourceWrapper.apply)
      .getOrElse(new SampleWeatherSource())
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
  private def buildCoordinateSource(
      coordinateSourceConfig: InputConfig.CoordinateSource
  ): IdCoordinateSource = {
    val definedCoordSources = Vector(
      coordinateSourceConfig.sampleParams,
      coordinateSourceConfig.csvParams,
      coordinateSourceConfig.sqlParams,
    ).find(_.isDefined).flatten

    definedCoordSources match {
      case Some(
            BaseCsvParams(csvSep, directoryPath, _)
          ) =>
        val idCoordinateFactory =
          coordinateSourceConfig.gridModel.toLowerCase match {
            case "icon"  => new IconIdCoordinateFactory()
            case "cosmo" => new CosmoIdCoordinateFactory()
          }

        new CsvIdCoordinateSource(
          idCoordinateFactory,
          new CsvDataSource(
            csvSep,
            Paths.get(directoryPath),
            new FileNamingStrategy(),
          ),
        )
      case Some(
            BaseSqlParams(
              jdbcUrl,
              password,
              schemaName,
              tableName,
              userName,
            )
          ) =>
        new SqlIdCoordinateSource(
          new SqlConnector(jdbcUrl, userName, password),
          schemaName,
          tableName,
          new SqlIdCoordinateFactory(),
        )
      case Some(_: SampleParams) =>
        // sample coordinates, no check required
        SampleWeatherSource.SampleIdCoordinateSource
      case None =>
        throw new SourceException(
          s"Expected an IdCoordinateSource, but no source where defined in $coordinateSourceConfig."
        );
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
    WattsPerSquareMeter(0.0),
    WattsPerSquareMeter(0.0),
    Kelvin(0d),
    MetersPerSecond(0d),
  )

  def toWeatherData(
      weatherValue: WeatherValue
  ): WeatherData = {
    WeatherData(
      weatherValue.getSolarIrradiance.getDiffuseIrradiance.toScala match {
        case Some(irradiance) =>
          WattsPerSquareMeter(
            irradiance
              .to(PowerSystemUnits.WATT_PER_SQUAREMETRE)
              .getValue
              .doubleValue()
          )
        case None => EMPTY_WEATHER_DATA.diffIrr
      },
      weatherValue.getSolarIrradiance.getDirectIrradiance.toScala match {
        case Some(irradiance) =>
          WattsPerSquareMeter(
            irradiance
              .to(PowerSystemUnits.WATT_PER_SQUAREMETRE)
              .getValue
              .doubleValue()
          )
        case None => EMPTY_WEATHER_DATA.dirIrr
      },
      weatherValue.getTemperature.getTemperature.toScala match {
        case Some(temperature) =>
          Kelvin(
            temperature
              .to(Units.KELVIN)
              .getValue
              .doubleValue()
          )
        case None => EMPTY_WEATHER_DATA.temp
      },
      weatherValue.getWind.getVelocity.toScala match {
        case Some(windVel) =>
          MetersPerSecond(
            windVel
              .to(Units.METRE_PER_SECOND)
              .getValue
              .doubleValue()
          )
        case None => EMPTY_WEATHER_DATA.windVel
      },
    )

  }

  /** Weather package private case class to combine the provided agent
    * coordinates into one single entity
    */
  private[weather] final case class AgentCoordinates(
      latitude: Double,
      longitude: Double,
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
    *   Mapping from weather coordinate to its weight in averaging
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
