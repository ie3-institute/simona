/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.weather

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.timeseries.individual.IndividualTimeSeries
import edu.ie3.datamodel.models.value.WeatherValue
import edu.ie3.simona.ontology.messages.services.WeatherMessage.ValueWithWeight
import edu.ie3.util.scala.quantities.QuantitySquantsConversions._
import squants.Quantity

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.jdk.OptionConverters.RichOptional

object WeatherValueInterpolation extends LazyLogging {

  /** Method for interpolating weather values.
    *
    * @param timeSeries
    *   with weather data
    * @param dateTime
    *   timestamp for which an interpolation is needed
    * @param typeString
    *   string containing the searched value
    * @param empty
    *   default value, if no other value was found
    * @tparam V
    *   type of value
    * @return
    *   a new quantity
    */
  def interpolate[V <: Quantity[V]](
      timeSeries: IndividualTimeSeries[WeatherValue],
      dateTime: ZonedDateTime,
      typeString: String,
      empty: V
  ): V = {
    getValueOptions[V](timeSeries, dateTime, typeString) match {
      case Some((preVal, preWeight, nextVal, nextWeight)) =>
        val interval: Long = preWeight + nextWeight

        val weightedQuantity1 = preVal * preWeight
        val weightedQuantity2 = nextVal * nextWeight

        (weightedQuantity1 + weightedQuantity2) / interval
      case None =>
        logger.warn(
          s"Interpolating value with unit ${empty.unit} for timestamp $dateTime was not possible. The default value is used."
        )
        empty
    }
  }

  /** Method for getting interval and values for an interpolation-
    *
    * @param timeSeries
    *   with weather data
    * @param dateTime
    *   timestamp for which an interpolation is needed
    * @param typeString
    *   string containing the searched value
    * @tparam V
    *   type of value
    * @return
    *   an option
    */
  private def getValueOptions[V](
      timeSeries: IndividualTimeSeries[WeatherValue],
      dateTime: ZonedDateTime,
      typeString: String
  ): Option[(V, Long, V, Long)] = {
    if (timeSeries.getEntries.size() < 3) {
      logger.info(
        s"Not enough entries in time series $timeSeries to interpolate weather data. At least three values are needed, found ${timeSeries.getEntries.size()}."
      )
      None
    } else {
      val intervalStart: ZonedDateTime = dateTime.minusHours(2)
      val intervalEnd: ZonedDateTime = dateTime.plusHours(2)

      val previous: Option[ValueWithWeight[V]] = getValue(
        timeSeries,
        dateTime,
        intervalStart,
        dateTime,
        typeString
      )
      val next: Option[ValueWithWeight[V]] =
        getValue(timeSeries, dateTime, dateTime, intervalEnd, typeString)

      (previous, next) match {
        case (Some(previous), Some(next)) =>
          Some((previous.value, previous.weight, next.value, next.weight))
        case (_, _) =>
          logger.warn(
            s"Interpolating value $typeString for timestamp $dateTime was not possible. The default value is used."
          )
          None
      }
    }
  }

  /** Method to get a weather value with its weight from an interval of a time
    * series.
    *
    * @param timeSeries
    *   given time series
    * @param timestamp
    *   given timestamp
    * @param intervalStart
    *   start of the interval
    * @param intervalEnd
    *   end of the interval
    * @param typeString
    *   value that is searched
    * @return
    *   an option of a quantity with a weight
    */
  private def getValue[V](
      timeSeries: IndividualTimeSeries[WeatherValue],
      timestamp: ZonedDateTime,
      intervalStart: ZonedDateTime,
      intervalEnd: ZonedDateTime,
      typeString: String
  ): Option[ValueWithWeight[V]] = {
    val values: List[ValueWithWeight[V]] =
      timeSeries.getEntries.asScala.flatMap { weatherValue =>
        val time: ZonedDateTime = weatherValue.getTime

        // calculates the time difference to the given timestamp
        val weight = if (time.isBefore(timestamp)) {
          ChronoUnit.SECONDS.between(time, timestamp)
        } else {
          ChronoUnit.SECONDS.between(timestamp, time)
        }

        // check is the found timestamp is in the defined interval
        if (time.isAfter(intervalStart) && time.isBefore(intervalEnd)) {
          getValue[V](weatherValue.getValue, typeString).map { value =>
            ValueWithWeight(value, weight)
          }
        } else {
          // if timestamp is not inside is not inside the interval none is returned
          None
        }
      }.toList

    if (values.isEmpty) {
      None
    } else {
      // sorting the list to return the value with the least time difference
      val sortedSet: Set[ValueWithWeight[V]] = values.sortBy { x =>
        x.weight
      }.toSet

      sortedSet.headOption
    }
  }

  /** Method to get a value from a [[WeatherValue]]..
    *
    * @param weatherValue
    *   given value
    * @param typeString
    *   value that is searched
    * @return
    *   an option for a quantity
    */
  private def getValue[V](
      weatherValue: WeatherValue,
      typeString: String
  ): Option[V] = {
    typeString match {
      case "diffIrr" =>
        weatherValue.getSolarIrradiance.getDiffuseIrradiance.toScala
          .map(v => v.toSquants)
          .asInstanceOf[Option[V]]
      case "dirIrr" =>
        weatherValue.getSolarIrradiance.getDirectIrradiance.toScala
          .map(v => v.toSquants)
          .asInstanceOf[Option[V]]
      case "temp" =>
        weatherValue.getTemperature.getTemperature.toScala
          .map(v => v.toSquants)
          .asInstanceOf[Option[V]]
      case "windVel" =>
        weatherValue.getWind.getVelocity.toScala
          .map(v => v.toSquants)
          .asInstanceOf[Option[V]]
      case _ =>
        logger.warn(
          s"Getting value of type $typeString is not implemented yet."
        )
        None
    }
  }
}
