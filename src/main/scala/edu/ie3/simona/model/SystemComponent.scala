/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model

import java.time.ZonedDateTime
import java.util.UUID

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.exceptions.{
  InvalidActionRequestException,
  InvalidParameterException,
}
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.scala.OperationInterval

import scala.jdk.OptionConverters._
import scala.util.{Failure, Success, Try}

/** Interface that is implemented by all grid elements (e.g. lines,
  * transformers, nodes, switches) to provide shared basic attributes and
  * functions.
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  */
abstract class SystemComponent(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
) extends LazyLogging {

  private val elementType: String = this.getClass.getSimpleName

  // check if an uuid is provided
  if (Option.apply(uuid).isEmpty)
    throw new InvalidParameterException(
      s"Uuid of $elementType $id cannot be null!"
    )

  private var _inOperation = false

  /** Enable the corresponding element.
    */
  def enable(): Try[String] = {
    if (_inOperation) {
      Failure(
        new InvalidActionRequestException(
          s"$elementType $id is already in operation!"
        )
      )
    } else {
      _inOperation = true
      Success(s"$elementType $id enabled!")
    }
  }

  /** Disable the corresponding element.
    */
  def disable(): Try[String] = {
    if (_inOperation) {
      _inOperation = false
      Success(s"$elementType $id disabled!")
    } else {
      Failure(
        new InvalidActionRequestException(
          s"$elementType $id is already out of operation!"
        )
      )
    }
  }

  def isInOperation: Boolean = _inOperation

  /** Determine, if the system actually is in operation at the given tick
    *
    * @param tick
    *   The questioned tick
    * @return
    *   true, if the system is in operation
    */
  def isInOperation(tick: Long): Boolean =
    isInOperation && operationInterval.includes(tick)
}

case object SystemComponent {

  /** Determine the operation interval in ticks based on the given
    * [[ZonedDateTime]] s
    * @param startDate
    *   The start date of the simulation
    * @param endDate
    *   The end date of the simulation
    * @param operationTime
    *   Operation time span provided by input model
    * @return
    *   The tick interval, in which the system ist in operation
    */
  def determineOperationInterval(
      startDate: ZonedDateTime,
      endDate: ZonedDateTime,
      operationTime: OperationTime,
  ): OperationInterval = {
    val operationStartOpt = operationTime.getStartDate.toScala
    val operationEndOpt = operationTime.getEndDate.toScala

    // check if operates from and operates until is valid
    (operationStartOpt, operationEndOpt) match {
      case (Some(start), Some(end)) if end.isBefore(start) =>
        throw new InvalidParameterException(
          "The defined operation end is before it's operation start."
        )
      case _ =>
    }

    val startTick = operationStartOpt match {
      case Some(operationStart) =>
        operationStart.toTick(startDate)
      case None => 0
    }

    val endTick = operationEndOpt match {
      case Some(operationEnd) => operationEnd.toTick(startDate)
      case None               => endDate.toTick(startDate)
    }

    OperationInterval(startTick, endTick)
  }
}
