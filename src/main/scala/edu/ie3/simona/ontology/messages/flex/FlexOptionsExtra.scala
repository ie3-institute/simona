/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.IssueFlexControl
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.ClassTag

/** Trait for static functionality required for dealing with flexibility
  * options.
  *
  * @tparam FO
  *   The type of flex options.
  */
trait FlexOptionsExtra[FO <: FlexOptions: ClassTag] {

  val classTag: ClassTag[FO] = summon[ClassTag[FO]]

  /** The type of flexibility associated with this extra class.
    */
  val flexType: FlexType

  /** Casts flex options to the required type. Only call this method if you're
    * sure the flex options are of the required type. An exception is thrown
    * otherwise.
    *
    * @param fo
    *   The flex options to type cast
    * @return
    *   The flex options cast to the required type.
    */
  def castFlexOptions(fo: FlexOptions): FO =
    fo match {
      case matched: FO =>
        matched
      case other =>
        throw new CriticalFailureException(
          s"Received flex options of type ${other.getClass.getSimpleName}, but expected ${implicitly[ClassTag[FO]].runtimeClass.getSimpleName}."
        )
    }

  /** Determines the set point given a flex options message and a flex control
    * message. Should also validate the resulting power.
    *
    * @param flexOptions
    *   The flex options.
    * @param flexCtrl
    *   The flex control message.
    * @return
    *   The resulting power set point.
    */
  def determineFlexPower(
      flexOptions: FO,
      flexCtrl: IssueFlexControl,
  ): Power

  /** Checks whether given setPower fits the provided flex options, i.e. whether
    * the set point is feasible given the flex options. Throws a
    * [[edu.ie3.simona.exceptions.FlexException]], if is not.
    *
    * @param flexOptions
    *   The flex options that the set point has to fit.
    * @param setPower
    *   The set point.
    */
  def checkSetPower(
      flexOptions: FO,
      setPower: Power,
  ): Unit

  /** Creates a result entity for the provided flex options.
    *
    * @param flexOptions
    *   The flex options to create a result entity for.
    * @param modelUuid
    *   The UUID of the model that the flex options were computed for.
    * @param dateTime
    *   The date and time for which the flex options are valid.
    * @return
    *   A result entity.
    */
  def createResult(
      flexOptions: FO,
      modelUuid: UUID,
      dateTime: ZonedDateTime,
  ): FlexOptionsResult

  /** Creates flex options for zero KW of power and no available flexibility.
    */
  def zero(tick: Long): FO

}

object FlexOptionsExtra {

  def apply(flexType: FlexType): FlexOptionsExtra[?] =
    flexType match {
      case FlexType.PowerLimit =>
        PowerLimitFlexOptions
      case FlexType.EnergyBoundaries =>
        EnergyBoundariesFlexOptions
    }

}
