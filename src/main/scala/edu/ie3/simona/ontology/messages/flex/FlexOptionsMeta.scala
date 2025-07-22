/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.IssueFlexControl
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, FlexType}
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.ClassTag

trait FlexOptionsMeta[FO <: FlexOptions: ClassTag] {

  val classTag: ClassTag[FO] = summon[ClassTag[FO]]

  val flexType: FlexType

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
    * the set point is feasible given the flex options.
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

  def createResult(
      flexOptions: FO,
      modelUuid: UUID,
      dateTime: ZonedDateTime,
  ): FlexOptionsResult

}

object FlexOptionsMeta {

  def apply(flexType: FlexType): FlexOptionsMeta[?] =
    flexType match {
      case FlexType.MinMax =>
        MinMaxFlexOptions
    }

}
