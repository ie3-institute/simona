/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.model.em.EmSetPoint
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueDisaggregatedControl,
  IssueFlexControl,
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import squants.Power

import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional

object FlexConversion {

  /** Method to convert an [[EmSetPoint]] to an [[IssueFlexControl]] message.
    * @param tick
    *   The current tick of the simulation.
    * @param setPoint
    *   To convert.
    * @return
    *   The converted [[IssueFlexControl]] message.
    */
  def convert(tick: Long, setPoint: EmSetPoint): IssueFlexControl = {
    val disaggregated = setPoint.disaggregated

    (disaggregated.isEmpty, setPoint.power.toScala) match {
      case (true, None) =>
        IssueNoControl(tick)
      case (true, Some(power)) =>
        IssuePowerControl(tick, convertPower(power))
      case _ =>
        val setPowers = disaggregated.asScala.map { case (receiver, p) =>
          receiver -> convertPower(p)
        }.toMap

        IssueDisaggregatedControl(tick, setPowers)
    }
  }

  /** Converts a [[PValue]] into a [[Power]] value.
    * @param pValue
    *   To convert.
    * @return
    *   The power value.
    */
  private def convertPower(pValue: PValue): Power =
    pValue.getP.toScala.map(_.toSquants).getOrElse(zeroKW)
}
