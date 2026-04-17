/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.model.em.SetPoint
import edu.ie3.simona.exceptions.FlexException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  toQuantity,
  toSquants,
}
import squants.Power

import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.{RichOption, RichOptional}

object FlexConversion {

  def convert(tick: Long, setPoint: SetPoint): IssueFlexControl =
    setPoint match {
      case setPoint: SetPoint.AggregatedSetPoint if setPoint.power.isPresent =>
        IssuePowerControl(tick, convertPower(setPoint.power.get))

      case setPoint: SetPoint.AggregatedSetPoint =>
        IssueNoControl(tick)

      case other =>
        throw new FlexException(s"Cannot convert set point: $other")
    }

  private def convertPower(pValue: PValue): Power =
    pValue.getP.toScala.map(p => p.toSquants).getOrElse(zeroKW)
}
