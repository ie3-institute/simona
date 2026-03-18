package edu.ie3.simona.api

import edu.ie3.simona.api.data.model.em.EmSetPoint
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.IssueFlexControl.convertPower
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{IssueDisaggregatedControl, IssueFlexControl, IssueNoControl, IssuePowerControl}

object FlexConversion {


  def convert(tick: Long, setPoint: EmSetPoint): IssueFlexControl  = {
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


  private def convertPower(pValue: PValue): Power =
    pValue.getP.toScala.map(p => p.toSquants).getOrElse(zeroKW)
}
