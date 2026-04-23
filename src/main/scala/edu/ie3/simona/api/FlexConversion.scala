/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.model.em.{
  SetPoint,
  EnergyBoundariesFlexOptions as ExtEnergyBoundariesFlexOptions,
  FlexOptions as ExtFlexOptions,
  PowerLimitFlexOptions as ExtPowerLimitFlexOptions,
}
import edu.ie3.simona.exceptions.FlexException
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueDisaggregatedControl,
  IssueFlexControl,
  IssueNoControl,
  IssuePowerControl,
}
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  toQuantity,
  toSquants,
}
import squants.Power

import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional

object FlexConversion {

  def convert(tick: Long, setPoint: SetPoint): IssueFlexControl =
    setPoint match {
      case setPoint: SetPoint.AggregatedSetPoint if setPoint.power.isPresent =>
        IssuePowerControl(tick, convertPower(setPoint.power.get))

      case setPoint: SetPoint.AggregatedSetPoint =>
        IssueNoControl(tick)

      case setPoint: SetPoint.DisaggregatedSetPoints =>
        val setPowers = setPoint.disaggregated.asScala.map {
          case (receiver, p) =>
            receiver -> convertPower(p)
        }.toMap

        IssueDisaggregatedControl(tick, setPowers)

      case other =>
        throw new FlexException(s"Cannot convert set point: $other")
    }

  def convertOptions[F <: ExtFlexOptions](
      externalFlexOptions: F
  ): FlexOptions =
    externalFlexOptions match {
      case options: ExtPowerLimitFlexOptions =>
        PowerLimitFlexOptions(options)

      case options: ExtEnergyBoundariesFlexOptions =>
        EnergyBoundariesFlexOptions(options)

      case other =>
        throw FlexException(s"Cannot convert flex option: $other")
    }

  private def convertPower(pValue: PValue): Power =
    pValue.getP.toScala.map(p => p.toSquants).getOrElse(zeroKW)
}
