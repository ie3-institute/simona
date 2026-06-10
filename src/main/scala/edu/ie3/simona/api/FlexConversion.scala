/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.api.data.model.em.SetPoint.{
  AggregatedSetPoint,
  DisaggregatedSetPoints,
}
import edu.ie3.simona.api.data.model.em.{
  SetPoint,
  EnergyBoundariesFlexOptions as ExtEnergyBoundariesFlexOptions,
  FlexOptions as ExtFlexOptions,
  PowerLimitFlexOptions as ExtPowerLimitFlexOptions,
}
import edu.ie3.simona.exceptions.FlexException
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
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
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  toQuantity,
  toSquants,
}
import squants.Power

import java.util.UUID
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.*

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

  def convert(receiver: UUID, ctrlMsg: IssueFlexControl): SetPoint =
    ctrlMsg match {
      case IssueDisaggregatedControl(tick, setPowers) =>
        val disaggregated = setPowers.map { case (uuid, power) =>
          uuid -> new PValue(power.toQuantity)
        }.asJava

        new DisaggregatedSetPoints(receiver, disaggregated)

      case IssueNoControl(tick) =>
        new AggregatedSetPoint(
          receiver
        )

      case IssuePowerControl(tick, setPower) =>
        new AggregatedSetPoint(
          receiver,
          setPower.toQuantity,
        )

      case other =>
        throw new FlexException(s"Cannot convert control message: $other")
    }

  def convertOptions(
      externalFlexOptions: ExtFlexOptions
  ): FlexOptions = {
    externalFlexOptions match {
      case options: ExtPowerLimitFlexOptions =>
        convertPowerLimit(options)

      case options: ExtEnergyBoundariesFlexOptions =>
        convertEnergyBoundaries(options)

      case other =>
        throw FlexException(s"Cannot convert flex option: $other")
    }
  }

  private def convertPowerLimit(
      flexOptions: ExtPowerLimitFlexOptions
  ): PowerLimitFlexOptions =
    PowerLimitFlexOptions(
      flexOptions.pRef.toSquants,
      flexOptions.pMin.toSquants,
      flexOptions.pMax.toSquants,
    )

  private def convertEnergyBoundaries(
      extOptions: ExtEnergyBoundariesFlexOptions
  ): EnergyBoundariesFlexOptions = {
    val convertedAssetBoundaries = extOptions.energyBoundaries.asScala.map {
      assetBoundaries =>
        val energyLimits = assetBoundaries.energyLimits.asScala
          .map {
            case (tick: Long, limits) =>
              tick -> new ClosedInterval(
                limits.getLower.toSquants,
                limits.getUpper.toSquants,
              )
            case _ =>
              throw new FlexException(
                "Error occurred while converting EnergyBoundariesFlexOptions."
              )
          }
          .to(SortedMap)

        new AssetEnergyBoundaries(
          assetBoundaries.currentEnergy.toSquants,
          energyLimits,
          new ClosedInterval(
            assetBoundaries.powerLimits.getLower.toSquants,
            assetBoundaries.powerLimits.getUpper.toSquants,
          ),
          assetBoundaries.etaCharge.toSquants,
          assetBoundaries.etaDischarge.toSquants,
          assetBoundaries.tickDisconnect.toScala,
        )
    }

    EnergyBoundariesFlexOptions(convertedAssetBoundaries.toSeq)
  }

  def convertOptions(
      flexOptions: FlexOptions,
      recipient: UUID,
      model: UUID,
  ): ExtFlexOptions = flexOptions match {
    case PowerLimitFlexOptions(ref, min, max) =>
      new ExtPowerLimitFlexOptions(
        recipient,
        model,
        ref.toQuantity,
        min.toQuantity,
        max.toQuantity,
      )

    case option =>
      throw new FlexException(
        s"Converting $option to external model is not supported yet."
      )
  }

  private def convertPower(pValue: PValue): Power =
    pValue.getP.toScala.map(p => p.toSquants).getOrElse(zeroKW)
}
