/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.datamodel.models.input.AssetInput
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexOptions,
  PowerLimitFlexOptions,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.energy.Kilowatts

/** Offers method for aggregating flex options from connected agents which will
  * then be provided to a superior EmAgent or sent out as a flex result
  */
trait EmAggregateFlex[FO <: FlexOptions] {

  /** Aggregates flex options of connected devices to one flex options object
    * that describes the flexibility of this EmAgent.
    *
    * @param flexOptions
    *   The flex options of all connected agents.
    * @return
    *   The aggregated flex options.
    */
  def aggregateFlexOptions(
      flexOptions: Iterable[
        (? <: AssetInput, FO)
      ]
  ): FO

}

object EmAggregateFlex {

  def parsePowerLimitModel
      : PartialFunction[String, EmAggregateFlex[PowerLimitFlexOptions]] = {
    case "SELF_OPT_EXCL_REG" =>
      EmAggregatePowerOpt(zeroKW, curtailRegenerative = false)
    case "SELF_OPT"   => EmAggregatePowerOpt(zeroKW, curtailRegenerative = true)
    case "SIMPLE_SUM" => EmAggregateSimpleSum

    case powerTargetAbsString
        if powerTargetAbsString.startsWith("SELF_POWER_") =>
      val pattern = """SELF_POWER_([\d.]+)(_EXCL_REG)?""".r
      powerTargetAbsString match {
        case pattern(value, exclReg) =>
          try {
            val powerTargetAbs = BigDecimal(value)
            val curtailRegenerative = exclReg == null
            EmAggregatePowerOpt(
              Kilowatts(powerTargetAbs),
              curtailRegenerative,
            )
          } catch {
            case _: NumberFormatException =>
              throw new CriticalFailureException(
                s"Invalid numeric value in aggregate flex strategy: $powerTargetAbsString"
              )
          }
        case _ =>
          throw new CriticalFailureException(
            s"Invalid format for aggregate flex strategy: $powerTargetAbsString"
          )
      }
  }

  def parseOptimizingModel: PartialFunction[String, EmAggregateFlex[
    EnergyBoundariesFlexOptions
  ]] = { case "SIMPLE_BOUNDARIES" =>
    EmAggregateEnergyBoundariesSimple
  }

}
