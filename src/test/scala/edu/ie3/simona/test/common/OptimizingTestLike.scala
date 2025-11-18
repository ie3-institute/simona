/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.model.em.opt.OptimizedFlexStrat.{
  AssetVarContainer,
  StepResults,
}
import optimus.algebra.Const
import optimus.optimization.model.MPVar
import org.scalatest.Assertions.fail
import org.scalatest.OptionValues.convertOptionToValuable
import squants.{Dimensionless, Power}
import squants.energy.{KilowattHours, Kilowatts}

import scala.collection.immutable.SortedMap

trait OptimizingTestLike {

  extension (seq: Seq[Int])
    def toPowerMap(using ticks: Seq[Long]): SortedMap[Long, Power] =
      SortedMap.from(ticks.zip(seq.map(Kilowatts.apply)))

  extension (res: StepResults)

    /** Energy value related to the start of the optimization, which is always
      * set to 0 kWh. The value is converted back to the physical model from the
      * adapted model that was used for optimization.
      *
      * @param conversion
      *   The conversion factor.
      * @return
      *   The energy value in kWh.
      */
    def energyVal(using conversion: EnergyConversionFactor): Double = {
      val solution = res.state
        .getOrElse(fail("No state provided in StepResults!")) match {
        case variable: MPVar =>
          variable.value.value
        case const: Const =>
          const.value
      }

      solution * conversion.factor
    }

    /** Power value in kW.
      *
      * @return
      *   The power value in kW.
      */
    def pVal: Double =
      res.getOperationResult.toKilowatts

  final case class EnergyConversionFactor(factor: Double)

  object EnergyConversionFactor {
    def apply(
        regularChargingEta: Dimensionless,
        adaptedEta: Dimensionless,
    ): EnergyConversionFactor =
      new EnergyConversionFactor(regularChargingEta / adaptedEta)
  }

  def buildDebugString(
      assetVars: AssetVarContainer
  )(using EnergyConversionFactor): String =
    s"\n\tDEBUGGING asset ${assetVars.assetUuid}:" +
      assetVars.results
        .map { res =>
          s"\n\t\tTrajectory: ${res
              .map(step =>
                step.getOperationResult.toString +
                  step.state.map(_ => s" ( -> ${KilowattHours(step.energyVal).toString})").getOrElse("")
              )
              .mkString(", ")}"
        }
        .mkString("")

}
