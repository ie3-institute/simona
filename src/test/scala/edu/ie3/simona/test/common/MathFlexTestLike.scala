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
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.{
  StorageOperationVars,
  StorageStateVars,
}
import optimus.algebra.Const
import optimus.optimization.model.MPVar
import org.scalatest.Assertions.fail
import org.scalatest.OptionValues.convertOptionToValuable
import squants.{Dimensionless, Power}
import squants.energy.{KilowattHours, Kilowatts}

import scala.collection.immutable.SortedMap

/** todo rename
  */
trait MathFlexTestLike {

  extension (seq: Seq[Int])
    def toPowerMap(using ticks: Seq[Long]): SortedMap[Long, Power] =
      SortedMap.from(ticks.zip(seq.map(Kilowatts.apply)))

  extension (state: StorageStateVars)
    def energyVal(using conversion: EnergyConversionFactor): Double = {
      val solution = state.storedEnergy match {
        case constant: Const => constant.value
        case mpVar: MPVar    => mpVar.value.value
      }
      solution * conversion.factor
    }

  extension (res: StepResults)
    def energyVal(using conversion: EnergyConversionFactor): Double = {
      val solution = res.state
        .getOrElse(fail("No state provided in StepResults!"))
        .value
        .value
      solution * conversion.factor
    }

  extension (state: StorageOperationVars)
    def pVal: Double =
      state.p.value.value

  extension (res: StepResults)
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
