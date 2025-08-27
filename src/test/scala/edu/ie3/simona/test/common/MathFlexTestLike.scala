/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.model.em.OptimizedFlexStrat.AssetVarContainer
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.{
  StorageOperationVars,
  StorageStateVars,
}
import optimus.algebra.Const
import optimus.optimization.model.MPVar
import org.scalatest.OptionValues.convertOptionToValuable
import squants.{Dimensionless, Power}
import squants.energy.Kilowatts

import scala.collection.SortedMap

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

  extension (state: StorageOperationVars)
    def pVal: Double =
      state.p.value.value

  final case class EnergyConversionFactor(factor: Double)

  object EnergyConversionFactor {
    def apply(
        regularChargingEta: Dimensionless,
        adaptedEta: Dimensionless,
    ): EnergyConversionFactor =
      new EnergyConversionFactor(regularChargingEta.toEach / adaptedEta.toEach)
  }

  def buildDebugString(
      batVars: AssetVarContainer[StorageStateVars, StorageOperationVars]
  )(using EnergyConversionFactor): String =
    "\n\tDEBUGGING:" +
      s"\n\t\tOperation values: ${batVars.operationVars.map(_.pVal).mkString(", ")}" +
      s"\n\t\tState values: ${batVars.states.map(_.energyVal).mkString(", ")}"

}
