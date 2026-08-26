/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.model.em.opt.FlexibilityOptimization.TimeParams
import edu.ie3.simona.model.em.opt.impl.ObjectiveFactory.{
  AssetStepSymbols,
  AssetSymbolContainer,
}
import edu.ie3.simona.service.Data.SecondaryData.{
  ProsumerPrice,
  SecondarySeriesData,
}
import edu.ie3.util.scala.quantities.EuroPerKilowattHour
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

import java.util.UUID
import scala.collection.immutable.SortedMap

trait OptimizingTestLike extends Matchers {

  extension [A](seq: Seq[A])(using num: Numeric[A])
    def toPowerMap(timeParams: TimeParams): SortedMap[Long, Power] =
      SortedMap.from(timeParams.ticks.zip(seq.map(Kilowatts.apply)))

  extension (seq: Seq[(Double, Double)])
    def toPriceData(timeParams: TimeParams): SecondarySeriesData =
      SecondarySeriesData(SortedMap.from(timeParams.ticks.zip(seq.map {
        case (sell, buy) =>
          ProsumerPrice(EuroPerKilowattHour(sell), EuroPerKilowattHour(buy))
      })))

  extension (symbols: AssetStepSymbols) {

    /** The state of energy in kWh, if applicable (NaN else).
      */
    def energyVal: Double =
      symbols.getStepEndEnergyResult.toKilowattHours

    /** Power value in kW.
      */
    def pVal: Double =
      symbols.getOperatingPowerResult.toKilowatts

  }

  extension (symbolsSeq: IndexedSeq[? <: AssetStepSymbols]) {

    /** Sum of actual loss in kWh.
      */
    def actualLossSum: Double =
      symbolsSeq.map(_.getActualLoss.toKilowattHours).sum

    /** Sum of excess loss in kWh.
      */
    def excessLossSum: Double =
      symbolsSeq.map(_.getExcessLoss.toKilowattHours).sum

  }

  extension (
      containers: Iterable[AssetSymbolContainer[? <: AssetStepSymbols]]
  ) {

    def vars(uuid: UUID): AssetSymbolContainer[? <: AssetStepSymbols] =
      containers
        .find(_.assetUuid == uuid)
        .getOrElse(fail(s"No asset symbols for ($uuid) found."))

    def res(uuid: UUID): IndexedSeq[? <: AssetStepSymbols] =
      vars(uuid).results.headOption
        .getOrElse(fail(s"Empty results for ($uuid)."))
        .values
        .toIndexedSeq

    def checkModelStateError(using tolerance: Energy): Unit = {
      val allErrors = containers.flatMap { container =>
        val containerErrors = container.results.flatMap { assetSymbolsSeq =>
          val errors = assetSymbolsSeq
            .map { case (_, assetSymbols) =>
              s"${assetSymbols.parameters.stepStartTick} -> ${assetSymbols.parameters.stepEndTick}"
                -> assetSymbols.getExcessLoss
            }
            .filter { case (_, error) =>
              error > tolerance
            }

          Option.when(errors.nonEmpty)(errors)
        }

        if containerErrors.nonEmpty then
          Some(
            s"\n\t\t${container.assetUuid}:" + containerErrors
              .map { errors =>
                s"\n\t\t\tState errors: ${errors
                    .map { case (tickRange, error) =>
                      s"$tickRange: ${error.in(KilowattHours).rounded(6).toString}"
                    }
                    .mkString(", ")}"
              }
              .mkString("")
          )
        else None
      }

      if allErrors.nonEmpty then
        fail(
          s"Model state errors higher than allowed tolerance $tolerance \n\tDEBUGGING state errors:" + allErrors
            .mkString("")
        )
    }

    def checkStructure(expectedAssets: Int, expectedTimeSteps: Int): Unit = {
      containers.toSeq should have size expectedAssets
      containers.foreach(_.results should have size 1)
      containers.foreach(
        _.results.foreach(_ should have size expectedTimeSteps)
      )
    }

  }

  extension (
      container: AssetSymbolContainer[? <: AssetStepSymbols]
  ) {

    def getEnergyResultsTable: Seq[String] = {
      val head = "step,p,e_stored,e_actual_loss,e_excess_loss\n"

      container.results.map { assetSymbolsSeq =>
        val firstRow = assetSymbolsSeq.headOption
          .map { case (_, assetSymbols) =>
            s"0,0,${assetSymbols.getStepStartEnergyResult.toRoundedKiloWattHours},0,0\n"
          }
          .getOrElse("")

        val dataRows = assetSymbolsSeq.zipWithIndex
          .map { case ((_, assetSymbols), step) =>
            s"${step + 1},${assetSymbols.getOperatingPowerResult.toRoundedKiloWatts},${assetSymbols.getStepEndEnergyResult.toRoundedKiloWattHours},${assetSymbols.getActualLoss.toRoundedKiloWattHours},${assetSymbols.getExcessLoss.toRoundedKiloWattHours}\n"
          }
          .mkString("")

        head + firstRow + dataRows

      }

    }

  }

  extension (power: Power) {
    def toRoundedKiloWatts: String =
      power.in(Kilowatts).rounded(6).toKilowatts.toString
  }

  extension (energy: Energy) {
    def toRoundedKiloWattHours: String =
      energy.in(KilowattHours).rounded(6).toKilowattHours.toString
  }

  def buildDebugString(
      containers: Iterable[AssetSymbolContainer[? <: AssetStepSymbols]]
  ): String =
    s"\n\tDEBUGGING asset symbols:" +
      containers
        .map { container =>
          s"\n\t\t${container.assetUuid}:" +
            container.results
              .map { sortedVars =>
                s"\n\t\t\tTrajectory: ${sortedVars
                    .map { case (_, vars) =>
                      vars.getOperatingPowerResult.toRoundedKiloWatts +
                        s" (-> ${vars.getStepEndEnergyResult.toRoundedKiloWattHours})"
                    }
                    .mkString(", ")}"
              }
              .mkString("")
        }
        .mkString("")

}
