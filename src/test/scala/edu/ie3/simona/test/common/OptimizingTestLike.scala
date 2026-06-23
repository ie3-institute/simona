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

  extension (vars: AssetStepSymbols) {

    /** The state of energy in kWh, if applicable (NaN else).
      */
    def energyVal: Double =
      vars.getStepEndEnergyResult.toKilowattHours

    /** Power value in kW.
      */
    def pVal: Double =
      vars.getOperatingPowerResult.toKilowatts

  }

  // todo extension on results
  extension (
      containers: Iterable[AssetSymbolContainer[? <: AssetStepSymbols]]
  ) {

    def vars(uuid: UUID): AssetSymbolContainer[? <: AssetStepSymbols] =
      containers
        .find(_.assetUuid == uuid)
        .getOrElse(fail(s"No asset symbols for battery ($uuid) found."))

    def res(uuid: UUID): IndexedSeq[? <: AssetStepSymbols] =
      vars(uuid).results.headOption
        .getOrElse(fail(s"Empty results for battery ($uuid)."))
        .values
        .toIndexedSeq

    def checkModelStateError(using tolerance: Energy): Unit =
      containers.flatMap(_.getStateCalcErrors).foreach { error =>
        assert(
          error < tolerance,
          s"Model state calculation error $error is higher than allowed ($tolerance).",
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

  def buildDebugString(
      containers: Iterable[AssetSymbolContainer[? <: AssetStepSymbols]]
  ): String =
    s"\n\tDEBUGGING asset symbols:" +
      containers
        .map { container =>
          s"\n\t\t ${container.assetUuid}:" +
            container.results
              .map { sortedVars =>
                s"\n\t\t\tTrajectory: ${sortedVars
                    .map { case (_, vars) =>
                      vars.getOperatingPowerResult.in(Kilowatts).rounded(6).toString +
                        s" ( -> ${vars.getStepEndEnergyResult.in(KilowattHours).rounded(6).toString})"
                    }
                    .mkString(", ")}"
              }
              .mkString("")
        }
        .mkString("")

}
