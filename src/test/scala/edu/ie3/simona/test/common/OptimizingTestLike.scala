/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.model.em.opt.OptimizingFlexStrat.{
  AssetStepSymbols,
  AssetSymbolContainer,
}
import edu.ie3.simona.service.Data.SecondaryData.{
  ProsumerPrice,
  SecondarySeriesData,
}
import edu.ie3.util.scala.quantities.EuroPerKilowattHour
import org.scalatest.Assertions
import squants.Power
import squants.energy.{KilowattHours, Kilowatts}

import java.util.UUID
import scala.collection.immutable.SortedMap

trait OptimizingTestLike extends Assertions {

  extension [A](seq: Seq[A])(using num: Numeric[A], ticks: Seq[Long])
    def toPowerMap: SortedMap[Long, Power] =
      SortedMap.from(ticks.zip(seq.map(Kilowatts.apply)))

  extension (seq: Seq[(Double, Double)])
    def toPriceData(using ticks: Seq[Long]): SecondarySeriesData =
      SecondarySeriesData(SortedMap.from(ticks.zip(seq.map { case (sell, buy) =>
        ProsumerPrice(EuroPerKilowattHour(sell), EuroPerKilowattHour(buy))
      })))

  extension (vars: AssetStepSymbols) {

    /** The state of energy in kWh, if applicable (NaN else).
      */
    def energyVal: Double =
      vars.getStateOfEnergyResult.toKilowattHours

    /** Power value in kW.
      */
    def pVal: Double =
      vars.getOperatingPowerResult.toKilowatts

  }

  extension [AV <: AssetStepSymbols](
      containers: Iterable[AssetSymbolContainer[AV]]
  ) {

    def vars(uuid: UUID): AssetSymbolContainer[AV] = containers
      .find(_.assetUuid == uuid)
      .getOrElse(fail(s"No asset symbols for battery ($uuid) found."))

    def res(uuid: UUID): IndexedSeq[AV] = vars(uuid).results.headOption
      .getOrElse(fail(s"Empty results for battery ($uuid)."))
      .values
      .toIndexedSeq

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
                        s" ( -> ${vars.getStateOfEnergyResult.in(KilowattHours).rounded(6).toString})"
                    }
                    .mkString(", ")}"
              }
              .mkString("")
        }
        .mkString("")

}
