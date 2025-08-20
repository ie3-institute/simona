/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.simona.model.em.OptimizedFlexStratIT.toPowerMap
import edu.ie3.simona.model.participant.PowerMathFlexOptions
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.StorageMathFlexOptions
import edu.ie3.simona.model.participant.storage.StorageMathFlexModelSpec.{
  EnergyConversionFactor,
  energyVal,
  pVal,
}
import edu.ie3.simona.test.common.UnitSpec
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import squants.{Each, Power}
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.Hours

import java.util.UUID
import scala.collection.SortedMap

class OptimizedFlexStratIT extends UnitSpec {

  // Testing tolerances
  given Double = 1e-6

  "An optimized flex strat" when {
    "provided with battery and constant constraints" should {
      "compensate the load and feed-in with battery flexibility" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)
        val m1 = model

        given ticks: Seq[Long] = Range.Long(0, 12 * 3600, 3600)

        // 34 kWh of feed-in in total, more than battery can store
        val pvVars = OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-1"),
          PowerMathFlexOptions(
            Seq(0, -6, -8, -8, -12, 0, 0, 0, 0, 0, 0, 0).toPowerMap
          ),
          ticks,
        )

        // 30 kWh of load in total, more than battery can provide
        val loadVars = OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-2"),
          PowerMathFlexOptions(
            Seq(0, 0, 0, 0, 0, 0, 5, 10, 3, 7, 5, 0).toPowerMap
          ),
          ticks,
        )

        val batFo = StorageMathFlexOptions.createAdaptedFlexOptions(
          KilowattHours(0),
          KilowattHours(20),
          Kilowatts(10),
          Each(0.8),
          Each(0.8),
        )
        val batVars = OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-3"),
          batFo,
          ticks,
        )

        // since energy values have been adapted, we need this
        // factor to convert back to "real" values
        given EnergyConversionFactor =
          EnergyConversionFactor(Each(0.8), batFo.eta)

        val objective = OptimizedFlexStrat.buildObjective(
          Iterable(pvVars, loadVars, batVars),
          Kilowatts(0),
          Hours(1),
        )

        model.minimize(objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        batVars.states(0).energyVal should approximate(0d)

        batVars.states.slice(0, 5).foreach {
          _.energyVal should (be >= 0d and be < 20d)
        }

        batVars.states.slice(5, 7).foreach {
          _.energyVal should approximate(20d)
        }

        batVars.states.slice(7, 12).foreach {
          _.energyVal should (be >= 0d and be < 20d)
        }

        batVars.states.slice(12, 14).foreach {
          _.energyVal should approximate(0d)
        }

        // we should've charged 20 kWh plus 5 kWh losses
        val totalCharged = batVars.operationVars.slice(0, 6).map(_.pVal).sum
        totalCharged should approximate(25)

        // we should've discharged 20 kWh minus 4 kWh losses
        val totalDischarged = batVars.operationVars.slice(6, 13).map(_.pVal).sum
        totalDischarged should approximate(-16d)

        model.release()

      }
    }
  }

}

object OptimizedFlexStratIT {

  extension (seq: Seq[Int])
    def toPowerMap(using ticks: Seq[Long]): SortedMap[Long, Power] =
      SortedMap.from(ticks.zip(seq.map(Kilowatts.apply)))

}
