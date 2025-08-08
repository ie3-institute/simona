/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.simona.model.em.OptimizedFlexStratIT.toPowerMap
import edu.ie3.simona.model.participant.PowerMathFlexOptions
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.StorageMathFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import squants.{Each, Power}
import squants.energy.{KilowattHours, Kilowatts}

import java.util.UUID
import scala.collection.SortedMap

class OptimizedFlexStratIT extends UnitSpec {

  "An optimized flex strat" when {
    "provided with battery and constant constraints" should {
      "compensate the load and feed-in with battery flexibility" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        given ticks: Seq[Long] = Range.Long(0, 12 * 3600, 3600)

        val pvVars = OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-1"),
          PowerMathFlexOptions(
            Seq(0, -6, -8, -8, -12, 0, 0, 0, 0, 0, 0, 0).toPowerMap
          ),
          ticks,
        )

        val loadVars = OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-2"),
          PowerMathFlexOptions(
            Seq(0, 0, 0, 0, 0, 0, 5, 10, 3, 10, 10, 1).toPowerMap
          ),
          ticks,
        )

        val batVars = OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-3"),
          StorageMathFlexOptions(
            KilowattHours(0),
            KilowattHours(20),
            Kilowatts(10),
            Each(0.8),
          ),
          ticks,
        )

        val objective = OptimizedFlexStrat.buildObjective(
          Iterable(pvVars, loadVars, batVars),
          Kilowatts(0),
        )

        model.minimize(objective)

        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        println(batVars.operationVars.map(_.getPowerSolution))

      }
    }
  }

}

object OptimizedFlexStratIT {

  extension (seq: Seq[Int])
    def toPowerMap(using ticks: Seq[Long]): SortedMap[Long, Power] =
      SortedMap.from(ticks.zip(seq.map(Kilowatts.apply)))

}
