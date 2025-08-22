/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.simona.model.em.OptimizedFlexStrat.{
  AssetVarContainer,
  LinearizedQuadraticPowerObjectiveFactory,
  MinAbsPowerObjectiveFactory,
}
import edu.ie3.simona.model.participant.PowerSeriesMathFlexOptions
import edu.ie3.simona.model.participant.PowerSeriesMathFlexOptions.{
  PowerOperationVars,
  PowerStateVars,
}
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.{
  StorageMathFlexOptions,
  StorageOperationVars,
  StorageStateVars,
}
import edu.ie3.simona.test.common.{MathFlexTestLike, UnitSpec}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.Hours

import java.util.UUID

class OptimizedFlexStratIT extends UnitSpec with MathFlexTestLike {

  // Testing tolerances
  given Double = 1e-6

  "An optimized flex strat" when {
    "provided with battery and constant constraints" should {

      given ticks: Seq[Long] = Range.Long.inclusive(0, 12 * 3600, 3600)

      // 33 kWh of feed-in in total, more than battery can store
      def createPvVars(using
          MPModel
      ): AssetVarContainer[PowerStateVars, PowerOperationVars] =
        OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-1"),
          PowerSeriesMathFlexOptions(
            Seq(0, -6, -8, -7, -12, 0, 0, 0, 0, 0, 0, 0).toPowerMap
          ),
          ticks,
        )

      // 31 kWh of load in total, more than battery can provide
      def createLoadVars(using
          MPModel
      ): AssetVarContainer[PowerStateVars, PowerOperationVars] =
        OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-2"),
          PowerSeriesMathFlexOptions(
            Seq(0, 0, 0, 0, 0, 0, 5, 10, 3, 7, 6, 0).toPowerMap
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
      def createBatVars(using
          MPModel
      ): AssetVarContainer[StorageStateVars, StorageOperationVars] =
        OptimizedFlexStrat.addAssetConstraints(
          UUID.fromString("0-0-0-0-3"),
          batFo,
          ticks,
        )

      // since energy values have been adapted, we need this
      // factor to convert back to "real" values
      given EnergyConversionFactor =
        EnergyConversionFactor(Each(0.8), batFo.eta)

      "compensate the load and feed-in with battery flexibility" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val pvVars = createPvVars
        val loadVars = createLoadVars
        val batVars = createBatVars

        val objective = OptimizedFlexStrat.buildObjective(
          Iterable(pvVars, loadVars, batVars),
          Kilowatts(0),
          Hours(1),
          MinAbsPowerObjectiveFactory,
        )

        model.minimize(objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Since excess power costs the same at all points in time and
          at all magnitudes, there are many optimal solutions.
          Thus, we only test for things that are true for every optimal
          solution: We know when the battery should be definitely
          full/empty and how much energy was charged/discharged.
         */

        {
          batVars.states(0).energyVal should approximate(0d)

          batVars.states.slice(0, 5).foreach {
            _.energyVal should (be >= 0d and be < 20d)
          }

          batVars.states.slice(5, 7).foreach {
            _.energyVal should approximate(20d)
          }

          batVars.states.slice(7, 11).foreach {
            _.energyVal should (be >= 0d and be < 20d)
          }

          batVars.states.slice(11, 13).foreach {
            _.energyVal should approximate(0d)
          }

          // we should've charged 20 kWh plus 5 kWh losses
          val totalCharged = batVars.operationVars.slice(0, 6).map(_.pVal).sum
          totalCharged should approximate(25)

          // we should've discharged 20 kWh minus 4 kWh losses
          val totalDischarged =
            batVars.operationVars.slice(6, 12).map(_.pVal).sum
          totalDischarged should approximate(-16d)

        } withClue buildDebugString(batVars)

        model.release()

      }

      "minimize peaks" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val pvVars = createPvVars
        val loadVars = createLoadVars
        val batVars = createBatVars

        val objective = OptimizedFlexStrat.buildObjective(
          Iterable(pvVars, loadVars, batVars),
          Kilowatts(0),
          Hours(1),
          LinearizedQuadraticPowerObjectiveFactory(
            stepCount = 10,
            lastStep = 10d, // 10 kW
          ),
        )

        model.minimize(objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          When using the (linearized) quadratic objective, the
          test is designed in a way such that only one optimal
          solution exists: During the first phase (charging),
          battery power is utilized so that 2 kW remains at every
          time step. During the second phase (discharging), there's
          exactly enough energy available to go down to 3 kW at
          every time step.
         */

        {
          batVars.states(0).energyVal should approximate(0d)

          // 0 kW to compensate
          batVars.operationVars(0).pVal should approximate(0d)
          batVars.states(1).energyVal should approximate(0d)

          // 6 kW of feed-in to compensate, 2 kW remains
          batVars.operationVars(1).pVal should approximate(4d)
          batVars.states(2).energyVal should approximate(3.2d)

          // 8 kW of feed-in to compensate, 2 kW remains
          batVars.operationVars(2).pVal should approximate(6d)
          batVars.states(3).energyVal should approximate(8d)

          // 7 kW of feed-in to compensate, 2 kW remains
          batVars.operationVars(3).pVal should approximate(5d)
          batVars.states(4).energyVal should approximate(12d)

          // 12 kW of feed-in to compensate, 2 kW remains
          batVars.operationVars(4).pVal should approximate(10d)
          batVars.states(5).energyVal should approximate(20d)

          // 0 kW to compensate
          batVars.operationVars(5).pVal should approximate(0d)
          batVars.states(6).energyVal should approximate(20d)

          // 5 kW of load to compensate, 3 kW remains
          batVars.operationVars(6).pVal should approximate(-2d)
          batVars.states(7).energyVal should approximate(17.5d)

          // 10 kW of load to compensate, 3 kW remains
          batVars.operationVars(7).pVal should approximate(-7d)
          batVars.states(8).energyVal should approximate(8.75d)

          // 3 kW of load to compensate, 3 kW remains
          batVars.operationVars(8).pVal should approximate(0d)
          batVars.states(9).energyVal should approximate(8.75d)

          // 7 kW of load to compensate, 3 kW remains
          batVars.operationVars(9).pVal should approximate(-4d)
          batVars.states(10).energyVal should approximate(3.75d)

          // 6 kW of load to compensate, 3 kW remains
          batVars.operationVars(10).pVal should approximate(-3d)
          batVars.states(11).energyVal should approximate(0d)

          // 0 kW to compensate
          batVars.operationVars(11).pVal should approximate(0d)
          batVars.states(12).energyVal should approximate(0d)

          // we should've charged 20 kWh plus 5 kWh losses
          val totalCharged = batVars.operationVars.slice(0, 6).map(_.pVal).sum
          totalCharged should approximate(25)

          // we should've discharged 20 kWh minus 4 kWh losses
          val totalDischarged =
            batVars.operationVars.slice(6, 12).map(_.pVal).sum
          totalDischarged should approximate(-16d)

        } withClue buildDebugString(batVars)

        model.release()

      }
    }
  }

}
