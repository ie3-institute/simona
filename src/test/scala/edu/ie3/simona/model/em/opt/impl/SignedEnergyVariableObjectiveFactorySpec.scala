/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt.impl

import edu.ie3.simona.model.em.opt.FlexibilityOptimization
import edu.ie3.simona.model.em.opt.impl.SignedEnergyVariableObjectiveFactory.PriceObjectiveFactory
import edu.ie3.simona.test.common.{OptimizingTestLike, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import optimus.optimization.enums.SolutionStatus
import squants.energy.Energy

class SignedEnergyVariableObjectiveFactorySpec
    extends UnitSpec
    with OptimizingTestLike
    with PowerObjectiveTestScenario {

  // Testing tolerances
  given Double = 1e-6

  // state results should be exact
  val stateEnergyTolerance: Energy = zeroKWh

  "A signed energy variable objective factory" when {

    "provided with energy boundary flex options and an objective factory" should {

      "minimize peaks when using price-based objective" in {

        val results = FlexibilityOptimization.optimize(
          paramsPriceObjectiveTest.copy(objectiveFactory =
            PriceObjectiveFactory
          )
        )

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          When using the price-based objective, the test is
          designed in a way such that only one optimal solution
          exists: First, selling prices are relatively high,
          disincentivizing battery charging. Then, selling
          prices are negative, thus battery charges.
          In the second half, the additional load is covered by
          the grid, since prices are relatively low. Later, when
          prices are high, the battery is used instead.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 3,
            expectedTimeSteps = 12,
          )

          results.assetSymbols.checkModelStateError(using
            stateEnergyTolerance
          )

          val batRes = results.assetSymbols.res(batUUID)

          // 0 kW to compensate
          batRes(0).pVal should approximate(0d)
          batRes(0).energyVal should approximate(0d)

          // 0 kW: we're selling since prices are good
          batRes(1).pVal should approximate(0d)
          batRes(1).energyVal should approximate(0d)

          // NON-OPTIMAL SOLUTION, proper objective would be concave
          // 7.8 kW (8 kW would be optimal) charging: negative prices, we're charging battery instead
          batRes(2).pVal should approximate(7.763158d)
          batRes(2).energyVal should approximate(3.105263d)

          // NON-OPTIMAL SOLUTION, proper objective would be concave
          // 7.2 kW (7 kW would be optimal) charging: negative prices, we're charging battery instead
          batRes(3).pVal should approximate(7.236842d)
          batRes(3).energyVal should approximate(6d)

          // 10 kW charging: negative prices, we're charging battery instead
          batRes(4).pVal should approximate(10d)
          batRes(4).energyVal should approximate(10d)

          // 0 kW to compensate
          batRes(5).pVal should approximate(0d)
          batRes(5).energyVal should approximate(10d)

          // 0 kW: cheap prices, we're covering load with grid power
          batRes(6).pVal should approximate(0d)
          batRes(6).energyVal should approximate(10d)

          // 0 kW: cheap prices, we're covering load with grid power
          batRes(7).pVal should approximate(0d)
          batRes(7).energyVal should approximate(10d)

          // 3 kW discharging: expensive prices, we're discharging battery instead
          batRes(8).pVal should approximate(-4d)
          batRes(8).energyVal should approximate(7.5d)

          // 7 kW discharging: expensive prices, we're discharging battery instead
          batRes(9).pVal should approximate(-7d)
          batRes(9).energyVal should approximate(3.125d)

          // 6 kW discharging: expensive prices, we're discharging battery instead
          batRes(10).pVal should approximate(-5d)
          batRes(10).energyVal should approximate(0d)

          // 0 kW to compensate
          batRes(11).pVal should approximate(0d)
          batRes(11).energyVal should approximate(0d)

          // we should've charged with 20 kW plus 5 kW losses in total
          val inputCharged = batRes.slice(0, 6).map(_.pVal).sum
          inputCharged should approximate(25)

          // we should've discharged with 20 kW minus 4 kW losses in total
          val outputDischarged =
            batRes.slice(6, 12).map(_.pVal).sum
          outputDischarged should approximate(-16d)

        } withClue buildDebugString(results.assetSymbols)
      }

      "not produce too small powers by impact of soft constraints" in {

        val results = FlexibilityOptimization.optimize(
          paramsSoftConstraintsTest.copy(objectiveFactory =
            PriceObjectiveFactory
          )
        )

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          The price-based objective works as expected with low prices.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 1,
            expectedTimeSteps = 1,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // should work properly: selling as much as possible
          // (-9.6 kW), since there are no soft constraints
          batRes(0).pVal should approximate(-9.6d)
          batRes(0).energyVal should approximate(0d)

        } withClue buildDebugString(results.assetSymbols)

      }
    }
  }
}
