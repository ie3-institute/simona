/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt.impl

import edu.ie3.simona.model.em.opt.FlexibilityOptimization
import edu.ie3.simona.model.em.opt.FlexibilityOptimization.OptimizationParams
import edu.ie3.simona.model.em.opt.impl.SignedEnergyVariableObjectiveFactory.{
  MinAbsPowerObjectiveFactory,
  PriceObjectiveFactory,
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
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

    "provided with simple battery flex options with losses" should {

      "balance out additional power within maximum battery power" in {

        val results = FlexibilityOptimization.optimize(
          paramsLowAddPower.copy(objectiveFactory = MinAbsPowerObjectiveFactory)
        )

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
        EXPECTED RESULTS

        Non-optimal results, because objective cannot be modeled
        perfectly with this objective factory.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 2,
            expectedTimeSteps = 4,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          batRes(0).pVal should approximate(-3.902439)
          batRes(0).energyVal should approximate(3.560976)

          batRes(1).pVal should approximate(10)
          batRes(1).energyVal should approximate(7.560976)

          batRes(2).pVal should approximate(-7.804878)
          batRes(2).energyVal should approximate(2.682927)

          batRes(3).pVal should approximate(2.439024)
          batRes(3).energyVal should approximate(3.658537)

        } withClue buildDebugString(results.assetSymbols)
      }

      "produce exact results when provided with positive fixed power" in {

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val fixedPosOnly: EnergyBoundariesFlexOptions =
          EnergyBoundariesFlexOptions(
            AssetEnergyBoundaries(
              Seq(1, 2, 1, 0).toPowerMap(fourHalfHours)
            )
          )

        val paramsPosOnly: OptimizationParams = paramsLowAddPower.copy(
          flexOptionsById = paramsLowAddPower.flexOptionsById.toMap.updated(
            loadUUID,
            fixedPosOnly,
          )
        )

        val results = FlexibilityOptimization.optimize(
          paramsPosOnly.copy(objectiveFactory = MinAbsPowerObjectiveFactory)
        )

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 2,
            expectedTimeSteps = 4,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // discharging 2.5 kWh plus 0.6125 kWh losses
          batRes(0).pVal should approximate(-5)
          batRes(0).energyVal should approximate(2.875)

          // charging 5 kWh minus 1 kWh losses
          batRes(1).pVal should approximate(10)
          batRes(1).energyVal should approximate(6.875)

          // discharging 5 kWh plus 1.25 kWh losses
          batRes(2).pVal should approximate(-10)
          batRes(2).energyVal should approximate(0.625)

          // charging 1 kWh minus 0.2 kWh losses
          batRes(3).pVal should approximate(2)
          batRes(3).energyVal should approximate(1.425)

        } withClue buildDebugString(results.assetSymbols)
      }
    }

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

    "provided with demonstrative example with price objective" should {

      "produce suboptimal results without excess loss" in {

        val results = FlexibilityOptimization.optimize(
          paramsExcessLossPrices.copy(objectiveFactory = PriceObjectiveFactory)
        )

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL
        val batRes = results.assetSymbols.res(batUUID)

        {
          results.objectiveValue.value should approximate(-0.39125)

          batRes.actualLossSum should approximate(4.5)
          batRes.excessLossSum should approximate(0.0d)

          batRes(2).energyVal should approximate(10.0d)
          batRes(3).energyVal should approximate(0.0d)

        } withClue buildDebugString(results.assetSymbols)

      }

    }

  }

}
