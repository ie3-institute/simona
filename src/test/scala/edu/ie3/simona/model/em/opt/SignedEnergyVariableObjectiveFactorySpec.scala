/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.test.common.{OptimizingTestLike, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}

class SignedEnergyVariableObjectiveFactorySpec
    extends UnitSpec
    with OptimizingTestLike
    with PowerObjectiveTestScenario {

  // Testing tolerances
  given Double = 1e-6

  "An optimizing flex strat" when {

    "provided with a SignedEnergyVariableObjectiveFactory" should {

      given ticks: Seq[Long] = ticksScenario1

      "minimize peaks when using price-based objective" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val (assetSymbols, objectiveContainer) =
          OptimizingFlexStrat.buildModel(
            flexOptions = flexOptionsScenario1,
            sampleTime = halfHour,
            ticks = ticks,
            target = zeroKW,
            receivedData = Seq(priceDataScenario1),
            objectiveFactory = SignedEnergyVariableObjectiveFactory,
          )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

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
          val batRes = assetSymbols.res(batUUID)

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

        } withClue buildDebugString(assetSymbols)

        model.release()

      }

      "not produce too small powers by impact of soft constraints" in {

        given ticks: Seq[Long] = Seq(0L, halfHourTicks)

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val loadFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(1, 0).toPowerMap(oneHalfHour)
          )
        )

        // low efficiency for simplicity of the test
        val batFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            eStorage = KilowattHours(20),
            currentEnergy = KilowattHours(20),
            pMax = Kilowatts(10),
            etaCharge = Each(0.8),
            etaDischarge = Each(0.8),
            currentTick = 0L,
          )
        )

        val flexOptions = Map(
          loadUUID -> loadFlex,
          batUUID -> batFlex,
        )

        val priceData = Seq((0.1d, 0.21d), (0.1d, 1d)).toPriceData(oneHalfHour)

        val (assetSymbols, objectiveContainer) =
          OptimizingFlexStrat.buildModel(
            flexOptions = flexOptions,
            sampleTime = halfHour,
            ticks = ticks,
            target = zeroKW,
            receivedData = Seq(priceData),
            objectiveFactory = SignedEnergyVariableObjectiveFactory,
          )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          The price-based objective works as expected with low prices.
         */

        {
          val batRes = assetSymbols.res(batUUID)

          // should work properly: selling as much as possible
          batRes(0).pVal should approximate(-10d)
          batRes(0).energyVal should approximate(13.75d)

        } withClue buildDebugString(assetSymbols)

        model.release()

      }
    }
  }

}
