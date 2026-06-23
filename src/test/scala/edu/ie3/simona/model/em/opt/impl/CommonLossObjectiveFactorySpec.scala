/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt.impl

import CommonLossObjectiveFactorySpec.*
import edu.ie3.simona.model.em.opt.impl.CommonLossObjectiveFactory
import edu.ie3.simona.model.em.opt.FlexibilityOptimization
import edu.ie3.simona.test.common.{OptimizingTestLike, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import optimus.optimization.enums.SolutionStatus
import org.scalatest.OptionValues
import squants.energy.{Energy, KilowattHours, Kilowatts, WattHours}
import squants.{Dimensionless, Each, Power, Time}

class CommonLossObjectiveFactorySpec
    extends UnitSpec
    with OptimizingTestLike
    with PowerObjectiveTestScenario {

  // Testing tolerances
  given Double = 1e-6
  given Energy = WattHours(1e-9)
  val stateEnergyTolerance: Energy = WattHours(1e-3)

  "An optimizing flex strat" when {

    "provided with a flex energy model to adapt" should {

      "create an adapted model correctly" in {

        val currentEnergy = KilowattHours(10)
        val eStorage = KilowattHours(20)
        val pMax = Kilowatts(10)

        val etas = Seq(.6, .65, .7, .75, .8, .85, 0.9, .92, .95, .98, 1)

        forEvery(Table("etaCharging", etas*)) { etaCharging =>
          forEvery(Table("etaDischarging", etas*)) { etaDischarging =>

            val classic = ClassicModel(
              currentEnergy = currentEnergy,
              eStorage = eStorage,
              pMax = pMax,
              etaCharging = Each(etaCharging),
              etaDischarging = Each(etaDischarging),
            )

            val etaAvg = CommonLossObjectiveFactory
              .calculateCommonEta(Each(etaCharging), Each(etaDischarging))
            val conversionFactor = CommonLossObjectiveFactory
              .calculateConversionFactor(Each(etaCharging), etaAvg)

            val adapted = AdaptedModel(
              currentEnergy = currentEnergy * conversionFactor,
              eStorage = eStorage * conversionFactor,
              pMax = pMax,
              etaAvg = etaAvg,
            )

            // charging until full, with maximum power
            val power1 = pMax
            val duration1 = (eStorage - currentEnergy) / (power1 * etaCharging)
            // discharging three quarters, with half power
            val power2 = -pMax / 2
            val duration2 = eStorage * 0.75 / (-power2 * 1 / etaDischarging)
            // charging until half, with quarter power
            val power3 = pMax / 4
            val duration3 = eStorage / 4 / (power3 * etaCharging)
            // discharging until empty, with maximum power
            val power4 = -pMax
            val duration4 = eStorage / 2 / (-power4 * 1 / etaDischarging)

            val results = IndexedSeq(classic, adapted).map(
              _.charge(power1, duration1)
                .charge(power2, duration2)
                .charge(power3, duration3)
                .charge(power4, duration4)
                .currentEnergy
            )

            // comparing both models:
            // battery should be exactly empty
            results(0) should approximate(zeroKWh)
            results(1) should approximate(zeroKWh)

          }
        }
      }

    }

    "provided with simple battery flex options with losses" should {

      "balance out additional power within maximum battery power" in {

        val results = FlexibilityOptimization.optimize(paramsLowAddPower)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to fully cover the additional power
         */

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

      "balance out additional power exceeding maximum battery power" in {

        val results = FlexibilityOptimization.optimize(paramsHighAddPower)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to cover the additional power
          up to its maximum power
         */

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

      "balance out additional power exceeding energy storage capacity" in {

        val results = FlexibilityOptimization.optimize(paramsHighAddEnergy)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Since excess power costs the same at all points in time and
          at all magnitudes, there are many optimal solutions.
          Thus, we only test for things that are true for every optimal
          solution: We know when the battery should be definitely
          full/empty and how much energy was charged/discharged.

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 2,
            expectedTimeSteps = 4,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // possibly charging
          batRes(0).pVal should be >= 0d
          batRes(0).energyVal should (be >= 6d and be <= 12d)

          // possibly charging, now we should have reached 12 kWh
          batRes(1).pVal should be >= 0d
          batRes(1).energyVal should approximate(12d)

          // we should've charged with 12 kW plus 3 kW losses in total
          batRes(0).pVal + batRes(1).pVal should approximate(15d)

          // possibly discharging
          batRes(2).pVal should be <= 0d
          batRes(2).energyVal should (be >= 0d and be <= 12d)

          // possibly discharging, now we should have reached 0 kWh
          batRes(3).pVal should be <= 0d
          batRes(3).energyVal should approximate(0d)

          // we should've discharged with 24 kW minus 4.8 kW losses in total
          batRes(2).pVal + batRes(3).pVal should approximate(-19.2d)

        } withClue buildDebugString(results.assetSymbols)
      }

      "balance out additional power exceeding maximum battery power and energy storage capacity" in {

        val results =
          FlexibilityOptimization.optimize(paramsHighAddPowerAndEnergy)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Since excess power costs the same at all points in time and
          at all magnitudes, there are many optimal solutions.
          Thus, we only test for things that are true for every optimal
          solution: We know when the battery should be definitely
          full/empty and how much energy was charged/discharged.

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 2,
            expectedTimeSteps = 4,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // possibly charging
          batRes(0).pVal should be >= 0d
          batRes(0).energyVal should (be >= 6d and be <= 12d)

          // possibly charging, now we should have reached 12 kWh
          batRes(1).pVal should be >= 0d
          batRes(1).energyVal should approximate(12d)

          // we should've charged with 12 kW plus 3 kW losses in total
          batRes(0).pVal + batRes(1).pVal should approximate(15d)

          // possibly discharging
          batRes(2).pVal should be <= 0d
          batRes(2).energyVal should (be >= 0d and be <= 12d)

          // possibly discharging, now we should have reached 0 kWh
          // (6 kWh below starting energy)
          batRes(3).pVal should be <= 0d
          batRes(3).energyVal should approximate(0d)

          // we should've discharged with 24 kW minus 4.8 kW losses in total
          batRes(2).pVal + batRes(3).pVal should approximate(-19.2d)

        } withClue buildDebugString(results.assetSymbols)
      }

      "balance out additional power exceeding energy storage capacity when discharging first" in {

        val results = FlexibilityOptimization.optimize(paramsDischargeFirst)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          The soft constraints are vital here. Without them,
          optimization would overestimate the losses of discharging
          in the first half in order allow for more charging in the
          second half.

          The optimal solution accepts a higher discharging power in
          the first half in order to charge more in the second half.
          Here, we do not achieve optimal results, because the soft
          constraint on pAbs pushes down p as well, so it prefers
          a lower total amount of power.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 2,
            expectedTimeSteps = 4,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // discharging 0.5 kWh plus 0.125 kWh losses
          batRes(0).pVal should approximate(-1d)
          batRes(0).energyVal should approximate(5.375d)

          // discharging 0.5 kWh plus 0.125 kWh losses
          batRes(1).pVal should approximate(-1d)
          batRes(1).energyVal should approximate(4.75d)

          // possibly charging
          batRes(2).pVal should be >= 0d
          batRes(2).energyVal should (be >= 4.75d and be <= 12d)

          // possibly charging, now we should have reached 12 kWh
          batRes(3).pVal should be >= 0d
          batRes(3).energyVal should approximate(12d)

          // we should've charged with 14.5 kW plus 3.625 kW losses in total
          batRes(2).pVal + batRes(3).pVal should approximate(18.125d)

        } withClue buildDebugString(results.assetSymbols)
      }

    }

    "provided with battery flex options without losses" should {

      "balance out additional power within maximum battery power" in {

        val results = FlexibilityOptimization.optimize(paramsNoLoss)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to fully cover the additional power.
          No losses should be subtracted.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 2,
            expectedTimeSteps = 4,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // discharging 5 kWh
          batRes(0).pVal should approximate(-5)
          batRes(0).energyVal should approximate(3.5)

          // charging 10 kWh
          batRes(1).pVal should approximate(10)
          batRes(1).energyVal should approximate(8.5)

          // discharging 10 kWh
          batRes(2).pVal should approximate(-10)
          batRes(2).energyVal should approximate(3.5)

          // charging 2 kWh
          batRes(3).pVal should approximate(2)
          batRes(3).energyVal should approximate(4.5)

        } withClue buildDebugString(results.assetSymbols)
      }

    }

    "provided with energy boundary flex options that partly disconnect early" should {

      "consider the restrictions of disconnecting the asset" in {

        val results = FlexibilityOptimization.optimize(paramsEvcsDisconnect)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          EV should be charged with the power of the first two steps.
          After that, the EV is disconnected and the battery needs to
          balance out the additional power.
         */

        {
          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)
          batRes.size shouldBe 4

          val evcsRes = results.assetSymbols.res(bat2UUID)
          evcsRes.size shouldBe 2

          // EV needs to take the 4 kW to reach its target
          evcsRes(0).pVal should approximate(4)
          evcsRes(0).energyVal should approximate(7)
          // battery is left with 0
          batRes(0).pVal should approximate(0)
          batRes(0).energyVal should approximate(5)

          // EV needs to take the 4 kW to reach its target
          evcsRes(1).pVal should approximate(4)
          evcsRes(1).energyVal should approximate(9)
          // battery is left with 0
          batRes(1).pVal should approximate(0)
          batRes(1).energyVal should approximate(5)

          // EV is not available from here on

          // discharging 5 kWh
          batRes(2).pVal should approximate(-8)
          batRes(2).energyVal should approximate(0)

          // charging 3.2 kWh
          batRes(3).pVal should approximate(8)
          batRes(3).energyVal should approximate(3.2)

        } withClue buildDebugString(results.assetSymbols)
      }

    }

    "provided with energy boundary flex options and an objective factory" should {

      "compensate fixed powers when using linear objective" in {

        val results = FlexibilityOptimization.optimize(paramsMinAbsPowerTest)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Since excess power costs the same at all points in time and
          at all magnitudes, there are many optimal solutions.
          Thus, we only test for things that are true for every optimal
          solution: We know when the battery should be definitely
          full/empty and how much energy was charged/discharged.

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        {
          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)
          batRes should have size 12

          batRes.slice(0, 4).foreach {
            _.energyVal should (be >= 0d and be < 10d)
          }

          batRes.slice(4, 6).foreach {
            _.energyVal should approximate(10d)
          }

          batRes.slice(6, 10).foreach {
            _.energyVal should (be >= 0d and be < 10d)
          }

          batRes.slice(10, 12).foreach {
            _.energyVal should approximate(0d)
          }

          // we should've charged with 20 kW plus 5 kW losses in total
          val inputCharged = batRes.slice(0, 6).map(_.pVal).sum
          inputCharged should approximate(25)

          // we should've discharged with 20 kW minus 4 kW losses in total
          val outputDischarged =
            batRes.slice(6, 12).map(_.pVal).sum
          outputDischarged should approximate(-16d)

        } withClue buildDebugString(results.assetSymbols)
      }

      "minimize peaks when using quadratic objective" in {

        val results = FlexibilityOptimization.optimize(paramsLinQuadPowerTest)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          When using the (linearized) quadratic objective, the
          test is designed in a way such that only one optimal
          solution exists: During the first phase (charging),
          battery power is utilized so that 2 kW remains at every
          time step. During the second phase (discharging), there's
          exactly enough energy available to go down to 4 kW at
          every time step.

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 3,
            expectedTimeSteps = 12,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // 0 kW to compensate
          batRes(0).pVal should approximate(0d)
          batRes(0).energyVal should approximate(0d)

          // 6 kW of feed-in to compensate, 2 kW remains
          batRes(1).pVal should approximate(4d)
          batRes(1).energyVal should approximate(1.6d)

          // 8 kW of feed-in to compensate, 2 kW remains
          batRes(2).pVal should approximate(6d)
          batRes(2).energyVal should approximate(4d)

          // 7 kW of feed-in to compensate, 2 kW remains
          batRes(3).pVal should approximate(5d)
          batRes(3).energyVal should approximate(6d)

          // 12 kW of feed-in to compensate, 2 kW remains
          batRes(4).pVal should approximate(10d)
          batRes(4).energyVal should approximate(10d)

          // 0 kW to compensate
          batRes(5).pVal should approximate(0d)
          batRes(5).energyVal should approximate(10d)

          // 8 kW of load to compensate, 4 kW remains
          batRes(6).pVal should approximate(-4d)
          batRes(6).energyVal should approximate(7.5d)

          // 12 kW of load to compensate, 4 kW remains
          batRes(7).pVal should approximate(-8d)
          batRes(7).energyVal should approximate(2.5d)

          // 4 kW of load to compensate, 4 kW remains
          batRes(8).pVal should approximate(0d)
          batRes(8).energyVal should approximate(2.5d)

          // 7 kW of load to compensate, 4 kW remains
          batRes(9).pVal should approximate(-3d)
          batRes(9).energyVal should approximate(0.625d)

          // 5 kW of load to compensate, 4 kW remains
          batRes(10).pVal should approximate(-1d)
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

      "minimize peaks when using price-based objective" in {

        val results = FlexibilityOptimization.optimize(paramsPriceObjectiveTest)

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

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // 0 kW to compensate
          batRes(0).pVal should approximate(0d)
          batRes(0).energyVal should approximate(0d)

          // 0 kW: we're selling since prices are good
          batRes(1).pVal should approximate(0d)
          batRes(1).energyVal should approximate(0d)

          // 8 kW charging: negative prices, we're charging battery instead
          batRes(2).pVal should approximate(8d)
          batRes(2).energyVal should approximate(3.2d)

          // 7 kW charging: negative prices, we're charging battery instead
          batRes(3).pVal should approximate(7d)
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

          // 4 kW discharging: expensive prices, we're discharging battery instead
          batRes(8).pVal should approximate(-4d)
          batRes(8).energyVal should approximate(7.5d)

          // 7 kW discharging: expensive prices, we're discharging battery instead
          batRes(9).pVal should approximate(-7d)
          batRes(9).energyVal should approximate(3.125d)

          // 5 kW discharging: expensive prices, we're discharging battery instead
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

        val results =
          FlexibilityOptimization.optimize(paramsSoftConstraintsTest)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          The price-based objective with adapted charging
          efficiency does not work properly with prices below
          loss factor.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 1,
            expectedTimeSteps = 1,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val batRes = results.assetSymbols.res(batUUID)

          // 0 kW, since result is pushed down by soft constraint
          batRes(0).pVal should approximate(0d)
          batRes(0).energyVal should approximate(6d)

        } withClue buildDebugString(results.assetSymbols)
      }
    }

    "provided with energy boundary flex options including two batteries" should {

      "minimize peaks when using price-based objective" in {

        val results = FlexibilityOptimization.optimize(paramsTwoBatteries)

        results.solutionStatus shouldBe SolutionStatus.OPTIMAL

        /*
        EXPECTED RESULTS
        There are three phases that provide different incentives to the
        battery optimization. The constant load/feed-in is modeled in a
        way that allows only for one optimal solution. See detailed
        explanations below.
         */

        {
          results.assetSymbols.checkStructure(
            expectedAssets = 3,
            expectedTimeSteps = 12,
          )

          results.assetSymbols.checkModelStateError(using stateEnergyTolerance)

          val bat1Res = results.assetSymbols.res(batUUID)
          val bat2Res = results.assetSymbols.res(bat2UUID)

          /*
          First period (steps 0-3):
          Prices are positive, so there is an overall incentive to sell energy.
          However, we have loads that exactly drain the batteries to zero at
          the end of this period.
          Furthermore, the high load at the end (14 kW) require both
          batteries to be discharging with maximum power.
          Thus, the second battery (high power, low storage capacity) needs to
          be spared to cover the high load and cannot contribute beforehand.
           */
          bat1Res(0).pVal should approximate(-4d)
          bat1Res(0).energyVal should approximate(7.5d)
          bat2Res(0).pVal should approximate(0d)
          bat2Res(0).energyVal should approximate(6.25d)

          bat1Res(1).pVal should approximate(-4d)
          bat1Res(1).energyVal should approximate(5d)
          bat2Res(1).pVal should approximate(0d)
          bat2Res(1).energyVal should approximate(6.25d)

          bat1Res(2).pVal should approximate(-4d)
          bat1Res(2).energyVal should approximate(2.5d)
          bat2Res(2).pVal should approximate(0d)
          bat2Res(2).energyVal should approximate(6.25d)

          bat1Res(3).pVal should approximate(-4d)
          bat1Res(3).energyVal should approximate(0d)
          bat2Res(3).pVal should approximate(-10d)
          bat2Res(3).energyVal should approximate(0d)

          /*
          Second period (steps 4-9):
          Prices are negative, so there is an overall incentive to buy energy.
          There is also some feed-in that fluctuates. Overall, the battery is
          filled with both feed-in and bought energy.
          During step 7 and 8, there is a high feed-in that requires both
          batteries to charge. The second battery can only be used in these
          two steps, thus can't be used at other times during this period.

          The optimal solution would charge and discharge the second battery
          in order to waste more energy. But, since the soft constraint on
          absolute power is used here, power is pushed to zero.
           */
          bat1Res(4).pVal should approximate(4d)
          bat1Res(4).energyVal should approximate(1.6d)
          bat2Res(4).pVal should approximate(0d)
          bat2Res(4).energyVal should approximate(0d)

          bat1Res(5).pVal should approximate(4d)
          bat1Res(5).energyVal should approximate(3.2d)
          bat2Res(5).pVal should approximate(0d)
          bat2Res(5).energyVal should approximate(0d)

          bat1Res(6).pVal should approximate(4d)
          bat1Res(6).energyVal should approximate(4.8d)
          bat2Res(6).pVal should approximate(0d)
          bat2Res(6).energyVal should approximate(0d)

          bat1Res(7).pVal should approximate(4d)
          bat1Res(7).energyVal should approximate(6.4d)
          bat2Res(7).pVal should approximate(10d)
          bat2Res(7).energyVal should approximate(4d)

          bat1Res(8).pVal should approximate(4d)
          bat1Res(8).energyVal should approximate(8d)
          bat2Res(8).pVal should approximate(5.625d)
          bat2Res(8).energyVal should approximate(6.25d)

          bat1Res(9).pVal should approximate(4d)
          bat1Res(9).energyVal should approximate(9.6d)
          bat2Res(9).pVal should approximate(0d)
          bat2Res(9).energyVal should approximate(6.25d)

          /*
          Third period (steps 10-11):
          Prices are positive again, so we generally want to sell again.
          Period 10 has a high load again that uses the full storage
          capacity of battery 2 (similar to end of phase 1).
           */
          bat1Res(10).pVal should approximate(-4d)
          bat1Res(10).energyVal should approximate(7.1d)
          bat2Res(10).pVal should approximate(-10d)
          bat2Res(10).energyVal should approximate(0d)

          bat1Res(11).pVal should approximate(-4d)
          bat1Res(11).energyVal should approximate(4.6d)
          bat2Res(11).pVal should approximate(0d)
          bat2Res(11).energyVal should approximate(0d)

        } withClue buildDebugString(results.assetSymbols)
      }

    }

  }

}

object CommonLossObjectiveFactorySpec extends OptionValues {

  trait BatteryTesting {
    val currentEnergy: Energy

    def charge(power: Power, duration: Time): BatteryTesting
  }

  final case class ClassicModel(
      override val currentEnergy: Energy,
      eStorage: Energy,
      pMax: Power,
      etaCharging: Dimensionless,
      etaDischarging: Dimensionless,
  ) extends BatteryTesting {
    def charge(power: Power, duration: Time): BatteryTesting = {
      val netPower =
        if power > zeroKW then power * etaCharging.toEach
        else power * 1 / etaDischarging.toEach

      copy(currentEnergy = currentEnergy + netPower * duration)
    }
  }

  final case class AdaptedModel(
      override val currentEnergy: Energy,
      eStorage: Energy,
      pMax: Power,
      etaAvg: Dimensionless,
  ) extends BatteryTesting {
    def charge(power: Power, duration: Time): BatteryTesting = {
      val newEnergy =
        currentEnergy + (power - power.abs * (1 - etaAvg.toEach)) * duration

      copy(currentEnergy = newEnergy)
    }
  }

}
