/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.model.em.opt.SplitPowerVarsObjectiveFactory.MinAbsPowerObjectiveFactory
import edu.ie3.simona.model.em.opt.SplitPowerVarsObjectiveFactory.SplitPowerVarsAdditionalConstraints.*
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.test.common.{OptimizingTestLike, UnitSpec}
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW, zeroKWh}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}

import scala.collection.immutable.SortedMap

class SplitPowerVarsObjectiveFactorySpec
    extends UnitSpec
    with OptimizingTestLike
    with PowerObjectiveTestScenario {

  val objectiveFactory = MinAbsPowerObjectiveFactory(BinaryConstraint)

  given Double = 1e-6
  private val constraintTolerance = 1e-3

  "An optimizing flex strat" when {

    "provided with simple battery flex options with losses" should {

      given ticks: Seq[Long] =
        Range.Long.inclusive(0, 4 * halfHourTicks, halfHourTicks)

      // low efficiency for simplicity of the test
      val batFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          eStorage = KilowattHours(12),
          currentEnergy = KilowattHours(6),
          pMax = Kilowatts(10),
          etaCharge = Each(0.8),
          etaDischarge = Each(0.8),
          currentTick = 0L,
        )
      )

      "balance out additional power within maximum battery power" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(5, -10, 10, -2).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptions,
          sampleTime = halfHour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        assetSymbols.toSeq should have size 2
        assetSymbols.foreach(_.results should have size 1)
        assetSymbols.foreach(_.results.foreach(_ should have size 4))

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
        EXPECTED RESULTS
        Battery should be able to fully cover the additional power
         */

        val batRes = assetSymbols.res(batUUID)

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

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

        } withClue buildDebugString(assetSymbols)

        model.release()
      }

      "balance out additional power exceeding maximum battery power" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(5, -60, 110, -2).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptions,
          sampleTime = halfHour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        assetSymbols.toSeq should have size 2
        assetSymbols.foreach(_.results should have size 1)
        assetSymbols.foreach(_.results.foreach(_ should have size 4))

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to cover the additional power
          up to its maximum power
         */

        val batRes = assetSymbols.res(batUUID)

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

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

        } withClue buildDebugString(assetSymbols)

        model.release()
      }

      "balance out additional power exceeding energy storage capacity" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(-10, -10, 10, 10).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptions,
          sampleTime = halfHour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        assetSymbols.toSeq should have size 2
        assetSymbols.foreach(_.results should have size 1)
        assetSymbols.foreach(_.results.foreach(_ should have size 4))

        model.minimize(objectiveContainer.objective)
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

        val batRes = assetSymbols.res(batUUID)

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

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

        } withClue buildDebugString(assetSymbols)

        model.release()

      }

      "balance out additional power exceeding maximum battery power and energy storage capacity" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(-10, -50, 20, 30).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptions,
          sampleTime = halfHour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        assetSymbols.toSeq should have size 2
        assetSymbols.foreach(_.results should have size 1)
        assetSymbols.foreach(_.results.foreach(_ should have size 4))

        model.minimize(objectiveContainer.objective)
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

        val batRes = assetSymbols.res(batUUID)

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

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

        } withClue buildDebugString(assetSymbols)

        model.release()

      }

      "balance out additional power exceeding energy storage capacity when discharging first" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(1, 1, -10, -10).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptions,
          sampleTime = halfHour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        assetSymbols.toSeq should have size 2
        assetSymbols.foreach(_.results should have size 1)
        assetSymbols.foreach(_.results.foreach(_ should have size 4))

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          The soft constraints are vital here. Without them,
          optimization would overestimate the losses of discharging
          in the first half in order allow for more charging in the
          second half.

          The optimal solution accepts a higher discharging power in
          the first half in order to charge more in the second half,
          because a higher total charging/discharging power means
          more losses that help covering more the feed-in in the
          second half.
         */

        val batRes = assetSymbols.res(batUUID)

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // discharging 1.1 kWh plus 1.375 kWh losses
          batRes(0).pVal should (be >= -2.2d and be <= -1d)
          batRes(0).energyVal should (be >= 4.625d and be <= 5.375d)

          // discharging 0.5 kWh plus 0.125 kWh losses
          batRes(1).pVal should (be >= -2.2d and be <= -1d)
          batRes(1).energyVal should approximate(4d)

          // possibly charging
          batRes(2).pVal should be >= 0d
          batRes(2).energyVal should (be >= 4.75d and be <= 12d)

          // possibly charging, now we should have reached 12 kWh
          batRes(3).pVal should be >= 0d
          batRes(3).energyVal should approximate(12d)

          // we should've charged with 16 kW plus 4 kW losses in total
          batRes(2).pVal + batRes(3).pVal should approximate(20d)

        } withClue buildDebugString(assetSymbols)

        model.release()

      }

    }

    "provided with battery flex options without losses" should {

      given ticks: Seq[Long] =
        Range.Long.inclusive(0, 4 * hourTicks, hourTicks)

      // no losses, thus efficiency = 1
      val batFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          eStorage = KilowattHours(24),
          currentEnergy = KilowattHours(12),
          pMax = Kilowatts(10),
          etaCharge = onePU,
          etaDischarge = onePU,
          currentTick = 0L,
        )
      )

      "balance out additional power within maximum battery power" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(5, -10, 10, -2).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptions,
          sampleTime = hour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to fully cover the additional power.
          No losses should be subtracted.
         */

        val batRes = assetSymbols.res(batUUID)

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // discharging 5 kWh
          batRes(0).pVal should approximate(-5)
          batRes(0).energyVal should approximate(7)

          // charging 10 kWh
          batRes(1).pVal should approximate(10)
          batRes(1).energyVal should approximate(17)

          // discharging 10 kWh
          batRes(2).pVal should approximate(-10)
          batRes(2).energyVal should approximate(7)

          // charging 2 kWh
          batRes(3).pVal should approximate(2)
          batRes(3).energyVal should approximate(9)

        } withClue buildDebugString(assetSymbols)

        model.release()
      }

    }

    "provided with energy boundary flex options that partly disconnect early" should {

      given ticks: Seq[Long] =
        Range.Long.inclusive(0, 4 * halfHourTicks, halfHourTicks)

      // low efficiency for simplicity of the test
      val batFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          eStorage = KilowattHours(12),
          currentEnergy = KilowattHours(5),
          pMax = Kilowatts(10),
          etaCharge = Each(0.8),
          etaDischarge = Each(0.8),
          currentTick = 0L,
        )
      )
      val evcsFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          currentEnergy = KilowattHours(5d),
          energyLimits = SortedMap(
            // half full in the beginning
            0L -> new ClosedInterval(
              zeroKWh,
              KilowattHours(10d),
            ),
            // we need to be 90% full when disconnecting
            3600L -> new ClosedInterval(
              KilowattHours(9d),
              KilowattHours(10d),
            ),
          ),
          powerLimits = ClosedInterval(Kilowatts(-11d), Kilowatts(11)),
          tickDisconnect = Some(3600L),
        )
      )

      "consider the restrictions of disconnecting the asset" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        // power sequence to be balanced out by battery
        // positive values are loads, negative values are feed-ins
        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(-4, -4, 8, -8).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
          bat2UUID -> evcsFlex,
        )

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptions,
          sampleTime = halfHour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          EV should be charged with the power of the first two steps.
          After that, the EV is disconnected and the battery needs to
          balance out the additional power.
         */

        val batRes = assetSymbols.res(batUUID)
        batRes.size shouldBe 4

        val evcsRes = assetSymbols.res(bat2UUID)
        evcsRes.size shouldBe 2

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

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

        } withClue buildDebugString(assetSymbols)

        model.release()

      }

    }

    "provided with energy boundary flex options and an objective factory" should {

      given ticks: Seq[Long] = ticksScenario1

      "compensate fixed powers when using linear objective" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val (assetSymbols, objectiveContainer) = OptimizingFlexStrat.buildModel(
          flexOptions = flexOptionsScenario1,
          sampleTime = halfHour,
          ticks = ticks,
          target = zeroKW,
          receivedData = Seq.empty,
          objectiveFactory = objectiveFactory,
        )

        model.minimize(objectiveContainer.objective)
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

        val batRes = assetSymbols.res(batUUID)

        {
          objectiveContainer.accuracyChecks.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          batRes should have size 12

          batRes.slice(0, 4).foreach {
            _.energyVal should (be >= 0d and be < 10d)
          }

          batRes.slice(4, 6).foreach {
            _.energyVal should approximate(10d)
          }

          batRes.slice(6, 10).foreach {
            _.energyVal should (be >= 0d and be <= 10d)
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

        } withClue buildDebugString(assetSymbols)

        model.release()

      }

    }

  }

}
