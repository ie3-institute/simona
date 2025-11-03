/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.model.em.opt.OptimizedFlexStratSpec.*
import edu.ie3.simona.model.em.opt.PowerObjectiveFactory.{
  LinearizedQuadraticPowerObjectiveFactory,
  MinAbsPowerObjectiveFactory,
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.test.common.{OptimizingTestLike, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import org.scalatest.OptionValues
import squants.energy.{Energy, KilowattHours, Kilowatts, WattHours}
import squants.time.Hours
import squants.{Dimensionless, Each, Power, Time}

import java.util.UUID

class OptimizedFlexStratSpec extends UnitSpec with OptimizingTestLike {

  private val pvUUID = UUID.fromString("0-0-0-0-1")
  private val loadUUID = UUID.fromString("0-0-0-0-2")
  private val batUUID = UUID.fromString("0-0-0-0-3")

  private val sampleTime = Hours(1)
  private val sampleTicks = sampleTime.toSeconds.toLong

  // Testing tolerances
  given Double = 1e-6
  given Energy = WattHours(1e-9)
  private val constraintTolerance = 1e-3

  "An optimized flex strat" when {

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

            // using the implementation to derive an adapted model
            val adaptedFlexOptions = OptimizedFlexStrat.adaptEnergyBoundaries(
              AssetEnergyBoundaries(
                eStorage = eStorage,
                currentEnergy = currentEnergy,
                pMax = pMax,
                etaCharge = Each(etaCharging),
                etaDischarge = Each(etaDischarging),
                0L,
              )
            )
            val adaptedLimits = adaptedFlexOptions.energyLimits(0)

            val adapted = AdaptedModel(
              currentEnergy = -adaptedLimits.getLower,
              eStorage = adaptedLimits.getUpper - adaptedLimits.getLower,
              pMax = adaptedFlexOptions.powerLimits.getUpper,
              etaAvg = adaptedFlexOptions.etaCharge,
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

      given ticks: Seq[Long] =
        Range.Long.inclusive(0, 4 * sampleTicks, sampleTicks)

      // low efficiency for simplicity of the test
      val batFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          eStorage = KilowattHours(24),
          currentEnergy = KilowattHours(12),
          pMax = Kilowatts(10),
          etaCharge = Each(0.8),
          etaDischarge = Each(0.8),
          currentTick = 0L,
        )
      )

      // since energy values have been adapted, we need this
      // factor to convert back to "real" values
      given EnergyConversionFactor =
        EnergyConversionFactor(
          batFlex.energyBoundaries.headOption.value.etaCharge,
          OptimizedFlexStrat
            .adaptEnergyBoundaries(batFlex.energyBoundaries.headOption.value)
            .etaCharge,
        )

      "balance out additional power within maximum battery power" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(5, -10, 10, -2).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        assetVars.toSeq should have length 2
        assetVars.foreach(_.results should have length 1)
        assetVars.foreach(_.results.foreach(_ should have length 4))

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          zeroKW,
          MinAbsPowerObjectiveFactory,
        )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to fully cover the additional power
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // discharging 5 kWh plus 1.25 kWh losses
          batRes(0).pVal should approximate(-5)
          batRes(0).energyVal should approximate(-6.25)

          // charging 10 kWh minus 2 kWh losses
          batRes(1).pVal should approximate(10)
          batRes(1).energyVal should approximate(1.75)

          // discharging 10 kWh plus 2.5 kWh losses
          batRes(2).pVal should approximate(-10)
          batRes(2).energyVal should approximate(-10.75)

          // charging 2 kWh minus 0.4 kWh losses
          batRes(3).pVal should approximate(2)
          batRes(3).energyVal should approximate(-9.15)

        } withClue buildDebugString(batVars)

        model.release()
      }

      "balance out additional power exceeding maximum battery power" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(5, -60, 110, -2).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        assetVars.toSeq should have length 2
        assetVars.foreach(_.results should have length 1)
        assetVars.foreach(_.results.foreach(_ should have length 4))

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          zeroKW,
          MinAbsPowerObjectiveFactory,
        )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to cover the additional power
          up to its maximum power
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // discharging 5 kWh plus 1.25 kWh losses
          batRes(0).pVal should approximate(-5)
          batRes(0).energyVal should approximate(-6.25)

          // charging 10 kWh minus 2 kWh losses
          batRes(1).pVal should approximate(10)
          batRes(1).energyVal should approximate(1.75)

          // discharging 10 kWh plus 2.5 kWh losses
          batRes(2).pVal should approximate(-10)
          batRes(2).energyVal should approximate(-10.75)

          // charging 2 kWh minus 0.4 kWh losses
          batRes(3).pVal should approximate(2)
          batRes(3).energyVal should approximate(-9.15)

        } withClue buildDebugString(batVars)

        model.release()
      }

      "balance out additional power exceeding energy storage capacity" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(-10, -10, 10, 10).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        assetVars.toSeq should have length 2
        assetVars.foreach(_.results should have length 1)
        assetVars.foreach(_.results.foreach(_ should have length 4))

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          zeroKW,
          MinAbsPowerObjectiveFactory,
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

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // possibly charging
          batRes(0).pVal should be >= 0d
          batRes(0).energyVal should (be >= 0d and be <= 12d)

          // possibly charging, now we should have reached 24 kWh
          // (12 kWh above starting energy)
          batRes(1).pVal should be >= 0d
          batRes(1).energyVal should approximate(12d)

          // we should've charged 12 kWh plus 3 kWh losses
          batRes(0).pVal + batRes(1).pVal should approximate(15d)

          // possibly discharging
          batRes(2).pVal should be <= 0d
          batRes(2).energyVal should (be >= -12d and be <= 12d)

          // possibly discharging, now we should have reached 0 kWh
          // (12 kWh below starting energy)
          batRes(3).pVal should be <= 0d
          batRes(3).energyVal should approximate(-12d)

          // we should've discharged 24 kWh minus 4.8 kWh losses
          batRes(2).pVal + batRes(3).pVal should approximate(-19.2d)

        } withClue buildDebugString(batVars)

        model.release()

      }

      "balance out additional power exceeding maximum battery power and energy storage capacity" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(-10, -50, 20, 30).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        assetVars.toSeq should have length 2
        assetVars.foreach(_.results should have length 1)
        assetVars.foreach(_.results.foreach(_ should have length 4))

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          zeroKW,
          MinAbsPowerObjectiveFactory,
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

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // possibly charging
          batRes(0).pVal should be >= 0d
          batRes(0).energyVal should (be >= 0d and be <= 12d)

          // possibly charging, now we should have reached 24 kWh
          // (12 kWh above starting energy)
          batRes(1).pVal should be >= 0d
          batRes(1).energyVal should approximate(12d)

          // we should've charged 12 kWh plus 3 kWh losses
          batRes(0).pVal + batRes(1).pVal should approximate(15d)

          // possibly discharging
          batRes(2).pVal should be <= 0d
          batRes(2).energyVal should (be >= -12d and be <= 12d)

          // possibly discharging, now we should have reached 0 kWh
          // (12 kWh below starting energy)
          batRes(3).pVal should be <= 0d
          batRes(3).energyVal should approximate(-12d)

          // we should've discharged 24 kWh minus 4.8 kWh losses
          batRes(2).pVal + batRes(3).pVal should approximate(-19.2d)

        } withClue buildDebugString(batVars)

        model.release()

      }
    }

    "provided with battery flex options without losses" should {

      given ticks: Seq[Long] =
        Range.Long.inclusive(0, 4 * sampleTicks, sampleTicks)

      // low efficiency for simplicity of the test
      val batFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          eStorage = KilowattHours(24),
          currentEnergy = KilowattHours(12),
          pMax = Kilowatts(10),
          etaCharge = Each(1),
          etaDischarge = Each(1),
          currentTick = 0L,
        )
      )

      // dummy energy conversion factor
      given EnergyConversionFactor =
        EnergyConversionFactor(
          batFlex.energyBoundaries.headOption.value.etaCharge,
          Each(1),
        )

      "balance out additional power within maximum battery power" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val constFlex = EnergyBoundariesFlexOptions(
          AssetEnergyBoundaries(
            Seq(5, -10, 10, -2).toPowerMap
          )
        )

        val flexOptions = Map(
          loadUUID -> constFlex,
          batUUID -> batFlex,
        )

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          zeroKW,
          MinAbsPowerObjectiveFactory,
        )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Battery should be able to fully cover the additional power.
          No losses should be subtracted.
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // discharging 5 kWh
          batRes(0).pVal should approximate(-5)
          batRes(0).energyVal should approximate(-5)

          // charging 10 kWh
          batRes(1).pVal should approximate(10)
          batRes(1).energyVal should approximate(5)

          // discharging 10 kWh
          batRes(2).pVal should approximate(-10)
          batRes(2).energyVal should approximate(-5)

          // charging 2 kWh
          batRes(3).pVal should approximate(2)
          batRes(3).energyVal should approximate(-3)

        } withClue buildDebugString(batVars)

        model.release()
      }

    }

    "provided with energy boundary flex options and an objective factory" should {

      given ticks: Seq[Long] =
        Range.Long.inclusive(0, 12 * sampleTicks, sampleTicks)

      // 33 kWh of feed-in in total, more than battery can store
      val pvFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          Seq(0, -6, -8, -7, -12, 0, 0, 0, 0, 0, 0, 0).toPowerMap
        )
      )

      // 31 kWh of load in total, more than battery can provide
      val loadFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          Seq(0, 0, 0, 0, 0, 0, 5, 10, 3, 7, 6, 0).toPowerMap
        )
      )

      // low efficiency for simplicity of the test
      val batFlex = EnergyBoundariesFlexOptions(
        AssetEnergyBoundaries(
          eStorage = KilowattHours(20),
          currentEnergy = KilowattHours(0),
          pMax = Kilowatts(10),
          etaCharge = Each(0.8),
          etaDischarge = Each(0.8),
          currentTick = 0L,
        )
      )

      val flexOptions = Map(
        pvUUID -> pvFlex,
        loadUUID -> loadFlex,
        batUUID -> batFlex,
      )

      // since energy values have been adapted, we need this
      // factor to convert back to "real" values
      given EnergyConversionFactor =
        EnergyConversionFactor(
          batFlex.energyBoundaries.headOption.value.etaCharge,
          OptimizedFlexStrat
            .adaptEnergyBoundaries(batFlex.energyBoundaries.headOption.value)
            .etaCharge,
        )

      "compensate fixed powers when using linear objective" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          zeroKW,
          MinAbsPowerObjectiveFactory,
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

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          batRes should have size 12

          batRes.slice(0, 4).foreach {
            _.energyVal should (be >= 0d and be < 20d)
          }

          batRes.slice(4, 6).foreach {
            _.energyVal should approximate(20d)
          }

          batRes.slice(6, 10).foreach {
            _.energyVal should (be >= 0d and be < 20d)
          }

          batRes.slice(10, 12).foreach {
            _.energyVal should approximate(0d)
          }

          // we should've charged 20 kWh plus 5 kWh losses
          val inputCharged = batRes.slice(0, 6).map(_.pVal).sum
          inputCharged should approximate(25)

          // we should've discharged 20 kWh minus 4 kWh losses
          val outputDischarged =
            batRes.slice(6, 12).map(_.pVal).sum
          outputDischarged should approximate(-16d)

        } withClue buildDebugString(batVars)

        model.release()

      }

      "minimize peaks when using quadratic objective" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          zeroKW,
          LinearizedQuadraticPowerObjectiveFactory(
            segmentCount = 10,
            lastSegment = 10d, // 10 kW
          ),
        )

        model.minimize(objectiveContainer.objective)
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

          The soft constraints are vital here. Without them,
          optimization would overestimate the losses in the first half
          in order to achieve total power closer to zero.
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // 0 kW to compensate
          batRes(0).pVal should approximate(0d)
          batRes(0).energyVal should approximate(0d)

          // 6 kW of feed-in to compensate, 2 kW remains
          batRes(1).pVal should approximate(4d)
          batRes(1).energyVal should approximate(3.2d)

          // 8 kW of feed-in to compensate, 2 kW remains
          batRes(2).pVal should approximate(6d)
          batRes(2).energyVal should approximate(8d)

          // 7 kW of feed-in to compensate, 2 kW remains
          batRes(3).pVal should approximate(5d)
          batRes(3).energyVal should approximate(12d)

          // 12 kW of feed-in to compensate, 2 kW remains
          batRes(4).pVal should approximate(10d)
          batRes(4).energyVal should approximate(20d)

          // 0 kW to compensate
          batRes(5).pVal should approximate(0d)
          batRes(5).energyVal should approximate(20d)

          // 5 kW of load to compensate, 3 kW remains
          batRes(6).pVal should approximate(-2d)
          batRes(6).energyVal should approximate(17.5d)

          // 10 kW of load to compensate, 3 kW remains
          batRes(7).pVal should approximate(-7d)
          batRes(7).energyVal should approximate(8.75d)

          // 3 kW of load to compensate, 3 kW remains
          batRes(8).pVal should approximate(0d)
          batRes(8).energyVal should approximate(8.75d)

          // 7 kW of load to compensate, 3 kW remains
          batRes(9).pVal should approximate(-4d)
          batRes(9).energyVal should approximate(3.75d)

          // 6 kW of load to compensate, 3 kW remains
          batRes(10).pVal should approximate(-3d)
          batRes(10).energyVal should approximate(0d)

          // 0 kW to compensate
          batRes(11).pVal should approximate(0d)
          batRes(11).energyVal should approximate(0d)

          // we should've charged 20 kWh plus 5 kWh losses
          val inputCharged = batRes.slice(0, 6).map(_.pVal).sum
          inputCharged should approximate(25)

          // we should've discharged 20 kWh minus 4 kWh losses
          val outputDischarged =
            batRes.slice(6, 12).map(_.pVal).sum
          outputDischarged should approximate(-16d)

        } withClue buildDebugString(batVars)

        model.release()

      }
    }
  }

}

object OptimizedFlexStratSpec extends OptionValues {

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
