/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.em.OptimizedFlexStrat
import edu.ie3.simona.model.participant.storage.StorageMathFlexModel.StorageMathFlexOptions
import edu.ie3.simona.model.participant.storage.StorageMathFlexModelSpec.*
import edu.ie3.simona.test.common.{MathFlexTestLike, UnitSpec}
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import optimus.algebra.{Double2Const, Zero}
import optimus.optimization.*
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.MPFloatVar
import org.scalatest.OptionValues
import squants.energy.{Energy, KilowattHours, Kilowatts, WattHours}
import squants.time.Hours
import squants.{Dimensionless, Each, Power, Time}

import java.util.UUID

class StorageMathFlexModelSpec extends UnitSpec with MathFlexTestLike {

  // Testing tolerances
  given Double = 1e-6
  given Energy = WattHours(1e-9)
  val constraintTolerance = 1e-3

  "StorageMathFlexModelSpec" should {

    "be created with correct parameters" in {

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

          // charging until full, with maximum power
          val power1 = pMax
          val duration1 = (eStorage - currentEnergy) / (power1 * etaCharging)
          // discharging three quarters, with half power
          val power2 = -pMax / 2
          val time2 = eStorage * 0.75 / (-power2 * 1 / etaDischarging)
          // charging until half, with quarter power
          val power3 = pMax / 4
          val time3 = eStorage / 4 / (power3 * etaCharging)
          // discharging until empty, with maximum power
          val power4 = -pMax
          val time4 = eStorage / 2 / (-power4 * 1 / etaDischarging)

          val adaptedFlexOptions =
            StorageMathFlexOptions.createAdaptedFlexOptions(
              currentEnergy,
              eStorage,
              pMax,
              Each(etaCharging),
              Each(etaDischarging),
            )

          val adapted = AdaptedModel(
            currentEnergy = adaptedFlexOptions.currentEnergy,
            eStorage = adaptedFlexOptions.eStorage,
            pMax = adaptedFlexOptions.pMax,
            etaAvg = adaptedFlexOptions.eta,
          )

          val results = IndexedSeq(classic, adapted).map(
            _.charge(power1, duration1)
              .charge(power2, time2)
              .charge(power3, time3)
              .charge(power4, time4)
              .currentEnergy
          )

          // battery should be exactly empty
          results(0) should approximate(zeroKWh)
          results(1) should approximate(zeroKWh)

        }
      }
    }

    "balance out additional power with zero excess" in {

      // low efficiency for simplicity of the test
      val fo = StorageMathFlexOptions.createAdaptedFlexOptions(
        currentEnergy = KilowattHours(50),
        eStorage = KilowattHours(100),
        pMax = Kilowatts(10),
        etaCharging = Each(0.8),
        etaDischarging = Each(0.8),
      )

      val stepResolution = Hours(1)
      val tickResolution = stepResolution.toSeconds.toLong

      given model: MPModel = MPModel(SolverLib.oJSolver)

      // since energy values have been adapted, we need this
      // factor to convert back to "real" values
      given EnergyConversionFactor = EnergyConversionFactor(Each(0.8), fo.eta)

      val container = OptimizedFlexStrat.addAssetConstraints(
        assetUuid = UUID.randomUUID(),
        flexOptions = fo,
        ticks = Range.Long(0, tickResolution * 5, tickResolution),
      )

      container.states should have length 5
      container.operationVars should have length 4

      // additional powers for each time step, all within pMax
      val addPower = Seq(5d, -10d, 10d, -2d)

      val mainObjectiveDifferences =
        container.operationVars.zip(addPower).map { case (opVar, add) =>
          val d = MPFloatVar.positive("d")
          model.add(d >:= opVar.getPowerExpression + add)
          model.add(d >:= -(opVar.getPowerExpression + add))
          d
        }

      val softConstraints =
        container.operationVars.flatMap(_.getSoftConstraints(stepResolution))

      val objective = mainObjectiveDifferences
        .appendedAll(softConstraints.map(_.getExpression))
        .reduceLeftOption(_ + _)
        .getOrElse(Zero)

      model.minimize(objective)

      model.start()

      model.getStatus shouldBe SolutionStatus.OPTIMAL

      // Battery should be able to fully cover the additional power

      {
        softConstraints.foreach { constraint =>
          withClue(constraint.getWarningMessage) {
            constraint.getError should be < constraintTolerance
          }
        }

        container.states(0).energyVal should approximate(50)

        // discharging 5 kWh plus 1.25 kWh losses
        container.operationVars(0).pVal should approximate(-5)
        container.states(1).energyVal should approximate(43.75)

        // charging 10 kWh minus 2 kWh losses
        container.operationVars(1).pVal should approximate(10)
        container.states(2).energyVal should approximate(51.75)

        // discharging 10 kWh plus 2.5 kWh losses
        container.operationVars(2).pVal should approximate(-10)
        container.states(3).energyVal should approximate(39.25)

        // charging 2 kWh minus 0.4 kWh losses
        container.operationVars(3).pVal should approximate(2)
        container.states(4).energyVal should approximate(40.85)

      } withClue buildDebugString(container)

      model.release()
    }

    "balance out additional power with large excess" in {

      // low efficiency for simplicity of the test
      val fo = StorageMathFlexOptions.createAdaptedFlexOptions(
        currentEnergy = KilowattHours(50),
        eStorage = KilowattHours(100),
        pMax = Kilowatts(10),
        etaCharging = Each(0.8),
        etaDischarging = Each(0.8),
      )

      val stepResolution = Hours(1)
      val tickResolution = stepResolution.toSeconds.toLong

      given model: MPModel = MPModel(SolverLib.oJSolver)

      // since energy values have been adapted, we need this
      // factor to convert back to "real" values
      given EnergyConversionFactor = EnergyConversionFactor(Each(0.8), fo.eta)

      val container = OptimizedFlexStrat.addAssetConstraints(
        assetUuid = UUID.randomUUID(),
        flexOptions = fo,
        ticks = Range.Long(0, tickResolution * 5, tickResolution),
      )

      container.states should have length 5
      container.operationVars should have length 4

      // additional powers for each time step, some far beyond pMax
      val addPower = Seq(5d, -60d, 110d, -2d)

      val mainObjectiveDifferences =
        container.operationVars.zip(addPower).map { case (opVar, add) =>
          val d = MPFloatVar.positive("d")
          model.add(d >:= opVar.getPowerExpression + add)
          model.add(d >:= -(opVar.getPowerExpression + add))
          d
        }

      val softConstraints =
        container.operationVars.flatMap(_.getSoftConstraints(stepResolution))

      val objective = mainObjectiveDifferences
        .appendedAll(softConstraints.map(_.getExpression))
        .reduceLeftOption(_ + _)
        .getOrElse(Zero)

      model.minimize(objective)

      model.start()

      model.getStatus shouldBe SolutionStatus.OPTIMAL

      // Battery should be able to fully cover the additional power

      {
        softConstraints.foreach { constraint =>
          withClue(constraint.getWarningMessage) {
            constraint.getError should be < constraintTolerance
          }
        }

        container.states(0).energyVal should approximate(50)

        // discharging 5 kWh plus 1.25 kWh losses
        container.operationVars(0).pVal should approximate(-5)
        container.states(1).energyVal should approximate(43.75)

        // charging 10 kWh minus 2 kWh losses
        container.operationVars(1).pVal should approximate(10)
        container.states(2).energyVal should approximate(51.75)

        // discharging 10 kWh plus 2.5 kWh losses
        container.operationVars(2).pVal should approximate(-10)
        container.states(3).energyVal should approximate(39.25)

        // charging 2 kWh minus 0.4 kWh losses
        container.operationVars(3).pVal should approximate(2)
        container.states(4).energyVal should approximate(40.85)

      } withClue buildDebugString(container)

      model.release()
    }

    "work correctly at extreme values" in {
      // low efficiency for simplicity of the test
      val fo = StorageMathFlexOptions.createAdaptedFlexOptions(
        currentEnergy = KilowattHours(10),
        eStorage = KilowattHours(20),
        pMax = Kilowatts(10),
        etaCharging = Each(0.8),
        etaDischarging = Each(0.8),
      )

      val stepResolution = Hours(1)
      val tickResolution = stepResolution.toSeconds.toLong

      given model: MPModel = MPModel(SolverLib.oJSolver)

      // since energy values have been adapted, we need this
      // factor to convert back to "real" values
      given EnergyConversionFactor = EnergyConversionFactor(Each(0.8), fo.eta)

      val container = OptimizedFlexStrat.addAssetConstraints(
        assetUuid = UUID.randomUUID(),
        flexOptions = fo,
        ticks = Range.Long(0, tickResolution * 5, tickResolution),
      )

      container.states should have length 5
      container.operationVars should have length 4

      // additional powers for each time step
      val addPower = Seq(-5d, -10d, 10d, 10d)

      val mainObjectiveDifferences =
        container.operationVars.zip(addPower).map { case (opVar, add) =>
          val d = MPFloatVar.positive("d")
          model.add(d >:= opVar.getPowerExpression + add)
          model.add(d >:= -(opVar.getPowerExpression + add))
          d
        }

      val softConstraints =
        container.operationVars.flatMap(_.getSoftConstraints(stepResolution))

      val objective = mainObjectiveDifferences
        .appendedAll(softConstraints.map(_.getExpression))
        .reduceLeftOption(_ + _)
        .getOrElse(Zero)

      model.minimize(objective)

      model.start()

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
        softConstraints.foreach { constraint =>
          withClue(constraint.getWarningMessage) {
            constraint.getError should be < constraintTolerance
          }
        }

        container.states(0).energyVal should approximate(10)

        // possibly charging
        container.operationVars(0).pVal should be >= 0d
        container.states(1).energyVal should (be >= 0d and be <= 20d)

        // possibly charging, now we should have reached 20 kWh
        container.operationVars(1).pVal should be >= 0d
        container.states(2).energyVal should approximate(20d)

        // we should've charged 10 kWh plus 2.5 kWh losses
        val totalCharged =
          container.operationVars(0).pVal + container.operationVars(1).pVal
        totalCharged should approximate(12.5d)

        // possibly discharging
        container.operationVars(2).pVal should be <= 0d
        container.states(3).energyVal should (be >= 0d and be <= 20d)

        // possibly discharging, now we should have reached 0 kWh
        container.operationVars(3).pVal should be <= 0d
        container.states(4).energyVal should approximate(0d)

        // we should've discharged 20 kWh minus 4 kWh losses
        val totalDischarged =
          container.operationVars(2).pVal + container.operationVars(3).pVal
        totalDischarged should approximate(-16d)

      } withClue buildDebugString(container)

      model.release()

    }

  }

}

object StorageMathFlexModelSpec extends OptionValues {

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
