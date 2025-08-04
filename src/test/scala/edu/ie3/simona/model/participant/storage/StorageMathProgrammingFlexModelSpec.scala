/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.storage

import edu.ie3.simona.model.em.OptimizedFlexStrat
import edu.ie3.simona.model.participant.storage.StorageMathProgrammingFlexModel.{
  MPFlexOptions,
  StorageOperationVars,
  StorageStateVars,
}
import edu.ie3.simona.model.participant.storage.StorageMathProgrammingFlexModelSpec.*
import edu.ie3.simona.test.common.UnitSpec
import optimus.algebra.{Double2Const, Expression, Zero}
import optimus.optimization.*
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import optimus.optimization.model.MPFloatVar
import org.scalatest.OptionValues
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.Hours

import java.util.UUID

class StorageMathProgrammingFlexModelSpec extends UnitSpec {

  // Testing tolerances
  given Double = 1e-10

  "StorageMathProgrammingFlexModelSpec" should {

    "balance out additional power with zero excess" in {

      // low efficiency for simplicity of the test
      val fo = MPFlexOptions(
        currentEnergy = KilowattHours(50),
        eStorage = KilowattHours(100),
        pMax = Kilowatts(10),
        eta = Each(0.8),
      )

      implicit val model: MPModel = MPModel(SolverLib.oJSolver)

      val timestepResolution = Hours(1)

      val container = OptimizedFlexStrat.addConstraints(
        assetUuid = UUID.randomUUID(),
        flexOptions = fo,
        timeSteps = 4,
        stepResolution = timestepResolution,
      )

      container.states should have length 5
      container.operationVars should have length 4

      // powers all within pMax
      val addPower = Seq(5d, -10d, 10d, -2d)

      val mainObjectiveDifferences =
        container.operationVars.zip(addPower).map { case (opVar, add) =>
          val d = MPFloatVar(0, Double.PositiveInfinity)
          model.add(d >:= opVar.getPowerExpression + add)
          model.add(d >:= -(opVar.getPowerExpression + add))

          d
        }

      val softConstraints =
        container.operationVars.flatMap(_.getSoftConstraints)

      val objective = mainObjectiveDifferences
        .appendedAll(softConstraints)
        .foldLeft[Expression](Zero) { case (sum, expr) =>
          sum + expr
        }

      model.minimize(objective)

      model.start()

      println(model.getStatus)

      model.getStatus shouldBe SolutionStatus.OPTIMAL

      // Battery should be able to fully cover the additional power,
      // and soft constraints should amount to zero as well
      model.objectiveValue should approximate(0)

      container.states(0).energyVal should approximate(50)

      // discharging 5 kWh plus 1.25 kWh losses
      container.operationVars(0).pChargeVal should approximate(0)
      container.operationVars(0).pDischargeVal should approximate(5)
      container.states(1).energyVal should approximate(43.75)

      // charging 10 kWh minus 2 kWh losses
      container.operationVars(1).pChargeVal should approximate(10)
      container.operationVars(1).pDischargeVal should approximate(0)
      container.states(2).energyVal should approximate(51.75)

      // discharging 10 kWh plus 2.5 kWh losses
      container.operationVars(2).pChargeVal should approximate(0)
      container.operationVars(2).pDischargeVal should approximate(10)
      container.states(3).energyVal should approximate(39.25)

      // charging 2 kWh minus 0.4 kWh losses
      container.operationVars(3).pChargeVal should approximate(2)
      container.operationVars(3).pDischargeVal should approximate(0)
      container.states(4).energyVal should approximate(40.85)

      model.release()
    }

    "balance out additional power with large excess" in {

      // low efficiency for simplicity of the test
      val fo = MPFlexOptions(
        currentEnergy = KilowattHours(50),
        eStorage = KilowattHours(100),
        pMax = Kilowatts(10),
        eta = Each(0.8),
      )

      implicit val model: MPModel = MPModel(SolverLib.oJSolver)

      val timestepResolution = Hours(1)

      val container = OptimizedFlexStrat.addConstraints(
        assetUuid = UUID.randomUUID(),
        flexOptions = fo,
        timeSteps = 4,
        stepResolution = timestepResolution,
      )

      container.states should have length 5
      container.operationVars should have length 4

      // powers all within pMax
      val addPower = Seq(5d, -60d, 110d, -2d)

      val objective = container.operationVars
        .zip(addPower)
        .foldLeft[Expression](Zero) { case (sum, (bat, add)) =>
          val d = MPFloatVar(0, Double.PositiveInfinity)
          model.add(d >:= bat.getPowerExpression + add)
          model.add(d >:= -(bat.getPowerExpression + add))
          sum + d + bat.getSoftConstraints.getOrElse(Zero)
        }

      model.minimize(objective)

      model.start()

      model.getStatus shouldBe SolutionStatus.OPTIMAL

      // Battery should be able to fully cover the additional power,
      // and soft constraints should amount to zero as well
      model.objectiveValue should approximate(150)

      container.states(0).energyVal should approximate(50)

      // discharging 5 kWh plus 1.25 kWh losses
      container.operationVars(0).pChargeVal should approximate(0)
      container.operationVars(0).pDischargeVal should approximate(5)
      container.states(1).energyVal should approximate(43.75)

      // charging 10 kWh minus 2 kWh losses
      container.operationVars(1).pChargeVal should approximate(10)
      container.operationVars(1).pDischargeVal should approximate(0)
      container.states(2).energyVal should approximate(51.75)

      // discharging 10 kWh plus 2.5 kWh losses
      container.operationVars(2).pChargeVal should approximate(0)
      container.operationVars(2).pDischargeVal should approximate(10)
      container.states(3).energyVal should approximate(39.25)

      // charging 2 kWh minus 0.4 kWh losses
      container.operationVars(3).pChargeVal should approximate(2)
      container.operationVars(3).pDischargeVal should approximate(0)
      container.states(4).energyVal should approximate(40.85)

      model.release()
    }

    "work correctly at extreme values" in {
      // battery full, almost full, half full, almost empty, empty

      // todo variable time? nope?

    }
  }

}

object StorageMathProgrammingFlexModelSpec extends OptionValues {

  extension (state: StorageStateVars)
    def energyVal: Double = state.storedEnergy.value.value

  extension (state: StorageOperationVars)
    def pChargeVal: Double = state.pCharge.value.value
    def pDischargeVal: Double = state.pDischarge.value.value

}
