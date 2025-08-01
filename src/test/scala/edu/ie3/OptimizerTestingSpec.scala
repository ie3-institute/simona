/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3

import edu.ie3.simona.model.participant.storage.StorageMathProgrammingFlexModel.MPFlexOptions
import optimus.algebra.{Double2Const, Expression, Int2Const, Zero}
import optimus.optimization.*
import optimus.optimization.enums.SolverLib
import optimus.optimization.model.{MPBinaryVar, MPFloatVar, MPVar}
import org.scalatest.wordspec.AnyWordSpecLike
import squants.{Each, Time}
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.Hours

class OptimizerTestingSpec extends AnyWordSpecLike {

  "example" in {
    implicit val model: MPModel = MPModel(SolverLib.oJSolver)

    val x = MPFloatVar("x", 100, 200)
    val y = MPFloatVar("y", 80, 170)

    maximize(-2 * x + 5 * y)
    add(y >:= -x + 200)

    start()

    println(s"objective: $objectiveValue")
    println(s"x = ${x.value} y = ${y.value}")

    release()
  }

  def addFlexOptionsConstraints(soc: MPVar, p: MPVar)(using model: MPModel) =
    val zCharge = MPBinaryVar()
    val zDischarge = MPBinaryVar()

    add(soc <:= 1 - 0.0001 + (1e10 * (1 - zCharge)))
    add(p <:= 100 * zCharge)
    add(soc >:= 0.0001 - (1e10 * (1 - zDischarge)))
    add(p >:= -100 * zDischarge)

  def addStateConstraints(socOld: MPVar, p: MPVar, time: Time, socNew: MPVar)(
      using model: MPModel
  ) =
    add(socNew := socOld + p * time.toHours)

  "battery" in {
    implicit val model: MPModel = MPModel(SolverLib.oJSolver)

    val zCharge = MPBinaryVar("z_charge")
    val zDischarge = MPBinaryVar("z_discharge")
    val p_0 = MPFloatVar("p_0", -100, 100)
    val soc_0 = MPFloatVar("soc_0", 0, 1)

    minimize(p_0)
    add(soc_0 := 1)

    add(soc_0 <:= 1 - 0.0001 + (1e10 * (1 - zCharge)))
    add(p_0 <:= 100 * zCharge)
    add(soc_0 >:= 0.0001 - (1e10 * (1 - zDischarge))) // ??
    add(p_0 >:= -100 * zDischarge)

    val time = Hours(1)

    val p_1 = MPFloatVar("p_1", -100, 100)
    val soc_1 = MPFloatVar("soc_1", 0, 1)

    add(soc_1 := soc_0 + p_0 * time.toHours)

    start()

    println(model.getStatus)

    println(s"objective: $objectiveValue")
    println(s"z_charge = ${zCharge.value}")
    println(s"z_discharge = ${zDischarge.value}")
    println(s"soc_0 = ${soc_0.value}")
    println(s"soc_1 = ${soc_1.value}")

    release()
  }

  "Storage model" should {

    "balance out additional power" in {

      val fo = MPFlexOptions(
        currentEnergy = KilowattHours(50),
        eStorage = KilowattHours(100),
        pMax = Kilowatts(10),
        eta = Each(0.8)
      )

      implicit val model: MPModel = MPModel(SolverLib.oJSolver)

      val timeSpan = Hours(1)

      val state0 = fo.addInitialState
      val op0 = fo.addOperationConstraints(state0)

      val state1 = fo.addNewStateConstraints(state0, op0, timeSpan)
      val op1 = fo.addOperationConstraints(state1)

      val state2 = fo.addNewStateConstraints(state1, op1, timeSpan)
      val op2 = fo.addOperationConstraints(state2)

      val state3 = fo.addNewStateConstraints(state2, op2, timeSpan)
      val op3 = fo.addOperationConstraints(state3)

      val state4 = fo.addNewStateConstraints(state3, op3, timeSpan)

      val batOps = Seq(op0, op1, op2, op3)
      val addPower = Seq(1d, -11d, 10d, 2d)

      val objective = batOps.zip(addPower).foldLeft[Expression](Zero) {
        case (sum, (bat, add)) =>
          val total = bat.pCharge + bat.pDischarge + add
          sum + total * total
      }

      model.minimize(objective)

      start()

      println(model.getStatus)

      println(s"objective: $objectiveValue")
      println(s"soc0 = ${state0.storedEnergy.value}")
      println(s"op0.pCharge = ${op0.pCharge.value}")
      println(s"op0.pDischarge = ${op0.pDischarge.value}")
      println(s"soc1 = ${state1.storedEnergy.value}")
      println(s"op1.pCharge = ${op1.pCharge.value}")
      println(s"op1.pDischarge = ${op1.pDischarge.value}")
      println(s"soc2 = ${state2.storedEnergy.value}")
      println(s"op2.pCharge = ${op2.pCharge.value}")
      println(s"op2.pDischarge = ${op2.pDischarge.value}")
      println(s"soc3 = ${state3.storedEnergy.value}")
      println(s"op3.pCharge = ${op3.pCharge.value}")
      println(s"op3.pDischarge = ${op3.pDischarge.value}")

      release()
    }

    "work correctly at extreme values" in {
      // battery full, almost full, half full, almost empty, empty

      // todo variable time? nope?

      val fo = MPFlexOptions(
        currentEnergy = KilowattHours(50),
        eStorage = KilowattHours(100),
        pMax = Kilowatts(10),
        eta = Each(0.8)
      )

      implicit val model: MPModel = MPModel(SolverLib.oJSolver)

      val timeSpan = Hours(1)

      val state0 = fo.addInitialState
      val op0 = fo.addOperationConstraints(state0)

      val state1 = fo.addNewStateConstraints(state0, op0, timeSpan)
      val op1 = fo.addOperationConstraints(state1)

      val batOps = Seq(op0, op1)
      val addPower = Seq(1d, -11d)

      val objective = batOps.zip(addPower).foldLeft[Expression](Zero) {
        case (sum, (bat, add)) =>
          val total = bat.pCharge + bat.pDischarge + add
          sum + total * total
      }

      model.minimize(objective)

      start()

      println(model.getStatus)

      println(s"objective: $objectiveValue")
      println(s"soc0 = ${state0.storedEnergy.value}")
      println(s"op0.pCharge = ${op0.pCharge.value}")
      println(s"op0.pDischarge = ${op0.pDischarge.value}")
      println(s"soc1 = ${state1.storedEnergy.value}")
      println(s"op1.pCharge = ${op1.pCharge.value}")
      println(s"op1.pDischarge = ${op1.pDischarge.value}")

      release()

    }
  }

}
