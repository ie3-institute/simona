/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import breeze.numerics.constants.e
import edu.ie3.datamodel.models.input.system.characteristic.{CosPhiFixed, CosPhiP, QV}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.MockParticipant
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Kilovars, Megavars, ReactivePower}
import org.scalatest.matchers.should.Matchers
import squants._
import squants.energy._

import java.util.UUID

class SystemParticipantSpec extends UnitSpec with Matchers {

  private implicit val positiveTolerance: ReactivePower = Megavars(
    1e-5
  )


  "SystemParticipant" should {
    "calculate reactive power correctly for fixed cos phi" in {
      val loadMock = new MockParticipant(
        UUID.fromString("b69f6675-5284-4e28-add5-b76952ec1ec2"),
        "System participant calculateQ Test",
        OperationInterval(0L, 86400L),
        QControl(new CosPhiFixed("cosPhiFixed:{(0.0,0.9)}")),
        Kilowatts(200),
        1d,
      )

      val adjustedVoltage =
        Each(1) // not applicable for cos phi_fixed but required

      val testCases = Table(
        ("varCharacteristicString", "pVal", "expectedQ"),
        ("cosPhiFixed:{(0.0,0.9)}", 0, Megavars(0)),
        ("cosPhiFixed:{(0.0,0.9)}", 50, Megavars(0.024216)),
        ("cosPhiFixed:{(0.0,0.9)}", 100, Megavars(0.048432)),
        ("cosPhiFixed:{(0.0,0.9)}", 200, Megavars(0)),
        ("cosPhiFixed:{(0.0,-0.9)}", -50, Megavars(-0.024216)),
        ("cosPhiFixed:{(0.0,-0.9)}", -100, Megavars(-0.048432)),
        ("cosPhiFixed:{(0.0,-0.9)}", -200, Megavars(0)),
        ("cosPhiFixed:{(0.0,1.0)}", 100, Megavars(0)),
      )

      forAll(testCases) { (_, pVal, expectedQ) =>
        val power = Kilowatts(pVal)
        val qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)
        qCalc should approximate(expectedQ)
      }
    }
  }

  "calculate reactive power correctly for cosphi_p" in {
    val loadMock = new MockParticipant(
      UUID.fromString("3d28b9f7-929a-48e3-8696-ad2330a04225"),
      "Load calculateQ Test",
      OperationInterval(0L, 86400L),
      QControl(
        new CosPhiP("cosPhiP:{(0,1),(0.05,1),(0.1,1),(0.95,0.91),(1,0.9)}")
      ),
      Kilowatts(102),
      1d,
    )

    val adjustedVoltage =
      Each(1) // needed for method call but not applicable for cos phi_p

    val testCases = Table(
      ("varCharacteristicString", "pVal", "expectedQ"),
      (
        "cosPhiP:{(0,1),(0.05,1),(0.1,1),(0.95,0.91),(1,0.9)}",
        100,
        Megavars(0.020099),
      ),
      (
        "cosPhiP:{(0,-1),(0.05,-1),(0.1,-1),(0.95,-0.91),(1,-0.9)}",
        100,
        Megavars(-0.020099),
      ),
    )

    forAll(testCases) { (_, pVal, expectedQ) =>
      val power = Kilowatts(pVal)
      val qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)
      qCalc should approximate(expectedQ)
    }
  }

  "calculate reactive power correctly for generation unit with cosphi_p" in {
    val loadMock = new MockParticipant(
      UUID.fromString("30f84d97-83b4-4b71-9c2d-dbc7ebb1127c"),
      "Generation calculateQ Test",
      OperationInterval(0L, 86400L),
      QControl(
        new CosPhiP("cosPhiP:{(-1,0.9),(-0.95,0.91),(-0.9,0.92),(0,1)}")
      ),
      Kilowatts(101),
      1d,
    )

    val adjustedVoltage =
      Each(1) // needed for method call but not applicable for cos phi_p

    val testCases = Table(
      ("varCharacteristicString", "pVal", "expectedQ"),
      (
        "cosPhiP:{(-1,0.9),(-0.95,0.91),(-0.9,0.92),(0,1)}",
        -100,
        Megavars(-0.014177),
      ),
      (
        "cosPhiP:{(-1,-0.9),(-0.95,-0.91),(-0.9,-0.92),(0,-1)}",
        -100,
        Megavars(0.014177),
      ),
    )

    forAll(testCases) { (_, pVal, expectedQ) =>
      val power = Kilowatts(pVal)
      val qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)
      qCalc should approximate(expectedQ)
    }
  }

  "calculate reactive power correctly for a standard q_v characteristic" in {
    val loadMock = new MockParticipant(
      UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
      "System participant calculateQ Test",
      OperationInterval(0L, 86400L),
      QControl(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
      Kilowatts(200),
      0.98,
    )

    val testCases = Table(
      ("adjustedVoltageVal", "expectedQ"),
      (0.9, Megavars(-0.039799)),
      (0.93, Megavars(-0.039799)),
      (0.95, Megavars(-0.019899)),
      (0.97, Megavars(0)),
      (1.00, Megavars(0)),
      (1.03, Megavars(0)),
      (1.05, Megavars(0.019899)),
      (1.07, Megavars(0.039799)),
      (1.1, Megavars(0.039799)),
    )

    forAll(testCases) { (adjustedVoltageVal, expectedQ) =>
      val adjustedVoltage = Each(adjustedVoltageVal)
      val p = Kilowatts(42)
      val qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)
      qCalc should approximate(expectedQ)
    }
  }

  "calculate reactive power correctly for q_v characteristic if active power is zero and cosPhiRated is 1" in {
    val loadMock = new MockParticipant(
      UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
      "System participant calculateQ Test",
      OperationInterval(0L, 86400L),
      QControl(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
      Kilowatts(200),
      1d,
    )

    val testCases = Table(
      ("adjustedVoltageVal", "expectedQ"),
      (0.9, Megavars(0)),
      (0.93, Megavars(0)),
      (0.95, Megavars(0)),
      (0.97, Megavars(0)),
      (1.00, Megavars(0)),
      (1.03, Megavars(0)),
      (1.05, Megavars(0)),
      (1.07, Megavars(0)),
      (1.1, Megavars(0)),
    )


    forAll(testCases) { (adjustedVoltageVal, expectedQ) =>
      val adjustedVoltage = Each(adjustedVoltageVal)
      val p = Kilowatts(0)
      val qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)
      qCalc should approximate(expectedQ)
    }
  }

  "calculate reactive power correctly for q_v characteristic if active power is not zero and cosPhiRated is 0.95" in {
    val loadMock = new MockParticipant(
      UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
      "System participant calculateQ Test",
      OperationInterval(0L, 86400L),
      QControl(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
      Kilowatts(200),
      0.95,
    )

    val testCases = Table(
      ("adjustedVoltageVal", "expectedQ"),
      (0.9, Megavars(0.43882)),
      (0.93, Megavars(0.43882)),
      (0.95, Megavars(0.21941)),
      (0.97, Megavars(0)),
      (1.00, Megavars(0)),
      (1.03, Megavars(0)),
      (1.05, Megavars(0.21941)),
      (1.07, Megavars(0.43882)),
      (1.1, Megavars(0.43882)),
    )

    forAll(testCases) { (adjustedVoltageVal, expectedQ) =>
      val adjustedVoltage = Each(adjustedVoltageVal)
      val p = Kilowatts(42)
      val qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)
      qCalc should approximate(expectedQ)
    }
  }
}
