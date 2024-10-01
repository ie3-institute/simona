/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.characteristic.{
  CosPhiFixed,
  CosPhiP,
  QV,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.MockParticipant
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower}
import org.scalatest.matchers.should.Matchers
import squants._
import squants.energy._

import java.util.UUID
import scala.language.postfixOps

class SystemParticipantSpec extends UnitSpec with Matchers {

  private implicit val tolerance: ReactivePower = Megavars(
    1e-5
  )

  "SystemParticipant" should {
    "calculate reactive power correctly for fixed cos phi" in {
      val adjustedVoltage =
        Each(1) // not applicable for cos phi_fixed but required

      val testCases = Table(
        ("varCharacteristicString", "pVal", "qSol"),
        ("cosPhiFixed:{(0.0,0.9)}", 0, Megavars(0)),
        ("cosPhiFixed:{(0.0,0.9)}", 50, Megavars(0.024216)),
        ("cosPhiFixed:{(0.0,0.9)}", 100, Megavars(0.048432)),
        ("cosPhiFixed:{(0.0,0.9)}", 200, Megavars(0)),
        ("cosPhiFixed:{(0.0,0.9)}", -50, Megavars(-0.024216)),
        ("cosPhiFixed:{(0.0,0.9)}", -100, Megavars(-0.048432)),
        ("cosPhiFixed:{(0.0,0.9)}", -200, Megavars(0)),
        ("cosPhiFixed:{(0.0,1.0)}", 100, Megavars(0))
      )


      forAll(testCases) { (varCharacteristicString, pVal, expectedQ) =>
        val loadMock = new MockParticipant(
          UUID.fromString("b69f6675-5284-4e28-add5-b76952ec1ec2"),
          "System participant calculateQ Test",
          OperationInterval(0L, 86400L),
          QControl(new CosPhiFixed(varCharacteristicString)),
          Kilowatts(200),
          1d,
        )
        val power = Kilowatts(pVal)
        val qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)
        qCalc should approximate(expectedQ)
      }
    }
  }

  "calculate reactive power correctly for cosphi_p" in {

    val adjustedVoltage =
      Each(1) // needed for method call but not applicable for cos phi_p

    val testCases = Table(
      ("varCharacteristicString", "pVal", "expectedQ"),
      (
        "cosPhiP:{(0,1),(0.05,1),(0.1,1),(0.15,1),(0.2,1),(0.25,1),(0.3,1),(0.35,1),(0.4,1),(0.45,1),(0.5,1),(0.55,0.99),(0.6,0.98),(0.65,0.97),(0.7,0.96),(0.75,0.95),(0.8,0.94),(0.85,0.93),(0.9,0.92),(0.95,0.91),(1,0.9)}",
        100,
        Megavars(0.020099),
      ),
      (
        "cosPhiP:{(0,-1),(0.05,-1),(0.1,-1),(0.15,-1),(0.2,-1),(0.25,-1),(0.3,-1),(0.35,-1),(0.4,-1),(0.45,-1),(0.5,-1),(0.55,-0.99),(0.6,-0.98),(0.65,-0.97),(0.7,-0.96),(0.75,-0.95),(0.8,-0.94),(0.85,-0.93),(0.9,-0.92),(0.95,-0.91),(1,-0.9)}",
        100,
        Megavars(-0.020099),
      ),
    )

    // first line is "with P" -> negative Q (influence on voltage level: increase) is expected
    // second line is "against P" -> positive Q (influence on voltage level: decrease) is expected

    forAll(testCases) { (varCharacteristicString, pVal, expectedQ) =>
      val loadMock = new MockParticipant(
        UUID.fromString("30f84d97-83b4-4b71-9c2d-dbc7ebb1127c"),
        "Generation calculateQ Test",
        OperationInterval(0L, 86400L),
        QControl(
          new CosPhiP(varCharacteristicString)
        ),
        Kilowatts(102),
        1d,
      )
      val power = Kilowatts(pVal)
      val qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)
      (qCalc - expectedQ).abs should be <= Megavars(1e-6)
    }
  }

  "calculate reactive power correctly for generation unit with cosphi_p" in {
    val adjustedVoltage =
      Each(1) // needed for method call but not applicable for cos phi_p

    val testCases = Table(
      ("varCharacteristicString", "pVal", "expectedQ"),
      (
        "cosPhiP:{(-1,0.9),(-0.95,0.91),(-0.9,0.92),(-0.85,0.93),(-0.8,0.94),(-0.75,0.95),(-0.7,0.96),(-0.65,0.97),(-0.6,0.98),(-0.55,0.99),(-0.5,1),(-0.45,1),(-0.4,1),(-0.35,1),(-0.3,1),(-0.25,1),(-0.2,1),(-0.15,1),(-0.1,1),(-0.05,1),(0,1)}",
        -100,
        Megavars(-0.014177),
      ),
      (
        "cosPhiP:{(-1,-0.9),(-0.95,-0.91),(-0.9,-0.92),(-0.85,-0.93),(-0.8,-0.94),(-0.75,-0.95),(-0.7,-0.96),(-0.65,-0.97),(-0.6,-0.98),(-0.55,-0.99),(-0.5,-1),(-0.45,-1),(-0.4,-1),(-0.35,-1),(-0.3,-1),(-0.25,-1),(-0.2,-1),(-0.15,-1),(-0.1,-1),(-0.05,-1),(0,-1)}",
        -100,
        Megavars(0.014177),
      ),
    )

    // first line is "with P" -> negative Q (influence on voltage level: increase) is expected
    // second line is "against P" -> positive Q (influence on voltage level: decrease) is expected

    forAll(testCases) { (varCharacteristicString, pVal, expectedQ) =>
      val loadMock = new MockParticipant(
        UUID.fromString("30f84d97-83b4-4b71-9c2d-dbc7ebb1127c"),
        "Generation calculateQ Test",
        OperationInterval(0L, 86400L),
        QControl(
          new CosPhiP(varCharacteristicString)
        ),
        Kilowatts(101),
        1d,
      )
      val power = Kilowatts(pVal)
      val qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)
      //(qCalc - expectedQ).abs should be < 0.001
      (qCalc - expectedQ).abs should be <= Megavars(1e-6)
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
      (0.9, Megavars(-0.06244)),
      (0.93, Megavars(-0.06244)),
      (0.95, Megavars(-0.03122)),
      (0.97, Megavars(0)),
      (1.00, Megavars(0)),
      (1.03, Megavars(0)),
      (1.05, Megavars(0.03122)),
      (1.07, Megavars(0.06244)),
      (1.1, Megavars(0.06244)),
    )

    forAll(testCases) { (adjustedVoltageVal, expectedQ) =>
      val adjustedVoltage = Each(adjustedVoltageVal)
      val p = Kilowatts(42)
      val qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)
      qCalc should approximate(expectedQ)
    }
  }

  "calculate reactive power correctly for a standard q_v characteristic if active power is 195 and cosPhiRated is 0.95" in {
    val activePower: Power = Kilowatts(195)
    val loadMock = new MockParticipant(
      UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
      "System participant calculateQ Test",
      OperationInterval(0L, 86400L),
      QControl(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
      Kilowatts(200),
      0.95
    )

    val testCases = Table(
      ("adjustedVoltageVal", "expectedQ"),
      (0.9, Megavars(-0.04444)),
      (0.93, Megavars(-0.04444)),
      (0.95, Megavars(-0.03122)),
      (0.97, Megavars(0)),
      (1.00, Megavars(0)),
      (1.03, Megavars(0)),
      (1.05, Megavars(0.03122)),
      (1.07, Megavars(0.04444)),
      (1.1, Megavars(0.04444))
    )

    forAll(testCases) { (adjustedVoltageVal, expectedQ) =>
      val adjustedVoltage: Dimensionless = Each(adjustedVoltageVal)
      val qCalc = loadMock.calculateReactivePower(activePower, adjustedVoltage)
      qCalc should approximate(expectedQ)
    }
  }


}
