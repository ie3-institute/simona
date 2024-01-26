/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

/*
package edu.ie3.simona.model.participant
import edu.ie3.simona.test.common.UnitSpec

import edu.ie3.datamodel.models.input.system.characteristic.{CosPhiFixed, CosPhiP, QV}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.model.MockParticipant
import edu.ie3.util.scala.OperationInterval


class SystemParticipantSpec extends UnitSpec {
  def calculateQTest(): Unit = {
    // Your Scala test code here

    // given: "the mocked system participant model with a q_v characteristic"
    val varCharacteristicString = "cosPhiFixed:{(0.0,0.9)}"
    val loadMock = new MockParticipant(
      UUID.fromString("b69f6675-5284-4e28-add5-b76952ec1ec2"),
      "System participant calculateQ Test",
      OperationInterval(0L, 86400L),
      1d,
      QControl(CosPhiFixed(varCharacteristicString)),
      Sq(200, Kilowatts),
      1d
    )
    val adjustedVoltage: Dimensionless = Sq(1, Each)

    // when: "the reactive power is calculated"
    val pVal = 100
    val power: Power = Sq(pVal, Kilowatts)
    val qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)

    // then: "compare the results in watt"
    Math.abs(qCalc.toKilovars - qSol.doubleValue) < 0.0001 shouldBe true

    // where:
    val testData = Table(
      ("varCharacteristicString", "pVal", "qSol"),
      ("cosPhiFixed:{(0.0,0.9)}", 0, 0),
      ("cosPhiFixed:{(0.0,0.9)}", 50, 24.216105241892627),
      ("cosPhiFixed:{(0.0,0.9)}", 100, 48.432210483785254),
      ("cosPhiFixed:{(0.0,0.9)}", 200, 0),
      ("cosPhiFixed:{(0.0,0.9)}", -50, -24.216105241892627),
      ("cosPhiFixed:{(0.0,0.9)}", -100, -48.432210483785254),
      ("cosPhiFixed:{(0.0,0.9)}", -200, 0),
      ("cosPhiFixed:{(0.0,1.0)}", 100, 0)
    )

    forAll(testData) { (varCharacteristicString, pVal, qSol) =>
      s"varCharacteristicString: $varCharacteristicString, pVal: $pVal, qSol: $qSol" in {
        // Your individual test cases here
      }
    }
}
*/