/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.simona.test.common.input.SwitchInputTestData
import edu.ie3.simona.test.common.model.grid.BasicGridWithSwitches
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}

import scala.util.Try

class SwitchModelSpec extends UnitSpec with DefaultTestData {

  "A SwitchesInputModel " should {

    "result in a valid SwitchModel" in new SwitchInputTestData {
      val switchModel: SwitchModel = SwitchModel(
        switchInput,
        defaultSimulationStart,
        defaultSimulationEnd,
      )

      inside(switchModel) {
        case SwitchModel(
              uuid,
              id,
              operationInterval,
              nodeAUuid,
              nodeBUuid,
            ) =>
          uuid should be(switchInput.getUuid)
          id should be(switchInput.getId)
          operationInterval should be(defaultOperationInterval)
          nodeAUuid should be(switchInput.getNodeA.getUuid)
          nodeBUuid should be(switchInput.getNodeB.getUuid)
      }
      switchModel.isClosed shouldBe true
    }

  }

  "An invalid SwitchesInputModel" should {

    "throw a InvalidGridException if it's nodeA == nodeB" in new SwitchInputTestData {
      val exception: InvalidGridException = intercept[InvalidGridException] {
        SwitchModel.validateInputModel(loopSwitchInput)
      }

      exception.getMessage shouldBe s"Switch ${loopSwitchInput.getUuid} has the same nodes on port A and B! " +
        s"NodeA: ${loopSwitchInput.getNodeA.getUuid}, NodeB: ${loopSwitchInput.getNodeB.getUuid}"

    }

    "throw a InvalidGridException if the nominal voltage of both nodes is not equal " in new SwitchInputTestData {
      val exception: InvalidGridException = intercept[InvalidGridException] {
        SwitchModel.validateInputModel(invalidSwitchInput)
      }

      exception.getMessage shouldBe s"Nodes of switch ${invalidSwitchInput.getUuid} have different volt levels! " +
        s"vNom: (nodeA: ${invalidSwitchInput.getNodeA.getVoltLvl.getNominalVoltage}, NodeB: ${invalidSwitchInput.getNodeB.getVoltLvl.getNominalVoltage})"
    }
  }

  "A valid SwitchModel" should {

    "be able to be enabled and disabled on request" in new BasicGridWithSwitches {

      switch1.isInOperation shouldBe false

      switch1.enable()

      switch1.isInOperation shouldBe true

      switch1.disable()

      switch1.isInOperation shouldBe false

    }

    "be able to be opened and close on request" in new BasicGridWithSwitches {

      switch1.isClosed shouldBe true

      switch1.open()

      switch1.isClosed shouldBe false

    }

    "result in a Failure if a closed switch is already closed or an open switch is already open" in new BasicGridWithSwitches {

      val tryToCloseVal: Try[String] = switch1.close()
      tryToCloseVal.failure.exception.getMessage shouldBe s"Switch ${switch1.id} is already closed!"

      switch1.open()
      val tryToOpenVal: Try[String] = switch1.open()

      tryToOpenVal.failure.exception.getMessage shouldBe s"Switch ${switch1.id} is already open!"

    }

  }

}
