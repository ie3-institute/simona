/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.exceptions.InvalidGridException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.NodeInputTestData
import edu.ie3.simona.test.common.model.grid.FiveLinesWithNodes
import squants.Each

class NodeInputModelSpec extends UnitSpec with NodeInputTestData {
  implicit val dimensionlessTolerance: squants.Dimensionless = Each(1e-12)

  "A valid NodeInputModel" should {

    "be validated without an exception" in {
      NodeModel.validateInputModel(nodeInputNoSlackNs04KvA)
    }

    "result in a valid NodeModel" in {

      val validNodeModel =
        NodeModel(
          nodeInputNoSlackNs04KvA,
          defaultSimulationStart,
          defaultSimulationEnd,
        )

      inside(validNodeModel) {
        case NodeModel(
              uuid,
              id,
              operationInterval,
              isSlack,
              vTarget,
              voltLvl,
              subnet,
            ) =>
          uuid shouldBe nodeInputNoSlackNs04KvA.getUuid
          id shouldBe nodeInputNoSlackNs04KvA.getId
          operationInterval shouldBe defaultOperationInterval
          isSlack shouldBe nodeInputNoSlackNs04KvA.isSlack
          vTarget should approximate(
            Each(nodeInputNoSlackNs04KvA.getvTarget.getValue.doubleValue())
          )
          voltLvl shouldBe nodeInputNoSlackNs04KvA.getVoltLvl
          subnet shouldBe -1
      }

    }

  }

  "An invalid NodeInputModel" should {

    "throw a InvalidGridException if vTarget is invalid" in {
      val exception = intercept[InvalidGridException] {
        NodeModel.validateInputModel(nodeInputNoSlackNs04KvWrongVTarget)
      }

      exception.getMessage shouldBe s"Invalid vTarget parameter in node ${nodeInputNoSlackNs04KvWrongVTarget.getUuid}: ${nodeInputNoSlackNs04KvWrongVTarget.getvTarget()}"

    }

  }

  "A valid NodeModel" should {

    "be able to be enabled and disabled on request" in new FiveLinesWithNodes {

      // init the node, nodes are disabled by default
      val node: NodeModel = super.node0

      node.isInOperation shouldBe false

      node.enable()

      node.isInOperation shouldBe true

      node.disable()

      node.isInOperation shouldBe false

    }

  }
}
