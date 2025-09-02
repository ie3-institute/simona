/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.control

import breeze.linalg.DenseMatrix
import breeze.math.Complex
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.powerflow.model.enums.NodeType
import edu.ie3.simona.model.control.TransformerControlGroupModel.RegulationCriterion
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.matchers.QuantityMatchers
import squants.{Dimensionless, Each}

import java.util.UUID

class TransformerControlGroupModelSpec extends UnitSpec with QuantityMatchers {

  implicit val tolerance: Dimensionless = Each(1e-10)

  "Checking the function of transformer control groups" should {
    val regulationFunction =
      PrivateMethod[RegulationCriterion](Symbol("regulationFunction"))

    val regulationCriterion =
      TransformerControlGroupModel invokePrivate regulationFunction(1.1, 0.9)

    val dut = TransformerControlGroupModel(
      Set(
        UUID.fromString(
          "d4d650be-87b7-4cf6-be7f-03f0bbcde3e3"
        ),
        UUID.fromString(
          "08b8d2ca-993d-45cd-9456-f009ecb47bc0"
        ),
        UUID.fromString(
          "324f49e5-1c35-4c49-afb1-3cf41696bf93"
        ),
      ),
      regulationCriterion,
    )

    val uuidToIndex = Map(
      UUID.fromString(
        "d4d650be-87b7-4cf6-be7f-03f0bbcde3e3"
      ) -> 0,
      UUID.fromString(
        "08b8d2ca-993d-45cd-9456-f009ecb47bc0"
      ) -> 1,
      UUID.fromString(
        "324f49e5-1c35-4c49-afb1-3cf41696bf93"
      ) -> 2,
    )

    "return no regulation need, if everything is fine" in {
      val result = ValidNewtonRaphsonPFResult(
        0,
        Array(
          StateData(0, NodeType.SL, Complex.one, Complex.zero),
          StateData(1, NodeType.PQ, Complex.one, Complex.zero),
          StateData(2, NodeType.PQ, Complex.one, Complex.zero),
        ),
        DenseMatrix.zeros(1, 1),
      )

      val actual = dut.determineRegulationNeed(result, uuidToIndex)

      actual shouldBe None
    }

    "return no regulation need, if requests are contradictory" in {
      val result = ValidNewtonRaphsonPFResult(
        0,
        Array(
          StateData(0, NodeType.SL, Complex.one, Complex.zero),
          StateData(1, NodeType.PQ, Complex.one * 0.88, Complex.zero),
          StateData(2, NodeType.PQ, Complex.one * 1.11, Complex.zero),
        ),
        DenseMatrix.zeros(1, 1),
      )

      val actual = dut.determineRegulationNeed(result, uuidToIndex)

      actual shouldBe None
    }

    "return the biggest positive regulation need" in {
      val result = ValidNewtonRaphsonPFResult(
        0,
        Array(
          StateData(0, NodeType.SL, Complex.one, Complex.zero),
          StateData(1, NodeType.PQ, Complex.one * 0.85, Complex.zero),
          StateData(2, NodeType.PQ, Complex.one * 0.88, Complex.zero),
        ),
        DenseMatrix.zeros(1, 1),
      )

      val actual = dut.determineRegulationNeed(result, uuidToIndex)

      actual match {
        case Some(regulationNeed) =>
          regulationNeed should approximate(Each(0.05))
        case None => fail("Did expect to receive a regulation need.")
      }
    }

    "return the biggest negative regulation need" in {
      val result = ValidNewtonRaphsonPFResult(
        0,
        Array(
          StateData(0, NodeType.SL, Complex.one, Complex.zero),
          StateData(1, NodeType.PQ, Complex.one * 1.15, Complex.zero),
          StateData(2, NodeType.PQ, Complex.one * 1.11, Complex.zero),
        ),
        DenseMatrix.zeros(1, 1),
      )

      val actual = dut.determineRegulationNeed(result, uuidToIndex)

      actual match {
        case Some(regulationNeed) =>
          regulationNeed should approximate(Each(-0.05))
        case None => fail("Did expect to receive a regulation need.")
      }
    }
  }
}
