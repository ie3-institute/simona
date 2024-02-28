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

class TransformerControlGroupModelSpec extends UnitSpec with QuantityMatchers {

  implicit val tolerance: Dimensionless = Each(1e-10)

  "Checking the function of transformer control groups" should {
    val regulationFunction =
      PrivateMethod[RegulationCriterion](Symbol("regulationFunction"))

    val regulationCriterion =
      TransformerControlGroupModel invokePrivate regulationFunction(1.1, 0.9)

    val dut = TransformerControlGroupModel(regulationCriterion)

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

      val actual = dut.determineRegulationNeed(result)

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

      val actual = dut.determineRegulationNeed(result)

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

      val actual = dut.determineRegulationNeed(result)

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

      val actual = dut.determineRegulationNeed(result)

      actual match {
        case Some(regulationNeed) =>
          regulationNeed should approximate(Each(-0.05))
        case None => fail("Did expect to receive a regulation need.")
      }
    }
  }
}
