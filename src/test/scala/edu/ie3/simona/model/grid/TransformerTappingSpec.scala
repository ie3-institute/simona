/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble

class TransformerTappingSpec extends UnitSpec {

  "A TransformerTapping" should {

    val dummyTransformer = DummyTransformer(
      new TransformerTappingModel(
        1.asPercent,
        0,
        5,
        -5,
        0,
        true,
        ConnectorPort.A,
      )
    )

    "return all possible voltage changes correctly" in {

      val cases = Table(
        ("increase", "decrease", "expectedChanges"),
        (
          0.03.asPu,
          (-0.01).asPu,
          List((-0.01).asPu, 0.asPu, 0.01.asPu, 0.02.asPu, 0.03.asPu),
        ),
        (0.03.asPu, 0.01.asPu, List(0.01.asPu, 0.02.asPu, 0.03.asPu)),
        (
          (-0.01).asPu,
          (-0.03).asPu,
          List((-0.03).asPu, (-0.02).asPu, (-0.01).asPu),
        ),
        (0.01.asPu, 0.02.asPu, List(0.01.asPu)),
        ((-0.02).asPu, (-0.01).asPu, List((-0.01).asPu)),
      )

      forAll(cases) { (increase, decrease, expectedChanges) =>
        val actual =
          dummyTransformer.getPossibleVoltageChanges(increase, decrease)

        actual shouldBe expectedChanges
      }
    }
  }

  private final case class DummyTransformer(
      override protected val transformerTappingModel: TransformerTappingModel
  ) extends TransformerTapping {}

}
