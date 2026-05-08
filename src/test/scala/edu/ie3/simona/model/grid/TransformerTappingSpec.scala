/*
 * © 2024-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.simona.test.common.UnitSpec
import squants.{Dimensionless, Each, Percent}

class TransformerTappingSpec extends UnitSpec {

  private given Conversion[Double, Dimensionless] = (d: Double) => Each(d)

  "A TransformerTapping" should {

    val dummyTransformer = DummyTransformer(
      new TransformerTappingModel(
        Percent(1),
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
        (0.03, -0.01, List(-0.01, 0, 0.01, 0.02, 0.03)),
        (0.03, 0.01, List(0.01, 0.02, 0.03)),
        (-0.01, -0.03, List(-0.03, -0.02, -0.01)),
        (0.01, 0.02, List(0.01)),
        (-0.02, -0.01, List(-0.01)),
      )

      forAll(cases) { (increase, decrease, expectedChanges) =>
        val actual =
          dummyTransformer
            .getPossibleVoltageChanges(increase, decrease)
            .map(_.toEach)

        actual shouldBe expectedChanges
      }
    }
  }

  private final case class DummyTransformer(
      override protected val transformerTappingModel: TransformerTappingModel
  ) extends TransformerTapping {}

}
