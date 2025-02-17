/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant2.ParticipantModel.{FixedState}
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.simona.test.common.UnitSpec
import squants.Power
import squants.energy.Watts

class FixedLoadModelSpec extends UnitSpec with LoadInputTestData {

  private implicit val tolerance: Power = Watts(1e-2)

  "A fixed load model" should {

    "return the desired power in 1,000 calculations" in {

      val cases = Table(
        ("reference", "expectedPower"),
        ("power", Watts(268.6)),
        ("energy", Watts(342.47)),
      )

      forAll(cases) { (reference, expectedPower) =>
        val config = LoadRuntimeConfig(
          modelBehaviour = "fixed",
          reference = reference,
        )

        val model = FixedLoadModel(
          loadInput,
          config,
        )

        (0 until 1000).foreach { tick =>
          val (operatingPoint, nextTick) = model.determineOperatingPoint(
            FixedState(tick)
          )

          operatingPoint.activePower should approximate(expectedPower)
          operatingPoint.reactivePower shouldBe None
          nextTick shouldBe None
        }

      }
    }

  }
}
