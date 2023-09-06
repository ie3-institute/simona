/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LoadInputTestData
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Watts}

class LoadModelSpec
    extends UnitSpec
    with LoadInputTestData
    with PrivateMethodTester
    with TableDrivenPropertyChecks {

  private implicit val powerTolerance: squants.Power = Watts(1)

  "The load model object" should {

    val foreSeenScalingFactor: Double = 1.42

    "build a correct ProfileLoadModel from correct input" in {

      val params = Table(
        ("reference", "sRated"),
        (
          LoadReference.ActivePower(Watts(268.6)),
          Watts(282.7368421052632)
        ),
        (
          LoadReference.EnergyConsumption(KilowattHours(3000.0)),
          Watts(848.2105263157896)
        )
      )

      forAll(params) {
        (foreSeenReference: LoadReference, expsRated: squants.Power) =>
          {
            val actual = ProfileLoadModel(
              loadInput,
              defaultOperationInterval,
              foreSeenScalingFactor,
              foreSeenReference
            )
            inside(actual) {
              case ProfileLoadModel(
                    uuid,
                    id,
                    operationInterval,
                    scalingFactor,
                    qControl,
                    sRated,
                    cosPhiRated,
                    loadProfile,
                    reference
                  ) =>
                uuid shouldBe loadInput.getUuid
                id shouldBe loadInput.getId
                operationInterval shouldBe defaultOperationInterval
                scalingFactor shouldBe foreSeenScalingFactor
                qControl shouldBe QControl(loadInput.getqCharacteristics)
                (sRated ~= expsRated) shouldBe true
                cosPhiRated shouldBe loadInput.getCosPhiRated
                loadProfile shouldBe loadInput.getLoadProfile
                reference shouldBe foreSeenReference
            }
          }
      }
    }

    "build a correct RandomLoadModel from correct input" in {

      val params = Table(
        ("reference", "sRated"),
        (
          LoadReference.ActivePower(Watts(268.6)),
          Watts(311.0105263157895)
        ),
        (
          LoadReference.EnergyConsumption(KilowattHours(3000.0)),
          Watts(700.7341868650454)
        )
      )

      forAll(params) {
        (foreSeenReference: LoadReference, expsRated: squants.Power) =>
          {
            val actual = RandomLoadModel(
              loadInput,
              defaultOperationInterval,
              foreSeenScalingFactor,
              foreSeenReference
            )
            inside(actual) {
              case RandomLoadModel(
                    uuid,
                    id,
                    operationInterval,
                    scalingFactor,
                    qControl,
                    sRated,
                    cosPhiRated,
                    reference
                  ) =>
                uuid shouldBe loadInput.getUuid
                id shouldBe loadInput.getId
                operationInterval shouldBe defaultOperationInterval
                scalingFactor shouldBe foreSeenScalingFactor
                qControl shouldBe QControl(loadInput.getqCharacteristics)
                (sRated ~= expsRated) shouldBe true
                cosPhiRated shouldBe loadInput.getCosPhiRated
                reference shouldBe foreSeenReference
            }
          }
      }
    }
  }
}
