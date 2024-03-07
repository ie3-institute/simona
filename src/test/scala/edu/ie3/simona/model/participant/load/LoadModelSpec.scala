/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LoadInputTestData
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Power, Watts}

class LoadModelSpec
    extends UnitSpec
    with LoadInputTestData
    with PrivateMethodTester
    with TableDrivenPropertyChecks {

  private implicit val powerTolerance: Power = Watts(1e-3)

  "The load model object" should {

    "build a correct ProfileLoadModel from correct input" in {

      val params = Table(
        ("reference", "scaling", "sRated"),
        (
          LoadReference.ActivePower(Watts(268.6)),
          1d,
          Watts(282.7368),
        ),
        (
          LoadReference.EnergyConsumption(KilowattHours(3000.0)),
          1d,
          Watts(848.2105),
        ),
        (
          LoadReference.ActivePower(Watts(268.6)),
          1.5d,
          Watts(424.1053),
        ),
        (
          LoadReference.EnergyConsumption(KilowattHours(3000.0)),
          1.5d,
          Watts(1272.3158),
        ),
      )

      forAll(params) {
        (reference: LoadReference, scaling: Double, expectedSRated: Power) =>
          {
            val actual = ProfileLoadModel(
              loadInput,
              defaultOperationInterval,
              scaling,
              reference,
            )
            inside(actual) {
              case ProfileLoadModel(
                    uuid,
                    id,
                    operationInterval,
                    qControl,
                    sRated,
                    cosPhiRated,
                    loadProfile,
                    actualReference,
                  ) =>
                uuid shouldBe loadInput.getUuid
                id shouldBe loadInput.getId
                operationInterval shouldBe defaultOperationInterval
                qControl shouldBe QControl(loadInput.getqCharacteristics)
                sRated should approximate(expectedSRated)
                cosPhiRated shouldBe loadInput.getCosPhiRated
                loadProfile shouldBe loadInput.getLoadProfile
                actualReference shouldBe reference.scale(scaling)
            }
          }
      }
    }

    "build a correct RandomLoadModel from correct input" in {

      val params = Table(
        ("reference", "scaling", "sRated"),
        (
          LoadReference.ActivePower(Watts(268.6)),
          1d,
          Watts(311.0105),
        ),
        (
          LoadReference.EnergyConsumption(KilowattHours(3000.0)),
          1d,
          Watts(770.8076),
        ),
        (
          LoadReference.ActivePower(Watts(268.6)),
          1.5d,
          Watts(466.5158),
        ),
        (
          LoadReference.EnergyConsumption(KilowattHours(3000.0)),
          1.5d,
          Watts(1156.2114),
        ),
      )

      forAll(params) {
        (reference: LoadReference, scaling: Double, expectedSRated: Power) =>
          {
            val actual = RandomLoadModel(
              loadInput,
              defaultOperationInterval,
              scaling,
              reference,
            )
            inside(actual) {
              case RandomLoadModel(
                    uuid,
                    id,
                    operationInterval,
                    qControl,
                    sRated,
                    cosPhiRated,
                    actualReference,
                  ) =>
                uuid shouldBe loadInput.getUuid
                id shouldBe loadInput.getId
                operationInterval shouldBe defaultOperationInterval
                qControl shouldBe QControl(loadInput.getqCharacteristics)
                sRated should approximate(expectedSRated)
                cosPhiRated shouldBe loadInput.getCosPhiRated
                actualReference shouldBe reference.scale(scaling)
            }
          }
      }
    }
  }
}
