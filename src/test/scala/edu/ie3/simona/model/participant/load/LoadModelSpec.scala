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
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}
import javax.measure.Quantity
import javax.measure.quantity.Power
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.WATT

class LoadModelSpec
    extends UnitSpec
    with LoadInputTestData
    with PrivateMethodTester
    with TableDrivenPropertyChecks {

  implicit val quantityTolerance: Double = 1e-6 // Equals to 1 W power

  "The load model object" should {

    val foreSeenScalingFactor: Double = 1.42

    "build a correct ProfileLoadModel from correct input" in {

      val params = Table(
        ("reference", "sRated"),
        (
          LoadReference.ActivePower(Quantities.getQuantity(268.6, WATT)),
          Quantities.getQuantity(282.7368421052632, PowerSystemUnits.VOLTAMPERE)
        ),
        (
          LoadReference.EnergyConsumption(
            Quantities.getQuantity(3000d, KILOWATTHOUR)
          ),
          Quantities.getQuantity(848.2105263157896, PowerSystemUnits.VOLTAMPERE)
        )
      )

      forAll(params) {
        (foreSeenReference: LoadReference, expsRated: Quantity[Power]) =>
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
                QuantityUtil.isEquivalentAbs(
                  sRated,
                  expsRated,
                  quantityTolerance
                ) shouldBe true
                cosPhiRated shouldBe loadInput.getCosPhiRated
                loadProfile shouldBe loadInput.getStandardLoadProfile
                reference shouldBe foreSeenReference
            }
          }
      }
    }

    "build a correct RandomLoadModel from correct input" in {

      val params = Table(
        ("reference", "sRated"),
        (
          LoadReference.ActivePower(Quantities.getQuantity(268.6, WATT)),
          Quantities.getQuantity(311.0105263157895, PowerSystemUnits.VOLTAMPERE)
        ),
        (
          LoadReference.EnergyConsumption(
            Quantities.getQuantity(3000d, KILOWATTHOUR)
          ),
          Quantities.getQuantity(700.7341868650454, PowerSystemUnits.VOLTAMPERE)
        )
      )

      forAll(params) {
        (foreSeenReference: LoadReference, expsRated: Quantity[Power]) =>
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
                QuantityUtil.isEquivalentAbs(
                  sRated,
                  expsRated,
                  quantityTolerance
                ) shouldBe true
                cosPhiRated shouldBe loadInput.getCosPhiRated
                reference shouldBe foreSeenReference
            }
          }
      }
    }
  }
}
