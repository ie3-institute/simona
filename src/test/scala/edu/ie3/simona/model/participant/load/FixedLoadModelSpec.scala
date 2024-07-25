/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ModelState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Power
import squants.energy.{KilowattHours, Kilowatts, Watts}

class FixedLoadModelSpec
    extends UnitSpec
    with LoadInputTestData
    with TableDrivenPropertyChecks {

  private implicit val tolerance: Power = Watts(1d)

  "A fixed load model" should {

    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-12-31T23:59:00Z")
    val foreSeenOperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        loadInput.getOperationTime,
      )

    "be instantiated from valid input correctly" in {
      val testData = Table(
        ("reference", "expectedReferenceActivePower"),
        (ActivePower(Watts(268.6)), 268.6),
        (EnergyConsumption(KilowattHours(3000d)), 342.24),
      )

      forAll(testData) { (reference, expectedReferenceActivePower: Double) =>
        val actual = new FixedLoadModel(
          loadInput.getUuid,
          loadInput.getId,
          foreSeenOperationInterval,
          QControl.apply(loadInput.getqCharacteristics),
          Kilowatts(
            loadInput.getsRated
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue()
          ),
          loadInput.getCosPhiRated,
          reference,
        )

          actual shouldBe expectedReferenceActivePower
      }
    }

    "return approximately the same power in 10,000 calculations" in {

      val testData = Table(
        ("reference", "expectedPower"),
        (ActivePower(Watts(268.6)), Watts(268.6)),
        (EnergyConsumption(KilowattHours(3000d)), Watts(342.24)),
      )

      forAll(testData) { (reference, expectedPower: Power) =>
        val dut = new FixedLoadModel(
          loadInput.getUuid,
          loadInput.getId,
          foreSeenOperationInterval,
          QControl.apply(loadInput.getqCharacteristics),
          Kilowatts(
            loadInput.getsRated
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue()
          ),
          loadInput.getCosPhiRated,
          reference,
        )

        for (_ <- 0 until 10000) {
          math.abs(
            dut
              .calculateActivePower(
                ModelState.ConstantState,
                FixedLoadModel.FixedLoadRelevantData,
              )
              .toWatts - expectedPower.toWatts
          ) should be < tolerance.toWatts
        }
      }
    }

    "consider the (global) scaling factor correctly" in {
      val testData = Table(
        ("reference", "expectedPower"),
        (ActivePower(Watts(268.6d)), Watts(268.6)),
        (EnergyConsumption(KilowattHours(3000d)), Watts(342.24)),
      )

      forAll(testData) { (reference, expectedPower: Power) =>
        val relevantData = FixedLoadModel.FixedLoadRelevantData

        var scale = 0.1
        while (scale <= 2) {
          val scaledSRated = Kilowatts(
            loadInput.getsRated
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue() * scale
          )
          val dut = new FixedLoadModel(
            loadInput.getUuid,
            loadInput.getId,
            foreSeenOperationInterval,
            QControl.apply(loadInput.getqCharacteristics),
            scaledSRated,
            loadInput.getCosPhiRated,
            reference,
          )

          val calculatedPower = dut
            .calculateActivePower(ModelState.ConstantState, relevantData)
            .toWatts * scale
          val expectedScaledPower = expectedPower.toWatts * scale

          calculatedPower should be(expectedScaledPower +- tolerance.toWatts)

          scale += 0.1
        }
      }
    }
  }
}
