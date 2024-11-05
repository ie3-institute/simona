/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ModelState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption,
}
import edu.ie3.simona.test.common.input.LoadInputTestData
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.Kilovoltamperes
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Power
import squants.energy.{KilowattHours, Watts}

class FixedLoadModelSpec
    extends UnitSpec
    with LoadInputTestData
    with DefaultTestData
    with TableDrivenPropertyChecks {

  private implicit val tolerance: Power = Watts(1d)

  "A fixed load model" should {

    val defaultOperationInterval =
      SystemComponent.determineOperationInterval(
        defaultSimulationStart,
        defaultSimulationEnd,
        loadInput.getOperationTime,
      )

    "be instantiated from valid input correctly" in {
      val testData = Table(
        ("reference", "expectedReferenceActivePower"),
        (ActivePower(Watts(268.6)), Watts(268.6)),
        (EnergyConsumption(KilowattHours(3000d)), Watts(342.24)),
      )

      forAll(testData) { (reference, expectedReferenceActivePower: Power) =>
        val actual = new FixedLoadModel(
          loadInput.getUuid,
          loadInput.getId,
          defaultOperationInterval,
          QControl.apply(loadInput.getqCharacteristics),
          Kilovoltamperes(
            loadInput.getsRated
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue()
          ),
          loadInput.getCosPhiRated,
          reference,
        )

        val calculatedPower = actual
          .calculateActivePower(
            ModelState.ConstantState,
            FixedLoadModel.FixedLoadRelevantData,
          )

        calculatedPower should approximate(expectedReferenceActivePower)
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
          defaultOperationInterval,
          QControl.apply(loadInput.getqCharacteristics),
          Kilovoltamperes(
            loadInput.getsRated
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue()
          ),
          loadInput.getCosPhiRated,
          reference,
        )

        (1 to 10000).foreach { _ =>
          val calculatedPower = dut
            .calculateActivePower(
              ModelState.ConstantState,
              FixedLoadModel.FixedLoadRelevantData,
            )
          calculatedPower should approximate(expectedPower)
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

        val scales: LazyList[Double] =
          LazyList.iterate(0.0)(_ + 0.1).takeWhile(_ <= 2.0)

        scales.foreach { scale =>
          val scaledSRated = Kilovoltamperes(
            loadInput.getsRated
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue() * scale
          )
          val dut = new FixedLoadModel(
            loadInput.getUuid,
            loadInput.getId,
            defaultOperationInterval,
            QControl.apply(loadInput.getqCharacteristics),
            scaledSRated,
            loadInput.getCosPhiRated,
            reference,
          )

          val calculatedPower = dut
            .calculateActivePower(
              ModelState.ConstantState,
              relevantData,
            ) * scale
          val expectedScaledPower = expectedPower * scale

          calculatedPower should approximate(expectedScaledPower)
        }
      }
    }
  }
}
