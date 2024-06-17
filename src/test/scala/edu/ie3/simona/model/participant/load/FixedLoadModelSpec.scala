/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ModelState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference.{ActivePower, EnergyConsumption}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Power
import squants.energy.{KilowattHours, Kilowatts, Watts}
import tech.units.indriya.quantity.Quantities

import java.util.UUID

class FixedLoadModelSpec extends UnitSpec with TableDrivenPropertyChecks {

  private implicit val tolerance: Power = Watts(1d)

  "A fixed load model" should {
    val loadInput =
      new LoadInput(
        UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
        "testLoad",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        new NodeInput(
          UUID.fromString("e5c1cde5-c161-4a4f-997f-fcf31fecbf57"),
          "TestNodeInputModel",
          OperatorInput.NO_OPERATOR_ASSIGNED,
          OperationTime.notLimited(),
          Quantities.getQuantity(1d, PowerSystemUnits.PU),
          false,
          NodeInput.DEFAULT_GEO_POSITION,
          GermanVoltageLevelUtils.LV,
          -1
        ),
        new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
        null,
        BdewStandardLoadProfile.H0,
        false,
        Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR),
        Quantities.getQuantity(282.74d, PowerSystemUnits.VOLTAMPERE),
        0.95
      )

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
        (EnergyConsumption(KilowattHours(3000d)), 342.24)
      )

      forAll(testData) { (reference, expectedReferenceActivePower: Double) =>
        val actual = new FixedLoadModel(
          loadInput.getUuid,
          loadInput.getId,
          foreSeenOperationInterval,
          QControl.apply(loadInput.getqCharacteristics),
          Kilowatts(loadInput.getsRated.to(PowerSystemUnits.KILOWATT).getValue.doubleValue()),
          loadInput.getCosPhiRated,
          reference
        )

        math.abs(actual.activePower.toWatts - expectedReferenceActivePower) should be < tolerance.toWatts
      }
    }

    "return approximately the same power in 10,000 calculations" in {

      val testData = Table(
        ("reference", "expectedPower"),
        (ActivePower(Watts(268.6)), Watts(268.6)),
        (EnergyConsumption(KilowattHours(3000d)), Watts(342.24))
      )

      forAll(testData) { (reference, expectedPower: Power) =>
        val dut = new FixedLoadModel(
          loadInput.getUuid,
          loadInput.getId,
          foreSeenOperationInterval,
          QControl.apply(loadInput.getqCharacteristics),
          Kilowatts(loadInput.getsRated.to(PowerSystemUnits.KILOWATT).getValue.doubleValue()),
          loadInput.getCosPhiRated,
          reference
        )

        for (_ <- 0 until 10000){
          math.abs(dut.calculateActivePower(ModelState.ConstantState, FixedLoadModel.FixedLoadRelevantData).toWatts - expectedPower.toWatts) should be < tolerance.toWatts
        }
      }




      }

  }
}
