/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.input.system.FixedFeedInInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.Sq
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{Kilowatts, Power, Watts}
import tech.units.indriya.quantity.Quantities

import java.util.UUID

class FixedFeedModelSpec extends UnitSpec with TableDrivenPropertyChecks {
  implicit val tolerance: Power = Watts(1d)
  "Having a fixed feed model" when {
    val fixedFeedInput = new FixedFeedInInput(
      UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
      "testFixedFeed",
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
        -1,
      ),
      new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
      null,
      Quantities.getQuantity(282.74d, PowerSystemUnits.VOLTAMPERE),
      0.95,
    )

    def simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

    def simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2020-12-31T23:59:00Z")

    def foreSeenOperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        fixedFeedInput.getOperationTime,
      )

    val expectedPower = Sq.create(
      fixedFeedInput
        .getsRated()
        .to(PowerSystemUnits.KILOWATT)
        .getValue
        .doubleValue() * -1 * fixedFeedInput.getCosPhiRated * 1.0,
      Kilowatts,
    )

    "The fixed feed model" should {
      "return approximately correct power calculations" in {
        val actualModel = new FixedFeedInModel(
          fixedFeedInput.getUuid,
          fixedFeedInput.getId,
          foreSeenOperationInterval,
          QControl.apply(fixedFeedInput.getqCharacteristics()),
          Sq.create(
            fixedFeedInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue(),
            Kilowatts,
          ),
          fixedFeedInput.getCosPhiRated,
        )

        actualModel.calculateActivePower(
          ModelState.ConstantState,
          CalcRelevantData.FixedRelevantData,
        ) =~ expectedPower
      }
    }

  }
}
