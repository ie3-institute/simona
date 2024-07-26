/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.FixedFeedInputTestData
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{Kilowatts, Power, Watts}

class FixedFeedModelSpec
    extends UnitSpec
    with FixedFeedInputTestData
    with TableDrivenPropertyChecks {
  implicit val tolerance: Power = Watts(1d)
  "Having a fixed feed model" when {

    "The fixed feed model" should {
      "return approximately correct power calculations" in {
        val expectedPower = Kilowatts(
          fixedFeedInput
            .getsRated()
            .to(PowerSystemUnits.KILOWATT)
            .getValue
            .doubleValue() * -1 * fixedFeedInput.getCosPhiRated
        )

        val actualModel = new FixedFeedInModel(
          fixedFeedInput.getUuid,
          fixedFeedInput.getId,
          defaultOperationInterval,
          QControl.apply(fixedFeedInput.getqCharacteristics()),
          Kilowatts(
            fixedFeedInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue()
          ),
          fixedFeedInput.getCosPhiRated,
        )

        actualModel.calculateActivePower(
          ModelState.ConstantState,
          CalcRelevantData.FixedRelevantData,
        ) shouldBe expectedPower
      }
    }

  }
}
