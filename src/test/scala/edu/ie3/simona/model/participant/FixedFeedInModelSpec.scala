/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.model.participant.ParticipantModel.FixedState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.FixedFeedInputTestData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.MEGAVOLTAMPERE
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  Kilovoltamperes,
  Megavoltamperes,
}
import squants.Power
import squants.energy.Kilowatts

class FixedFeedInModelSpec extends UnitSpec with FixedFeedInputTestData {

  protected given powerTolerance: Power = Kilowatts(1e-9)
  protected given apparentPowerTolerance: ApparentPower = Megavoltamperes(1e-9)
  protected given doubleTolerance: Double = 1e-9

  "The fixed feed in model" should {

    "build a correct FixedFeedModel from valid input" in {

      val model = FixedFeedInModel.Factory(fixedFeedInput).create()

      model.uuid shouldBe fixedFeedInput.getUuid
      model.sRated should approximate(
        Megavoltamperes(
          fixedFeedInput.getsRated().to(MEGAVOLTAMPERE).getValue.doubleValue
        )
      )
      model.cosPhiRated should approximate(fixedFeedInput.getCosPhiRated)
      model.qControl shouldBe QControl(fixedFeedInput.getqCharacteristics)

    }

    "return approximately correct power calculations" in {

      val model = FixedFeedInModel.Factory(fixedFeedInput).create()

      val expectedPower = Kilovoltamperes(
        fixedFeedInput
          .getsRated()
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue * -1
      ).toActivePower(fixedFeedInput.getCosPhiRated)

      val (operatingPoint, nextTick) =
        model.determineOperatingPoint(FixedState(0))
      operatingPoint.activePower should approximate(expectedPower)
      nextTick shouldBe None

    }

  }

}
