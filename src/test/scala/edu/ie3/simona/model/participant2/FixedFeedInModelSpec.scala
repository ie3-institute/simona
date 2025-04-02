/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.simona.config.RuntimeConfig.FixedFeedInRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantModel.FixedState
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.FixedFeedInputTestData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.MEGAVOLTAMPERE
import edu.ie3.util.scala.quantities.{Kilovoltamperes, Megavoltamperes}

class FixedFeedInModelSpec extends UnitSpec with FixedFeedInputTestData {

  "The fixed feed in model" should {

    "build a correct FixedFeedModel from valid input" in {

      val model = FixedFeedInModel.Factory(fixedFeedInput).create()

      model.uuid shouldBe fixedFeedInput.getUuid
      model.sRated shouldBe Megavoltamperes(
        fixedFeedInput.getsRated().to(MEGAVOLTAMPERE).getValue.doubleValue
      )
      model.cosPhiRated shouldBe fixedFeedInput.getCosPhiRated
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
      operatingPoint.activePower shouldBe expectedPower
      nextTick shouldBe None

    }

  }

}
