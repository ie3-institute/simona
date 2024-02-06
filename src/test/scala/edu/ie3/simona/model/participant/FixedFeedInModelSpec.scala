/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.FixedFeedInRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.test.common.input.FixedFeedInputTestData
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.quantities.PowerSystemUnits.MEGAVOLTAMPERE
import org.scalatest.PrivateMethodTester
import squants.energy.{Kilowatts, Megawatts, Watts}

class FixedFeedInModelSpec
    extends UnitSpec
    with FixedFeedInputTestData
    with DefaultTestData
    with PrivateMethodTester {

  private implicit val powerTolerance: squants.Power = Watts(
    1.0
  ) // Equals to 1 W power

  "The fixed feed in model object" should {

    val foreSeenScalingFactor: Double = 1.0

    "build a correct FixedFeedModel from correct input" in {
      val simonaConfig: SimonaConfig =
        createSimonaConfig(
          LoadModelBehaviour.FIX,
          LoadReference.ActivePower(Kilowatts(0.0))
        )
      val modelConfig = ConfigUtil
        .ParticipantConfigUtil(
          simonaConfig.simona.runtime.participant
        )
        .getOrDefault[FixedFeedInRuntimeConfig](fixedFeedInput.getUuid)

      val actualModel = FixedFeedInModel.apply(
        fixedFeedInput,
        modelConfig,
        defaultSimulationStart,
        defaultSimulationEnd
      )

      inside(actualModel) {
        case FixedFeedInModel(
              uuid,
              id,
              operationInterval,
              scalingFactor,
              qControl,
              sRated,
              cosPhiRated
            ) =>
          uuid shouldBe fixedFeedInput.getUuid
          id shouldBe fixedFeedInput.getId
          operationInterval shouldBe defaultOperationInterval
          scalingFactor shouldBe foreSeenScalingFactor
          qControl shouldBe QControl(fixedFeedInput.getqCharacteristics)
          sRated should approximate(
            Megawatts(
              fixedFeedInput.getsRated().to(MEGAVOLTAMPERE).getValue.doubleValue
            )
          )
          cosPhiRated shouldBe fixedFeedInput.getCosPhiRated
      }
    }
  }
}
