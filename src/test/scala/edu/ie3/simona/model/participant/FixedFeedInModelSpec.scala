/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.RuntimeConfig.FixedFeedInRuntimeConfig
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.simona.test.common.input.FixedFeedInputTestData
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.MEGAVOLTAMPERE
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  Kilovoltamperes,
  Megavoltamperes,
  Voltamperes,
}
import org.scalatest.PrivateMethodTester
import squants.energy.Kilowatts

class FixedFeedInModelSpec
    extends UnitSpec
    with FixedFeedInputTestData
    with DefaultTestData
    with PrivateMethodTester {

  // Equals to 1 VA power
  private implicit val powerTolerance: ApparentPower = Voltamperes(
    1.0
  )

  "The fixed feed in model object" should {

    "build a correct FixedFeedModel from correct input" in {
      val simonaConfig: SimonaConfig =
        createSimonaConfig(
          LoadModelBehaviour.FIX,
          LoadReference.ActivePower(Kilowatts(0.0)),
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
        defaultSimulationEnd,
      )

      inside(actualModel) {
        case FixedFeedInModel(
              uuid,
              id,
              operationInterval,
              qControl,
              sRated,
              cosPhiRated,
            ) =>
          uuid shouldBe fixedFeedInput.getUuid
          id shouldBe fixedFeedInput.getId
          operationInterval shouldBe defaultOperationInterval
          qControl shouldBe QControl(fixedFeedInput.getqCharacteristics)
          sRated should approximate(
            Megavoltamperes(
              fixedFeedInput.getsRated().to(MEGAVOLTAMPERE).getValue.doubleValue
            )
          )
          cosPhiRated shouldBe fixedFeedInput.getCosPhiRated
      }
    }

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
        Kilovoltamperes(
          fixedFeedInput
            .getsRated()
            .to(PowerSystemUnits.KILOVOLTAMPERE)
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
