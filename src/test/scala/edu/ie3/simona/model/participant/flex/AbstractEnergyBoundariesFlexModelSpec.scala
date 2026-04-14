/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.flex

import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  OperationChangeIndicator,
}
import edu.ie3.simona.model.participant.flex.AbstractEnergyBoundariesFlexModelSpec.TestEnergyBoundariesFlexModel
import edu.ie3.simona.model.participant.storage.StorageModel.StorageState
import edu.ie3.simona.ontology.messages.flex.FlexOptions
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.Hours

class AbstractEnergyBoundariesFlexModelSpec extends UnitSpec {

  val flexModel = TestEnergyBoundariesFlexModel(hasEnergyFlexibility = true)
  val noFlexModel = TestEnergyBoundariesFlexModel(hasEnergyFlexibility = false)

  "Flex models for EnergyBoundariesFlexOptions" should {

    "determine the next tick correctly" in {

      val cases = Table(
        ("currentTick", "expNextTick"),
        (0L, 3600L),
        (900L, 3600L),
        (3599L, 3600L),
        (3600L, 7200L),
        (7200L, 10800L),
      )

      forAll(cases) { (currentTick, expNextTick) =>
        Seq(flexModel, noFlexModel).foreach { model =>
          model.determineNextActivation(
            state = StorageState(KilowattHours(1), currentTick),
            operatingPoint = ActivePowerOperatingPoint(Kilowatts(1)),
            setPower = Kilowatts(1),
            dataTimeType = DataTimeType.CurrentAndForecast(Hours(12), Hours(1)),
          ) shouldEqual OperationChangeIndicator(
            changesAtNextActivation = model.hasEnergyFlexibility,
            changesAtTick = Some(expNextTick),
          )
        }
      }

    }

    "not activate at next tick if operating power is zero" in {

      Seq(flexModel, noFlexModel).foreach {
        _.determineNextActivation(
          state = StorageState(KilowattHours(1), 0L),
          operatingPoint = ActivePowerOperatingPoint(zeroKW),
          setPower = zeroKW,
          dataTimeType = DataTimeType.CurrentAndForecast(Hours(12), Hours(1)),
        ) shouldEqual OperationChangeIndicator(
          changesAtTick = Some(3600L)
        )
      }

    }

  }

}

object AbstractEnergyBoundariesFlexModelSpec {

  class TestEnergyBoundariesFlexModel(
      override val hasEnergyFlexibility: Boolean
  ) extends AbstractEnergyBoundariesFlexModel[StorageState] {

    override def determineFlexOptions(
        state: StorageState,
        dataTimeType: DataTimeType,
    ): FlexOptions =
      throw new NotImplementedError()

  }

}
