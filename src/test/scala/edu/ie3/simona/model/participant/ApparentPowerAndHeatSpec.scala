/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPowerAndHeat
import edu.ie3.simona.model.participant.ApparentPowerAndHeatSpec.ApparentPowerAndHeatMock
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{
  Kilovoltamperes,
  Megavars,
  ReactivePower,
  Vars,
}
import squants.energy.{Megawatts, Watts}
import squants.{Each, Power}

import java.util.UUID

class ApparentPowerAndHeatSpec extends UnitSpec {
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val reactivePowerTolerance: ReactivePower = Vars(1e-3)
  "Mixing in the trait for apparent power and heat participants" when {
    "requesting a result outside of the operation interval" should {
      "return zero result" in {
        ApparentPowerAndHeatMock.calculatePower(
          50L,
          Each(1.0d),
          ConstantState,
          FixedRelevantData,
        ) match {
          case ComplexPowerAndHeat(p, q, qDot) =>
            p should approximate(Megawatts(0d))
            q should approximate(Megavars(0d))
            qDot should approximate(Megawatts(0d))
        }
      }
    }
    "requesting result within the active interval" should {
      "return the correct values" in {
        ApparentPowerAndHeatMock.calculatePower(
          10L,
          Each(1.0d),
          ConstantState,
          FixedRelevantData,
        ) match {
          case ComplexPowerAndHeat(p, q, qDot) =>
            p should approximate(Megawatts(43d))
            q should approximate(Megavars(0d))
            qDot should approximate(Megawatts(42d))
        }
      }
    }
  }
}

object ApparentPowerAndHeatSpec {
  object ApparentPowerAndHeatMock
      extends SystemParticipant[
        FixedRelevantData.type,
        ComplexPowerAndHeat,
        ConstantState.type,
      ](
        UUID.randomUUID(),
        "ParticipantMock",
        OperationInterval.apply(0L, 42L),
        CosPhiFixed(0.97),
        Kilovoltamperes(42d),
        0.97,
      )
      with ApparentPowerAndHeatParticipant[
        FixedRelevantData.type,
        ConstantState.type,
      ] {
    this.enable()

    override def calculateHeat(
        tick: Long,
        modelState: ConstantState.type,
        data: CalcRelevantData.FixedRelevantData.type,
    ): Power = Megawatts(42d)

    override protected def calculateActivePower(
        modelState: ConstantState.type,
        data: CalcRelevantData.FixedRelevantData.type,
    ): Power = Megawatts(43d)

    override def determineFlexOptions(
        data: CalcRelevantData.FixedRelevantData.type,
        lastState: ModelState.ConstantState.type,
    ): FlexOptions =
      MinMaxFlexOptions.noFlexOption(
        calculateActivePower(ConstantState, data)
      )

    override def handleControlledPowerChange(
        data: CalcRelevantData.FixedRelevantData.type,
        lastState: ModelState.ConstantState.type,
        setPower: Power,
    ): (ModelState.ConstantState.type, FlexChangeIndicator) =
      (lastState, FlexChangeIndicator())
  }
}
