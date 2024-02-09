/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.model.participant.ApparentPowerAndHeatSpec.ApparentPowerAndHeatMock
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower, Vars}
import squants.energy.{Kilowatts, Megawatts, Watts}
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
          case ApparentPowerAndHeat(p, q, qDot) =>
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
          case ApparentPowerAndHeat(p, q, qDot) =>
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
        ApparentPowerAndHeat,
        ConstantState.type,
      ](
        UUID.randomUUID(),
        "ParticipantMock",
        OperationInterval.apply(0L, 42L),
        1.0,
        CosPhiFixed(0.97),
        Kilowatts(42d),
        0.97,
      )
      with ApparentPowerAndHeatParticipant[
        FixedRelevantData.type,
        ConstantState.type,
      ] {
    this.enable()

    /** Calculate the heat of the asset. As for electrical assets, positive
      * values are understood as consumption and negative as production
      *
      * @param tick
      *   Current instant in simulation time
      * @param data
      *   Needed calculation relevant data
      * @return
      *   Heat production or consumption of the asset
      */
    override def calculateHeat(
        tick: Long,
        modelState: ConstantState.type,
        data: CalcRelevantData.FixedRelevantData.type,
    ): Power = Megawatts(42d)

    /** Calculate the active power behaviour of the model
      *
      * @param data
      *   Further needed, secondary data
      * @return
      *   Active power
      */
    override protected def calculateActivePower(
        modelState: ConstantState.type,
        data: CalcRelevantData.FixedRelevantData.type,
    ): Power = Megawatts(43d)

    /** @param data
      * @param lastState
      * @return
      *   flex options
      */
    override def determineFlexOptions(
        data: CalcRelevantData.FixedRelevantData.type,
        lastState: ModelState.ConstantState.type,
    ): FlexibilityMessage.ProvideFlexOptions =
      ProvideMinMaxFlexOptions.noFlexOption(
        this.getUuid,
        calculateActivePower(ConstantState, data),
      )

    /** @param data
      * @param lastState
      * @param setPower
      *   power that has been set by EmAgent
      * @return
      *   updated relevant data and an indication at which circumstances flex
      *   options will change next
      */
    override def handleControlledPowerChange(
        data: CalcRelevantData.FixedRelevantData.type,
        lastState: ModelState.ConstantState.type,
        setPower: Power,
    ): (ModelState.ConstantState.type, FlexChangeIndicator) =
      (lastState, FlexChangeIndicator())
  }
}
