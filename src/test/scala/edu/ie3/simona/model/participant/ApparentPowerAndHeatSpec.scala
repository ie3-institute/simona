/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.model.participant.ApparentPowerAndHeatSpec.ApparentPowerAndHeatMock
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.ontology.messages.FlexibilityMessage
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import javax.measure.quantity.Power

class ApparentPowerAndHeatSpec extends UnitSpec {
  "Mixing in the trait for apparent power and heat participants" when {
    "requesting a result outside of the operation interval" should {
      "return zero result" in {
        ApparentPowerAndHeatMock.calculatePower(
          50L,
          Quantities.getQuantity(1.0, StandardUnits.VOLTAGE_MAGNITUDE),
          Some(ConstantState),
          FixedRelevantData
        ) match {
          case ApparentPowerAndHeat(p, q, qDot) =>
            p should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
            )
            q should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT)
            )
            qDot should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
            )
        }
      }
    }
    "requesting result within the active interval" should {
      "return the correct values" in {
        ApparentPowerAndHeatMock.calculatePower(
          10L,
          Quantities.getQuantity(1.0, StandardUnits.VOLTAGE_MAGNITUDE),
          Some(ConstantState),
          FixedRelevantData
        ) match {
          case ApparentPowerAndHeat(p, q, qDot) =>
            p should equalWithTolerance(
              Quantities.getQuantity(43d, StandardUnits.ACTIVE_POWER_RESULT)
            )
            q should equalWithTolerance(
              Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT)
            )
            qDot should equalWithTolerance(
              Quantities.getQuantity(42d, StandardUnits.ACTIVE_POWER_RESULT)
            )
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
        ConstantState.type
      ](
        UUID.randomUUID(),
        "ParticipantMock",
        OperationInterval.apply(0L, 42L),
        1.0,
        CosPhiFixed(0.97),
        Quantities.getQuantity(42d, StandardUnits.ACTIVE_POWER_IN),
        0.97
      )
      with ApparentPowerAndHeatParticipant[
        FixedRelevantData.type,
        ConstantState.type
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
        maybeModelState: Option[ConstantState.type],
        data: CalcRelevantData.FixedRelevantData.type
    ): ComparableQuantity[Power] =
      Quantities.getQuantity(42d, StandardUnits.ACTIVE_POWER_RESULT)

    /** Calculate the active power behaviour of the model
      *
      * @param data
      *   Further needed, secondary data
      * @return
      *   Active power
      */
    override protected def calculateActivePower(
        maybeModelState: Option[ConstantState.type],
        data: CalcRelevantData.FixedRelevantData.type
    ): ComparableQuantity[Power] =
      Quantities.getQuantity(43d, StandardUnits.ACTIVE_POWER_RESULT)

    /** @param data
      * @param lastState
      * @return
      *   flex options
      */
    override def determineFlexOptions(
        data: CalcRelevantData.FixedRelevantData.type,
        lastState: ModelState.ConstantState.type
    ): FlexibilityMessage.ProvideFlexOptions =
      ProvideFlexOptions.noFlexOption(
        this.getUuid,
        calculateActivePower(Some(ConstantState), data)
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
        setPower: ComparableQuantity[Power]
    ): (ModelState.ConstantState.type, FlexChangeIndicator) =
      (lastState, FlexChangeIndicator())
  }
}
