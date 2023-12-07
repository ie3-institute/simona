/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.model.participant.ApparentPowerAndHeatSpec.ApparentPowerAndHeatMock
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower, Vars}
import squants.Each
import squants.energy.{Kilowatts, Megawatts, Power, Watts}

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
          FixedRelevantData
        ) match {
          case ApparentPowerAndHeat(p, q, qDot) =>
            (p =~ Megawatts(0d)) shouldBe true
            (q =~ Megavars(0d)) shouldBe true
            (qDot =~ Megawatts(0d)) shouldBe true

        }
      }
    }
    "requesting result within the active interval" should {
      "return the correct values" in {
        ApparentPowerAndHeatMock.calculatePower(
          10L,
          Each(1.0d),
          FixedRelevantData
        ) match {
          case ApparentPowerAndHeat(p, q, qDot) =>
            (p =~ Megawatts(43d)) shouldBe true
            (q =~ Megavars(0d)) shouldBe true
            (qDot =~ Megawatts(42d)) shouldBe true

        }
      }
    }
  }
}

object ApparentPowerAndHeatSpec {
  object ApparentPowerAndHeatMock
      extends SystemParticipant[FixedRelevantData.type, ApparentPowerAndHeat](
        UUID.randomUUID(),
        "ParticipantMock",
        OperationInterval.apply(0L, 42L),
        1.0,
        CosPhiFixed(0.97),
        Kilowatts(42d),
        0.97
      )
      with ApparentPowerAndHeatParticipant[FixedRelevantData.type] {
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
        data: CalcRelevantData.FixedRelevantData.type
    ): Power =
      Megawatts(42d)

    /** Calculate the active power behaviour of the model
      *
      * @param data
      *   Further needed, secondary data
      * @return
      *   Active power
      */
    override protected def calculateActivePower(
        data: CalcRelevantData.FixedRelevantData.type
    ): Power =
      Megawatts(43d)
  }
}
