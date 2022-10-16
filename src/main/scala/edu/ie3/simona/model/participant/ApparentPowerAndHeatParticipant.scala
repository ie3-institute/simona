/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.{Dimensionless, Power}

trait ApparentPowerAndHeatParticipant[
    CD <: CalcRelevantData,
    MS <: ModelState
] {
  this: SystemParticipant[CD, ApparentPowerAndHeat, MS] =>
  override def calculatePower(
      tick: Long,
      voltage: ComparableQuantity[Dimensionless],
      maybeModelState: Option[MS],
      data: CD
  ): ApparentPowerAndHeat = {
    val apparentPower =
      calculateApparentPower(tick, voltage, maybeModelState, data)
    val heat =
      if (isInOperation(tick))
        calculateHeat(tick, maybeModelState, data).to(
          StandardUnits.ACTIVE_POWER_RESULT
        )
      else
        Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)

    ApparentPowerAndHeat(apparentPower.p, apparentPower.q, heat)
  }

  /** Calculate the heat of the asset. As for electrical assets, positive values
    * are understood as consumption and negative as production
    * @param tick
    *   Current instant in simulation time
    * @param maybeModelState
    *   Current state of the model
    * @param data
    *   Needed calculation relevant data
    * @return
    *   Heat production or consumption of the asset
    */
  def calculateHeat(
      tick: Long,
      maybeModelState: Option[MS],
      data: CD
  ): ComparableQuantity[Power]
}
