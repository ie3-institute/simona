/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import squants.energy.Megawatts
import squants.{Dimensionless, Power}

trait ApparentPowerAndHeatParticipant[CD <: CalcRelevantData] {
  this: SystemParticipant[CD, ApparentPowerAndHeat] =>
  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      data: CD
  ): ApparentPowerAndHeat = {
    val apparentPower = calculateApparentPower(tick, voltage, data)
    val heat =
      if (isInOperation(tick))
        calculateHeat(tick, data) * scalingFactor
      else
        Megawatts(0d)

    ApparentPowerAndHeat(apparentPower.p, apparentPower.q, heat)
  }

  /** Calculate the heat of the asset. As for electrical assets, positive values
    * are understood as consumption and negative as production
    * @param tick
    *   Current instant in simulation time
    * @param data
    *   Needed calculation relevant data
    * @return
    *   Heat production or consumption of the asset
    */
  def calculateHeat(tick: Long, data: CD): Power
}
