/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import squants.Dimensionless

trait ApparentPowerParticipant[CD <: CalcRelevantData] {
  this: SystemParticipant[CD, ApparentPower] =>
  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      data: CD
  ): ApparentPower = calculateApparentPower(tick, voltage, data)
}
