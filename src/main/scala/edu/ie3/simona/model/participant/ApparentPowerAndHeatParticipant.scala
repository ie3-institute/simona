/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.service.Data.PrimaryData.ComplexPowerAndHeat
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.{Dimensionless, Power}

trait ApparentPowerAndHeatParticipant[
    CD <: CalcRelevantData,
    MS <: ModelState,
] {
  this: SystemParticipant[CD, ComplexPowerAndHeat, MS] =>
  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      modelState: MS,
      data: CD,
  ): ComplexPowerAndHeat = {
    val apparentPower =
      calculateApparentPower(tick, voltage, modelState, data)
    val heat =
      if (isInOperation(tick))
        calculateHeat(tick, modelState, data)
      else
        zeroMW

    ComplexPowerAndHeat(apparentPower.p, apparentPower.q, heat)
  }

  /** Calculate the heat of the asset. As for electrical assets, positive values
    * are understood as consumption and negative as production
    * @param tick
    *   Current instant in simulation time
    * @param modelState
    *   Current state of the model
    * @param data
    *   Needed calculation relevant data
    * @return
    *   Heat production or consumption of the asset
    */
  def calculateHeat(
      tick: Long,
      modelState: MS,
      data: CD,
  ): Power
}
