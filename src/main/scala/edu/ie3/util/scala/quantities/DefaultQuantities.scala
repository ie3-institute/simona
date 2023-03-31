/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.Each
import squants.energy.{KilowattHours, Kilowatts, Megawatts}

object DefaultQuantities {

  val zeroKWH: squants.Energy = KilowattHours(0d)
  val zeroKW: squants.Power = Kilowatts(0d)
  val zeroMW: squants.Power = Megawatts(0d)
  val zeroMVAr: ReactivePower = Megavars(0d)
  val zeroPU: squants.Dimensionless = Each(0d)

}
