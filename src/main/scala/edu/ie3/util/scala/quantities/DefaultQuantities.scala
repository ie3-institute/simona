/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.{Each, Energy, Power, Dimensionless}
import squants.energy.{KilowattHours, Kilowatts, Megawatts}

object DefaultQuantities {

  val zeroKWH: Energy = KilowattHours(0d)
  val zeroKW: Power = Kilowatts(0d)
  val zeroMW: Power = Megawatts(0d)
  val zeroMVAr: ReactivePower = Megavars(0d)
  val zeroPU: Dimensionless = Each(0d)

}
