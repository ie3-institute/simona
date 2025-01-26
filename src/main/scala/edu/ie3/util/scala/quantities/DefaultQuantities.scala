/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.energy.{KilowattHours, Kilowatts, MegawattHours, Megawatts}
import squants.{Dimensionless, Each, Energy, Power}

object DefaultQuantities {

  val zeroKW: Power = Kilowatts(0d)
  val zeroMW: Power = Megawatts(0d)

  val zeroKVAr: ReactivePower = Kilovars(0d)
  val zeroMVAr: ReactivePower = Megavars(0d)

  val zeroKWh: Energy = KilowattHours(0d)
  val zeroMWh: Energy = MegawattHours(0d)

  val zeroPU: Dimensionless = Each(0d)

}
