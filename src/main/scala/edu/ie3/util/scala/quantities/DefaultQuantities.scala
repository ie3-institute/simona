/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR, PU}

import javax.measure.quantity.{Dimensionless, Energy, Power}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities.getQuantity

object DefaultQuantities {

  val zeroKWH: ComparableQuantity[Energy] = getQuantity(0d, KILOWATTHOUR)
  val zeroKW: ComparableQuantity[Power] = getQuantity(0d, KILOWATT)
  val zeroPU: ComparableQuantity[Dimensionless] = getQuantity(0d, PU)

}
