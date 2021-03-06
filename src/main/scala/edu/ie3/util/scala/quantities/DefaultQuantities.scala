/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import javax.measure.quantity.{Energy, Power}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities.getQuantity

case object DefaultQuantities {

  val zeroKWH: ComparableQuantity[Energy] = getQuantity(0d, KILOWATTHOUR)
  val zeroKW: ComparableQuantity[Power] = getQuantity(0d, KILOWATT)

}
