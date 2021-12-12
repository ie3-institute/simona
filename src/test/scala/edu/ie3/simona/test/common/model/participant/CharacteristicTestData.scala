/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.participant

import edu.ie3.simona.model.system.Characteristic
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.quantity.Quantities.getQuantity

import javax.measure.quantity.Dimensionless
import scala.collection.SortedSet

trait CharacteristicTestData {
  val xy1 = new XYPair(getQuantity(1, PU), getQuantity(2, PU))
  val xy2 = new XYPair(getQuantity(2, PU), getQuantity(4, PU))
  val xy3 = new XYPair(getQuantity(3, PU), getQuantity(8, PU))
  val xy4 = new XYPair(getQuantity(4, PU), getQuantity(16, PU))
  val xy5 = new XYPair(getQuantity(5, PU), getQuantity(32, PU))

  object TestCharacteristic
      extends Characteristic[Dimensionless, Dimensionless] {
    override protected val xyCoordinates
        : SortedSet[XYPair[Dimensionless, Dimensionless]] =
      SortedSet.apply(xy1, xy2, xy3, xy4, xy5)
  }
}
