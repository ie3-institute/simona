/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.system

import edu.ie3.simona.model.system.Characteristic
import edu.ie3.simona.model.system.Characteristic.XYPair
import squants.{Dimensionless, Each}

import scala.collection.SortedSet

trait CharacteristicTestData {
  protected val xy1: XYPair[Dimensionless, Dimensionless] =
    XYPair(Each(1), Each(2))
  protected val xy2: XYPair[Dimensionless, Dimensionless] =
    XYPair(Each(2), Each(4))
  protected val xy3: XYPair[Dimensionless, Dimensionless] =
    XYPair(Each(3), Each(8))
  protected val xy4: XYPair[Dimensionless, Dimensionless] =
    XYPair(Each(4), Each(16))
  protected val xy5: XYPair[Dimensionless, Dimensionless] =
    XYPair(Each(5), Each(32))

  object TestCharacteristic
      extends Characteristic[Dimensionless, Dimensionless] {
    override protected val xyCoordinates
        : SortedSet[XYPair[Dimensionless, Dimensionless]] =
      SortedSet.apply(xy1, xy2, xy3, xy4, xy5)
  }
}
