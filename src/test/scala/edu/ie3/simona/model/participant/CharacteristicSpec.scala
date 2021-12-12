/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.characteristic.CharacteristicPoint
import edu.ie3.simona.model.system.Characteristic.XYPair
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.participant.CharacteristicTestData
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.quantity.Quantities.getQuantity

import javax.measure.quantity.Dimensionless

class CharacteristicSpec extends UnitSpec with CharacteristicTestData {

  final val defaultTolerance = 1e-12

  "The XYPair class" should {

    "build a correct XYPair given a characteristic point" in {
      val p1 = new CharacteristicPoint[Dimensionless, Dimensionless](
        getQuantity(1, PU),
        getQuantity(2, PU)
      )
      val p2 = new CharacteristicPoint[Dimensionless, Dimensionless](
        getQuantity(2, PU),
        getQuantity(4, PU)
      )
      val apply1 =
        XYPair.xYPairFromCharacteristicPoint[Dimensionless, Dimensionless](p1)
      val apply2 =
        XYPair.xYPairFromCharacteristicPoint[Dimensionless, Dimensionless](p2)

      xy1 shouldBe apply1
      xy2 shouldBe apply2
    }

    "compare two XYPairs correctly" in {
      val comparison1 = xy1.compare(xy2)
      val comparison2 = xy2.compare(xy5)
      val comparison3 = xy3.compare(xy1)
      val comparison4 = xy4.compare(xy3)
      val comparison5 = xy5.compare(xy5)

      comparison1 shouldBe -1
      comparison2 shouldBe -1
      comparison3 shouldBe 1
      comparison4 shouldBe 1
      comparison5 shouldBe 0
    }
  }

  "The characteristic trait" should {

    "interpolate its xy-coordinates correctly, given a key" in {
      val testCharacteristic = TestCharacteristic
      val interpolation1 = testCharacteristic.interpolateXy(getQuantity(1, PU))
      val interpolation2 = testCharacteristic.interpolateXy(getQuantity(2, PU))
      val interpolation3 = testCharacteristic.interpolateXy(getQuantity(3, PU))
      val interpolation4 =
        testCharacteristic.interpolateXy(getQuantity(1.5, PU))
      val interpolation5 =
        testCharacteristic.interpolateXy(getQuantity(2.5, PU))

      interpolation1 match {
        case (x, y) =>
          x should equalWithTolerance(getQuantity(1, PU), defaultTolerance)
          y should equalWithTolerance(getQuantity(2, PU), defaultTolerance)
      }
      interpolation2 match {
        case (x, y) =>
          x should equalWithTolerance(getQuantity(2, PU), defaultTolerance)
          y should equalWithTolerance(getQuantity(4, PU), defaultTolerance)
      }
      interpolation3 match {
        case (x, y) =>
          x should equalWithTolerance(getQuantity(3, PU), defaultTolerance)
          y should equalWithTolerance(getQuantity(8, PU), defaultTolerance)
      }
      interpolation4 match {
        case (x, y) =>
          x should equalWithTolerance(getQuantity(1.5, PU), defaultTolerance)
          y should equalWithTolerance(getQuantity(3, PU), defaultTolerance)
      }
      interpolation5 match {
        case (x, y) =>
          x should equalWithTolerance(getQuantity(2.5, PU), defaultTolerance)
          y should equalWithTolerance(getQuantity(6, PU), defaultTolerance)
      }
    }
  }

}
