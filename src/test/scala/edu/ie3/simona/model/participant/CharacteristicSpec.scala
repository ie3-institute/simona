/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.participant.CharacteristicTestData
import squants.Each

class CharacteristicSpec extends UnitSpec with CharacteristicTestData {

  private implicit val puTolerance: squants.Dimensionless = Each(1e-12)

  "The XYPair class" should {

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
      val interpolation1 = testCharacteristic.interpolateXy(Each(1))
      val interpolation2 = testCharacteristic.interpolateXy(Each(2))
      val interpolation3 = testCharacteristic.interpolateXy(Each(3))
      val interpolation4 =
        testCharacteristic.interpolateXy(Each(1.5))
      val interpolation5 =
        testCharacteristic.interpolateXy(Each(2.5))

      interpolation1 match {
        case (x, y) =>
          x should approximate(Each(1))
          y should approximate(Each(2))
      }
      interpolation2 match {
        case (x, y) =>
          x should approximate(Each(2))
          y should approximate(Each(4))
      }
      interpolation3 match {
        case (x, y) =>
          x should approximate(Each(3))
          y should approximate(Each(8))
      }
      interpolation4 match {
        case (x, y) =>
          x should approximate(Each(1.5))
          y should approximate(Each(3))
      }
      interpolation5 match {
        case (x, y) =>
          x should approximate(Each(2.5))
          y should approximate(Each(6))
      }
    }
  }

}
