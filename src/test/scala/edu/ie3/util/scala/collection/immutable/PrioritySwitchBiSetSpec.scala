/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.collection.immutable

import edu.ie3.simona.test.common.UnitSpec

class PrioritySwitchBiSetSpec extends UnitSpec {
  private type Key = Int
  private type Value = String

  private val item1: Value = "test1"
  private val item2: Value = "test2"
  private val item3: Value = "test3"
  private val item4: Value = "test4"
  private val item5: Value = "test5"
  private val item6: Value = "test6"

  "A PriorityMultiBiSet" should {
    "be created correctly emptily" in {
      val prioSet = PrioritySwitchBiSet.empty[Key, Value]

      prioSet.isEmpty shouldBe true
      prioSet.nonEmpty shouldBe false
      prioSet.headKeyOption shouldBe None
      prioSet.headKeyIndexOption shouldBe None
      prioSet.values shouldBe Vector.empty
      prioSet.takeNextValueFor(0) shouldBe None
    }

    "behave correctly when adding to an empty map" in {
      val prioSet00 = PrioritySwitchBiSet.empty[Key, Value]

      val prioSet01 = prioSet00.set(1, item1)
      prioSet01.isEmpty shouldBe false
      prioSet01.nonEmpty shouldBe true
      prioSet01.headKeyOption shouldBe Some(1)
      prioSet01.headKeyIndexOption shouldBe Some(0)
      prioSet01.values shouldBe Vector(item1)
      prioSet01.indexOf(item1) shouldBe Some(0)
      prioSet01.indexOf(item2) shouldBe None
      prioSet01.takeNextValueFor(0) shouldBe None

      val (val02, prioSet02) = prioSet01.takeNextValueFor(1).value
      val02 shouldBe item1
      prioSet02.isEmpty shouldBe true
      prioSet02.headKeyOption shouldBe None
      prioSet02.headKeyIndexOption shouldBe None
      prioSet02.values shouldBe Vector(item1)
    }

    "behave correctly when adding and retrieving multiple values" in {
      val prioSet00 = PrioritySwitchBiSet.empty[Key, Value]

      val prioSet01 = prioSet00.set(10, item1)
      prioSet01.nonEmpty shouldBe true
      prioSet01.headKeyOption shouldBe Some(10)
      prioSet01.headKeyIndexOption shouldBe Some(0)
      prioSet01.values shouldBe Vector(item1)
      prioSet01.indexOf(item1) shouldBe Some(0)

      val prioSet02 = prioSet01.set(2, item2)
      prioSet02.nonEmpty shouldBe true
      prioSet02.headKeyOption shouldBe Some(2)
      prioSet02.headKeyIndexOption shouldBe Some(1)
      prioSet02.values shouldBe Vector(item1, item2)
      prioSet02.indexOf(item2) shouldBe Some(1)

      // moving item2 to 5
      val prioSet03 = prioSet02.set(5, item2)
      prioSet03.nonEmpty shouldBe true
      prioSet03.headKeyOption shouldBe Some(5)
      prioSet03.headKeyIndexOption shouldBe Some(1)
      prioSet03.values shouldBe Vector(item1, item2)
      prioSet03.indexOf(item2) shouldBe Some(1)
      prioSet03.takeNextValueFor(2) shouldBe None

      val prioSet04 = prioSet03.set(5, item4)
      prioSet04.nonEmpty shouldBe true
      prioSet04.headKeyOption shouldBe Some(5)
      prioSet04.headKeyIndexOption shouldBe Some(1)
      prioSet04.values shouldBe Vector(item1, item2, item4)
      prioSet04.indexOf(item4) shouldBe Some(2)

      val prioSet05 = prioSet04.set(5, item3)
      prioSet05.headKeyOption shouldBe Some(5)
      prioSet05.headKeyIndexOption shouldBe Some(1)
      prioSet05.values shouldBe Vector(item1, item2, item4, item3)
      prioSet05.indexOf(item3) shouldBe Some(3)

      val prioSet06 = prioSet05.set(15, item5)
      prioSet06.headKeyOption shouldBe Some(5)
      prioSet06.headKeyIndexOption shouldBe Some(1)
      prioSet06.values shouldBe Vector(item1, item2, item4, item3, item5)
      prioSet06.indexOf(item5) shouldBe Some(4)

      // moving item4 to 15
      val prioSet07 = prioSet06.set(15, item4)
      prioSet07.headKeyOption shouldBe Some(5)
      prioSet07.headKeyIndexOption shouldBe Some(1)
      prioSet07.values shouldBe Vector(item1, item2, item4, item3, item5)

      // moving item1 to 15
      val prioSet08 = prioSet07.set(15, item1)
      prioSet08.headKeyOption shouldBe Some(5)
      prioSet08.headKeyIndexOption shouldBe Some(1)
      prioSet08.values shouldBe Vector(item1, item2, item4, item3, item5)

      // priority indices should not have changed
      prioSet08.indexOf(item1) shouldBe Some(0)
      prioSet08.indexOf(item2) shouldBe Some(1)
      prioSet08.indexOf(item3) shouldBe Some(3)
      prioSet08.indexOf(item4) shouldBe Some(2)
      prioSet08.indexOf(item5) shouldBe Some(4)
      prioSet08.indexOf(item6) shouldBe None

      // retrieving values now. They should come in order:
      // 5 -> (item2, item3), 15 -> (item1, item4, item5)

      val (val09, prioSet09) = prioSet08.takeNextValueFor(5).value
      val09 shouldBe item2
      prioSet09.isEmpty shouldBe false
      prioSet09.headKeyOption shouldBe Some(5)
      prioSet09.headKeyIndexOption shouldBe Some(3)

      val (val10, prioSet10) = prioSet09.takeNextValueFor(5).value
      val10 shouldBe item3
      prioSet10.isEmpty shouldBe false
      prioSet10.headKeyOption shouldBe Some(15)
      prioSet10.headKeyIndexOption shouldBe Some(0)

      val (val11, prioSet11) = prioSet10.takeNextValueFor(15).value
      val11 shouldBe item1
      prioSet11.isEmpty shouldBe false
      prioSet11.headKeyOption shouldBe Some(15)
      prioSet11.headKeyIndexOption shouldBe Some(2)

      val (val12, prioSet12) = prioSet11.takeNextValueFor(15).value
      val12 shouldBe item4
      prioSet12.isEmpty shouldBe false
      prioSet12.headKeyOption shouldBe Some(15)
      prioSet12.headKeyIndexOption shouldBe Some(4)

      // moving item5 to 10
      val prioSet13 = prioSet12.set(10, item5)
      prioSet13.isEmpty shouldBe false
      prioSet13.headKeyOption shouldBe Some(10)
      prioSet13.headKeyIndexOption shouldBe Some(4)
      prioSet13.takeNextValueFor(15) shouldBe None

      val (val14, prioSet14) = prioSet13.takeNextValueFor(10).value
      val14 shouldBe item5
      prioSet14.isEmpty shouldBe true
      prioSet14.nonEmpty shouldBe false
      prioSet14.headKeyOption shouldBe None
      prioSet14.headKeyIndexOption shouldBe None

      // priority indices should not have changed
      prioSet14.indexOf(item1) shouldBe Some(0)
      prioSet14.indexOf(item2) shouldBe Some(1)
      prioSet14.indexOf(item3) shouldBe Some(3)
      prioSet14.indexOf(item4) shouldBe Some(2)
      prioSet14.indexOf(item5) shouldBe Some(4)
      prioSet14.indexOf(item6) shouldBe None

      prioSet14.values shouldBe Vector(item1, item2, item4, item3, item5)
    }

  }

}
