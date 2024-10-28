/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import squants.energy.Watts

import java.util.UUID

class MinMaxFlexibilityMessageTest extends UnitSpec {

  "Creating a ProvideMinMaxFlexibilityMessage" should {

    "succeed if there is no flexibility" in {
      val res = ProvideMinMaxFlexOptions.noFlexOption(
        modelUuid = UUID.randomUUID(),
        power = Watts(1),
      )

      res.ref shouldBe Watts(1)
      res.min shouldBe Watts(1)
      res.max shouldBe Watts(1)
    }

    "succeed if minimum, reference and maximum power are in order" in {
      val res = ProvideMinMaxFlexOptions(
        modelUuid = UUID.randomUUID(),
        ref = Watts(1),
        min = Watts(0),
        max = Watts(2),
      )

      res.ref shouldBe Watts(1)
      res.min shouldBe Watts(0)
      res.max shouldBe Watts(2)
    }

    "throw an exception if minimum power is greater then reference power" in {
      intercept[CriticalFailureException] {
        ProvideMinMaxFlexOptions(
          modelUuid = UUID.randomUUID(),
          ref = Watts(1),
          min = Watts(2),
          max = Watts(2),
        )
      }.getMessage should include("is greater than reference power")
    }

    "throw an exception if reference power is greater then maximum power" in {
      intercept[CriticalFailureException] {
        ProvideMinMaxFlexOptions(
          modelUuid = UUID.randomUUID(),
          ref = Watts(1),
          min = Watts(1),
          max = Watts(0),
        )
      }.getMessage should include("is greater than maximum power")
    }
  }
}
