/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil

class TypeDayProfileSpec extends UnitSpec {
  "A type day profile" should {
    "throw an exception, when it is meant to be instantiated with too less values" in {
      val tooLessValues = Range(0, 10).map(_.toDouble).toArray
      intercept[IllegalArgumentException] {
        TypeDayProfile(tooLessValues)
      }.getMessage shouldBe "You may only instantiate type day parameters with 96 values."
    }

    "be able to return the correct values on request" in {
      val typeDayProfile = TypeDayProfile(Range(0, 96).map(_.toDouble).toArray)
      typeDayProfile.getQuarterHourEnergy(
        TimeUtil.withDefaults.toZonedDateTime("2020-04-22T00:15:00Z")
      ) shouldBe 1d
      typeDayProfile.getQuarterHourEnergy(
        TimeUtil.withDefaults.toZonedDateTime("2020-04-22T14:30:00Z")
      ) shouldBe 58d
      typeDayProfile.getQuarterHourEnergy(
        TimeUtil.withDefaults.toZonedDateTime("2020-04-22T19:00:00Z")
      ) shouldBe 76d
    }

    "returns the correct maximum value" in {
      TypeDayProfile(
        Range(0, 96).map(_.toDouble).toArray
      ).getMaxValue shouldBe 95d
    }
  }
}
