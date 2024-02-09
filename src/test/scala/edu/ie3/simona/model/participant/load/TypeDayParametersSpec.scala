/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.model.participant.load.random.{
  RandomLoadParameters,
  TypeDayParameters,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil

class TypeDayParametersSpec extends UnitSpec {
  "A set of type day parameters" should {
    "throw an exception, when it is meant to be instantiated with too less values" in {
      val tooLessValues = Range(0, 100, 10)
        .map(cnt =>
          RandomLoadParameters(
            cnt.toDouble,
            (cnt + 1).toDouble,
            (cnt + 2).toDouble,
          )
        )
        .toArray
      intercept[IllegalArgumentException] {
        TypeDayParameters(tooLessValues)
      }.getMessage shouldBe "You may only instantiate type day parameters with 96 values. Apparent: 10."
    }

    "be able to return the correct values on request" in {
      val typeDayParameters = TypeDayParameters(
        Range(0, 960, 10)
          .map(cnt =>
            RandomLoadParameters(
              cnt.toDouble,
              (cnt + 1).toDouble,
              (cnt + 2).toDouble,
            )
          )
          .toArray
      )
      typeDayParameters.getQuarterHourParameters(
        TimeUtil.withDefaults.toZonedDateTime("2020-04-22 00:15:00")
      ) shouldBe RandomLoadParameters(10d, 11d, 12d)
      typeDayParameters.getQuarterHourParameters(
        TimeUtil.withDefaults.toZonedDateTime("2020-04-22 14:30:00")
      ) shouldBe RandomLoadParameters(580d, 581d, 582d)
      typeDayParameters.getQuarterHourParameters(
        TimeUtil.withDefaults.toZonedDateTime("2020-04-22 19:00:00")
      ) shouldBe RandomLoadParameters(760d, 761d, 762d)
    }
  }
}
