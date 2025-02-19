/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load.random

import edu.ie3.simona.model.participant2.load.DayType
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import org.scalatest.PrivateMethodTester
import org.scalatest.prop.TableDrivenPropertyChecks

import java.io.InputStreamReader

class RandomLoadParamStoreSpec
    extends UnitSpec
    with PrivateMethodTester
    with TableDrivenPropertyChecks {

  "A random load parameter store" should {
    "is able to build a correct descriptor tree" in {
      val buildDescriptorTree =
        PrivateMethod[Map[DayType.Value, Map[RandomLoadParameters.Value, Int]]](
          Symbol("buildDescriptorTree")
        )
      val actual = RandomLoadParamStore invokePrivate buildDescriptorTree(
        List(
          "kSa",
          "kSu",
          "kWd",
          "mySa",
          "mySu",
          "myWd",
          "sigmaSa",
          "sigmaSu",
          "sigmaWd",
          "quarterHour",
        )
      )
      val expected = Map(
        DayType.weekday -> Map(
          RandomLoadParameters.K -> 2,
          RandomLoadParameters.MY -> 5,
          RandomLoadParameters.SIGMA -> 8,
        ),
        DayType.saturday -> Map(
          RandomLoadParameters.K -> 0,
          RandomLoadParameters.MY -> 3,
          RandomLoadParameters.SIGMA -> 6,
        ),
        DayType.sunday -> Map(
          RandomLoadParameters.K -> 1,
          RandomLoadParameters.MY -> 4,
          RandomLoadParameters.SIGMA -> 7,
        ),
      )

      actual shouldBe expected
    }

    "be instantiated correctly" in {
      val reader: InputStreamReader = new InputStreamReader(
        this.getClass
          .getResourceAsStream("random_load_parameters_test.csv")
      )
      val parameterMap =
        RandomLoadParamStore(reader) invokePrivate PrivateMethod[
          Map[DayType.Value, TypeDayParameters]
        ](Symbol("parameterMap"))()
      parameterMap.size shouldBe 3
    }

    "return the correct parameters" in {
      val reader: InputStreamReader = new InputStreamReader(
        this.getClass
          .getResourceAsStream("random_load_parameters_test.csv")
      )
      val randomParameterStore = RandomLoadParamStore(reader)

      val params =
        Table(
          ("timestamp", "expected"),
          (
            "2019-04-01T05:00:00Z",
            RandomLoadParameters(
              0.146539300680161,
              0.0430354326963425,
              0.0201929099857807,
            ),
          ), // Weekday
          (
            "2019-06-02T00:00:00Z",
            RandomLoadParameters(
              0.295997023582459,
              0.0630703344941139,
              0.0370676517486572,
            ),
          ), // Sunday
          (
            "2019-01-05T02:15:00Z",
            RandomLoadParameters(
              0.132398754358292,
              0.0439879409968853,
              0.0208074823021889,
            ),
          ), // Saturday
        )

      forAll(params) {
        (
            timestamp: String,
            expected: RandomLoadParameters,
        ) =>
          val time = TimeUtil.withDefaults.toZonedDateTime(timestamp)
          randomParameterStore.parameters(time) shouldBe expected
      }
    }
  }
}
