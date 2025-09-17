/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.simona.config.RuntimeConfig.EmRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.scalatestplus.mockito.MockitoSugar
import squants.energy.Kilowatts

import java.util.UUID

class EmModelShellSpec extends UnitSpec with MockitoSugar {

  "EmModelShell" should {
    "apply PROPORTIONAL strategy correctly" in {
      val config = EmRuntimeConfig()
      val result = EmModelShell.apply(
        UUID.randomUUID(),
        "TestID1",
        "PROPORTIONAL",
        config,
      )

      result.modelStrategy shouldBe ProportionalFlexStrat
      result.aggregateFlex shouldBe EmAggregatePowerOpt(
        zeroKW,
        false,
      )
    }

    "apply PRIORITIZED strategy with curtailRegenerative correctly" in {
      val config = EmRuntimeConfig.apply(false, 1.0, null, "SELF_OPT", true)
      val result = EmModelShell.apply(
        UUID.randomUUID(),
        "TestID2",
        "PRIORITIZED",
        config,
      )

      result.modelStrategy shouldBe PrioritizedFlexStrat(true)
      result.aggregateFlex shouldBe EmAggregatePowerOpt(
        zeroKW,
        true,
      )
    }

    "throw CriticalFailureException for unknown model strategy" in {
      val config = EmRuntimeConfig()
      val exception = intercept[CriticalFailureException] {
        EmModelShell.apply(
          UUID.randomUUID(),
          "TestID3",
          "UNKNOWN_STRATEGY",
          config,
        )
      }

      exception.getMessage should include(
        "Unknown model flex strategy UNKNOWN_STRATEGY"
      )
    }

    "apply SIMPLE_SUM strategy correctly" in {
      val config = EmRuntimeConfig.apply(false, 1.0, null, "SIMPLE_SUM", true)
      val result = EmModelShell.apply(
        UUID.randomUUID(),
        "TestID4",
        "PROPORTIONAL",
        config,
      )

      result.aggregateFlex shouldBe EmAggregateSimpleSum
    }

    "apply SELF_POWER_* strategy with correct power limit" in {
      val config =
        EmRuntimeConfig.apply(false, 1.0, null, "SELF_POWER_100.5", true)
      val result = EmModelShell.apply(
        UUID.randomUUID(),
        "TestID5",
        "PROPORTIONAL",
        config,
      )

      result.aggregateFlex shouldBe EmAggregatePowerOpt(Kilowatts(100.5), true)
    }

    "apply SELF_POWER_*_EXCL_REG strategy correctly" in {
      val config = EmRuntimeConfig.apply(
        false,
        1.0,
        null,
        "SELF_POWER_200_EXCL_REG",
      )
      val result = EmModelShell.apply(
        UUID.randomUUID(),
        "TestID6",
        "PROPORTIONAL",
        config,
      )

      result.aggregateFlex shouldBe EmAggregatePowerOpt(Kilowatts(200), false)
    }

    "throw CriticalFailureException for invalid power limit format" in {
      val config =
        EmRuntimeConfig.apply(false, 1.0, null, "SELF_POWER_100.100.100")

      val exception = intercept[CriticalFailureException] {
        EmModelShell.apply(
          UUID.randomUUID(),
          "TestID7",
          "PROPORTIONAL",
          config,
        )
      }

      exception.getMessage should include(
        "Invalid numeric value in aggregate flex strategy: SELF_POWER_100.100.100"
      )
    }

    "throw CriticalFailureException for non numeric power limit format" in {
      val config =
        EmRuntimeConfig.apply(false, 1.0, null, "SELF_POWER_abc")

      val exception = intercept[CriticalFailureException] {
        EmModelShell.apply(
          UUID.randomUUID(),
          "TestID7",
          "PROPORTIONAL",
          config,
        )
      }

      exception.getMessage should include(
        "Invalid format for aggregate flex strategy: SELF_POWER_abc"
      )
    }

    "throw CriticalFailureException for invalid format in SELF_POWER strategy" in {
      val config = EmRuntimeConfig.apply(false, 1.0, null, "SELF_POWER_")

      val exception = intercept[CriticalFailureException] {
        EmModelShell.apply(
          UUID.randomUUID(),
          "TestID10",
          "PROPORTIONAL",
          config,
        )
      }

      exception.getMessage should include(
        "Invalid format for aggregate flex strategy: SELF_POWER_"
      )
    }

    "throw CriticalFailureException for unknown aggregate flex strategy" in {
      val config = EmRuntimeConfig.apply(false, 1.0, null, "UNKNOWN_FLEX")

      val exception = intercept[CriticalFailureException] {
        EmModelShell.apply(
          UUID.randomUUID(),
          "TestID11",
          "PROPORTIONAL",
          config,
        )
      }

      exception.getMessage should include(
        "Unknown aggregate flex strategy UNKNOWN_FLEX"
      )
    }
  }
}
