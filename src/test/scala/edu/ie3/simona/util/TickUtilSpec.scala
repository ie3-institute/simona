/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil

class TickUtilSpec extends UnitSpec {
  "TimeUtil" should {
    "round ticks and date/times correctly" in {
      val cases = Table(
        ("tick", "dateTime", "resolution", "expectedTick", "expectedDateTime"),
        // base scenario
        (0, "2024-01-02T00:00:00Z", 900, 0, "2024-01-02T00:00:00Z"),
        // unusual tick
        (1, "2024-01-02T00:00:00Z", 900, 1, "2024-01-02T00:00:00Z"),
        // uneven second
        (3600, "2024-01-02T00:00:10Z", 900, 3590, "2024-01-02T00:00:00Z"),
        // uneven minute
        (3600, "2024-01-02T00:07:00Z", 900, 3180, "2024-01-02T00:00:00Z"),
        // uneven minute and second
        (3600, "2024-01-02T00:07:07Z", 900, 3173, "2024-01-02T00:00:00Z"),
        // second-sized resolution, base scenario
        (3600, "2024-01-02T00:00:00Z", 15, 3600, "2024-01-02T00:00:00Z"),
        // second-sized resolution, uneven second
        (3603, "2024-01-02T00:00:18Z", 15, 3600, "2024-01-02T00:00:15Z"),
        // second-sized resolution, uneven minute
        (3600, "2024-01-02T00:05:00Z", 15, 3600, "2024-01-02T00:05:00Z"),
        // second-sized resolution, uneven minute and second
        (3607, "2024-01-02T00:05:07Z", 15, 3600, "2024-01-02T00:05:00Z"),
      )

      forAll(cases) {
        case (tick, dateTime, resolution, expectedTick, expectedDateTime) =>
          val (actualTick, actualDateTime) = TickUtil.roundToResolution(
            tick,
            TimeUtil.withDefaults.toZonedDateTime(dateTime),
            resolution,
          )

          actualTick should equal(expectedTick)
          TimeUtil.withDefaults.toString(actualDateTime) should
            equal(expectedDateTime)
      }
    }
  }
}
