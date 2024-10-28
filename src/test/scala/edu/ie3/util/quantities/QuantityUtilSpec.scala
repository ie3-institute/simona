/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.QuantityUtil
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{Kilojoules, Kilowatts, WattHours, Watts}
import squants.{Energy, Power}

import scala.util.{Failure, Success}

class QuantityUtilSpec extends UnitSpec with TableDrivenPropertyChecks {
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-6)
  private val unit = Kilowatts
  private val integrationUnit = Kilojoules
  private val values = Map(
    2L -> unit(5d),
    4L -> unit(15d),
    6L -> unit(-5d),
    8L -> unit(-10d),
  )

  "Integrating over quantities" when {
    "actually integrating" should {
      "lead to correct values" in {
        val cases = Table(
          ("windowStart", "windowEnd", "expectedResult"),
          (1L, 3L, integrationUnit(5d)),
          (2L, 4L, integrationUnit(10d)),
          (2L, 8L, integrationUnit(30d)),
          (0L, 12L, integrationUnit(-10d)),
        )

        forAll(cases) { (windowStart, windowEnd, expectedResult) =>
          QuantityUtil.integrate[Power, Energy](
            values,
            windowStart,
            windowEnd,
          ) should approximate(expectedResult)
        }
      }
    }
  }

  "Averaging over quantities" when {
    "putting in wrong information" should {
      "fail, if window start and end are the same" in {
        QuantityUtil.average[Power, Energy](
          values,
          0L,
          0L,
        ) match {
          case Failure(exception: IllegalArgumentException) =>
            exception.getMessage shouldBe "Cannot average over trivial time window."
          case Failure(exception) =>
            fail(
              "Averaging over values failed with wrong exception.",
              exception,
            )
          case Success(_) =>
            fail("Averaging with trivial window length should fail")
        }
      }

      "fail, if window start is after window end" in {
        QuantityUtil.average[Power, Energy](
          values,
          3L,
          0L,
        ) match {
          case Failure(exception: IllegalArgumentException) =>
            exception.getMessage shouldBe "Window end is before window start."
          case Failure(exception) =>
            fail(
              "Averaging over values failed with wrong exception.",
              exception,
            )
          case Success(_) =>
            fail("Averaging with flipped window start / end should fail")
        }
      }
    }

    "putting in correct information" should {
      "lead to correct values" in {
        val cases = Table(
          ("windowStart", "windowEnd", "expectedResult"),
          (1L, 3L, unit(2.5d)),
          (2L, 4L, unit(5d)),
          (2L, 8L, unit(5d)),
          (0L, 12L, unit(-0.8333333)),
        )

        forAll(cases) { (windowStart, windowEnd, expectedResult) =>
          QuantityUtil.average[Power, Energy](
            values,
            windowStart,
            windowEnd,
          ) match {
            case Success(result) =>
              result should approximate(expectedResult)
            case Failure(exception) =>
              fail(
                "Averaging with fine input should pass, but failed.",
                exception,
              )
          }
        }
      }
    }
  }
}
