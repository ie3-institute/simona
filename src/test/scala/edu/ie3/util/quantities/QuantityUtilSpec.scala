/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.quantities

import edu.ie3.simona.exceptions.QuantityException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.QuantityUtil
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{Kilowatts, MegawattHours, WattHours, Watts}
import squants.{Energy, Power}

import scala.util.{Failure, Success}

class QuantityUtilSpec extends UnitSpec with TableDrivenPropertyChecks {
  implicit val powerTolerance: Power = Watts(1e-3)
  implicit val energyTolerance: Energy = WattHours(1e-6)
  private val unit = Kilowatts
  private val integrationUnit = WattHours
  private val integrationClass = classOf[Energy]
  private val averagingClass = classOf[Power]
  private val values = Map(
    2L -> unit(5d),
    4L -> unit(15d),
    6L -> unit(-5d),
    8L -> unit(-10d)
  )

  "Integrating over quantities" when {
    "determining the start value" should {
      val startingValue =
        PrivateMethod[Power](Symbol("startingValue"))

      "throw an exception, if values are empty and unit of \"empty\" quantity cannot be determined" in {
        intercept[QuantityException] {
          QuantityUtil invokePrivate startingValue(
            Map.empty[Long, Power],
            1L
          )
        }.getMessage shouldBe "Unable to determine unit for dummy starting value."
      }

      "bring default value, if there is nothing before window starts" in {
        QuantityUtil invokePrivate startingValue(
          values,
          1L
        ) should be
        unit(0d)

      }

      "bring correct value, if there is something before window starts" in {
        QuantityUtil invokePrivate startingValue(
          values,
          2L
        ) should be
        unit(5d)

      }
    }

    "determining the end value" should {
      val endingValue =
        PrivateMethod[(Long, Power)](Symbol("endingValue"))

      "throw and exception, if there is no value before the window ends" in {
        intercept[QuantityException] {
          QuantityUtil invokePrivate endingValue(values, 1L)
        }.getMessage shouldBe "Cannot integrate over an empty set of values."
      }

      "bring correct value, if there is something before window ends" in {
        QuantityUtil invokePrivate endingValue(values, 2L) match {
          case (tick, value) =>
            tick shouldBe 2L
            (value =~ unit(5d)) shouldBe true
        }
      }
    }

    "actually integrating" should {
      "lead to correct values" in {
        val cases = Table(
          ("windowStart", "windowEnd", "expectedResult"),
          (1L, 3L, integrationUnit(5d)),
          (2L, 4L, integrationUnit(10d)),
          (2L, 8L, integrationUnit(30d)),
          (0L, 12L, integrationUnit(-10d))
        )

        forAll(cases) { (windowStart, windowEnd, expectedResult) =>
          QuantityUtil.integrate(
            values,
            windowStart,
            windowEnd,
            integrationClass,
            integrationUnit
          ) =~ expectedResult
        }
      }
    }
  }

  "Averaging over quantities" when {
    "putting in wrong information" should {
      "fail, if window start and end are the same" in {
        QuantityUtil.average(
          values,
          0L,
          0L,
          integrationClass,
          integrationUnit,
          averagingClass,
          unit
        ) match {
          case Failure(exception: IllegalArgumentException) =>
            exception.getMessage shouldBe "Cannot average over trivial time window."
          case Failure(exception) =>
            fail(
              "Averaging over values failed with wrong exception.",
              exception
            )
          case Success(_) =>
            fail("Averaging with trivial window length should fail")
        }
      }

      "fail, if window start is after window end" in {
        QuantityUtil.average(
          values,
          3L,
          0L,
          integrationClass,
          integrationUnit,
          averagingClass,
          unit
        ) match {
          case Failure(exception: IllegalArgumentException) =>
            exception.getMessage shouldBe "Window end is before window start."
          case Failure(exception) =>
            fail(
              "Averaging over values failed with wrong exception.",
              exception
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
          (0L, 12L, unit(-0.8333333))
        )

        forAll(cases) { (windowStart, windowEnd, expectedResult) =>
          QuantityUtil.average(
            values,
            windowStart,
            windowEnd,
            integrationClass,
            integrationUnit,
            averagingClass,
            unit
          ) match {
            case Success(result) =>
              result =~ expectedResult
            case Failure(exception) =>
              fail(
                "Averaging with fine input should pass, but failed.",
                exception
              )
          }
        }
      }
    }
  }
}
