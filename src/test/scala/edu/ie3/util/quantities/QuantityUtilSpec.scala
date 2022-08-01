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
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.{ProductUnit, Units}

import javax.measure.Quantity
import javax.measure.quantity.{Energy, Power}
import scala.util.{Failure, Success}

class QuantityUtilSpec extends UnitSpec with TableDrivenPropertyChecks {
  private val unit = PowerSystemUnits.KILOWATT
  private val integrationUnit =
    new ProductUnit[Energy](PowerSystemUnits.KILOWATT.multiply(Units.SECOND))
  private val integrationClass = classOf[Energy]
  private val averagingClass = classOf[Power]
  private val values = Map(
    2L -> Quantities.getQuantity(5d, unit),
    4L -> Quantities.getQuantity(15d, unit),
    6L -> Quantities.getQuantity(-5d, unit),
    8L -> Quantities.getQuantity(-10d, unit)
  )

  "Integrating over quantities" when {
    "determining the start value" should {
      val startingValue =
        PrivateMethod[Quantity[Power]](Symbol("startingValue"))

      "throw an exception, if values are empty and unit of \"empty\" quantity cannot be determined" in {
        intercept[QuantityException] {
          QuantityUtil invokePrivate startingValue(
            Map.empty[Long, Quantity[Power]],
            1L
          )
        }.getMessage shouldBe "Unable to determine unit for dummy starting value."
      }

      "bring default value, if there is nothing before window starts" in {
        QuantityUtil invokePrivate startingValue(
          values,
          1L
        ) should equalWithTolerance(
          Quantities.getQuantity(0d, unit)
        )
      }

      "bring correct value, if there is something before window starts" in {
        QuantityUtil invokePrivate startingValue(
          values,
          2L
        ) should equalWithTolerance(
          Quantities.getQuantity(5d, unit)
        )
      }
    }

    "determining the end value" should {
      val endingValue =
        PrivateMethod[(Long, Quantity[Power])](Symbol("endingValue"))

      "throw and exception, if there is no value before the window ends" in {
        intercept[QuantityException] {
          QuantityUtil invokePrivate endingValue(values, 1L)
        }.getMessage shouldBe "Cannot integrate over an empty set of values."
      }

      "bring correct value, if there is something before window ends" in {
        QuantityUtil invokePrivate endingValue(values, 2L) match {
          case (tick, value) =>
            tick shouldBe 2L
            value should equalWithTolerance(Quantities.getQuantity(5d, unit))
        }
      }
    }

    "actually integrating" should {
      "lead to correct values" in {
        val cases = Table(
          ("windowStart", "windowEnd", "expectedResult"),
          (1L, 3L, Quantities.getQuantity(5d, integrationUnit)),
          (2L, 4L, Quantities.getQuantity(10d, integrationUnit)),
          (2L, 8L, Quantities.getQuantity(30d, integrationUnit)),
          (0L, 12L, Quantities.getQuantity(-10d, integrationUnit))
        )

        forAll(cases) { (windowStart, windowEnd, expectedResult) =>
          QuantityUtil.integrate(
            values,
            windowStart,
            windowEnd,
            integrationClass,
            integrationUnit
          ) should equalWithTolerance(expectedResult, 1e-6)
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
          (1L, 3L, Quantities.getQuantity(2.5d, unit)),
          (2L, 4L, Quantities.getQuantity(5d, unit)),
          (2L, 8L, Quantities.getQuantity(5d, unit)),
          (0L, 12L, Quantities.getQuantity(-0.8333333, unit))
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
              result should equalWithTolerance(expectedResult, 1e-6)
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
