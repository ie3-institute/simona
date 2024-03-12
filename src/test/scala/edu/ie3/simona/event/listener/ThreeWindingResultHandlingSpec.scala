/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.simona.agent.grid.GridResultsSupport.PartialTransformer3wResult
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.electro.Amperes
import squants.space.Degrees

import java.util.UUID
import scala.util.{Failure, Success}

class ThreeWindingResultHandlingSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with ThreeWindingResultTestData
    with Transformer3wResultSupport {
  "Handling three winding results" when {
    "assembling joint values" should {
      val mockAResult = PartialTransformer3wResult.PortA(
        TimeUtil.withDefaults.toZonedDateTime("2021-06-23T20:51:00Z"),
        UUID.randomUUID(),
        Amperes(1d),
        Degrees(2d),
        -5,
      )
      val mockBResult = PartialTransformer3wResult.PortB(
        TimeUtil.withDefaults.toZonedDateTime("2021-06-23T20:51:00Z"),
        UUID.randomUUID(),
        Amperes(3d),
        Degrees(4d),
      )
      val mockCResult = PartialTransformer3wResult.PortC(
        TimeUtil.withDefaults.toZonedDateTime("2021-06-23T20:51:00Z"),
        UUID.randomUUID(),
        Amperes(5d),
        Degrees(6d),
      )

      "correctly indicate readiness" in {
        val cases = Table(
          ("a", "b", "c", "expected"),
          (None, None, None, false),
          (Some(mockAResult), None, None, false),
          (None, Some(mockBResult), None, false),
          (None, None, Some(mockCResult), false),
          (Some(mockAResult), Some(mockBResult), None, false),
          (None, Some(mockBResult), Some(mockCResult), false),
          (Some(mockAResult), None, Some(mockCResult), false),
          (Some(mockAResult), Some(mockBResult), Some(mockCResult), true),
        )

        forAll(cases) {
          (
              a: Option[PartialTransformer3wResult.PortA],
              b: Option[PartialTransformer3wResult.PortB],
              c: Option[PartialTransformer3wResult.PortC],
              expected: Boolean,
          ) =>
            val dut = AggregatedTransformer3wResult(a, b, c)

            dut.ready shouldBe expected
        }
      }

      "refuse consolidation, if not everything is at it's place" in {
        val cases = Table(
          ("a", "b", "c"),
          (None, None, None),
          (Some(mockAResult), None, None),
          (None, Some(mockBResult), None),
          (None, None, Some(mockCResult)),
          (Some(mockAResult), Some(mockBResult), None),
          (None, Some(mockBResult), Some(mockCResult)),
          (Some(mockAResult), None, Some(mockCResult)),
        )

        forAll(cases) {
          (
              a: Option[PartialTransformer3wResult.PortA],
              b: Option[PartialTransformer3wResult.PortB],
              c: Option[PartialTransformer3wResult.PortC],
          ) =>
            val dut = AggregatedTransformer3wResult(a, b, c)

            dut.consolidate match {
              case Failure(exception: IllegalArgumentException) =>
                exception.getMessage shouldBe "Unable to consolidate three winding results, as not all results are at place."
              case Failure(exception) =>
                fail("Consolidation failed with wrong exception.", exception)
              case Success(value) =>
                fail(
                  s"Consolidation succeeded with '$value', although it was meant to fail."
                )
            }
        }
      }

      "properly consolidate three partial results" in {
        AggregatedTransformer3wResult(
          Some(resultA),
          Some(resultB),
          Some(resultC),
        ).consolidate match {
          case Success(consolidated) =>
            consolidated.getTime shouldBe expected.getTime
            consolidated.getInputModel shouldBe expected.getInputModel
            consolidated.getiAMag should equalWithTolerance(expected.getiAMag)
            consolidated.getiAAng should equalWithTolerance(expected.getiAAng)
            consolidated.getiBMag should equalWithTolerance(expected.getiBMag)
            consolidated.getiBAng should equalWithTolerance(expected.getiBAng)
            consolidated.getiCMag should equalWithTolerance(expected.getiCMag)
            consolidated.getiCAng should equalWithTolerance(expected.getiCAng)
            consolidated.getTapPos shouldBe expected.getTapPos
          case Failure(exception) =>
            fail("Consolidation was meant to succeed but failed.", exception)
        }
      }
    }
  }
}
