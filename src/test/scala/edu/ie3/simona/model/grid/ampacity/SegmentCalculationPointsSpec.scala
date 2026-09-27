/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.test.common.UnitSpec
import org.locationtech.jts.geom.{Coordinate as JtsCoordinate, GeometryFactory}
import org.scalatest.matchers.should.Matchers

import java.util.UUID

class SegmentCalculationPointsSpec extends UnitSpec with Matchers {

  private val geometryFactory = new GeometryFactory()
  private val segmentUuid = UUID.randomUUID()

  private def rectangle(
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double,
  ) = {
    val coords = Array(
      new JtsCoordinate(xMin, yMin),
      new JtsCoordinate(xMax, yMin),
      new JtsCoordinate(xMax, yMax),
      new JtsCoordinate(xMin, yMax),
      new JtsCoordinate(xMin, yMin),
    )
    geometryFactory.createPolygon(coords)
  }

  "SegmentCalculationPoints.coordinateFor" should {

    "return the geometric midpoint for an undisturbed segment" in {
      val coordinate = SegmentCalculationPoints.coordinateFor(
        segmentUuid,
        (0.0, 0.0),
        (1.0, 0.0),
        Seq.empty,
      )

      coordinate.latitude shouldBe 0.0
      coordinate.longitude shouldBe 0.5
    }

    "return the geometric midpoint when no soil layer boundary is at a segment edge" in {
      // The layer covers the entire segment interior, but neither segment
      // endpoint lies on its boundary.
      val layer = rectangle(0.2, -0.1, 0.8, 0.1)

      val coordinate = SegmentCalculationPoints.coordinateFor(
        segmentUuid,
        (0.0, 0.0),
        (1.0, 0.0),
        Seq(layer),
      )

      coordinate.latitude shouldBe 0.0
      coordinate.longitude shouldBe 0.5
    }

    "return the boundary point if the segment end lies at a soil layer boundary" in {
      // Only the segment end lies on the layer boundary (right edge at x = 1.0).
      val layer = rectangle(0.5, -0.1, 1.0, 0.1)

      val coordinate = SegmentCalculationPoints.coordinateFor(
        segmentUuid,
        (0.0, 0.0),
        (1.0, 0.0),
        Seq(layer),
      )

      coordinate.latitude shouldBe 0.0
      coordinate.longitude shouldBe 1.0
    }

    "return the boundary point if the segment start lies at a soil layer boundary" in {
      // The layer starts at x = 0.0, where the segment starts.
      val layer = rectangle(0.0, -0.1, 0.5, 0.1)

      val coordinate = SegmentCalculationPoints.coordinateFor(
        segmentUuid,
        (0.0, 0.0),
        (1.0, 0.0),
        Seq(layer),
      )

      coordinate.latitude shouldBe 0.0
      coordinate.longitude shouldBe 0.0
    }

    "return the start boundary point if both segment ends lie at soil layer boundaries" in {
      // The layer spans exactly the segment [0, 1]; both ends lie on its
      // boundary. The start (the side the segment grows from) wins.
      val layer = rectangle(0.0, -0.1, 1.0, 0.1)

      val coordinate = SegmentCalculationPoints.coordinateFor(
        segmentUuid,
        (0.0, 0.0),
        (1.0, 0.0),
        Seq(layer),
      )

      coordinate.latitude shouldBe 0.0
      coordinate.longitude shouldBe 0.0
    }

    "work with diagonal segments" in {
      val coordinate = SegmentCalculationPoints.coordinateFor(
        segmentUuid,
        (0.0, 0.0),
        (1.0, 1.0),
        Seq.empty,
      )

      coordinate.latitude shouldBe 0.5 +- 1e-9
      coordinate.longitude shouldBe 0.5 +- 1e-9
    }

    "throw an exception if the start point is invalid (NaN coordinates)" in {
      val exception = the[GridAgentInitializationException] thrownBy {
        SegmentCalculationPoints.coordinateFor(
          segmentUuid,
          (Double.NaN, 0.0),
          (1.0, 0.0),
          Seq.empty,
        )
      }
      exception.getMessage should include(segmentUuid.toString)
    }

    "throw an exception if the end point is invalid (NaN coordinates)" in {
      a[GridAgentInitializationException] should be thrownBy {
        SegmentCalculationPoints.coordinateFor(
          segmentUuid,
          (0.0, 0.0),
          (Double.NaN, 0.0),
          Seq.empty,
        )
      }
    }
  }
}
