/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.util.Coordinate
import org.locationtech.jts.geom.{
  Geometry,
  GeometryFactory,
  Point,
  Coordinate as JtsCoordinate,
}

import java.util.UUID

/** Determines the calculation point (a [[Coordinate]]) of a thermal line
  * segment for the weather registration.
  *
  * The calculation point of a segment is the centroid (geometric midpoint) of
  * the straight segment.
  */
object SegmentCalculationPoints {

  private val geometryFactory = new GeometryFactory()

  /** Calculates the calculation point for the given thermal line segment.
    *
    * @param segmentUuid
    *   The uuid of the segment (used for the error message).
    * @param start
    *   The segment start coordinate.
    * @param end
    *   The segment end coordinate.
    * @param soilLayerBoundaries
    *   The boundaries of all soil layers (outer rings of their polygons).
    * @return
    *   The calculation point of the segment.
    */
  def coordinateFor(
      segmentUuid: UUID,
      start: Coordinate,
      end: Coordinate,
      soilLayerBoundaries: Seq[Geometry],
  ): Coordinate = {
    val startPoint = start.toPoint
    val endPoint = end.toPoint

    if !startPoint.isValid || !endPoint.isValid ||
      !startPoint.getCoordinate.isValid || !endPoint.getCoordinate.isValid
    then
      throw new GridAgentInitializationException(
        s"Cannot calculate the calculation point of thermal segment $segmentUuid: " +
          s"the segment geometry from (${start.longitude}, ${start.latitude}) to (${end.longitude}, ${end.latitude}) is invalid."
      )

    val centroid = geometryFactory.createPoint(
      new JtsCoordinate(
        (start.longitude + end.longitude) / 2.0,
        (start.latitude + end.latitude) / 2.0,
      )
    )

    val calculationPoint: Point = centroid

    Coordinate(calculationPoint.getY, calculationPoint.getX)
  }
}
