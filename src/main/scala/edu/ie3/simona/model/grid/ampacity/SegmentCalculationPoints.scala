/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.util.Coordinate
import org.locationtech.jts.geom.{
  Coordinate as JtsCoordinate,
  Geometry,
  GeometryFactory,
  Point,
}

import java.util.UUID

/** Determines the calculation point (a [[Coordinate]]) of a thermal line
  * segment for the weather registration.
  *
  * The calculation point of a segment is the centroid (geometric midpoint) of
  * the straight segment.
  */
object SegmentCalculationPoints {

  /** Maximum distance between two geographic points (in degrees) to treat them
    * as equal.
    */
  private val equalityTolerance = 1e-9

  private val geometryFactory = new GeometryFactory()

  /** Calculates the calculation point for the given thermal line segment.
    *
    * @param segmentUuid
    *   The uuid of the segment (used for the error message).
    * @param start
    *   The segment start point as (longitude, latitude).
    * @param end
    *   The segment end point as (longitude, latitude).
    * @param soilLayerBoundaries
    *   The boundaries of all soil layers (outer rings of their polygons).
    * @return
    *   The calculation point of the segment.
    */
  def coordinateFor(
      segmentUuid: UUID,
      start: (Double, Double),
      end: (Double, Double),
      soilLayerBoundaries: Seq[Geometry],
  ): Coordinate = {
    val startPoint =
      geometryFactory.createPoint(new JtsCoordinate(start._1, start._2))
    val endPoint =
      geometryFactory.createPoint(new JtsCoordinate(end._1, end._2))

    if !startPoint.isValid || !endPoint.isValid ||
      !startPoint.getCoordinate.isValid || !endPoint.getCoordinate.isValid
    then
      throw new GridAgentInitializationException(
        s"Cannot calculate the calculation point of thermal segment $segmentUuid: " +
          s"the segment geometry from ($start._1, $start._2) to ($end._1, $end._2) is invalid."
      )

    val centroid = geometryFactory.createPoint(
      new JtsCoordinate((start._1 + end._1) / 2.0, (start._2 + end._2) / 2.0)
    )

    val calculationPoint: Point = centroid

    Coordinate(calculationPoint.getY, calculationPoint.getX)
  }

  /** Checks whether the given point lies at or near the boundary of any of the
    * given soil layers.
    *
    * @param point
    *   The point to check.
    * @param soilLayerBoundaries
    *   The boundaries of all soil layers (outer rings of their polygons).
    * @param tolerance
    *   The maximum distance from a boundary at which the point is still
    *   considered to be at the boundary.
    * @return
    *   `true` if the point is at or within the given tolerance distance of a
    *   soil layer boundary, `false` otherwise.
    */
  private def isAtSoilLayerBoundary(
      point: Point,
      soilLayerBoundaries: Seq[Geometry],
      tolerance: Double,
  ): Boolean =
    soilLayerBoundaries.exists(boundary =>
      boundary.distance(point) <= tolerance
    )
}
