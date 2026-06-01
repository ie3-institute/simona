/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.util.geo.GeoUtils
import org.locationtech.jts.geom.Point

/** A simple coordinate class.
  *
  * @param latitude
  *   The latitude of the coordinate.
  * @param longitude
  *   The longitude of the coordinate.
  */
final case class Coordinate(
    latitude: Double,
    longitude: Double,
) {
  def toPoint: Point =
    GeoUtils.DEFAULT_GEOMETRY_FACTORY.createPoint(
      new org.locationtech.jts.geom.Coordinate(longitude, latitude)
    )
}

/** A simple coordinate class.
  *
  * @param latitude
  *   The latitude of the coordinate.
  * @param longitude
  *   The longitude of the coordinate. FIXME
  */
final case class Coordinate3D(
    latitude: Double,
    longitude: Double,
    depth: Double,
) {
  def toPoint: Point =
    GeoUtils.DEFAULT_GEOMETRY_FACTORY.createPoint(
      new org.locationtech.jts.geom.Coordinate(longitude, latitude, depth)
    )
}
