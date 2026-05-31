/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import org.locationtech.jts.geom.Geometry
import squants.Meters
import squants.space.Length

import java.util.UUID

/** Class defining a layer of soil represented by a horizontal geometry (e.g.
  * Polygon) and a vertical interval [zFrom, zTo].
  *
  * @param uuid
  *   unique id of the layer
  * @param geometry
  *   horizontal footprint
  * @param zFrom
  *   Depth where this layer starts
  * @param zTo
  *   Depth where this layer ends
  * @param soilType
  *   UUID of the soilType of this layer
  */
case class SoilLayer(
    uuid: UUID,
    geometry: Geometry,
    zFrom: Length,
    zTo: Length,
    soilType: UUID,
) {

  /** Helper method to calculation the thickness of some layer */
  def thickness: Length = Meters(math.abs(zFrom.toMeters - zTo.toMeters))
}
