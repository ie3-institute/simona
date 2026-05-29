/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import squants.Meters
import squants.space.Length

import java.util.UUID

/** Class defining a layer of soil
  * @param x
  *
  * @param y
  */
case class SoilLayer(
    x: Double,
    y: Double,
    zFrom: Length,
    zTo: Length,
    soilType: UUID,
) {

  /** Helper method to calculation the thickness of some layer
    */
  def thickness: Length = Meters(math.abs(zFrom.toMeters - zTo.toMeters))
}
