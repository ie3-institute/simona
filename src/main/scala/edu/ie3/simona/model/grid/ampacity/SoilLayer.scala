/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import java.util.UUID

/** */
case class SoilLayer(
    x: Double,
    y: Double,
    zFrom: Double,
    zTo: Double,
    soilType: UUID,
) {

  /** Helper method to calculation the thickness of some layer
    */
  def thickness: Double = math.abs(zFrom - zTo)
}
