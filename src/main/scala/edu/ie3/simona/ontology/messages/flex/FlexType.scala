/*
 * © 2025-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

/** Enumeration of flexibility types handled by SIMONA. Implementation resides
  * in subclasses of [[FlexOptions]].
  */
enum FlexType {
  case PowerLimit, EnergyBoundaries
}
