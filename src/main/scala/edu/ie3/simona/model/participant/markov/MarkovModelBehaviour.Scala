/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.simona.util.ParsableEnumeration

/** Enumeration to describe all eligible load model behaviours
  */
case object MarkovModelBehaviour extends ParsableEnumeration {
  val HOUSEHOLD: Value = Value("household")
}
