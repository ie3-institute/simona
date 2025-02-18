/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.simona.util.ParsableEnumeration

/** Enumeration to describe all eligible load model behaviours
  */
object LoadModelBehaviour extends ParsableEnumeration {
  val FIX: Value = Value("fix")
  val PROFILE: Value = Value("profile")
  val RANDOM: Value = Value("random")
}
