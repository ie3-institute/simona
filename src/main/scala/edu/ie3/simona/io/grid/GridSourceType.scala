/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import edu.ie3.simona.util.ParsableEnumeration

/** Enumeration to describe all eligible grid sources
  */
object GridSourceType extends ParsableEnumeration {
  val CSV: Value = Value("csv")
  val DB: Value = Value("db")
}
