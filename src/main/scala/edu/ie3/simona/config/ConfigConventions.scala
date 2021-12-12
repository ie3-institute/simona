/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import scala.util.matching.Regex

/** Contains centralized conventions used for config parameters that should be
  * used in several places to work with config parameters (e.g. regex'es that
  * are used in several places)
  */
object ConfigConventions {
  /*
   * Regex matches e.g. {MV, 10 kV} and captures "MV" and "10 kV"
   */
  val voltLvlRegex: Regex = """\{([\wäöüÄÖÜß-]+),[ ]+(.+)}""".r.unanchored

  // regex matches
  // /    1,2,3,12,5-10,6-10,10-5,10...100,100...1000,10-5
  // val gridIdsRegex: Regex = """(\d+-\d+|\d+\.\.\.\d+|^\d{1}$)""".r.unanchored

  val gridIdMinusRange: Regex = """(\d+)-(\d+)""".r.unanchored
  val gridIdDotRange: Regex = """(\d+)\.\.\.(\d+)""".r.unanchored
  val singleGridId: Regex = """(^\d{1,10}$)""".r.unanchored

  // regex matches
  // / 10 kV, 10 kVA, 10 V, 1 V, 1A
  val refSystemQuantRegex: Regex = """(\d+\s*\pL{1,4})""".r.unanchored
}
