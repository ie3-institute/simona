/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config.util

import edu.ie3.simona.util.ParsableEnumeration

/** Enumeration of known [[Notifier]] implementations including an identifying
  * String, so that they can be identified from e.g. configuration files
  */
case object NotifierIdentifier extends ParsableEnumeration {
  val BioMassPlant: Value = Value("bm")
  val ChpPlant: Value = Value("chp")
  val Ev: Value = Value("ev")
  val Evcs: Value = Value("evcs")
  val FixedFeedIn: Value = Value("fixedfeedin")
  val Load: Value = Value("load")
  val PvPlant: Value = Value("pv")
  val Storage: Value = Value("storage")
  val Wec: Value = Value("wec")
}
