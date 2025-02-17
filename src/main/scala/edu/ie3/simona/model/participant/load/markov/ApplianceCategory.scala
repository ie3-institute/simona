/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.markov

import edu.ie3.simona.util.ParsableEnumeration

final case class ApplianceCategory()

/** Enumeration of all considered appliance types
  */
case object ApplianceCategory extends ParsableEnumeration {
  // val K: Value = Value("k")

  val DISH_WASHER: Value = Value("dish_washer")
  val WASHING_MACHINE: Value = Value("washing_machine")
  val DRYER: Value = Value("dryer")
  val STOVE: Value = Value("stove")
  val FRIDGE: Value = Value("fridge")
  val FREEZER: Value = Value("freezer")
  val TELEVISION: Value = Value("television")
  val VIDEO_RECORDER: Value = Value("video_recorder")
  val PC: Value = Value("pc")
  val TELECOMMUNICATION: Value = Value("telecommunication")
  val LIGHTING: Value = Value("lighting")
  val WATER_HEATING: Value = Value("water_heating")
  val OTHER_LOAD: Value = Value("other_load")
}
