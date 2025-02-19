/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load.profile

import java.time.Month._
import java.time.ZonedDateTime

/** Describes the season (winter, summer, transition) as part of a load profile
  * key
  */
object Season extends Enumeration {
  val winter, summer, transition = Value

  /** Creates a season from given time
    * @param time
    *   the time
    * @return
    *   a season
    */
  def apply(time: ZonedDateTime): Season.Value = {
    val day = time.getDayOfMonth

    // winter:      1.11.-20.03.
    // summer:     15.05.-14.09.
    // transition: 21.03.-14.05. and
    //             15.09.-31.10.
    // (VDEW handbook)

    time.getMonth match {
      case NOVEMBER | DECEMBER | JANUARY | FEBRUARY => winter
      case x if x == MARCH && day <= 20             => winter
      case x if x == MAY && day >= 15               => summer
      case JUNE | JULY | AUGUST                     => summer
      case x if x == SEPTEMBER && day <= 14         => summer
      case _                                        => transition
    }
  }

  /** Creates a season from given String key, taken from the csv or database
    * table header
    * @param key
    *   season, such as "winter"
    * @return
    *   a season
    */
  def apply(key: String): Season.Value = key match {
    case "Wi" | "Winter"       => winter
    case "Su" | "Summer"       => summer
    case "Tr" | "Intermediate" => transition
    case _ =>
      throw new RuntimeException(
        "Malformed header, \"" + key + "\" not found as Season. Permissible keys: 'Wi', 'Winter', 'Su', 'Summer', 'Tr', 'Intermediate'"
      )
  }
}
