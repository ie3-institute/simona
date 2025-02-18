/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import java.time.DayOfWeek
import java.time.DayOfWeek.{SATURDAY, SUNDAY}

/** Describes the type of day (weekday, saturday or sunday) as part of a load
  * profile key
  */
object DayType extends Enumeration {
  val weekday, saturday, sunday = Value

  /** Creates a day type from given Java DayOfWeek object
    * @param dayOfWeek
    *   a DayOfWeek enum value
    * @return
    *   a day type
    */
  def apply(dayOfWeek: DayOfWeek): DayType.Value = dayOfWeek match {
    case SATURDAY => saturday
    case SUNDAY   => sunday
    case _        => weekday
  }

  /** Creates a day type from given String key, taken from the csv or database
    * table header
    * @param key
    *   day type, such as "We" for weekday
    * @return
    *   a day type
    */
  def apply(key: String): DayType.Value = key match {
    case "Wd" => weekday
    case "Sa" => saturday
    case "Su" => sunday
    case _ =>
      throw new RuntimeException(
        "Malformed header. '" + key + "' not found as DayType. Permissible keys: 'Wd', 'Sa', 'Su'"
      )
  }
}
