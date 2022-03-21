/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala

import java.time.ZonedDateTime

object ZonedDateTimeTools {
  implicit class RichZonedDateTime(zdt: ZonedDateTime) {
    def min(other: ZonedDateTime): ZonedDateTime = if (other.isAfter(zdt))
      zdt
    else other

    def max(other: ZonedDateTime): ZonedDateTime = if (other.isAfter(zdt))
      other
    else zdt
  }
}
