/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.markov

import java.time.LocalDateTime

object SwitchOnProbabilityKey {
  sealed trait Season
  object Season {
    case object Spring extends Season
    case object Summer extends Season
    case object Autumn extends Season
    case object Winter extends Season
  }

  sealed trait DayType
  object DayType {
    case object Weekday extends DayType
    case object Weekend extends DayType
  }

  case class SwitchOnProbabilityKey(
      season: Season,
      dayType: DayType,
      quarterlyHourOfDay: Int,
  )

  def extractFromDateTime(dateTime: LocalDateTime): SwitchOnProbabilityKey = {
    val season = getSeason(dateTime)
    val dayType = getDayType(dateTime)
    val quarterlyHourOfDay = getQuarterlyHourOfDay(dateTime)
    SwitchOnProbabilityKey(season, dayType, quarterlyHourOfDay)
  }

  private def getSeason(dateTime: LocalDateTime): Season = {
    val month = dateTime.getMonthValue
    if (month >= 3 && month <= 5) Season.Spring
    else if (month >= 6 && month <= 8) Season.Summer
    else if (month >= 9 && month <= 11) Season.Autumn
    else Season.Winter
  }

  private def getDayType(dateTime: LocalDateTime): DayType = {
    val dayOfWeek = dateTime.getDayOfWeek.getValue
    if (dayOfWeek >= 1 && dayOfWeek <= 5) DayType.Weekday
    else DayType.Weekend
  }

  private def getQuarterlyHourOfDay(dateTime: LocalDateTime): Int = {
    val hour = dateTime.getHour
    val minute = dateTime.getMinute
    val quarter = minute / 15
    hour * 4 + quarter
  }

  def getAll: Seq[SwitchOnProbabilityKey] = {
    for {
      season <- Seq(Season.Spring, Season.Summer, Season.Autumn, Season.Winter)
      dayType <- Seq(DayType.Weekday, DayType.Weekend)
      quarterlyHourOfDay <- 0 until (4 * 24)
    } yield SwitchOnProbabilityKey(season, dayType, quarterlyHourOfDay)
  }
}
