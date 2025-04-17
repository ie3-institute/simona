/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import edu.ie3.datamodel.exceptions.ParsingException
import edu.ie3.datamodel.models.profile.StandardLoadProfile
import edu.ie3.simona.model.participant.load.DayType

import java.time.ZonedDateTime

/** A key describing a load profile, consisting of consumer type, a season and a
  * day type. Is used to store load profile values for a single type.
  *
  * @param standardLoadProfile
  *   a consumer type
  * @param season
  *   a season
  * @param dayType
  *   a day type
  */
final case class LoadProfileKey(
    standardLoadProfile: StandardLoadProfile,
    season: Season.Value,
    dayType: DayType.Value,
)

object LoadProfileKey {

  /** Creates a load profile key from given csv header, i.e. "g0SSo"
    *
    * @param headerKey
    *   the header
    * @return
    *   a load profile key
    */
  def apply(headerKey: String): LoadProfileKey = {
    val regex = "([a-z][0-9])([A-Z][a-z])([A-Z][a-z])".r

    headerKey match {
      case regex(loadProfileKey, seasonKey, dayTypeKey) =>
        LoadProfileKey(loadProfileKey, seasonKey, dayTypeKey)
      case _ =>
        throw new RuntimeException(
          s"Provided load profile header key $headerKey is malformed. It has to be of the form ${regex.pattern} e.g. 'g0WiSu'."
        )
    }
  }

  /** Creates a load profile key from three Strings describing a load profile
    *
    * @param loadProfile
    *   Key describing the load profile
    * @param season
    *   Key describing the season
    * @param dayType
    *   Key describing the day type
    * @return
    *   a load profile key
    */
  def apply(
      loadProfile: String,
      season: String,
      dayType: String,
  ): LoadProfileKey = {
    try {
      new LoadProfileKey(
        StandardLoadProfile.parse(loadProfile),
        Season(season),
        DayType(dayType),
      )
    } catch {
      case e: ParsingException =>
        throw new IllegalArgumentException(
          s"Cannot parse '$loadProfile' to a now StandardLoadProfile.",
          e,
        )
    }
  }

  /** Creates a load profile key from a consumer type value and a ZonedDateTime
    * object
    *
    * @param loadProfile
    *   The standard load profile
    * @param time
    *   The time
    * @return
    *   a load profile key
    */
  def apply(
      loadProfile: StandardLoadProfile,
      time: ZonedDateTime,
  ): LoadProfileKey = {
    new LoadProfileKey(
      loadProfile,
      Season(time),
      DayType(time.getDayOfWeek),
    )
  }
}
