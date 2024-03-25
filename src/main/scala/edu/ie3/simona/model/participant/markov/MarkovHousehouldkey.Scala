/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import java.time.ZonedDateTime
import edu.ie3.datamodel.exceptions.ParsingException
import edu.ie3.datamodel.models.profile.StandardLoadProfile
import edu.ie3.simona.model.participant.load
import edu.ie3.simona.model.participant.load.{DayType, profile}

/** A key describing a MarkovHousehold profile, consisting of consumer type, a season and a
  * day type. Is used to store MarkovHousehold profile values for a single type.
  *
  * @param standardMarkovHousehold
  *   a consumer type
  * @param season
  *   a season
  * @param dayType
  *   a day type
  */
final case class MarkovHouseholdKey(
    standardMarkovHousehold: StandardLoadProfile,
    season: Season.Value,
    dayType: DayType.Value,
)

case object MarkovHouseholdKey {

  /** Creates a MarkovHousehold key from given csv header, i.e. "g0SSo"
    *
    * @param headerKey
    *   the header
    * @return
    *   a MarkovHousehold key
    */
  def apply(headerKey: String): MarkovHouseholdKey = {
    val regex = "([a-z][0-9])([A-Z][a-z])([A-Z][a-z])".r

    headerKey match {
      case regex(MarkovHouseholdKey, seasonKey, dayTypeKey) =>
        MarkovHouseholdKey(MarkovHouseholdKey, seasonKey, dayTypeKey)
      case _ =>
        throw new RuntimeException(
          s"Provided MarkovHousehold profile header key $headerKey is malformed. It has to be of the form ${regex.pattern} e.g. 'g0WiSu'."
        )
    }
  }

  /** Creates a MarkovHousehold key from three Strings describing a MarkovHousehold
    *
    * @param MarkovHousehold
    *   Key describing the MarkovHousehold
    * @param season
    *   Key describing the season
    * @param dayType
    *   Key describing the day type
    * @return
    *   a MarkovHousehold key
    */
  def apply(
      MarkovHousehold: String,
      season: String,
      dayType: String,
  ): MarkovHouseholdKey = {
    try {
      new MarkovHouseholdKey(
        StandardLoadProfile.parse(MarkovHousehold),
        Season(season),
        DayType(dayType),
      )
    } catch {
      case e: ParsingException =>
        throw new IllegalArgumentException(
          s"Cannot parse '$MarkovHousehold' to a now StandardLoadProfile.",
          e,
        )
    }
  }

  /** Creates a MarkovHousehold key from a consumer type value and a ZonedDateTime
    * object
    *
    * @param MarkovHousehold
    *   The standard MarkovHousehold
    * @param time
    *   The time
    * @return
    *   a MarkovHousehold key
    */
  def apply(
      MarkovHousehold: StandardLoadProfile,
      time: ZonedDateTime,
  ): MarkovHouseholdKey = {
    new MarkovHouseholdKey(
      MarkovHousehold,
      profile.Season(time),
      load.DayType(time.getDayOfWeek),
    )
  }
}
