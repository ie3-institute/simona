/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import edu.ie3.datamodel.models.profile.TemperatureDependantLoadProfile
import edu.ie3.simona.model.participant.load.profile.LoadProfileStore.initializeTypeDayValues
import edu.ie3.simona.model.participant.load.profile.TemperatureDependantLoadProfileStore.initializeTypeDayValues
import edu.ie3.util.osm.OsmUtils.logger
import org.apache.commons.csv.CSVFormat
import tech.units.indriya.ComparableQuantity

import java.io.{InputStreamReader, Reader}
import java.time.ZonedDateTime
import java.util
import javax.measure.quantity.Power

class TemperatureDependantLoadProfileStore private (val reader: Reader)
    extends LoadProfileStore[
      TemperatureDependantLoadProfile
    ]
    with LoadProfileStoreUtils {
  private val profileMap
      : Map[TemperatureDependantLoadProfileKey, TypeDayProfile] =
    initializeTypeDayValues[TemperatureDependantLoadProfileKey](reader, TemperatureDependantLoadProfileKey.apply)

  private val maxParamMap: Map[TemperatureDependantLoadProfile, Double] =
    initializeMaxConsumptionPerProfile(
      profileMap
    )

  /** Returns the load profiles entry (average power consumption for the
    * following quarter hour) for given time and load profile.
    *
    * @param time
    *   the requested time
    * @param loadProfile
    *   the requested load profile
    * @return
    *   a load in W
    */
  override def entry(
      time: ZonedDateTime,
      loadProfile: TemperatureDependantLoadProfile
  ): ComparableQuantity[Power] = ???

  /** Returns the maximum average power consumption per quarter hour for a given
    * load profile, calculated over all seasons and weekday types of given load
    * profile
    *
    * @param loadProfile
    *   the consumer type
    * @return
    *   the maximum load in W
    */
  override def maxPower(
      loadProfile: TemperatureDependantLoadProfile
  ): ComparableQuantity[Power] = ???
}

object TemperatureDependantLoadProfileStore {

  def getDefaultReader = {
    logger.info(
      "Loading default load profile file 'standard_load_profiles.csv' from jar."
    )
    new InputStreamReader(
      this.getClass.getResourceAsStream("/load/temp_dep_load_profiles.csv")
    )
  }

  private def initializeMaxConsumptionPerProfile(profileMap):Map[TemperatureDependantLoadProfile, Double] = {}

}
