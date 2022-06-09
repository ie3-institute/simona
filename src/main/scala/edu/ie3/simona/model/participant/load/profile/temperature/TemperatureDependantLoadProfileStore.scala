/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile.temperature

import edu.ie3.datamodel.models.profile.TemperatureDependantLoadProfile
import edu.ie3.simona.model.participant.load.profile.LoadProfileStore.initializeTypeDayValues
import edu.ie3.simona.model.participant.load.profile.temperature.TemperatureDependantLoadProfileStore.{getMaxTemperatures, getMinTemperatures, initializeMaxConsumptionPerProfile}
import edu.ie3.simona.model.participant.load.profile.{LoadProfileStore, LoadProfileStoreUtils, TypeDayProfile}
import edu.ie3.util.osm.OsmUtils.logger
import edu.ie3.util.quantities.QuantityUtils._
import tech.units.indriya.ComparableQuantity

import java.io.{InputStreamReader, Reader}
import java.time.{Duration, ZonedDateTime}
import javax.measure.quantity.{Power, Temperature}

class TemperatureDependantLoadProfileStore private (val reader: Reader)
    extends LoadProfileStore[
      TemperatureDependantLoadProfile
    ]
    with LoadProfileStoreUtils {

  private val profileMap
      : Map[TemperatureDependantLoadProfileKey, TypeDayProfile] =
    initializeTypeDayValues[TemperatureDependantLoadProfileKey](
      reader,
      TemperatureDependantLoadProfileKey.apply
    )

  private val minTemperatures
      : Map[TemperatureDependantLoadProfile, ComparableQuantity[Temperature]] =
    getMinTemperatures(profileMap)

  private val maxTemperatures
      : Map[TemperatureDependantLoadProfile, ComparableQuantity[Temperature]] =
    getMaxTemperatures(profileMap)

  private val maxParamMap: Map[TemperatureDependantLoadProfile, Double] =
    initializeMaxConsumptionPerProfile(
      minTemperatures,
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
      avgTemperature: ComparableQuantity[Temperature],
      time: ZonedDateTime,
      loadProfile: TemperatureDependantLoadProfile
  ): ComparableQuantity[Power] = {
    val temperature = getTemperature(loadProfile, avgTemperature)
    val key = TemperatureDependantLoadProfileKey(loadProfile, temperature)
    profileMap
      .getOrElse(
        key,
        throw new RuntimeException(
          "Value for LoadProfileKey " + key.toString + " not found."
        )
      )
      .getQuarterHourEnergy(time)
      .asWatt
  }

  private def getTemperature(
      loadProfile: TemperatureDependantLoadProfile,
      avgTemperature: ComparableQuantity[Temperature]
  ) = {
    minTemperatures
      .get(loadProfile)
      .zip(maxTemperatures.get(loadProfile)) match {
      case Some((minTemperature, maxTemperature)) =>
        if (avgTemperature.isLessThan(minTemperature))
          minTemperature
        else if (avgTemperature.isGreaterThan(maxTemperature))
          maxTemperature
        else
          avgTemperature
      case None =>
        throw new IllegalArgumentException(
          s"No minimum or maximum temperature present for load profile $loadProfile"
        )
    }
  }

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
  val resolution: Duration = Duration.ofMinutes(15)

  /** Default value store, that uses information from a file
    * 'standard_load_profiles.csv' placed in the resources folder of the project
    * / jar
    */
  private lazy val defaultStore = new TemperatureDependantLoadProfileStore(
    getDefaultReader
  )

  def apply(): TemperatureDependantLoadProfileStore = defaultStore

  private def getDefaultReader: Reader = {
    logger.info(
      "Loading default load profile file 'standard_load_profiles.csv' from jar."
    )
    new InputStreamReader(
      this.getClass.getResourceAsStream("/load/nbw_temp_dep_load_profiles.csv")
    )
  }

  private def getMinTemperatures(
      profileMap: Map[TemperatureDependantLoadProfileKey, TypeDayProfile]
  ): Map[TemperatureDependantLoadProfile, ComparableQuantity[Temperature]] = {
    profileMap
      .groupBy(_._1.temperatureDependantLoadProfile)
      .map(groupedProfiles =>
        (
          groupedProfiles._1,
          groupedProfiles._2
            .minBy(_._1.averageTemperature)
            ._1
            .averageTemperature
        )
      )

  }

  private def getMaxTemperatures(
      profileMap: Map[TemperatureDependantLoadProfileKey, TypeDayProfile]
  ): Map[TemperatureDependantLoadProfile, ComparableQuantity[Temperature]] = {
    profileMap
      .groupBy(_._1.temperatureDependantLoadProfile)
      .map(groupedProfiles =>
        (
          groupedProfiles._1,
          groupedProfiles._2
            .maxBy(_._1.averageTemperature)
            ._1
            .averageTemperature
        )
      )
  }

  private def initializeMaxConsumptionPerProfile(
      minTemperatures: Map[TemperatureDependantLoadProfile, ComparableQuantity[
        Temperature
      ]],
      profileMap: Map[TemperatureDependantLoadProfileKey, TypeDayProfile]
  ): Map[TemperatureDependantLoadProfile, Double] = {
    profileMap
      .groupBy(_._1.temperatureDependantLoadProfile)
      .map(groupedProfileMap => {
        val (loadProfile, loadProfileMap) = groupedProfileMap
        val minTemperature = minTemperatures.getOrElse(
          groupedProfileMap._1,
          throw new IllegalArgumentException(
            s"Can not find a minimum temperature for load profile $loadProfile"
          )
        )
        val (_, maxProfile) = loadProfileMap
          .find(
            _._1.averageTemperature == minTemperature
          )
          .getOrElse(
            throw new IllegalArgumentException(
              s"Can not find a suitable load profile of type $loadProfile for temperature $minTemperature"
            )
          )
        (loadProfile, maxProfile.getMaxValue)
      })
  }

}
