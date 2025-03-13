/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.service.load.LoadProfileStore.{
  convertPower,
  defaultReferenceScalingFactor,
}
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.{KilowattHours, Kilowatts}
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import javax.measure.quantity.Power
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional

/** Container class that stores all loaded load profiles.
  * @param profileToSource
  *   map: [[LoadProfile]] to [[LoadProfileSource]]
  */
final case class LoadProfileStore(
    profileToSource: Map[LoadProfile, LoadProfileSource[_, _]]
) {

  def contains(loadProfile: LoadProfile): Boolean =
    profileToSource.contains(loadProfile)

  /** Returns the load profiles entry (average power consumption of the current
    * interval) for given time and load profile.
    *
    * @param time
    *   the requested time
    * @param loadProfile
    *   the requested load profile
    * @return
    *   a load in kW
    */
  def entry(
      time: ZonedDateTime,
      loadProfile: LoadProfile,
  ): Option[squants.Power] =
    profileToSource
      .get(loadProfile)
      .flatMap(_.getValue(time).toScala)
      .map(_.getP.toScala)
      .flatMap(convertPower)

  /** Returns the profile energy scaling for the given load profile.
    *
    * @param loadProfile
    *   the given profile
    * @return
    *   the scaling
    */
  def profileScaling(loadProfile: LoadProfile): squants.Energy =
    profileToSource
      .get(loadProfile)
      .flatMap(_.getLoadProfileEnergyScaling.toScala) match {
      case Some(referenceScaling) =>
        KilowattHours(
          referenceScaling
            .to(PowerSystemUnits.KILOWATTHOUR)
            .getValue
            .doubleValue()
        )
      case None =>
        defaultReferenceScalingFactor
    }

  /** Returns the maximum average power consumption per quarter hour for a given
    * load profile, calculated over all seasons and weekday types of given load
    * profile
    *
    * @param loadProfile
    *   the consumer type
    * @return
    *   the maximum load in kW
    */
  def maxPower(loadProfile: LoadProfile): Option[squants.Power] =
    profileToSource
      .get(loadProfile)
      .map(_.getMaxPower.toScala)
      .flatMap(convertPower)

}

object LoadProfileStore {

  /** Default standard load profile energy scaling */
  private val defaultReferenceScalingFactor: squants.Energy = KilowattHours(
    1000d
  )

  def apply(
      sourceDefinition: Datasource
  ): LoadProfileStore = {

    // build all additional sources
    val additionalSources = LoadProfileSources.buildSources(sourceDefinition)

    val profileToSource = LoadProfileSource.getBdewLoadProfiles.asScala ++ Map(
      LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE -> LoadProfileSource.getRandomLoadProfile
    ) ++ additionalSources

    new LoadProfileStore(profileToSource.toMap)
  }

  def apply(): LoadProfileStore = {
    val profileToSource = LoadProfileSource.getBdewLoadProfiles.asScala ++ Map(
      LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE -> LoadProfileSource.getRandomLoadProfile
    )
    new LoadProfileStore(profileToSource.toMap)
  }

  /** Converts an option for [[ComparableQuantity]] power to an option for
    * [[squants.Power]].
    * @param power
    *   that should be converted
    * @return
    *   an option for [[squants.Power]]
    */
  private def convertPower(
      power: Option[ComparableQuantity[Power]]
  ): Option[squants.Power] =
    power
      .map(p =>
        Kilowatts(p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue())
      )
}
