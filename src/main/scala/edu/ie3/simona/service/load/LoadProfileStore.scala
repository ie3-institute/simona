/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.model.participant.load.LoadModel.ProfileLoadFactoryData
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  EnergyToSimona,
  PowerConversionSimona,
}
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.Optional
import javax.measure.quantity.{Energy, Power}
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional
import scala.language.implicitConversions

/** Container class that stores all loaded load profiles.
  * @param profileToSource
  *   Map: [[LoadProfile]] to [[LoadProfileSource]]
  */
final case class LoadProfileStore(
    profileToSource: Map[LoadProfile, LoadProfileSource[_, _]]
) {

  /** Converts an option for [[ComparableQuantity]] power to an option for
    * [[squants.Power]].
    * @param power
    *   That should be converted.
    * @return
    *   An option for [[squants.Power]].
    */
  implicit def convertPower(
      power: Optional[ComparableQuantity[Power]]
  ): Option[squants.Power] =
    power.toScala.map(_.toSquants)

  /** Converts an option for [[ComparableQuantity]] energy to an option for
    * [[squants.Energy]].
    * @param energy
    *   That should be converted.
    * @return
    *   An option for [[squants.Power]].
    */
  implicit def convertEnergy(
      energy: Optional[ComparableQuantity[Energy]]
  ): Option[squants.Energy] =
    energy.toScala.map(_.toSquants)

  /** Method to check whether this [[LoadProfileStore]] contains the given
    * [[LoadProfile]].
    * @param loadProfile
    *   That should be checked.
    * @return
    *   True, if this store contain the profile, else false.
    */
  def contains(loadProfile: LoadProfile): Boolean =
    profileToSource.contains(loadProfile)

  /** Returns the load profiles entry (average power consumption of the current
    * interval) for given time and load profile.
    *
    * @param time
    *   The requested time.
    * @param loadProfile
    *   The requested load profile.
    * @return
    *   A load in kW.
    */
  def entry(
      time: ZonedDateTime,
      loadProfile: LoadProfile,
  ): Option[squants.Power] =
    profileToSource
      .get(loadProfile)
      .flatMap(_.getValue(time).toScala)
      .flatMap(_.getP)

  /** Samples entries for random load profile.
    * @param time
    *   The requested time.
    * @param nr
    *   The number of values to sample.
    * @return
    *   A list of load values in kW.
    */
  def sampleRandomEntries(
      time: ZonedDateTime,
      nr: Int,
  ): Seq[squants.Power] =
    Range(0, nr, 1).flatMap(_ => entry(time, RANDOM_LOAD_PROFILE))

  /** @param loadProfile
    *   Given load profile.
    * @return
    *   An option for the [[ProfileLoadFactoryData]] for the given
    *   [[LoadProfile]].
    */
  def getProfileLoadFactoryData(
      loadProfile: LoadProfile
  ): Option[ProfileLoadFactoryData] =
    profileToSource.get(loadProfile).map { source =>
      ProfileLoadFactoryData(
        source.getMaxPower,
        source.getLoadProfileEnergyScaling,
      )
    }

}

object LoadProfileStore {

  def apply(
      sourceDefinition: Datasource
  ): LoadProfileStore = new LoadProfileStore(
    buildInProfiles ++ LoadProfileSources.buildSources(sourceDefinition)
  )

  def apply(): LoadProfileStore = new LoadProfileStore(buildInProfiles)

  /** Returns the build in [[LoadProfileSource]]s.
    */
  private def buildInProfiles: Map[LoadProfile, LoadProfileSource[_, _]] = {
    val bdew: Map[LoadProfile, LoadProfileSource[_, _]] =
      LoadProfileSource.getBdewLoadProfiles.asScala.toMap
    val random: Map[LoadProfile, LoadProfileSource[_, _]] = Map(
      RANDOM_LOAD_PROFILE -> LoadProfileSource.getRandomLoadProfile
    )
    bdew ++ random
  }
}
