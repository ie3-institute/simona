/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE
import edu.ie3.datamodel.models.profile.{LoadProfile, PowerProfileKey}
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.load.ProfileLoadModel.ProfileLoadFactoryData
import edu.ie3.simona.util.SimonaConstants.FIRST_TICK_IN_SIMULATION
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import tech.units.indriya.ComparableQuantity
import edu.ie3.datamodel.io.source.PowerValueSource.TimeSeriesInputValue
import edu.ie3.datamodel.models.value.PValue

import java.time.ZonedDateTime
import java.util.Optional
import javax.measure.quantity.{Energy, Power}
import scala.jdk.CollectionConverters.{ListHasAsScala, MapHasAsScala}
import scala.jdk.FunctionConverters.enrichAsScalaFromSupplier
import scala.jdk.OptionConverters.RichOptional
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import java.util.function.Supplier

/** Container class that stores all loaded load profiles.
  * @param profileToSource
  *   Map: [[PowerProfileKey]] to [[LoadProfileSource]]
  */
final case class LoadProfileStore(
    profileToSource: Map[PowerProfileKey, LoadProfileSource[?]]
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
  def contains(loadProfile: PowerProfileKey): Boolean =
    profileToSource.contains(loadProfile)

  /** Returns a map: [[LoadProfile]] to profile resolution in seconds.
    */
  def getProfileResolutions: Map[PowerProfileKey, Long] = profileToSource.keys
    .map(profile => profile -> LoadProfileSource.getResolution(profile))
    .toMap

  /** Method to find the next activation tick.
    * @param tick
    *   Current tick of the simulation.
    * @param startTime
    *   Of the simulation.
    * @return
    *   An option for the next tick.
    */
  def getNextActivationTick(
      tick: Long
  )(using startTime: ZonedDateTime): Option[Long] = {
    if tick < FIRST_TICK_IN_SIMULATION then {
      Some(FIRST_TICK_IN_SIMULATION)
    } else {
      val currentTime = startTime.plusSeconds(tick)

      profileToSource.view.flatMap { case (_, source) =>
        source.getNextTimeKey(currentTime).asScala.map(_.toTick)
      }.minOption
    }
  }

  /** Returns the load profiles entry function (supplying the average power
    * consumption of the current interval) for given time and load profile.
    *
    * @param time
    *   The requested time.
    * @param loadProfile
    *   The requested load profile.
    * @return
    *   A load in kW.
    */
  def entryFunc(
      time: ZonedDateTime,
      loadProfile: PowerProfileKey,
  ): () => squants.Power = {

    val source = profileToSource
      .getOrElse(
        loadProfile,
        throw new CriticalFailureException(
          s"Load profile $loadProfile is not available."
        ),
      )

    val supplier = source.getValueSupplier(new TimeSeriesInputValue(time))

    () =>
      supplier.asScala
        .apply()
        .toScala
        .flatMap(_.getP.toScala)
        .map(_.toSquants)
        .getOrElse(
          throw new CriticalFailureException(
            s"Load value function cannot be provided for load profile $loadProfile at time $time!"
          )
        )
  }

  /** @param loadProfile
    *   Given load profile.
    * @return
    *   An option for the [[ProfileLoadFactoryData]] for the given
    *   [[LoadProfile]].
    */
  def getProfileLoadFactoryData(
      loadProfile: PowerProfileKey
  ): Option[ProfileLoadFactoryData] =
    profileToSource.get(loadProfile).map { source =>
      ProfileLoadFactoryData(
        source.getMaxPower,
        source.getProfileEnergyScaling,
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
  private def buildInProfiles: Map[PowerProfileKey, LoadProfileSource[?]] = {
    val bdew: Map[LoadProfile, LoadProfileSource[?]] =
      LoadProfileSource.getBdewLoadProfiles.asScala.toMap
    val random: Map[LoadProfile, LoadProfileSource[?]] = Map(
      RANDOM_LOAD_PROFILE.getKey -> LoadProfileSource.getRandomLoadProfile
    )
    bdew ++ random
  }
}
