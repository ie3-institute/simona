/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.models.profile.LoadProfile
import edu.ie3.simona.service.load.LoadProfileStore.{
  AveragePower,
  MaxPower,
  ValueProvider,
}
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.{KilowattHours, Kilowatts, WattHours}
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.Optional
import javax.measure.quantity.{Energy, Power}
import scala.collection.mutable
import scala.jdk.CollectionConverters.MapHasAsScala
import scala.jdk.OptionConverters.RichOptional

/** Container class that stores all loaded load profiles.
  * @param profileToSource
  *   map: [[LoadProfile]] to [[LoadProfileSource]]
  * @param valueProvider
  *   map: [[LoadProfile]] to provider function
  */
final case class LoadProfileStore(
    profileToSource: Map[LoadProfile, LoadProfileSource[_, _]],
    maxPower: Map[LoadProfile, Option[MaxPower]],
    profileEnergyScaling: Map[LoadProfile, squants.Energy],
    private[load] val valueProvider: Map[LoadProfile, ValueProvider],
) {

  def getKnownProfiles: Set[LoadProfile] = valueProvider.keySet

  /** Returns the average and the max power options.
    * @param time
    *   the requested time
    * @param loadProfile
    *   the requested load profile
    * @return
    */
  def valueOptions(
      time: ZonedDateTime,
      loadProfile: LoadProfile,
  ): Option[(AveragePower, MaxPower)] =
    entry(time, loadProfile).zip(maxPower(loadProfile))

  /** Returns the load profiles entry (average power consumption for the
    * following quarter hour) for given time and load profile.
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
  ): Option[AveragePower] =
    valueProvider.get(loadProfile).map(_.apply(time)) match {
      case Some(value) => value
      case None =>
        throw new RuntimeException(
          s"Value for LoadProfile ${loadProfile.toString} and time $time not found."
        )
    }
}

object LoadProfileStore {
  type AveragePower = squants.Power
  type MaxPower = squants.Power
  private type ValueProvider = ZonedDateTime => Option[AveragePower]

  /** Default standard load profile energy scaling */
  private val defaultLoadProfileEnergyScaling: squants.Energy = KilowattHours(
    1000d
  )

  private val maxPowerMap: mutable.Map[LoadProfile, Option[MaxPower]] =
    mutable.Map()

  private val profileScalingMap: mutable.Map[LoadProfile, squants.Energy] =
    mutable.Map()

  def apply(
      sources: Map[LoadProfile, LoadProfileSource[_, _]]
  ): LoadProfileStore = {
    maxPowerMap.clear()
    profileScalingMap.clear()

    val profileToSource = LoadProfileSource.getBdewLoadProfiles.asScala ++ Map(
      LoadProfile.RandomLoadProfile.RANDOM_LOAD_PROFILE -> LoadProfileSource.getRandomLoadProfile
    ) ++ sources

    val valueProvider: mutable.Map[LoadProfile, ValueProvider] = mutable.Map()

    // adds all other load profiles to the map
    profileToSource.foreach { case (profile, lpts) =>
      valueProvider.put(
        profile,
        time => convert(lpts.getValue(time).flatMap(_.getP)),
      )
      maxPowerMap.put(profile, convert(lpts.getMaxPower))
      lpts.getLoadProfileEnergyScaling.map(scaling =>
        profileScalingMap.put(profile, convert(scaling))
      )
    }

    LoadProfileStore(
      profileToSource.toMap,
      maxPowerMap.toMap,
      profileScalingMap.toMap,
      valueProvider.toMap,
    )
  }

  def apply(): LoadProfileStore = LoadProfileStore(Map.empty)

  /** Returns the profile energy scaling for the given load profile.
    *
    * @param loadProfile
    *   the given profile
    * @return
    *   the scaling
    */
  def profileScaling(loadProfile: LoadProfile): squants.Energy =
    profileScalingMap.getOrElse(loadProfile, defaultLoadProfileEnergyScaling)

  /** Returns the maximum average power consumption per quarter hour for a given
    * load profile, calculated over all seasons and weekday types of given load
    * profile
    *
    * @param loadProfile
    *   the consumer type
    * @return
    *   the maximum load in kW
    */
  def maxPower(loadProfile: LoadProfile): Option[MaxPower] =
    maxPowerMap.get(loadProfile).flatten

  /** Method for scaling the provided power value.
    * @param power
    *   given power
    * @param loadProfileEnergyScaling
    *   scaling factor used by the source
    * @return
    *   the scaled power value
    */
  def scale(
      power: squants.Power,
      loadProfileEnergyScaling: squants.Energy,
  )(implicit tolerance: squants.Energy = WattHours(1e-3)): squants.Power = if (
    defaultLoadProfileEnergyScaling ~= loadProfileEnergyScaling
  ) {
    power
  } else {
    power / loadProfileEnergyScaling * defaultLoadProfileEnergyScaling
  }

  /** Converts an optional [[ComparableQuantity]] power to an option for
    * [[Power]].
    * @param power
    *   that should be converted
    * @return
    *   an option for [[squants.Power]]
    */
  private def convert(
      power: Optional[ComparableQuantity[Power]]
  ): Option[squants.Power] =
    power
      .map(p =>
        Kilowatts(p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue())
      )
      .toScala

  /** Converts an optional [[ComparableQuantity]] power to an option for
    * [[Energy]].
    * @param energy
    *   that should be converted
    * @return
    *   an option for [[squants.Energy]]
    */
  private def convert(
      energy: ComparableQuantity[Energy]
  ): squants.Energy =
    KilowattHours(
      energy.to(PowerSystemUnits.KILOWATTHOUR).getValue.doubleValue()
    )
}
