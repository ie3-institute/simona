/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import edu.ie3.datamodel.exceptions.SourceException
import edu.ie3.datamodel.io.source.LoadProfileSource
import edu.ie3.datamodel.models.profile.LoadProfile.RandomLoadProfile
import edu.ie3.datamodel.models.profile.{BdewStandardLoadProfile, LoadProfile}
import edu.ie3.datamodel.models.timeseries.repetitive.{
  BdewLoadProfileTimeSeries,
  RandomLoadProfileTimeSeries,
}
import edu.ie3.simona.service.load.LoadProfileStore.{AveragePower, MaxPower}
import edu.ie3.util.quantities.PowerSystemUnits
import squants.Energy
import squants.energy.{KilowattHours, Kilowatts, WattHours}
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.Optional
import javax.measure.quantity.Power
import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptional
import scala.util.Try

/** Container class that stores all loaded load profiles.
  * @param bdewLoadProfiles
  *   an option for [[BdewStandardLoadProfile]]s
  * @param random
  *   an option for a [[RandomLoadProfile]]
  * @param loadProfileSources
  *   all other sources
  * @param maxValueMap
  *   map: load profile to maximal power value
  * @param valueProvider
  *   map: [[LoadProfile]] to provider function
  */
final case class LoadProfileStore(
    bdewLoadProfiles: Option[
      Map[BdewStandardLoadProfile, BdewLoadProfileTimeSeries]
    ],
    random: Option[RandomLoadProfileTimeSeries],
    loadProfileSources: Map[LoadProfile, LoadProfileSource[_, _]],
    maxValueMap: Map[LoadProfile, Option[MaxPower]],
    private[load] val valueProvider: Map[LoadProfile, ZonedDateTime => Option[
      AveragePower
    ]],
) {

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
    maxValueMap.get(loadProfile).flatten
}

object LoadProfileStore {

  type AveragePower = squants.Power
  type MaxPower = squants.Power

  /** Default standard load profile energy scaling
    */
  val defaultLoadProfileEnergyScaling: squants.Energy = KilowattHours(1000d)

  private val profileScalingMap: mutable.Map[LoadProfile, squants.Energy] =
    mutable.Map()
  private val maxValueMap: mutable.Map[LoadProfile, Option[MaxPower]] =
    mutable.Map()

  def apply(
      bdewLoadProfiles: Option[
        Map[BdewStandardLoadProfile, BdewLoadProfileTimeSeries]
      ],
      random: Option[RandomLoadProfileTimeSeries],
      loadProfileSources: Map[LoadProfile, LoadProfileSource[_, _]],
  ): LoadProfileStore = {

    val valueProvider
        : mutable.Map[LoadProfile, ZonedDateTime => Option[AveragePower]] =
      mutable.Map()

    // adds all bdew standard load profiles to the map
    bdewLoadProfiles.foreach { bdewProfiles =>
      bdewProfiles.foreach { case (profile, lpts) =>
        valueProvider.put(
          profile,
          time => convert(lpts.getValue(time).flatMap(_.getP)),
        )
        maxValueMap.put(profile, convert(lpts.maxPower))
      }
    }

    // adds the random load profile to the map
    random.foreach { lpts =>
      valueProvider.put(
        RandomLoadProfile.RANDOM_LOAD_PROFILE,
        time => convert(lpts.getValue(time).flatMap(_.getP)),
      )
      maxValueMap.put(
        RandomLoadProfile.RANDOM_LOAD_PROFILE,
        convert(lpts.maxPower),
      )
    }

    // adds all other load profiles to the map
    loadProfileSources.foreach { case (profile, lpts) =>
      valueProvider.put(
        profile,
        time => convert(lpts.getValue(time).flatMap(_.getP)),
      )
      maxValueMap.put(profile, convert(lpts.getTimeSeries.maxPower))
    }

    LoadProfileStore(
      bdewLoadProfiles,
      random,
      loadProfileSources,
      maxValueMap.toMap,
      valueProvider.toMap,
    )
  }

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
    maxValueMap.get(loadProfile).flatten

  /** Method for scaling the provided power value.
    * @param power
    *   given power
    * @param loadProfileEnergyScaling
    *   scaling factor used by the source
    * @return
    *   the scaled power value
    */
  def scala(
      power: squants.Power,
      loadProfileEnergyScaling: Energy,
  )(implicit tolerance: Energy = WattHours(1e-3)): squants.Power = if (
    defaultLoadProfileEnergyScaling ~= loadProfileEnergyScaling
  ) {
    power
  } else {
    power / loadProfileEnergyScaling * defaultLoadProfileEnergyScaling
  }

  /** Converts an optional [[ComparableQuantity]] power to an option for
    * [[squants.Power]].
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
}
