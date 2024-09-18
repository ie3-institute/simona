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
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.util.quantities.PowerSystemUnits
import squants.Power
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util.Optional
import scala.collection.mutable
import scala.jdk.OptionConverters.RichOptional

/** Container class that stores all loaded load profiles.
  * @param bdewLoadProfiles
  *   an option for [[BdewStandardLoadProfile]]s
  * @param random
  *   an option for a [[RandomLoadProfile]]
  * @param loadProfileSources
  *   all other sources
  * @param valueProvider
  *   map: [[LoadProfile]] to provider function
  */
final case class LoadProfileStore(
    bdewLoadProfiles: Option[
      Map[BdewStandardLoadProfile, BdewLoadProfileTimeSeries]
    ],
    random: Option[RandomLoadProfileTimeSeries],
    loadProfileSources: Map[LoadProfile, LoadProfileSource[_, _]],
    private val valueProvider: Map[LoadProfile, ZonedDateTime => Option[Power]],
) {

  /** Method to get the provider function for a given load profile
    * @param profile
    *   given load profile
    * @return
    *   the function
    */
  def getValueProvider(profile: LoadProfile): ZonedDateTime => Option[Power] =
    valueProvider
      .getOrElse(
        profile,
        throw new SourceException(
          s"Could not find the given load profile: $profile!"
        ),
      )
}

object LoadProfileStore {
  def apply(
      bdewLoadProfiles: Option[
        Map[BdewStandardLoadProfile, BdewLoadProfileTimeSeries]
      ],
      random: Option[RandomLoadProfileTimeSeries],
      loadProfileSources: Map[LoadProfile, LoadProfileSource[_, _]],
  ): LoadProfileStore = {

    val profileToFunctionMap
        : mutable.Map[LoadProfile, ZonedDateTime => Optional[PValue]] =
      mutable.Map()

    // adds all bdew standard load profiles to the map
    bdewLoadProfiles.foreach { bdewProfiles =>
      bdewProfiles.foreach { case (profile, lpts) =>
        profileToFunctionMap.put(profile, time => lpts.getValue(time))
      }
    }

    // adds the random load profile to the map
    random.foreach { lpts =>
      profileToFunctionMap.put(
        RandomLoadProfile.RANDOM_LOAD_PROFILE,
        time => lpts.getValue(time),
      )
    }

    // adds all other load profiles to the map
    loadProfileSources.foreach { case (profile, lpts) =>
      profileToFunctionMap.put(profile, time => lpts.getValue(time))
    }

    // converts the pvalue to squants.Power
    val allFunctions = profileToFunctionMap.map { case (profile, function) =>
      profile -> function.andThen(
        _.toScala.flatMap(
          _.getP
            .map(p =>
              Kilowatts(p.to(PowerSystemUnits.KILOWATT).getValue.doubleValue())
            )
            .toScala
        )
      )
    }.toMap

    LoadProfileStore(
      bdewLoadProfiles,
      random,
      loadProfileSources,
      allFunctions,
    )
  }
}
