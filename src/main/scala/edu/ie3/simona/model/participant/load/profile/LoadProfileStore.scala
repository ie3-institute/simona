/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import breeze.numerics.round
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.profile.{
  BdewStandardLoadProfile,
  StandardLoadProfile,
}
import edu.ie3.simona.model.participant.load.profile.LoadProfileStore.{
  initializeMaxConsumptionPerProfile,
  initializeTypeDayValues,
}
import edu.ie3.simona.model.participant.load.{DayType, profile}
import org.apache.commons.csv.CSVFormat
import squants.energy.{KilowattHours, Watts}

import java.io.{InputStreamReader, Reader}
import java.time.{Duration, ZonedDateTime}
import java.util
import scala.jdk.CollectionConverters._
import scala.math.pow

// needs to be imported for max function
import scala.math.Ordering.Double.IeeeOrdering

/** Storage for a collection of standard load profiles. It is assumed, that each
  * entry is given in W - which especially holds true for the standard load
  * profiles provided by the german Federal Association of the Energy and Water
  * Industry (Bundesverband der Energie- und Wasserwirtschaft - bdew).
  *
  * Access via: LoadProfileStore()
  */
class LoadProfileStore private (val reader: Reader) {
  private val profileMap: Map[LoadProfileKey, TypeDayProfile] =
    initializeTypeDayValues(reader)
  private val maxParamMap: Map[StandardLoadProfile, Double] =
    initializeMaxConsumptionPerProfile(
      profileMap
    )

  /** Returns the load profiles entry (average power consumption for the
    * following quarter-hour) for given time and load profile.
    *
    * @param time
    *   the requested time
    * @param loadProfile
    *   the requested load profile
    * @return
    *   a load in W
    */
  def entry(
      time: ZonedDateTime,
      loadProfile: StandardLoadProfile,
  ): squants.Power = {
    val key = LoadProfileKey(loadProfile, time)
    profileMap.get(key) match {
      case Some(typeDayValues) =>
        val quarterHourEnergy = typeDayValues.getQuarterHourEnergy(time)
        val load = loadProfile match {
          case BdewStandardLoadProfile.H0 =>
            /* For the residential average profile, a dynamization has to be taken into account */
            val t = time.getDayOfYear // leap years are ignored
            LoadProfileStore.dynamization(quarterHourEnergy, t)
          case _ => quarterHourEnergy
        }
        Watts(load)
      case None =>
        throw new RuntimeException(
          "Value for LoadProfileKey " + key.toString + " not found."
        )
    }
  }

  /** Returns the maximum average power consumption per quarter-hour for a given
    * load profile, calculated over all seasons and weekday types of given load
    * profile
    *
    * @param loadProfile
    *   the consumer type
    * @return
    *   the maximum load in W
    */
  def maxPower(
      loadProfile: StandardLoadProfile
  ): squants.Power = {
    maxParamMap.get(loadProfile) match {
      case Some(value) =>
        Watts(value)
      case None =>
        throw new RuntimeException(
          "Max value for ConsumerType " + loadProfile.toString + " not found"
        )
    }
  }
}

object LoadProfileStore extends LazyLogging {
  val resolution: Duration = Duration.ofMinutes(15)

  /** Default value store, that uses information from a file
    * 'standard_load_profiles.csv' placed in the resources folder of the project
    * / jar
    */
  private lazy val defaultStore = new LoadProfileStore(getDefaultReader)

  /** Default standard load profile energy scaling
    */
  val defaultLoadProfileEnergyScaling: squants.Energy = KilowattHours(1000d)

  /** Default entry point to get the default implementation with the provided
    * default standard load profiles
    *
    * @return
    *   Instance of [[LoadProfileStore]] with default load profiles
    */
  def apply(): LoadProfileStore = defaultStore

  /** Default entry point to get an instance of [[LoadProfileStore]] with
    * customized load profiles provided by a specific reader including the
    * files. For the default implementation with the provided default standard
    * load profiles use [[apply()]] above.
    *
    * @param reader
    *   the reader containing the information where the file with custom load
    *   profiles is located
    * @return
    *   instance of [[LoadProfileStore]] with custom load profiles
    */
  def apply(reader: Reader): LoadProfileStore = new LoadProfileStore(reader)

  /** Calculates the dynamization factor for given day of year. Cf. <a
    * href="https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf">
    * Anwendung der repräsentativen Lastprofile - Step by step</a> page 19
    *
    * @param load
    *   load value
    * @param t
    *   day of year (1-366)
    * @return
    *   dynamization factor
    */
  private def dynamization(load: Double, t: Int): Double = {
    val factor = (-3.92e-10 * pow(t, 4) + 3.2e-7 * pow(t, 3)
      - 7.02e-5 * pow(t, 2) + 2.1e-3 * t + 1.24)
    val rndFactor = round(factor * 1e4) / 1e4 // round to 4 decimal places
    round(load * rndFactor * 1e1) / 1e1 // rounded to 1 decimal place
  }

  /** Initializes all type day values by receiving values from provided reader.
    *
    * @param reader
    *   a reader that is providing load profile values from a CSV file
    */
  def initializeTypeDayValues(
      reader: Reader = getDefaultReader
  ): Map[LoadProfileKey, TypeDayProfile] = {
    val parser = CSVFormat.Builder
      .create()
      .setHeader()
      .setSkipHeaderRecord(true)
      .build()
      .parse(reader)
    // records list is an ArrayList
    val records = parser.getRecords

    val headerKeys: util.List[String] = parser.getHeaderNames

    // skip last column "quarter-hour"
    (for (i <- Range(0, headerKeys.size() - 1)) yield {
      val headerKey = headerKeys.get(i)
      val profileKey = LoadProfileKey(headerKey)

      val values: Array[Double] =
        records.asScala.map(record => record.get(headerKey).toDouble).toArray

      profileKey -> TypeDayProfile(values)
    }).toMap
  }

  /** Initializes a mapping from standard load profile to highest occurring
    * energy consumption throughout the whole year
    *
    * @param profileMap
    *   mapping from standard load profile keys to type day values
    * @return
    *   maximum energy consumption within a year for each known standard load
    *   profile
    */
  private def initializeMaxConsumptionPerProfile(
      profileMap: Map[LoadProfileKey, TypeDayProfile]
  ): Map[StandardLoadProfile, Double] = {
    /* Get all standard load profiles, that have been put into the store */
    val knownLoadProfiles: Set[StandardLoadProfile] =
      profileMap.keySet.map(key => key.standardLoadProfile)

    knownLoadProfiles
      .flatMap(loadProfile => {
        (loadProfile match {
          case BdewStandardLoadProfile.H0 =>
            // max load for h0 is expected to be exclusively found in winter,
            // thus we only search there.
            DayType.values
              .map(dayType => {
                val key =
                  profile.LoadProfileKey(loadProfile, Season.winter, dayType)
                // maximum dynamization factor is on day 366 (leap year) or day 365 (regular year).
                // The difference between day 365 and day 366 is negligible, thus pick 366
                profileMap
                  .get(key)
                  .map(typeDay => dynamization(typeDay.getMaxValue, 366))
                  .getOrElse(0d)
              })
              .maxOption
          case _ =>
            (for (season <- Season.values; dayType <- DayType.values) yield {
              val key = profile.LoadProfileKey(loadProfile, season, dayType)
              profileMap.get(key) match {
                case Some(value) => Option(value.getMaxValue)
                case None        => None
              }
            }).flatten.maxOption
        }).map(maxConsumption => loadProfile -> maxConsumption)
      })
      .toMap
  }

  /** @return
    *   A reader pointing to the default load profile location
    */
  private def getDefaultReader: Reader = {
    logger.info(
      "Loading default load profile file 'standard_load_profiles.csv' from jar."
    )
    new InputStreamReader(
      this.getClass.getResourceAsStream("/load/standard_load_profiles.csv")
    )
  }
}
