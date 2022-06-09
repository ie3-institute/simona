/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import breeze.numerics.round

import java.time.ZonedDateTime
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.profile.{
  BdewStandardLoadProfile,
  LoadProfile,
  StandardLoadProfile,
  TemperatureDependantLoadProfile
}
import edu.ie3.simona.model.participant.load.{DayType, profile}
import edu.ie3.simona.model.participant.load.profile.StandardLoadProfileStore.{
  dynamization,
  getDefaultReader
}
import org.apache.commons.csv.CSVFormat

import javax.measure.quantity.Power
import tech.units.indriya.ComparableQuantity

import java.io.Reader
import java.util
import scala.jdk.CollectionConverters.CollectionHasAsScala
import scala.math.pow

/** Storage for a collection of standard load profiles. It is assumed, that each
  * entry is given in W - which especially holds true for the standard load
  * profiles provided by the german Federal Association of the Energy and Water
  * Industry (Bundesverband der Energie- und Wasserwirtschaft - bdew).
  *
  * Access via: LoadProfileStore()
  */
abstract class LoadProfileStore[P <: LoadProfile] extends LazyLogging {

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
  def entry(
      time: ZonedDateTime,
      loadProfile: P
  ): ComparableQuantity[Power]

  /** Returns the maximum average power consumption per quarter hour for a given
    * load profile, calculated over all seasons and weekday types of given load
    * profile
    *
    * @param loadProfile
    *   the consumer type
    * @return
    *   the maximum load in W
    */
  def maxPower(
      loadProfile: P
  ): ComparableQuantity[Power]

}

object LoadProfileStore {

  /** Initializes all type day values by receiving values from provided reader.
    *
    * @param reader
    *   a reader that is providing load profile values from a CSV file
    */
  def initializeTypeDayValues[K <: LoadProfileKey](
      reader: Reader,
      profileKeyConstructor: String => K
  ): Map[K, TypeDayProfile] = {
    val parser = CSVFormat.Builder
      .create()
      .setHeader()
      .setSkipHeaderRecord(true)
      .build()
      .parse(reader)
    // records list is an ArrayList
    val records = parser.getRecords

    val headerKeys: util.List[String] = parser.getHeaderNames

    // skip last column "quarter hour"
    (for (i <- Range(0, headerKeys.size() - 1)) yield {
      val headerKey = headerKeys.get(i)
      val profileKey = profileKeyConstructor(headerKey)

      val values: Array[Double] =
        records.asScala.map(record => record.get(headerKey).toDouble).toArray

      profileKey -> TypeDayProfile(values)
    }).toMap
  }

}
