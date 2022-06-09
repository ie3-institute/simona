/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile.temperature

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.profile.TemperatureDependantLoadProfile
import edu.ie3.simona.model.participant.load.profile.LoadProfileKey
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.Temperature

case class TemperatureDependantLoadProfileKey(
    temperatureDependantLoadProfile: TemperatureDependantLoadProfile,
    averageTemperature: ComparableQuantity[Temperature]
) extends LoadProfileKey

object TemperatureDependantLoadProfileKey {

  def apply(headerKey: String): TemperatureDependantLoadProfileKey = {
    val regex = "([a-z]+[0-9]*)((?<=)_-?[0-9]+)".r

    headerKey match {
      case regex(loadProfileId, avgTemperature) =>
        val temperature = Integer.valueOf(avgTemperature.stripPrefix("_"))
        TemperatureDependantLoadProfileKey(
          TemperatureDependantLoadProfile.parse(loadProfileId),
          Quantities.getQuantity(temperature, StandardUnits.TEMPERATURE)
        )
      case _ =>
        throw new RuntimeException(
          s"Provided load profile header key $headerKey is malformed. It has to be of the form ${regex.pattern} e.g. 'ep1_10'."
        )
    }
  }
}
