/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.profile

import edu.ie3.datamodel.models.profile.{
  NbwTemperatureDependantLoadProfile,
  TemperatureDependantLoadProfile
}
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Temperature

case class TemperatureDependantLoadProfileKey(
    temperatureDependantLoadProfile: TemperatureDependantLoadProfile,
    tmz: Int
) extends LoadProfileKey

object TemperatureDependantLoadProfileKey {
  def apply(
      temperatureDependantLoadProfile: TemperatureDependantLoadProfile,
      averageTemperature: ComparableQuantity[Temperature]
  ): TemperatureDependantLoadProfileKey = {???}

  object NbwLoadProfileConfiguration {
    NbwTemperatureDependantLoadProfile
  }

  final case class TemperatureDependantLoadProfileParams(
      referenceTemperature: ComparableQuantity[Temperature],
      minTemperature: ComparableQuantity[Temperature],

  )
}
