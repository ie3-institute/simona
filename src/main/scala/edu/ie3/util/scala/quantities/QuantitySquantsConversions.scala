/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.interfaces.Irradiance
import edu.ie3.util.scala.quantities
import squants.{Kelvin, Velocity}
import squants.motion.MetersPerSecond
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units
import javax.measure.quantity.{Speed, Temperature}

object QuantitySquantsConversions {

  implicit class IrradianceConversion(
      irradiance: ComparableQuantity[Irradiance]
  ) {
    def toSquants: quantities.Irradiance = WattsPerSquareMeter(
      irradiance
        .to(PowerSystemUnits.WATT_PER_SQUAREMETRE)
        .getValue
        .doubleValue()
    )
  }

  implicit class TemperatureConversion(temp: ComparableQuantity[Temperature]) {
    def toSquants: squants.thermal.Temperature = Kelvin(
      temp
        .to(Units.KELVIN)
        .getValue
        .doubleValue()
    )
  }

  implicit class WindVelocityConversion(windVel: ComparableQuantity[Speed]) {
    def toSquants: Velocity = MetersPerSecond(
      windVel
        .to(Units.METRE_PER_SECOND)
        .getValue
        .doubleValue()
    )
  }
}
