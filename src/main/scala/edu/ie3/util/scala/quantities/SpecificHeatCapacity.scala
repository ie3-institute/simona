/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants._
import squants.energy.WattHours

/** Represents the capacity of some substance or storage medium to hold thermal
  * energy.
  *
  * In kWh / (K * m³)
  *
  * Based on [[squants.thermal.ThermalCapacity]] by garyKeorkunian
  */
final class SpecificHeatCapacity private (
    val value: Double,
    val unit: SpecificHeatCapacityUnit
) extends Quantity[SpecificHeatCapacity] {

  def dimension = SpecificHeatCapacity

  def *(that: Temperature): EnergyDensity = WattHoursPerCubicMeter(
    this.toWattHoursPerKelvinCubicMeters * that.toCelsiusScale
  )
  def multiply(temperature: Temperature, volume: Volume): Energy = WattHours(
    this.toWattHoursPerKelvinCubicMeters * 1000 * temperature.toCelsiusScale * volume.toCubicMeters
  )

  def toWattHoursPerKelvinCubicMeters = to(WattHoursPerKelvinCubicMeters)
}

object SpecificHeatCapacity extends Dimension[SpecificHeatCapacity] {
  def apply[A](n: A, unit: SpecificHeatCapacityUnit)(implicit num: Numeric[A]) =
    new SpecificHeatCapacity(num.toDouble(n), unit)
  def apply(value: Any) = parse(value)
  def name = "SpecificHeatCapacity"
  def primaryUnit = WattHoursPerKelvinCubicMeters
  def siUnit = WattHoursPerKelvinCubicMeters
  def units = Set(WattHoursPerKelvinCubicMeters)
}

trait SpecificHeatCapacityUnit
    extends UnitOfMeasure[SpecificHeatCapacity]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = SpecificHeatCapacity(n, this)
}

object WattHoursPerKelvinCubicMeters
    extends SpecificHeatCapacityUnit
    with PrimaryUnit
    with SiUnit {
  def symbol = "kWh/K*m³"
}

object ThermalCapacityConversions {
  lazy val wattHoursPerKelvinCubicMeters = WattHoursPerKelvinCubicMeters(1)

  implicit class SpecificHeatCapacityConversions[A](n: A)(implicit
      num: Numeric[A]
  ) {
    def wattHoursPerKelvinCubicMeters = WattHoursPerKelvinCubicMeters(n)
  }

  implicit object SpecificHeatCapacityNumeric
      extends AbstractQuantityNumeric[SpecificHeatCapacity](
        SpecificHeatCapacity.primaryUnit
      )
}
