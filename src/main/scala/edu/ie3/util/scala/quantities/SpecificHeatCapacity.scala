/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants._
import squants.energy.KilowattHours

import scala.util.Try

/** Represents the capacity of some substance or storage medium to hold thermal
  * energy.
  *
  * In kWh / (K * m³)
  *
  * Based on [[squants.thermal.ThermalCapacity]] by garyKeorkunian
  */
final class SpecificHeatCapacity private (
    val value: Double,
    val unit: SpecificHeatCapacityUnit,
) extends Quantity[SpecificHeatCapacity] {

  def dimension: SpecificHeatCapacity.type = SpecificHeatCapacity

  /** Calculates the EnergyDensity of a medium with a given specific heat
    * capacity based on the temperature delta.
    * @param temperatureA
    *   First temperature
    * @param temperatureB
    *   Second temperature
    * @return
    *   Density of the energy stored in the medium
    */
  def calcEnergyDensity(
      temperatureA: Temperature,
      temperatureB: Temperature,
  ): EnergyDensity =
    KilowattHoursPerCubicMeter(
      this.toKilowattHoursPerKelvinCubicMeters * math.abs(
        temperatureA.toKelvinScale - temperatureB.toKelvinScale
      )
    )

  /** Calculates the Energy of a medium with a given specific heat capacity
    * based on the temperature delta and it's volume.
    * @param temperatureA
    *   First temperature of the medium (e.g. inlet temperature)
    * @param temperatureB
    *   Second temperature of the medium (e.g. outlet temperature)
    * @param volume
    *   Volume of the medium
    * @return
    *   Energy stored in the medium
    */

  def calcEnergy(
      temperatureA: Temperature,
      temperatureB: Temperature,
      volume: Volume,
  ): Energy =
    KilowattHours(
      this.toKilowattHoursPerKelvinCubicMeters * math.abs(
        temperatureA.toKelvinScale - temperatureB.toKelvinScale
      ) * volume.toCubicMeters
    )

  def toKilowattHoursPerKelvinCubicMeters: Double = to(
    KilowattHoursPerKelvinCubicMeters
  )
}

object SpecificHeatCapacity extends Dimension[SpecificHeatCapacity] {
  def apply[A](n: A, unit: SpecificHeatCapacityUnit)(implicit num: Numeric[A]) =
    new SpecificHeatCapacity(num.toDouble(n), unit)
  def apply(value: Any): Try[SpecificHeatCapacity] = parse(value)
  def name = "SpecificHeatCapacity"
  def primaryUnit: KilowattHoursPerKelvinCubicMeters.type =
    KilowattHoursPerKelvinCubicMeters
  def siUnit: KilowattHoursPerKelvinCubicMeters.type =
    KilowattHoursPerKelvinCubicMeters
  def units: Set[UnitOfMeasure[SpecificHeatCapacity]] = Set(
    KilowattHoursPerKelvinCubicMeters
  )
}

trait SpecificHeatCapacityUnit
    extends UnitOfMeasure[SpecificHeatCapacity]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): SpecificHeatCapacity =
    SpecificHeatCapacity(n, this)
}

object KilowattHoursPerKelvinCubicMeters
    extends SpecificHeatCapacityUnit
    with PrimaryUnit
    with SiUnit {
  def symbol = "kWh/Km³"
}
