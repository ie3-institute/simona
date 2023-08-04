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
    val unit: SpecificHeatCapacityUnit
) extends Quantity[SpecificHeatCapacity] {

  def dimension: SpecificHeatCapacity.type = SpecificHeatCapacity

  def *(temperatureA: Temperature, temperatureB: Temperature): EnergyDensity =
    KilowattHoursPerCubicMeter(
      this.toKilowattHoursPerKelvinCubicMeters * math.abs(
        temperatureA.toKelvinScale - temperatureB.toKelvinScale
      )
    )

  def multiply(
      temperatureA: Temperature,
      temperatureB: Temperature,
      volume: Volume
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
