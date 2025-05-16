/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.*
import squants.energy.{KilowattHours, WattHours}
import squants.space.CubicMeters

import scala.util.Try

/** Represents the EnergyDesity some substance or storage medium.
  *
  * In kWh/m³
  *
  * Based on [[squants.energy.EnergyDensity]] by garyKeorkunian
  */
final class EnergyDensity private (
    val value: Double,
    val unit: EnergyDensityUnit,
) extends Quantity[EnergyDensity] {

  def dimension: EnergyDensity.type = EnergyDensity

  def *(that: Volume): Energy = KilowattHours(
    this.toKilowattHoursPerCubicMeter * that.toCubicMeters
  )

  def toKilowattHoursPerCubicMeter: Double = to(KilowattHoursPerCubicMeter)

}

object EnergyDensity extends Dimension[EnergyDensity] {
  def apply[A](n: A, unit: EnergyDensityUnit)(implicit num: Numeric[A]) =
    new EnergyDensity(num.toDouble(n), unit)
  def apply(value: Any): Try[EnergyDensity] = parse(value)
  def name = "EnergyDensity"
  def primaryUnit: KilowattHoursPerCubicMeter.type = KilowattHoursPerCubicMeter
  def siUnit: KilowattHoursPerCubicMeter.type = KilowattHoursPerCubicMeter
  def units: Set[UnitOfMeasure[EnergyDensity]] = Set(KilowattHoursPerCubicMeter)
}

trait EnergyDensityUnit
    extends UnitOfMeasure[EnergyDensity]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): EnergyDensity =
    EnergyDensity(n, this)
}

object KilowattHoursPerCubicMeter
    extends EnergyDensityUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String = "k" + WattHours.symbol + "/" + CubicMeters.symbol

}
