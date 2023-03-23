/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants._
import squants.energy.WattHours
import squants.space.CubicMeters

/** Represents the EnergyDesity some substance or storage medium.
  *
  * In Wh/m³
  *
  * Based on [[squants.energy.EnergyDensity]] by garyKeorkunian
  */
final class EnergyDensity private (
    val value: Double,
    val unit: EnergyDensityUnit
) extends Quantity[EnergyDensity] {

  def dimension = EnergyDensity

  def *(that: Volume): Energy = WattHours(
    this.toWattHoursPerCubicMeter * that.toCubicMeters
  )

  def toWattHoursPerCubicMeter = to(WattHoursPerCubicMeter)
}

object EnergyDensity extends Dimension[EnergyDensity] {
  def apply[A](n: A, unit: EnergyDensityUnit)(implicit num: Numeric[A]) =
    new EnergyDensity(num.toDouble(n), unit)
  def apply(value: Any) = parse(value)
  def name = "EnergyDensity"
  def primaryUnit = WattHoursPerCubicMeter
  def siUnit = WattHoursPerCubicMeter
  def units = Set(WattHoursPerCubicMeter)
}

trait EnergyDensityUnit
    extends UnitOfMeasure[EnergyDensity]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = EnergyDensity(n, this)
}

object WattHoursPerCubicMeter
    extends EnergyDensityUnit
    with PrimaryUnit
    with SiUnit {
  val symbol = WattHours.symbol + "/" + CubicMeters.symbol
}

object EnergyDensityConversions {
  lazy val wattHoursPerCubicMeter = WattHoursPerCubicMeter(1)

  implicit class EnergyDensityConversions[A](n: A)(implicit num: Numeric[A]) {
    def wattHoursPerCubicMeter = WattHoursPerCubicMeter(n)
  }

  implicit object EnergyDensityNumeric
      extends AbstractQuantityNumeric[EnergyDensity](EnergyDensity.primaryUnit)
}
