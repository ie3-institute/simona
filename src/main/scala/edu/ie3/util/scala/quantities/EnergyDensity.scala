/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants._
import squants.energy.WattHours
import squants.space.CubicMeters

import scala.util.Try

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

  def dimension: EnergyDensity.type = EnergyDensity

  def *(that: Volume): Energy = WattHours(
    this.toWattHoursPerCubicMeter * that.toCubicMeters
  )

  private def toWattHoursPerCubicMeter: Double = to(WattHoursPerCubicMeter)
}

object EnergyDensity extends Dimension[EnergyDensity] {
  def apply[A](n: A, unit: EnergyDensityUnit)(implicit num: Numeric[A]) =
    new EnergyDensity(num.toDouble(n), unit)
  def apply(value: Any): Try[EnergyDensity] = parse(value)
  def name = "EnergyDensity"
  def primaryUnit: WattHoursPerCubicMeter.type = WattHoursPerCubicMeter
  def siUnit: WattHoursPerCubicMeter.type = WattHoursPerCubicMeter
  def units: Set[UnitOfMeasure[EnergyDensity]] = Set(WattHoursPerCubicMeter)
}

trait EnergyDensityUnit
    extends UnitOfMeasure[EnergyDensity]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): EnergyDensity = EnergyDensity(n, this)
}

object WattHoursPerCubicMeter
    extends EnergyDensityUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String = WattHours.symbol + "/" + CubicMeters.symbol
}

object EnergyDensityConversions {
  lazy val wattHoursPerCubicMeter: EnergyDensity = WattHoursPerCubicMeter(1)

  implicit class EnergyDensityConversions[A](n: A)(implicit num: Numeric[A]) {
    def wattHoursPerCubicMeter: EnergyDensity = WattHoursPerCubicMeter(n)
  }

  implicit object EnergyDensityNumeric
      extends AbstractQuantityNumeric[EnergyDensity](EnergyDensity.primaryUnit)
}
