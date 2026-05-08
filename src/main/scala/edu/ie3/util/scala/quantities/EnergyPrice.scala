/*
 * © 2023-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.energy.{Energy, KilowattHours, MegawattHours}
import squants.market.{EUR, Money}
import squants.{Dimension, PrimaryUnit, SiUnit, UnitConverter, UnitOfMeasure}

import scala.util.Try

/** Represents the price of energy in currency EURO
  */
final class EnergyPrice private (
    override val value: Double,
    override val unit: EnergyPriceUnit,
) extends squants.Quantity[EnergyPrice] {

  override def dimension: EnergyPrice.type = EnergyPrice

  def *(that: Energy): Money = EUR(
    this.toEuroPerKilowattHour * that.toKilowattHours
  )

  def toEuroPerWattHour: Double = to(EuroPerWattHour)
  def toEuroPerKilowattHour: Double = to(EuroPerKilowattHour)
  def toEuroPerMegawattHour: Double = to(EuroPerMegawattHour)
}

object EnergyPrice extends Dimension[EnergyPrice] {
  def apply[A](n: A, unit: EnergyPriceUnit)(implicit num: Numeric[A]) =
    new EnergyPrice(num.toDouble(n), unit)
  def apply(value: Any): Try[EnergyPrice] = parse(value)
  override def name = "EnergyPrice"
  override def primaryUnit: EuroPerWattHour.type = EuroPerWattHour
  override def siUnit: EuroPerWattHour.type = EuroPerWattHour
  override def units: Set[UnitOfMeasure[EnergyPrice]] = Set(
    EuroPerWattHour,
    EuroPerKilowattHour,
    EuroPerMegawattHour,
  )
}

trait EnergyPriceUnit extends UnitOfMeasure[EnergyPrice] with UnitConverter {
  override def apply[A](n: A)(implicit num: Numeric[A]): EnergyPrice =
    EnergyPrice(n, this)
}

object EuroPerWattHour extends EnergyPriceUnit with PrimaryUnit with SiUnit {
  override val symbol: String = EUR.symbol + "/Wh"
}

object EuroPerKilowattHour extends EnergyPriceUnit {
  override val conversionFactor: Double = 1d / KilowattHours.conversionFactor
  override val symbol: String = EUR.symbol + "/kWh"
}

object EuroPerMegawattHour extends EnergyPriceUnit {
  override val conversionFactor: Double = 1d / MegawattHours.conversionFactor
  override val symbol: String = EUR.symbol + "/MWh"
}
