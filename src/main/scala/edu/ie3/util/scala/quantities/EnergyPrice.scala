/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.energy.Energy
import squants.market.{EUR, Money}
import squants.{Dimension, PrimaryUnit, SiUnit, UnitConverter, UnitOfMeasure}

import scala.util.Try

/** Represents the price of one Kilowatthour in currency EURO
  */

final class EnergyPrice private (
    val value: Double,
    val unit: EnergyPriceUnit,
) extends squants.Quantity[EnergyPrice] {

  def dimension: EnergyPrice.type = EnergyPrice

  def *(that: Energy): Money = EUR(
    this.toEuroPerKilowattHour * that.toKilowattHours
  )

  def toEuroPerKilowattHour: Double = to(EuroPerKilowatthour)

}

object EnergyPrice extends Dimension[EnergyPrice] {
  def apply[A](n: A, unit: EnergyPriceUnit)(implicit num: Numeric[A]) =
    new EnergyPrice(num.toDouble(n), unit)
  def apply(value: Any): Try[EnergyPrice] = parse(value)
  def name = "EnergyPrice"
  def primaryUnit: EuroPerKilowatthour.type = EuroPerKilowatthour
  def siUnit: EuroPerKilowatthour.type = EuroPerKilowatthour
  def units: Set[UnitOfMeasure[EnergyPrice]] = Set(
    EuroPerKilowatthour
  )
}

trait EnergyPriceUnit extends UnitOfMeasure[EnergyPrice] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): EnergyPrice =
    EnergyPrice(n, this)
}

object EuroPerKilowatthour
    extends EnergyPriceUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String = EUR.symbol + "/kWh"
}
