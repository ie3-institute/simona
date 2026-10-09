/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.*
import squants.energy.Watts

import scala.util.Try

/** Represents the thermal resistivity, in K*m/W.
  */
final class ThermalResistivity private (
    val value: Double,
    val unit: ThermalResistivityUnit,
) extends Quantity[ThermalResistivity] {

  def dimension: ThermalResistivity.type = ThermalResistivity

  def toKelvinMetersPerWatt: Double = to(KelvinMetersPerWatt)
}

object ThermalResistivity extends Dimension[ThermalResistivity] {
  def apply[A](n: A, unit: ThermalResistivityUnit)(implicit num: Numeric[A]) =
    new ThermalResistivity(num.toDouble(n), unit)
  def apply(value: Any): Try[ThermalResistivity] = parse(value)
  def name = "ThermalResistivity"
  def primaryUnit: KelvinMetersPerWatt.type = KelvinMetersPerWatt
  def siUnit: KelvinMetersPerWatt.type = KelvinMetersPerWatt
  def units: Set[UnitOfMeasure[ThermalResistivity]] = Set(
    KelvinMetersPerWatt
  )
}

trait ThermalResistivityUnit
    extends UnitOfMeasure[ThermalResistivity]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): ThermalResistivity =
    ThermalResistivity(n, this)
}

object KelvinMetersPerWatt
    extends ThermalResistivityUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String =
    "(" + Kelvin.symbol + "*" + Meters.symbol + ")/" + Watts.symbol
}
