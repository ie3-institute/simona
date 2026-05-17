/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.*
import squants.energy.Joules

import scala.util.Try

/** Represents the thermal capacitance, in J/(m*K). //FIXME J/(m³*K)?
  */
final class ThermalCapacitance private (
    val value: Double,
    val unit: ThermalCapacitanceUnit,
) extends Quantity[ThermalCapacitance] {

  def dimension: ThermalCapacitance.type = ThermalCapacitance

  def toJoulesPerMeterKelvin: Double = to(JoulesPerMeterKelvin)
}

object ThermalCapacitance extends Dimension[ThermalCapacitance] {
  def apply[A](n: A, unit: ThermalCapacitanceUnit)(implicit num: Numeric[A]) =
    new ThermalCapacitance(num.toDouble(n), unit)
  def apply(value: Any): Try[ThermalCapacitance] = parse(value)
  def name = "ThermalCapacitance"
  def primaryUnit: JoulesPerMeterKelvin.type = JoulesPerMeterKelvin
  def siUnit: JoulesPerMeterKelvin.type = JoulesPerMeterKelvin
  def units: Set[UnitOfMeasure[ThermalCapacitance]] = Set(
    JoulesPerMeterKelvin
  )
}

trait ThermalCapacitanceUnit
    extends UnitOfMeasure[ThermalCapacitance]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): ThermalCapacitance =
    ThermalCapacitance(n, this)
}

object JoulesPerMeterKelvin
    extends ThermalCapacitanceUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String =
    Joules.symbol + "/(" + Meters.symbol + "*" + Kelvin.symbol + ")"
}
