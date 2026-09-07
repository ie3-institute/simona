/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.*
import squants.energy.Joules
import squants.space.CubicMeters

import scala.util.Try

/** Represents the thermal capacitance, in J/(m³*K).
  */
final class ThermalCapacitance private (
    val value: Double,
    val unit: ThermalCapacitanceUnit,
) extends Quantity[ThermalCapacitance] {

  def dimension: ThermalCapacitance.type = ThermalCapacitance

  def toJoulesPerCubicMeterKelvin: Double = to(JoulesPerCubicMeterKelvin)
}

object ThermalCapacitance extends Dimension[ThermalCapacitance] {
  def apply[A](n: A, unit: ThermalCapacitanceUnit)(implicit num: Numeric[A]) =
    new ThermalCapacitance(num.toDouble(n), unit)
  def apply(value: Any): Try[ThermalCapacitance] = parse(value)
  def name = "ThermalCapacitance"
  def primaryUnit: JoulesPerCubicMeterKelvin.type = JoulesPerCubicMeterKelvin
  def siUnit: JoulesPerCubicMeterKelvin.type = JoulesPerCubicMeterKelvin
  def units: Set[UnitOfMeasure[ThermalCapacitance]] = Set(
    JoulesPerCubicMeterKelvin
  )
}

trait ThermalCapacitanceUnit
    extends UnitOfMeasure[ThermalCapacitance]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): ThermalCapacitance =
    ThermalCapacitance(n, this)
}

object JoulesPerCubicMeterKelvin
    extends ThermalCapacitanceUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String =
    Joules.symbol + "/(" + CubicMeters.symbol + "*" + Kelvin.symbol + ")"
}