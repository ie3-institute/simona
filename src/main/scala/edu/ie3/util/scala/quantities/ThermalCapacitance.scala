/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.*
import squants.energy.Joules
import squants.space.CubicMeters
import squants.energy.KilowattHours

import scala.util.Try

/** Represents the thermal capacitance, in J/(m³*K).
  */
final class ThermalCapacitance private (
    val value: Double,
    val unit: ThermalCapacitanceUnit,
) extends Quantity[ThermalCapacitance] {

  def dimension: ThermalCapacitance.type = ThermalCapacitance

  def toJoulesPerCubicMeterKelvin: Double = to(JoulesPerCubicMeterKelvin)

  /** Calculates the EnergyDensity of a medium with a given thermal capacitance
    * based on the temperature delta. Returned energy density is in kWh/m³.
    */
  def calcEnergyDensity(
      temperatureA: Temperature,
      temperatureB: Temperature,
  ): EnergyDensity =
    KilowattHoursPerCubicMeter(
      this.toJoulesPerCubicMeterKelvin * math.abs(
        temperatureA.toKelvinScale - temperatureB.toKelvinScale
      ) / 3600000.0
    )

  /** Calculates the Energy of a medium with a given thermal capacitance based
    * on the temperature delta, and its volume. Returned energy is in kWh.
    */
  def calcEnergy(
      temperatureA: Temperature,
      temperatureB: Temperature,
      volume: Volume,
  ): Energy =
    KilowattHours(
      this.toJoulesPerCubicMeterKelvin * math.abs(
        temperatureA.toKelvinScale - temperatureB.toKelvinScale
      ) * volume.toCubicMeters / 3600000.0
    )

  /** Returns the value of this quantity in kWh / (K * m³) */
  def toKilowattHoursPerKelvinCubicMeters: Double =
    this.toJoulesPerCubicMeterKelvin / 3600000.0
}

object ThermalCapacitance extends Dimension[ThermalCapacitance] {
  def apply[A](n: A, unit: ThermalCapacitanceUnit)(implicit num: Numeric[A]) =
    new ThermalCapacitance(num.toDouble(n), unit)
  def apply(value: Any): Try[ThermalCapacitance] = parse(value)
  def name = "ThermalCapacitance"
  def primaryUnit: JoulesPerCubicMeterKelvin.type = JoulesPerCubicMeterKelvin
  def siUnit: JoulesPerCubicMeterKelvin.type = JoulesPerCubicMeterKelvin
  def units: Set[UnitOfMeasure[ThermalCapacitance]] = Set(
    JoulesPerCubicMeterKelvin,
    KilowattHoursPerCubicMeterKelvin,
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

object KilowattHoursPerCubicMeterKelvin
    extends ThermalCapacitanceUnit
    with SiUnit {
  val conversionFactor: Double = 3600000.0
  val symbol: String = "kWh/m³K"
}
