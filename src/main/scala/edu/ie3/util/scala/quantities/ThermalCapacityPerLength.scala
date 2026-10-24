/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.energy.{Joules, KilowattHours}
import squants.space.{Length, Meters}
import squants.thermal.{JoulesPerKelvin, Kelvin, ThermalCapacity}
import squants.*

import scala.util.Try

/** Represents the thermal capacity per unit of length, in J/(m*K).
  */
final class ThermalCapacityPerLength private (
    val value: Double,
    val unit: ThermalCapacityPerLengthUnit,
) extends Quantity[ThermalCapacityPerLength] {

  def dimension: ThermalCapacityPerLength.type = ThermalCapacityPerLength

  def toJoulesPerMeterKelvin: Double = to(JoulesPerMeterKelvin)

  /** Calculates the Energy of a medium with a given thermal capacity per length
    * based on the temperature delta, and its length. Returned energy is in kWh.
    */
  def calcEnergy(
      temperatureA: Temperature,
      temperatureB: Temperature,
      length: Length,
  ): Energy =
    KilowattHours(
      this.toJoulesPerMeterKelvin * math.abs(
        temperatureA.toKelvinScale - temperatureB.toKelvinScale
      ) * length.toMeters / 3600000.0
    )

  /** Multiplies this thermal capacity per length (J/(m*K)) with a given length.
    *
    * @param length
    *   The length of the medium.
    * @return
    *   The resulting thermal capacity in J/K.
    */
  def *(that: Length): ThermalCapacity =
    JoulesPerKelvin(this.toJoulesPerMeterKelvin * that.toMeters)
}

object ThermalCapacityPerLength extends Dimension[ThermalCapacityPerLength] {
  def apply[A](n: A, unit: ThermalCapacityPerLengthUnit)(implicit
      num: Numeric[A]
  ) =
    new ThermalCapacityPerLength(num.toDouble(n), unit)
  def apply(value: Any): Try[ThermalCapacityPerLength] = parse(value)
  def name = "ThermalCapacityPerLength"
  def primaryUnit: JoulesPerMeterKelvin.type = JoulesPerMeterKelvin
  def siUnit: JoulesPerMeterKelvin.type = JoulesPerMeterKelvin
  def units: Set[UnitOfMeasure[ThermalCapacityPerLength]] = Set(
    JoulesPerMeterKelvin,
    KilowattHoursPerMeterKelvin,
  )
}

trait ThermalCapacityPerLengthUnit
    extends UnitOfMeasure[ThermalCapacityPerLength]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): ThermalCapacityPerLength =
    ThermalCapacityPerLength(n, this)
}

object JoulesPerMeterKelvin
    extends ThermalCapacityPerLengthUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String =
    Joules.symbol + "/(" + Meters.symbol + Kelvin.symbol + ")"
}

object KilowattHoursPerMeterKelvin
    extends ThermalCapacityPerLengthUnit
    with SiUnit {
  val conversionFactor: Double = 3600000.0
  val symbol: String =
    KilowattHours.symbol + "/(" + Meters.symbol + Kelvin.symbol + ")"
}
