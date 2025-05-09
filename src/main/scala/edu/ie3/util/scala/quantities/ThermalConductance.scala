/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants._
import squants.energy.{WattHours, Watts}

import scala.util.Try

/** Represents the thermal conductance as the time rate of steady state heat
  * flow through a unit area of a material or construction induced by a unit
  * temperature difference between the body surfaces, in W/K.
  */
final class ThermalConductance private (
    val value: Double,
    val unit: ThermalConductanceUnit,
) extends Quantity[ThermalConductance] {

  def dimension: ThermalConductance.type = ThermalConductance

  /** Calculates the energy gain (temperatureOuter > temperatureInner) or energy
    * loss (temperatureOuter < temperatureInner) based on a given thermal
    * conductance and a temperature delta and the time duration.
    * @param temperatureInner
    *   Inner temperature of a medium
    * @param temperatureOuter
    *   Temperature outside the medium
    * @param time
    *   Time duration
    * @return
    */
  def calcThermalEnergyChange(
      temperatureInner: Temperature,
      temperatureOuter: Temperature,
      time: squants.Time,
  ): Energy = WattHours(
    this.toWattsPerKelvin * (temperatureInner.toKelvinScale - temperatureOuter.toKelvinScale) * time.toHours
  )

  def toWattsPerKelvin: Double = to(WattsPerKelvin)
}

object ThermalConductance extends Dimension[ThermalConductance] {
  def apply[A](n: A, unit: ThermalConductanceUnit)(implicit num: Numeric[A]) =
    new ThermalConductance(num.toDouble(n), unit)
  def apply(value: Any): Try[ThermalConductance] = parse(value)
  def name = "ThermalConductance"
  def primaryUnit: WattsPerKelvin.type = WattsPerKelvin
  def siUnit: WattsPerKelvin.type = WattsPerKelvin
  def units: Set[UnitOfMeasure[ThermalConductance]] = Set(
    WattsPerKelvin
  )
}

trait ThermalConductanceUnit
    extends UnitOfMeasure[ThermalConductance]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): ThermalConductance =
    ThermalConductance(n, this)
}

object WattsPerKelvin
    extends ThermalConductanceUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String = Watts.symbol + "/" + Kelvin.symbol
}
