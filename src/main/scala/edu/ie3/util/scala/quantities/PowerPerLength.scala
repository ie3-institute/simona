/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.{
  AbstractQuantityNumeric,
  Dimension,
  PrimaryUnit,
  Quantity,
  SiUnit,
  UnitConverter,
  UnitOfMeasure,
}
import squants.energy.{Power, Watts}
import squants.space.Length

/** Represents the power per unit of length, e.g. the thermal losses of a cable
  * expressed per meter of cable length.
  *
  * In W/m
  *
  * Based on [[squants.energy.Power]] by garyKeorkunian
  */
final class PowerPerLength private (
    val value: Double,
    val unit: PowerPerLengthUnit,
) extends Quantity[PowerPerLength] {

  def dimension: PowerPerLength.type = PowerPerLength

  def toWattsPerMeter: Double = to(WattsPerMeter)

  def *(that: Length): Power = Watts(
    this.toWattsPerMeter * that.toMeters
  )
}

object PowerPerLength extends Dimension[PowerPerLength] {
  def apply[A](n: A, unit: PowerPerLengthUnit)(implicit
      num: Numeric[A]
  ) =
    new PowerPerLength(num.toDouble(n), unit)
  def apply(value: Any) = parse(value)
  def name: String = "PowerPerLength"
  def primaryUnit: WattsPerMeter.type = WattsPerMeter
  def siUnit: WattsPerMeter.type = WattsPerMeter
  def units: Set[UnitOfMeasure[PowerPerLength]] = Set(WattsPerMeter)
}

trait PowerPerLengthUnit
    extends UnitOfMeasure[PowerPerLength]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) =
    PowerPerLength(n, this)
}

object WattsPerMeter extends PowerPerLengthUnit with PrimaryUnit with SiUnit {
  val symbol = "W/m"
}

object PowerPerLengthConversions {
  lazy val wattPerMeter: PowerPerLength = WattsPerMeter(1)

  implicit class PowerPerLengthConversions[A](n: A)(implicit
      num: Numeric[A]
  ) {
    def wattsPerMeter: PowerPerLength = WattsPerMeter(n)
  }

  implicit object PowerPerLengthNumeric
      extends AbstractQuantityNumeric[PowerPerLength](
        PowerPerLength.primaryUnit
      )
}
