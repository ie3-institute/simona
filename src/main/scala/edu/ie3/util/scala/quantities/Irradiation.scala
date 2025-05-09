/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants._
import squants.energy.WattHours
import squants.space.SquareMeters

import scala.util.Try

/** Introduces Irradiation as Squants
  */
final class Irradiation private (val value: Double, val unit: IrradiationUnit)
    extends Quantity[Irradiation] {

  def dimension: Irradiation.type = Irradiation

  def *(that: Area): Energy = WattHours(
    this.toWattHoursPerSquareMeter * that.toSquareMeters
  )
  // the Hours(1).toSeconds is to convert watt-hours to watt-seconds which
  // isn't a normal supported type in Squants

  def /(that: Time): Irradiance = WattsPerSquareMeter(
    this.toWattHoursPerSquareMeter / that.toHours
  )

  def toWattHoursPerSquareMeter: Double = to(WattHoursPerSquareMeter)
}

object Irradiation extends Dimension[Irradiation] {
  def apply[A](n: A, unit: IrradiationUnit)(implicit num: Numeric[A]) =
    new Irradiation(num.toDouble(n), unit)
  def apply(value: Any): Try[Irradiation] = parse(value)
  def name = "Irradiation"
  def primaryUnit: WattHoursPerSquareMeter.type = WattHoursPerSquareMeter
  def siUnit: WattHoursPerSquareMeter.type = WattHoursPerSquareMeter
  def units: Set[UnitOfMeasure[Irradiation]] = Set(WattHoursPerSquareMeter)
}

trait IrradiationUnit extends UnitOfMeasure[Irradiation] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): Irradiation =
    Irradiation(n, this)
}

object WattHoursPerSquareMeter
    extends IrradiationUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String = WattHours.symbol + "/" + SquareMeters.symbol
}

object IrradiationConversions {
  lazy val wattHoursPerSquareMeter: Irradiation = WattHoursPerSquareMeter(1)

  implicit class IrradiationConversions[A](n: A)(implicit num: Numeric[A]) {
    def wattHoursPerSquareMeter: Irradiation = WattHoursPerSquareMeter(n)
  }

  implicit object IrradiationNumeric
      extends AbstractQuantityNumeric[Irradiation](Irradiation.primaryUnit)
}
