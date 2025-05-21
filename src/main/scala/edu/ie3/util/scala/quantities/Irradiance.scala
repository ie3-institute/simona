/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.*
import squants.energy.{WattHours, Watts}
import squants.radio.{AreaTime, BecquerelsPerSquareMeterSecond, ParticleFlux}
import squants.space.SquareMeters
import squants.time.Hours

import scala.util.Try

/** Originally from squants.radio.Irradiance
  *
  * @author
  *   garyKeorkunian
  * @since 0.1
  *
  * @param value
  *   Double
  */
final class Irradiance private (val value: Double, val unit: IrradianceUnit)
    extends Quantity[Irradiance] {

  def dimension: Irradiance.type = Irradiance

  def *(that: Area): Power = Watts(
    this.toWattsPerSquareMeter * that.toSquareMeters
  )
  // the Hours(1).toSeconds is to convert watt-hours to watt-seconds which
  // isn't a normal supported type in Squants
  def *(that: AreaTime): Energy = WattHours(
    this.toWattsPerSquareMeter * that.toSquareMeterSeconds / Hours(1).toSeconds
  )

  def *(that: squants.Time): Irradiation = WattHoursPerSquareMeter(
    this.toWattsPerSquareMeter * that.toSeconds / Hours(1).toSeconds
  )

  def /(that: Energy): ParticleFlux = BecquerelsPerSquareMeterSecond(
    toWattsPerSquareMeter / (that.toWattHours * Hours(1).toSeconds)
  )
  def /(that: ParticleFlux): Energy = WattHours(
    (toWattsPerSquareMeter / that.toBecquerelsPerSquareMeterSecond) /
      Hours(1).toSeconds
  )

  def toWattsPerSquareMeter: Double = to(WattsPerSquareMeter)

}

object Irradiance extends Dimension[Irradiance] {
  def apply[A](n: A, unit: IrradianceUnit)(implicit num: Numeric[A]) =
    new Irradiance(num.toDouble(n), unit)
  def apply(value: Any): Try[Irradiance] = parse(value)
  def name = "Irradiance"
  def primaryUnit: WattsPerSquareMeter.type = WattsPerSquareMeter
  def siUnit: WattsPerSquareMeter.type = WattsPerSquareMeter
  def units: Set[UnitOfMeasure[Irradiance]] = Set(WattsPerSquareMeter)
}

trait IrradianceUnit extends UnitOfMeasure[Irradiance] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): Irradiance = Irradiance(n, this)
}

object WattsPerSquareMeter extends IrradianceUnit with PrimaryUnit with SiUnit {
  val symbol: String = Watts.symbol + "/" + SquareMeters.symbol
}

object IrradianceConversions {
  lazy val wattPerSquareMeter: Irradiance = WattsPerSquareMeter(1)

  implicit class IrradianceConversions[A](n: A)(implicit num: Numeric[A]) {
    def wattsPerSquareMeter: Irradiance = WattsPerSquareMeter(n)

  }

  implicit object IrradianceNumeric
      extends AbstractQuantityNumeric[Irradiance](Irradiance.primaryUnit)
}
