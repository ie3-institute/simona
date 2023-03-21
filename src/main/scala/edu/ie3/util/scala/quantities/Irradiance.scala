package edu.ie3.util.scala.quantities



import squants._
import squants.energy.{WattHours, Watts}
import squants.radio.{AreaTime, BecquerelsPerSquareMeterSecond, ParticleFlux}
import squants.space.SquareMeters
import squants.time.Hours

/**
 * Originally from squants.radio.Irradiance
 *
 * @author  garyKeorkunian
 * @since   0.1
 *
 * @param value Double
 */
final class Irradiance private (val value: Double, val unit: IrradianceUnit)
  extends Quantity[Irradiance] {

  def dimension = Irradiance

  def *(that: Area): Power = Watts(this.toWattsPerSquareMeter * that.toSquareMeters)
  // the Hours(1).toSeconds is to convert watt hours to watt seconds which
  // isn't a normal supported type in Squants
  def *(that: AreaTime): Energy = WattHours(
    this.toWattsPerSquareMeter * that.toSquareMeterSeconds / Hours(1).toSeconds)

  def *(that: squants.Time): Irradiation = WattHoursPerSquareMeter(
    this.toWattsPerSquareMeter * that.toSeconds)

  def /(that: Energy): ParticleFlux = BecquerelsPerSquareMeterSecond(
    toWattsPerSquareMeter / (that.toWattHours * Hours(1).toSeconds))
  def /(that: ParticleFlux): Energy = WattHours(
    (toWattsPerSquareMeter / that.toBecquerelsPerSquareMeterSecond) /
      Hours(1).toSeconds)

  def toWattsPerSquareMeter = to(WattsPerSquareMeter)

}

object Irradiance extends Dimension[Irradiance] {
  def apply[A](n: A, unit: IrradianceUnit)(implicit num: Numeric[A]) = new Irradiance(num.toDouble(n), unit)
  def apply(value: Any) = parse(value)
  def name = "Irradiance"
  def primaryUnit = WattsPerSquareMeter
  def siUnit = WattsPerSquareMeter
  def units = Set(WattsPerSquareMeter)
}

trait IrradianceUnit extends UnitOfMeasure[Irradiance] with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = Irradiance(n, this)
}

object WattsPerSquareMeter extends IrradianceUnit with PrimaryUnit with SiUnit {
  val symbol = Watts.symbol + "/" + SquareMeters.symbol
}

object IrradianceConversions {
  lazy val wattPerSquareMeter = WattsPerSquareMeter(1)


  implicit class IrradianceConversions[A](n: A)(implicit num: Numeric[A]) {
    def wattsPerSquareMeter = WattsPerSquareMeter(n)

  }

  implicit object IrradianceNumeric extends AbstractQuantityNumeric[Irradiance](Irradiance.primaryUnit)
}
