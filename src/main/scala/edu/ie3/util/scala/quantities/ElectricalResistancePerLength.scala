/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.{
  AbstractQuantityNumeric,
  Dimension,
  MetricSystem,
  PrimaryUnit,
  Quantity,
  SiUnit,
  UnitConverter,
  UnitOfMeasure,
}
import squants.electro.{ElectricalResistance, Ohms}
import squants.space.Length

/** Represents the electrical resistance per length.
  *
  * In Ohms/m
  *
  * Based on [[squants.electro.ElectricalResistance]] by garyKeorkunian
  */

final class ElectricalResistancePerLength private (
    val value: Double,
    val unit: ElectricalResistancePerLengthUnit,
) extends Quantity[ElectricalResistancePerLength] {

  def dimension = ElectricalResistancePerLength

  def *(that: Length): ElectricalResistance = Ohms(
    this.toOhmsPerMeter * that.toMeters
  )

  def toOhmsPerMeter = to(OhmsPerMeter)
  def toOhmsPerKilometer = to(OhmsPerKilometer)
  def toMilliohmsPerMeter = to(MilliohmsPerMeter)
  def toMicroohmsPerMeter = to(MicroohmsPerMeter)
}

object ElectricalResistancePerLength
    extends Dimension[ElectricalResistancePerLength] {
  def apply[A](n: A, unit: ElectricalResistancePerLengthUnit)(implicit
      num: Numeric[A]
  ) =
    new ElectricalResistancePerLength(num.toDouble(n), unit)
  def apply(value: Any) = parse(value)
  def name = "ElectricalResistancePerLength"
  def primaryUnit = OhmsPerMeter
  def siUnit = OhmsPerMeter
  def units =
    Set(OhmsPerMeter, OhmsPerKilometer, MilliohmsPerMeter, MicroohmsPerMeter)
}

trait ElectricalResistancePerLengthUnit
    extends UnitOfMeasure[ElectricalResistancePerLength]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) =
    ElectricalResistancePerLength(n, this)
}

object OhmsPerMeter
    extends ElectricalResistancePerLengthUnit
    with PrimaryUnit
    with SiUnit {
  val symbol = "Ω/m"
}

object OhmsPerKilometer extends ElectricalResistancePerLengthUnit {
  val symbol = "Ω/km"
  // 1 Ω/km = 1 Ω / 1000 m = 0.001 Ω/m
  val conversionFactor = MetricSystem.Milli
}

object MilliohmsPerMeter extends ElectricalResistancePerLengthUnit {
  val symbol = "mΩ/m"
  // 1 mΩ/m = 0.001 Ω / 1 m = 0.001 Ω/m
  val conversionFactor = MetricSystem.Milli
}

object MicroohmsPerMeter extends ElectricalResistancePerLengthUnit {
  val symbol = "µΩ/m"
  // 1 µΩ/m = 0.000001 Ω / 1 m = 10^-6 Ω/m
  val conversionFactor = MetricSystem.Micro
}

object ElectricalResistancePerLengthConversions {
  lazy val ohmPerMeter = OhmsPerMeter(1)
  lazy val ohmPerKilometer = OhmsPerKilometer(1)
  lazy val milliohmPerMeter = MilliohmsPerMeter(1)
  lazy val microohmPerMeter = MicroohmsPerMeter(1)

  implicit class ElectricalResistancePerLengthConversions[A](n: A)(implicit
      num: Numeric[A]
  ) {
    def ohmsPerMeter = OhmsPerMeter(n)
    def ohmsPerKilometer = OhmsPerKilometer(n)
    def milliohmsPerMeter = MilliohmsPerMeter(n)
    def microohmsPerMeter = MicroohmsPerMeter(n)
  }

  implicit object ElectricalResistancePerLengthNumeric
      extends AbstractQuantityNumeric[ElectricalResistancePerLength](
        ElectricalResistancePerLength.primaryUnit
      )
}
