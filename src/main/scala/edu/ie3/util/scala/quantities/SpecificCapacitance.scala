/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.electro.Farads
import squants.space.Meters
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

/** Represents the specific capacitance per unit length, e.g. of a cable
  * dielectric.
  *
  * In F/m
  */
final class SpecificCapacitance private (
    val value: Double,
    val unit: SpecificCapacitanceUnit,
) extends Quantity[SpecificCapacitance] {

  def dimension: SpecificCapacitance.type = SpecificCapacitance

  def toFaradsPerMeter: Double = to(FaradsPerMeter)
  def toMicrofaradsPerKilometer: Double = to(MicrofaradsPerKilometer)

  /** Multiplies this specific capacitance by a length to obtain a total
    * capacitance.
    */
  def *(that: squants.space.Length): squants.electro.Capacitance = Farads(
    this.toFaradsPerMeter * that.toMeters
  )
}

object SpecificCapacitance extends Dimension[SpecificCapacitance] {
  def apply[A](n: A, unit: SpecificCapacitanceUnit)(implicit
      num: Numeric[A]
  ): SpecificCapacitance =
    new SpecificCapacitance(num.toDouble(n), unit)
  def apply(value: Any) = parse(value)
  def name: String = "SpecificCapacitance"
  def primaryUnit: FaradsPerMeter.type = FaradsPerMeter
  def siUnit: FaradsPerMeter.type = FaradsPerMeter
  def units: Set[UnitOfMeasure[SpecificCapacitance]] =
    Set(FaradsPerMeter, MicrofaradsPerKilometer)
}

trait SpecificCapacitanceUnit
    extends UnitOfMeasure[SpecificCapacitance]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): SpecificCapacitance =
    SpecificCapacitance(n, this)
}

object FaradsPerMeter
    extends SpecificCapacitanceUnit
    with PrimaryUnit
    with SiUnit {
  val symbol: String = Farads.symbol + "/" + Meters.symbol
}

object MicrofaradsPerKilometer extends SpecificCapacitanceUnit {
  val symbol: String = "µF/km"
  // 1 µF/km = 1e-6 F / 1000 m = 1e-9 F/m
  val conversionFactor: Double = 1e-9
}

object SpecificCapacitanceConversions {
  lazy val faradPerMeter = FaradsPerMeter(1)
  lazy val microfaradPerKilometer = MicrofaradsPerKilometer(1)

  implicit class SpecificCapacitanceConversions[A](n: A)(implicit
      num: Numeric[A]
  ) {
    def faradsPerMeter: SpecificCapacitance = FaradsPerMeter(n)
    def microfaradsPerKilometer: SpecificCapacitance = MicrofaradsPerKilometer(
      n
    )
  }

  implicit object SpecificCapacitanceNumeric
      extends AbstractQuantityNumeric[SpecificCapacitance](
        SpecificCapacitance.primaryUnit
      )
}
