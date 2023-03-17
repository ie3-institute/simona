/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.energy.{WattHours, Watts, WattsPerHour}
import squants.time.{Hours, TimeIntegral}
import squants.{
  Dimension,
  Energy,
  MetricSystem,
  PowerRamp,
  PrimaryUnit,
  Quantity,
  SiUnit,
  Time,
  UnitConverter,
  UnitOfMeasure
}

final class ReactivePower private (
  val value: Double,
  val unit: ReactivePowerUnit
) extends Quantity[ReactivePower]
  with TimeIntegral[PowerRamp] {

  def dimension = ReactivePower

  protected[quantities] def timeIntegrated = WattHours(toVars)

  protected def timeDerived = WattsPerHour(toVars)

  protected[quantities] def time = Hours(1)

  def toMillivars = to(Millivars)
  def toVars = to(Vars)
  def toKilovars = to(Kilovars)
  def toMegavars = to(Megavars)
  def toGigavars = to(Gigavars)

  def toPower = Watts(toVars)

}

object ReactivePower extends Dimension[ReactivePower] {
  private[quantities] def apply[A](n: A, unit: ReactivePowerUnit)(implicit
    num: Numeric[A]
  ) = new ReactivePower(num.toDouble(n), unit)
  def apply(energy: Energy, time: Time): ReactivePower =
    apply(energy.toWattHours / time.toHours, Vars)
  def apply(value: Any) = parse(value)

  def name = "Power"
  def primaryUnit = Vars
  def siUnit = Vars
  def units = Set(Vars, Millivars, Kilovars, Megavars, Gigavars)
}

trait ReactivePowerUnit
  extends UnitOfMeasure[ReactivePower]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]) = ReactivePower(n, this)
}

object Millivars extends ReactivePowerUnit with SiUnit {
  val conversionFactor = MetricSystem.Milli
  val symbol = "mVar"
}

object Vars extends ReactivePowerUnit with PrimaryUnit with SiUnit {
  val symbol = "Var"
}

object Kilovars extends ReactivePowerUnit with SiUnit {
  val conversionFactor = MetricSystem.Kilo
  val symbol = "kVar"
}

object Megavars extends ReactivePowerUnit with SiUnit {
  val conversionFactor = MetricSystem.Mega
  val symbol = "MVar"
}

object Gigavars extends ReactivePowerUnit with SiUnit {
  val conversionFactor = MetricSystem.Giga
  val symbol = "GVar"
}
