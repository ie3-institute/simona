/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants.energy.*
import squants.time.{Hours, Time, TimeIntegral}
import squants.*

import scala.util.Try

final class ReactivePower private (
    val value: Double,
    val unit: ReactivePowerUnit,
) extends Quantity[ReactivePower]
    with TimeIntegral[PowerRamp] {

  def dimension: ReactivePower.type = ReactivePower

  def /(activePower: squants.Power): squants.Dimensionless = Each(
    this.toVars / activePower.toWatts
  )

  protected[quantities] def timeIntegrated: Energy = WattHours(toVars)

  protected def timeDerived: PowerRamp = WattsPerHour(toVars)

  protected[quantities] def time: Time = Hours(1)

  def toMillivars: Double = to(Millivars)
  def toVars: Double = to(Vars)
  def toKilovars: Double = to(Kilovars)
  def toMegavars: Double = to(Megavars)
  def toGigavars: Double = to(Gigavars)

  def toPower: Power = Watts(toVars)

}

object ReactivePower extends Dimension[ReactivePower] {
  private[quantities] def apply[A](n: A, unit: ReactivePowerUnit)(implicit
      num: Numeric[A]
  ) = new ReactivePower(num.toDouble(n), unit)
  def apply(energy: Energy, time: Time): ReactivePower =
    apply(energy.toWattHours / time.toHours, Vars)
  def apply(value: Any): Try[ReactivePower] = parse(value)

  def name = "Power"
  def primaryUnit: Vars.type = Vars
  def siUnit: Vars.type = Vars
  def units: Set[UnitOfMeasure[ReactivePower]] =
    Set(Vars, Millivars, Kilovars, Megavars, Gigavars)
}

trait ReactivePowerUnit
    extends UnitOfMeasure[ReactivePower]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): ReactivePower =
    ReactivePower(n, this)
}

object Millivars extends ReactivePowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Milli
  val symbol = "mVar"
}

object Vars extends ReactivePowerUnit with PrimaryUnit with SiUnit {
  val symbol = "Var"
}

object Kilovars extends ReactivePowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Kilo
  val symbol = "kVar"
}

object Megavars extends ReactivePowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Mega
  val symbol = "MVar"
}

object Gigavars extends ReactivePowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Giga
  val symbol = "GVar"
}
