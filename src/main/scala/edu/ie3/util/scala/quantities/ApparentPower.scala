/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import squants._
import squants.energy._
import squants.time.Time

import scala.math.{acos, sin}
import scala.util.Try

final class ApparentPower private (
    val value: Double,
    val unit: ApparentPowerUnit,
) extends Quantity[ApparentPower] {

  def dimension: ApparentPower.type = ApparentPower

  def toMillivoltamperes: Double = to(Millivoltamperes)
  def toVoltamperes: Double = to(Voltamperes)
  def toKilovoltamperes: Double = to(Kilovoltamperes)
  def toMegavoltamperes: Double = to(Megavoltamperes)
  def toGigavoltamperes: Double = to(Gigavoltamperes)

  def toPower(cosPhi: Double): Power = Watts(toVoltamperes * cosPhi)

  def toReactivePower(cosPhi: Double): ReactivePower =
    Vars(toVoltamperes * sin(acos(cosPhi)))
}

object ApparentPower extends Dimension[ApparentPower] {
  private[quantities] def apply[A](n: A, unit: ApparentPowerUnit)(implicit
      num: Numeric[A]
  ) = new ApparentPower(num.toDouble(n), unit)
  def apply(energy: Energy, time: Time): ApparentPower =
    apply(energy.toWattHours / time.toHours, Voltamperes)
  def apply(value: Any): Try[ApparentPower] = parse(value)

  def name = "Power"
  def primaryUnit: Voltamperes.type = Voltamperes
  def siUnit: Voltamperes.type = Voltamperes
  def units: Set[UnitOfMeasure[ApparentPower]] =
    Set(
      Voltamperes,
      Millivoltamperes,
      Kilovoltamperes,
      Megavoltamperes,
      Gigavoltamperes,
    )
}

trait ApparentPowerUnit
    extends UnitOfMeasure[ApparentPower]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): ApparentPower =
    ApparentPower(n, this)
}

object Millivoltamperes extends ApparentPowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Milli
  val symbol = "mVA"
}

object Voltamperes extends ApparentPowerUnit with PrimaryUnit with SiUnit {
  val symbol = "VA"
}

object Kilovoltamperes extends ApparentPowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Kilo
  val symbol = "kVA"
}

object Megavoltamperes extends ApparentPowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Mega
  val symbol = "MVA"
}

object Gigavoltamperes extends ApparentPowerUnit with SiUnit {
  val conversionFactor: Double = MetricSystem.Giga
  val symbol = "GVA"
}
