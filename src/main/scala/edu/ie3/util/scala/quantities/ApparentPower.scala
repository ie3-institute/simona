/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import breeze.math.Complex
import squants.energy._
import squants.{Angle, Power, Radians, SiUnit}

import java.lang.Math._
import scala.math.{atan2, hypot}

final case class ApparentPower private (
    value: Double,
    angle: Angle,
    unit: ApparentPowerUnit,
) {

  /** Returns the apparent power with an angle of zero degrees. This means: P ==
    * \|S|
    */
  def withZeroDegrees: Power = unit.powerUnit(value)

  def active: Double = value * cos(angle.toRadians)
  def reactive: Double = value * sin(angle.toRadians)

  def scale(that: Double): ApparentPower =
    ApparentPower(value * that, angle, unit)

  def applyCosPhi(cosPhi: Double): ApparentPower =
    copy(angle = Radians(acos(cosPhi)))

  def asComplex: Complex = Complex(active, reactive)

  def activePower: Power = unit.powerUnit(active)
  def reactivePower: ReactivePower = unit.reactivePowerUnit(reactive)

  def toMillivoltampere: ApparentPower = to(Millivoltampere)
  def toVoltampere: ApparentPower = to(Voltampere)
  def toKilovoltampere: ApparentPower = to(Kilovoltampere)
  def toMegavoltampere: ApparentPower = to(Megavoltampere)
  def toGigavoltampere: ApparentPower = to(Gigavoltampere)

  def to(unit: ApparentPowerUnit): ApparentPower = {
    ApparentPower(
      this.unit.powerUnit(active),
      this.unit.reactivePowerUnit(reactive),
      unit,
    )
  }
}

object ApparentPower {
  private[quantities] def apply(
      active: Power,
      reactive: ReactivePower,
      unit: ApparentPowerUnit,
  ): ApparentPower = {
    val re = active.to(unit.powerUnit)
    val imag = reactive.to(unit.reactivePowerUnit)

    new ApparentPower(
      hypot(re, imag),
      Radians(atan2(imag, re)),
      unit,
    )
  }

  def name: String = "Power"

  def units: Set[ApparentPowerUnit] = Set(
    Millivoltampere,
    Voltampere,
    Kilovoltampere,
    Megavoltampere,
    Gigavoltampere,
  )

  implicit class PowerConversion(val power: Power) {
    def asApparent(unit: ApparentPowerUnit): ApparentPower = unit.apply(power)
  }

  implicit class ReactivePowerConversion(val reactive: ReactivePower) {
    def asApparent(unit: ApparentPowerUnit): ApparentPower =
      unit.apply(reactive)
  }
}

trait ApparentPowerUnit {
  def apply(value: Double, angle: Angle): ApparentPower =
    ApparentPower(value, angle, this)

  def apply(active: Power, reactive: ReactivePower): ApparentPower =
    ApparentPower(active, reactive, this)

  def apply(active: Power): ApparentPower = apply(active, Vars(0))

  def apply(reactive: ReactivePower): ApparentPower = apply(Watts(0), reactive)

  def powerUnit: PowerUnit
  def reactivePowerUnit: ReactivePowerUnit
}

object Millivoltampere extends ApparentPowerUnit with SiUnit {
  val symbol = "mVA"
  val powerUnit: Milliwatts.type = Milliwatts
  val reactivePowerUnit: Millivars.type = Millivars
}

object Voltampere extends ApparentPowerUnit with SiUnit {
  val symbol = "VA"
  val powerUnit: Watts.type = Watts
  val reactivePowerUnit: Vars.type = Vars
}

object Kilovoltampere extends ApparentPowerUnit with SiUnit {
  val symbol = "kVA"
  val powerUnit: Kilowatts.type = Kilowatts
  val reactivePowerUnit: Kilovars.type = Kilovars
}

object Megavoltampere extends ApparentPowerUnit with SiUnit {
  val symbol = "MVA"
  val powerUnit: Megawatts.type = Megawatts
  val reactivePowerUnit: Megavars.type = Megavars
}

object Gigavoltampere extends ApparentPowerUnit with SiUnit {
  val symbol = "GVA"
  val powerUnit: Gigawatts.type = Gigawatts
  val reactivePowerUnit: Gigavars.type = Gigavars
}
