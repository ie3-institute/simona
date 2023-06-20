/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.quantities.ReactivePower
import squants.Each
import squants.electro.ElectricalResistanceConversions.ElectricalResistanceNumeric
import squants.electro.{ElectricCurrent, ElectricalConductance, ElectricalResistance, Kilovolts}
import squants.energy.Megawatts
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.{ElectricPotential, Power}


/** Provides the values a [[GridModel]] is referenced to as well as functions to
  * reference some standard parameters to the nominal impedance.
  */
final case class RefSystem (
    nominalVoltage: squants.electro.ElectricPotential,
    nominalCurrent: ElectricCurrent,
    nominalPower: squants.Power,
    nominalImpedance: ElectricalResistance
) {

  /** Calculates the referenced resistance r (real part of impedance z) of a
    * grid element
    *
    * @param r
    *   unreferenced resistance of the grid element
    * @return
    *   referenced resistance r in p.u.
    */
  def rInPu(
      r: ElectricalResistance
  ): squants.Dimensionless = {
    Each(r / nominalImpedance.toOhms)
  }

  /** Calculates the referenced reactance x (imaginary part of impedance z) of a
    * grid element.
    *
    * @param x
    *   unreferenced reactance of the grid element
    * @return
    *   referenced reactance x in p.u.
    */
  def xInPu(
      x: ElectricalResistance
  ): squants.Dimensionless =
    rInPu(x)

  /** Calculates the referenced susceptance b (imaginary part of admittance y)
    * of a grid element.
    *
    * @param b
    *   unreferenced susceptance of the grid element
    * @return
    *   referenced susceptance b in p.u.
    */
  def bInPu(
      b: ElectricalConductance
  ): squants.Dimensionless = {
    Each(b.toSiemens * nominalImpedance.toOhms)
  }

  /** Calculates the referenced conductance g (real part of admittance y) of a
    * grid element.
    *
    * @param g
    *   unreferenced conductance of the grid element
    * @return
    *   referenced conductance g in p.u.
    */
  def gInPu(
      g: ElectricalConductance
  ): squants.Dimensionless =
    bInPu(g)

  /** Converts a provided referenced active power value from p.u. into physical
    * SI value
    *
    * @param pInPu
    *   referenced active power value in p.u.
    * @return
    *   unreferenced active power value in Watt
    */
  def pInSi(pInPu: squants.Dimensionless): squants.Power =
    nominalPower * pInPu.value.doubleValue

  def pInSi(pInPu: Double): squants.Power =
    pInSi(pInPu)

  /** Converts a provided active power value from physical SI to referenced p.u.
    *
    * @param pInSi
    *   unreferenced active power value in p.u.
    * @return
    *   referenced active power value in p.u.
    */
  def pInPu(pInSi: squants.Power): squants.Dimensionless =
    Each(pInSi / nominalPower)

  /** Converts a provided reactive power value from p.u. into physical SI value
    *
    * @param qInPu
    *   referenced active power value in p.u.
    * @return
    *   unreferenced active power value in Var
    */
  def qInSi(qInPu: squants.Dimensionless): squants.Power =
    nominalPower * qInPu.value.doubleValue

  def qInSi(qInPu: Double): squants.Power =
    qInSi(qInPu.doubleValue)

  /** Converts a provided reactive power value from physical SI to referenced
    * p.u.
    *
    * @param qInSi
    *   unreferenced active power value in p.u.
    * @return
    *   referenced active power value in p.u.
    */
  def qInPu(qInSi: ReactivePower): squants.Dimensionless =
    qInSi / nominalPower

  /** Converts a provided voltage value from p.u. into physical SI value
    *
    * @param vInPu
    *   real or imaginary part of a referenced voltage value in p.u.
    * @return
    *   unreferenced voltage value in Volt
    */
  def vInSi(
      vInPu: squants.Dimensionless
  ): squants.electro.ElectricPotential = nominalVoltage * vInPu.value.doubleValue

  def vInSi(vInPu: Double): squants.electro.ElectricPotential =
    vInSi(vInPu)

  def vInSi(vInPu: Complex): (
    squants.electro.ElectricPotential,
      squants.electro.ElectricPotential
  ) =
    (
      vInSi(vInPu.real),
      vInSi(vInPu.imag)
    )

  /** Converts a provided voltage value from physical SI value into p.u. value
    *
    * @param vInSi
    *   real or imaginary part of a unreferenced physical SI value
    * @return
    *   referenced voltage value in p.u.
    */
  def vInPu(
      vInSi: squants.electro.ElectricPotential
  ): squants.Dimensionless =
    Each(vInSi / nominalVoltage)
}

object RefSystem {
  def apply(
      nominalPower: squants.Power,
      nominalVoltage: squants.electro.ElectricPotential
  ): RefSystem = {

    val nominalCurrent: ElectricCurrent =
      nominalPower / (nominalVoltage * Math.sqrt(3))

    val nominalImpedance: ElectricalResistance =
      nominalVoltage / (nominalCurrent * Math.sqrt(3))

    new RefSystem(
      nominalVoltage,
      nominalCurrent,
      nominalPower,
      nominalImpedance
    )
  }

  def apply(nominalPower: String, nominalVoltage: String): RefSystem = {
    // units for parsing are not initialized by default
    // hence we call them manually
    new PowerSystemUnits

    // parsed quantities are transformed to PowerSystemUnits,
    // which are compatible to other units used
    val sNom = Megawatts(Quantities
      .getQuantity(nominalPower)
      .asType(classOf[Power])
      .to(MEGAVOLTAMPERE).getValue.doubleValue())
    val vNom = Kilovolts(Quantities
      .getQuantity(nominalVoltage)
      .asType(classOf[ElectricPotential])
      .to(KILOVOLT).getValue.doubleValue())
    RefSystem(sNom, vNom)
  }

  /** Transfers the dimensionless impedance from one to another reference system
    *
    * @param impedance
    *   Dimensionless impedance to be transferred
    * @param from
    *   Source reference system
    * @param to
    *   Target reference system
    * @return
    *   Dimensionless impedance with regard the to target reference system
    */
  def transferImpedance(
      impedance: squants.Dimensionless,
      from: RefSystem,
      to: RefSystem
  ): squants.Dimensionless = {
    val ratio = from.nominalImpedance / to.nominalImpedance

    impedance * ratio
  }

  /** Transfers the dimensionless admittance from one to another reference
    * system
    *
    * @param admittance
    *   Dimensionless admittance to be transferred
    * @param from
    *   Source reference system
    * @param to
    *   Target reference system
    * @return
    *   Dimensionless admittance with regard the to target reference system
    */
  def transferAdmittance(
      admittance: squants.Dimensionless,
      from: RefSystem,
      to: RefSystem
  ): squants.Dimensionless = {
    val ratio = to.nominalImpedance / from.nominalImpedance

    admittance * ratio
  }
}
