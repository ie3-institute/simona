/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{ReactivePower, Vars}
import squants.electro._
import squants.energy.{Megawatts, Watts}
import squants.{Dimensionless, Each, Power}
import tech.units.indriya.quantity.Quantities

/** Provides the values a [[GridModel]] is referenced to as well as functions to
  * reference some standard parameters to the nominal impedance.
  */
final case class RefSystem private (
    nominalVoltage: ElectricPotential,
    nominalCurrent: ElectricCurrent,
    nominalPower: Power,
    nominalImpedance: ElectricalResistance,
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
  ): Dimensionless = {
    Each(r.toOhms / nominalImpedance.toOhms)
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
  ): Dimensionless =
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
  ): Dimensionless = {
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
  ): Dimensionless =
    bInPu(g)

  /** Converts a provided referenced active power value from p.u. into physical
    * SI value
    *
    * @param pInPu
    *   referenced active power value in p.u.
    * @return
    *   unreferenced active power value in Watt
    */
  def pInSi(pInPu: Dimensionless): Power =
    Watts(nominalPower.toWatts * pInPu.toEach)

  def pInSi(pInPu: Double): Power =
    pInSi(Each(pInPu))

  /** Converts a provided active power value from physical SI to referenced p.u.
    *
    * @param pInSi
    *   unreferenced active power value in p.u.
    * @return
    *   referenced active power value in p.u.
    */
  def pInPu(pInSi: Power): Dimensionless =
    Each(pInSi.toWatts / nominalPower.toWatts)

  /** Converts a provided reactive power value from p.u. into physical SI value
    *
    * @param qInPu
    *   referenced active power value in p.u.
    * @return
    *   unreferenced active power value in Var
    */
  def qInSi(qInPu: Dimensionless): ReactivePower =
    Vars(nominalPower.toWatts * qInPu.toEach)

  def qInSi(qInPu: Double): ReactivePower =
    qInSi(Each(qInPu))

  /** Converts a provided reactive power value from physical SI to referenced
    * p.u.
    *
    * @param qInSi
    *   unreferenced active power value in p.u.
    * @return
    *   referenced active power value in p.u.
    */
  def qInPu(qInSi: ReactivePower): Dimensionless =
    Each(qInSi.toVars / nominalPower.toWatts)

  /** Converts a provided voltage value from p.u. into physical SI value
    *
    * @param vInPu
    *   real or imaginary part of a referenced voltage value in p.u.
    * @return
    *   unreferenced voltage value in Volt
    */
  def vInSi(
      vInPu: Dimensionless
  ): ElectricPotential =
    Kilovolts(nominalVoltage.toKilovolts * vInPu.toEach)

  def vInSi(vInPu: Double): ElectricPotential =
    vInSi(Each(vInPu))

  def vInSi(vInPu: Complex): (
      ElectricPotential,
      ElectricPotential,
  ) =
    (
      vInSi(Each(vInPu.real)),
      vInSi(Each(vInPu.imag)),
    )

  /** Converts a provided voltage value from physical SI value into p.u. value
    *
    * @param vInSi
    *   real or imaginary part of an unreferenced physical SI value
    * @return
    *   referenced voltage value in p.u.
    */
  def vInPu(
      vInSi: ElectricPotential
  ): Dimensionless =
    Each(vInSi.toVolts / nominalVoltage.toVolts)
}

case object RefSystem {

  def apply(
      nominalPower: Power,
      nominalVoltage: ElectricPotential,
  ): RefSystem = {

    val nominalCurrent: ElectricCurrent =
      nominalPower / (nominalVoltage * Math.sqrt(3))

    val nominalImpedance: ElectricalResistance =
      nominalVoltage / (nominalCurrent * Math.sqrt(3))

    new RefSystem(
      nominalVoltage,
      nominalCurrent,
      nominalPower,
      nominalImpedance,
    )
  }

  def apply(nominalPower: String, nominalVoltage: String): RefSystem = {
    // units for parsing are not initialized by default
    // hence we call them manually
    new PowerSystemUnits

    // parsed quantities are transformed to PowerSystemUnits first for parsing and to squants second,
    // which are compatible to other units used
    val sNom = Megawatts(
      Quantities
        .getQuantity(nominalPower)
        .asType(classOf[javax.measure.quantity.Power])
        .to(PowerSystemUnits.MEGAVOLTAMPERE)
        .getValue
        .doubleValue()
    )
    val vNom = Kilovolts(
      Quantities
        .getQuantity(nominalVoltage)
        .asType(classOf[javax.measure.quantity.ElectricPotential])
        .to(PowerSystemUnits.KILOVOLT)
        .getValue
        .doubleValue()
    )
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
      impedance: Dimensionless,
      from: RefSystem,
      to: RefSystem,
  ): Dimensionless = {
    val ratio = from.nominalImpedance.toOhms / to.nominalImpedance.toOhms
    Each(impedance.toEach * ratio)
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
      admittance: Dimensionless,
      from: RefSystem,
      to: RefSystem,
  ): Dimensionless = {
    val ratio = to.nominalImpedance.toOhms / from.nominalImpedance.toOhms

    Each(admittance.toEach * ratio)
  }
}
