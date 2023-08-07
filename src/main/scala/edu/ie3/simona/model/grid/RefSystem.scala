/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid

import breeze.math.Complex
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.QuantityUtils.RichUnit
import edu.ie3.util.quantities.{PowerSystemUnits, QuantityUtil}

import javax.measure.Quantity
import javax.measure.quantity._
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

/** Provides the values a [[GridModel]] is referenced to as well as functions to
  * reference some standard parameters to the nominal impedance.
  */
final case class RefSystem private (
    nominalVoltage: ComparableQuantity[ElectricPotential],
    nominalCurrent: ComparableQuantity[ElectricCurrent],
    nominalPower: ComparableQuantity[Power],
    nominalImpedance: ComparableQuantity[ElectricResistance]
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
      r: Quantity[ElectricResistance]
  ): ComparableQuantity[Dimensionless] = {
    QuantityUtil
      .asComparable(r)
      .divide(nominalImpedance.to(OHM))
      .asType(classOf[Dimensionless])
      .to(PU)
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
      x: Quantity[ElectricResistance]
  ): ComparableQuantity[Dimensionless] =
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
      b: Quantity[ElectricConductance]
  ): ComparableQuantity[Dimensionless] = {
    QuantityUtil
      .asComparable(b)
      .to(SIEMENS)
      .multiply(nominalImpedance.to(OHM))
      .asType(classOf[Dimensionless])
      .to(PU)
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
      g: Quantity[ElectricConductance]
  ): ComparableQuantity[Dimensionless] =
    bInPu(g)

  /** Converts a provided referenced active power value from p.u. into physical
    * SI value
    *
    * @param pInPu
    *   referenced active power value in p.u.
    * @return
    *   unreferenced active power value in Watt
    */
  def pInSi(pInPu: Quantity[Dimensionless]): ComparableQuantity[Power] =
    nominalPower
      .multiply(pInPu)
      .asType(classOf[Power])
      .to(nominalPower.getUnit.toEquivalentIn(WATT))

  def pInSi(pInPu: Double): ComparableQuantity[Power] =
    pInSi(Quantities.getQuantity(pInPu, PU))

  /** Converts a provided active power value from physical SI to referenced p.u.
    *
    * @param pInSi
    *   unreferenced active power value in p.u.
    * @return
    *   referenced active power value in p.u.
    */
  def pInPu(pInSi: Quantity[Power]): ComparableQuantity[Dimensionless] =
    QuantityUtil
      .asComparable(pInSi)
      .divide(nominalPower)
      .asType(classOf[Dimensionless])
      .to(PU)

  /** Converts a provided reactive power value from p.u. into physical SI value
    *
    * @param qInPu
    *   referenced active power value in p.u.
    * @return
    *   unreferenced active power value in Var
    */
  def qInSi(qInPu: Quantity[Dimensionless]): ComparableQuantity[Power] =
    nominalPower
      .multiply(qInPu)
      .asType(classOf[Power])
      .to(nominalPower.getUnit.toEquivalentIn(VAR))

  def qInSi(qInPu: Double): ComparableQuantity[Power] =
    qInSi(Quantities.getQuantity(qInPu, PU))

  /** Converts a provided reactive power value from physical SI to referenced
    * p.u.
    *
    * @param qInSi
    *   unreferenced active power value in p.u.
    * @return
    *   referenced active power value in p.u.
    */
  def qInPu(qInSi: Quantity[Power]): ComparableQuantity[Dimensionless] =
    QuantityUtil
      .asComparable(qInSi)
      .divide(nominalPower)
      .asType(classOf[Dimensionless])
      .to(PU)

  /** Converts a provided voltage value from p.u. into physical SI value
    *
    * @param vInPu
    *   real or imaginary part of a referenced voltage value in p.u.
    * @return
    *   unreferenced voltage value in Volt
    */
  def vInSi(
      vInPu: Quantity[Dimensionless]
  ): ComparableQuantity[ElectricPotential] =
    nominalVoltage
      .multiply(vInPu)
      .asType(classOf[ElectricPotential])
      .to(nominalVoltage.getUnit)

  def vInSi(vInPu: Double): ComparableQuantity[ElectricPotential] =
    vInSi(Quantities.getQuantity(vInPu, PU))

  def vInSi(vInPu: Complex): (
      ComparableQuantity[ElectricPotential],
      ComparableQuantity[ElectricPotential]
  ) =
    (
      vInSi(Quantities.getQuantity(vInPu.real, PU)),
      vInSi(Quantities.getQuantity(vInPu.imag, PU))
    )

  /** Converts a provided voltage value from physical SI value into p.u. value
    *
    * @param vInSi
    *   real or imaginary part of a unreferenced physical SI value
    * @return
    *   referenced voltage value in p.u.
    */
  def vInPu(
      vInSi: Quantity[ElectricPotential]
  ): ComparableQuantity[Dimensionless] =
    QuantityUtil
      .asComparable(vInSi)
      .divide(nominalVoltage)
      .asType(classOf[Dimensionless])
      .to(PU)

}

case object RefSystem {

  def apply(
      nominalPower: Quantity[Power],
      nominalVoltage: Quantity[ElectricPotential]
  ): RefSystem = {

    val nominalCurrent: ComparableQuantity[ElectricCurrent] = QuantityUtil
      .asComparable(nominalPower)
      .divide(nominalVoltage.multiply(Math.sqrt(3)))
      .asType(classOf[ElectricCurrent])
      .to(AMPERE)
    val nominalImpedance: ComparableQuantity[ElectricResistance] =
      QuantityUtil
        .asComparable(nominalVoltage)
        .divide(nominalCurrent.multiply(Math.sqrt(3)))
        .asType(classOf[ElectricResistance])
        .to(OHM)

    new RefSystem(
      QuantityUtil.asComparable(nominalVoltage),
      nominalCurrent,
      QuantityUtil.asComparable(nominalPower),
      nominalImpedance
    )
  }

  def apply(nominalPower: String, nominalVoltage: String): RefSystem = {
    // units for parsing are not initialized by default
    // hence we call them manually
    new PowerSystemUnits

    // parsed quantities are transformed to PowerSystemUnits,
    // which are compatible to other units used
    val sNom = Quantities
      .getQuantity(nominalPower)
      .asType(classOf[Power])
      .to(MEGAVOLTAMPERE)
    val vNom = Quantities
      .getQuantity(nominalVoltage)
      .asType(classOf[ElectricPotential])
      .to(KILOVOLT)
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
      impedance: Quantity[Dimensionless],
      from: RefSystem,
      to: RefSystem
  ): ComparableQuantity[Dimensionless] = {
    val ratio = from.nominalImpedance
      .to(OHM)
      .divide(to.nominalImpedance.to(OHM))
      .getValue
      .doubleValue()

    QuantityUtil.asComparable(impedance).multiply(ratio)
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
      admittance: Quantity[Dimensionless],
      from: RefSystem,
      to: RefSystem
  ): ComparableQuantity[Dimensionless] = {
    val ratio = to.nominalImpedance
      .to(OHM)
      .divide(from.nominalImpedance.to(OHM))
      .getValue
      .doubleValue()

    QuantityUtil.asComparable(admittance).multiply(ratio)
  }
}
