/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import edu.ie3.util.quantities.PowerSystemUnits.*
import edu.ie3.util.quantities.interfaces.{
  Currency,
  EnergyPrice,
  SpecificConductance,
  SpecificResistance,
}
import edu.ie3.util.scala.quantities
import squants.electro.{Kilovolts, Ohms, Siemens}
import squants.energy.{KilowattHours, Kilowatts}
import squants.market.{EUR, Money}
import squants.space.SquareMeters
import squants.{Amperes, Each, Radians}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units.*

import javax.measure.quantity.*

/** Some utilities to improve the conversion between [[ComparableQuantity]] and
  * [[squants]].
  */
object QuantityConversionUtils {

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[Dimensionless]] into
    * [[squants.Dimensionless]].
    * @param quantity
    *   To convert.
    */
  implicit class DimensionlessToSimona(
      quantity: ComparableQuantity[Dimensionless]
  ) {

    /** Returns a quantity with unit [[Each]].
      */
    def toSquants: squants.Dimensionless = Each(
      quantity.to(PU).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[ElectricPotential]] into
    * [[squants.electro.ElectricPotential]].
    * @param quantity
    *   To convert.
    */
  implicit class VoltageToSimona(
      quantity: ComparableQuantity[ElectricPotential]
  ) {

    /** Returns a quantity with unit [[Kilovolts]].
      */
    def toSquants: squants.electro.ElectricPotential = Kilovolts(
      quantity.to(KILOVOLT).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[ElectricCurrent]] into
    * [[squants.electro.ElectricCurrent]].
    * @param quantity
    *   To convert.
    */
  implicit class CurrentToSimona(
      quantity: ComparableQuantity[ElectricCurrent]
  ) {

    /** Returns a quantity with unit [[Amperes]].
      */
    def toSquants: squants.ElectricCurrent = Amperes(
      quantity.to(AMPERE).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[Power]] into [[squants.Power]] or
    * [[ApparentPower]].
    * @param quantity
    *   To convert.
    */
  implicit class PowerConversionSimona(quantity: ComparableQuantity[Power]) {

    /** Returns a quantity with unit [[Kilowatts]].
      */
    def toSquants: squants.Power = Kilowatts(
      quantity.to(KILOWATT).getValue.doubleValue
    )

    /** Returns a quantity with unit [[Kilovoltamperes]].
      */
    def toApparent: ApparentPower = Kilovoltamperes(
      quantity.to(KILOVOLTAMPERE).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[Energy]] into [[squants.Energy]].
    * @param quantity
    *   To convert.
    */
  implicit class EnergyToSimona(quantity: ComparableQuantity[Energy]) {

    /** Returns a quantity with unit [[KilowattHours]].
      */
    def toSquants: squants.Energy = KilowattHours(
      quantity.to(KILOVARHOUR).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[EnergyPrice]] into
    * [[quantities.EnergyPrice]].
    * @param quantity
    *   To convert.
    */
  implicit class EnergyPriceToSimona(
      quantity: ComparableQuantity[EnergyPrice]
  ) {

    /** Returns a quantity with unit [[EuroPerKilowatthour]].
      */
    def toSquants: quantities.EnergyPrice = EuroPerKilowatthour(
      quantity.to(EURO_PER_KILOWATTHOUR).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[ElectricResistance]] into
    * [[squants.electro.ElectricalResistance]].
    * @param quantity
    *   To convert.
    */
  implicit class OhmToSimona(
      quantity: ComparableQuantity[ElectricResistance]
  ) {

    /** Returns a quantity with unit [[Ohms]].
      */
    def toSquants: squants.electro.ElectricalResistance = Ohms(
      quantity.to(OHM).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[SpecificResistance]] into
    * [[squants.electro.ElectricalResistance]].
    * @param quantity
    *   To convert.
    */
  implicit class OhmPerLengthToSimona(
      quantity: ComparableQuantity[SpecificResistance]
  ) {

    /** @param length
      *   Used to convert [[OHM_PER_KILOMETRE]] into [[OHM]].
      * @return
      *   a quantity with unit [[Ohms]].
      */
    def toSquants(implicit
        length: ComparableQuantity[Length]
    ): squants.electro.ElectricalResistance = Ohms(
      quantity
        .to(OHM_PER_KILOMETRE)
        .multiply(length.to(KILOMETRE))
        .getValue
        .doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[ElectricConductance]] into
    * [[squants.electro.ElectricalConductance]].
    * @param quantity
    *   To convert.
    */
  implicit class SiemensToSimona(
      quantity: ComparableQuantity[ElectricConductance]
  ) {
    def toSquants: squants.electro.ElectricalConductance = Siemens(
      quantity.to(SIEMENS).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[SpecificConductance]] into
    * [[squants.electro.ElectricalConductance]].
    * @param quantity
    *   To convert.
    */
  implicit class SiemensPerLengthToSimona(
      quantity: ComparableQuantity[SpecificConductance]
  ) {

    /** @param length
      *   Used to convert [[SIEMENS_PER_KILOMETRE]] into [[Siemens]].
      * @return
      *   a quantity with unit [[Siemens]].
      */
    def toSquants(implicit
        length: ComparableQuantity[Length]
    ): squants.electro.ElectricalConductance = Siemens(
      quantity
        .to(SIEMENS_PER_KILOMETRE)
        .multiply(length.to(KILOMETRE))
        .getValue
        .doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[Area]] into [[squants.Area]].
    * @param quantity
    *   To convert.
    */
  implicit class AreaToSimona(quantity: ComparableQuantity[Area]) {

    /** Returns a quantity with unit [[SquareMeters]].
      */
    def toSquants: squants.Area = SquareMeters(
      quantity.to(SQUARE_METRE).getValue.doubleValue
    )
  }

  /** Implicit class that contains a method to convert a given
    * [[ComparableQuantity]] with unit [[Angle]] into [[squants.Angle]].
    * @param quantity
    *   To convert.
    */
  implicit class RadiansConversionSimona(quantity: ComparableQuantity[Angle]) {

    /** Returns a quantity with unit [[Radians]].
      */
    def toSquants: squants.Angle = Radians(
      quantity.to(RADIAN).getValue.doubleValue
    )
  }

}
