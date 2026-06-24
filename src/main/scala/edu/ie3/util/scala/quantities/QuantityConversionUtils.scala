/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.quantities

import edu.ie3.util.quantities.PowerSystemUnits.*
import edu.ie3.util.quantities.interfaces.*
import edu.ie3.util.scala.quantities
import edu.ie3.util.scala.quantities.{
  ThermalCapacitance as ThermalCapacitanceSquants,
  ThermalResistivity as ThermalResistivitySquants,
}
import squants.electro.*
import squants.energy.{KilowattHours, Kilowatts}
import squants.motion.MetersPerSecond
import squants.radio.WattsPerSquareMeter
import squants.space.{CubicMeters, Millimeters, SquareMeters}
import squants.thermal.Celsius
import squants.time.Hertz
import squants.{Amperes, Each, Meters, Radians, Velocity}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.*

import java.util.Optional
import javax.measure.quantity.*
import scala.annotation.targetName
import scala.jdk.OptionConverters.RichOptional

/** Some utilities to improve the conversion between [[ComparableQuantity]] and
  * [[squants]].
  */
object QuantityConversionUtils {

  /** Extension for [[ComparableQuantity]] of type [[Dimensionless]] that allows
    * conversion into a [[squants.Dimensionless]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Dimensionless]) {

    def toSquants: squants.Dimensionless = Each(
      quantity.to(PU).getValue.doubleValue
    )
  }

  extension (quantity: squants.Dimensionless) {
    def toQuantity: ComparableQuantity[Dimensionless] =
      Quantities.getQuantity(quantity.toEach, PU)
  }

  extension (quantity: squants.Length) {
    def toQuantity: ComparableQuantity[Length] =
      Quantities.getQuantity(quantity.toMillimeters, MILLIMETRE)
  }

  extension (value: squants.Power) {
    def toQuantity: ComparableQuantity[Power] =
      Quantities.getQuantity(value.toMegawatts, MEGAWATT)
  }

  extension (value: squants.Energy) {
    def toQuantity: ComparableQuantity[Energy] =
      Quantities.getQuantity(value.toMegawattHours, MEGAWATTHOUR)
  }

  /** Extension for [[ComparableQuantity]] of type [[ElectricPotential]] that
    * allows conversion into a [[squants.electro.ElectricPotential]] squants
    * quantity.
    */
  extension (
      quantity: ComparableQuantity[javax.measure.quantity.ElectricPotential]
  ) {

    def toSquants: squants.electro.ElectricPotential = Kilovolts(
      quantity.to(KILOVOLT).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[ElectricCurrent]] that
    * allows conversion into a [[squants.ElectricCurrent]] squants quantity.
    */
  extension (
      quantity: ComparableQuantity[javax.measure.quantity.ElectricCurrent]
  ) {

    def toSquants: squants.ElectricCurrent = Amperes(
      quantity.to(AMPERE).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[Power]] that allows
    * conversion into a [[squants.Power]] or [[ApparentPower]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Power]) {

    /** Returns a quantity of type [[squants.Power]].
      */
    def toSquants: squants.Power = Kilowatts(
      quantity.to(KILOWATT).getValue.doubleValue
    )

    /** Returns a quantity of type [[ApparentPower]].
      */
    def toApparent: ApparentPower = Kilovoltamperes(
      quantity.to(KILOVOLTAMPERE).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[Energy]] that allows
    * conversion into a [[squants.Energy]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Energy]) {

    def toSquants: squants.Energy = KilowattHours(
      quantity.to(KILOWATTHOUR).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[Frequency]] that allows
    * conversion into a [[squants.time.Frequency]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Frequency]) {

    def toSquants: squants.time.Frequency = Hertz(
      quantity.to(HERTZ).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[ElectricResistance]] that
    * allows conversion into a [[squants.electro.ElectricalResistance]] squants
    * quantity.
    */
  extension (quantity: ComparableQuantity[ElectricResistance]) {

    def toSquants: squants.electro.ElectricalResistance = Ohms(
      quantity.to(OHM).getValue.doubleValue
    )
  }

  /** Extension for Optional of [[ComparableQuantity]] of type [[Length]] that
    * allows conversion into an Optional of [[squants.Length]] squants quantity.
    */
  extension (quantity: Optional[ComparableQuantity[Length]]) {
    @targetName("lengthOptionalToSquants")
    def toSquants: Option[squants.Length] =
      quantity.toScala.map(q => Meters(q.to(METRE).getValue.doubleValue))
  }

  /** Extension for [[ComparableQuantity]] of type [[Length]] that allows
    * conversion into a [[quantities.Length]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Length]) {
    def toSquants: squants.space.Length = Millimeters(
      quantity.to(MILLIMETRE).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[EnergyPrice]] that allows
    * conversion into a [[quantities.EnergyPrice]] squants quantity.
    */
  extension (quantity: ComparableQuantity[EnergyPrice]) {

    def toSquants: quantities.EnergyPrice = EuroPerKilowattHour(
      quantity.to(EURO_PER_KILOWATTHOUR).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[ElectricCapacitance]] that
    * allows conversion into a [[squants.electro.Capacitance]] squants quantity.
    */
  extension (quantity: ComparableQuantity[ElectricCapacitance]) {

    def toSquants: squants.electro.Capacitance = Farads(
      quantity.to(FARAD).getValue.doubleValue
    )
  }

  extension (quantity: ComparableQuantity[ElectricalResistivity]) {

    def toSquants: squants.electro.Resistivity =
      OhmMeters(
        quantity
          .to(OHM_METRE)
          .getValue
          .doubleValue
      )
  }

  /** Extension for [[ComparableQuantity]] of type [[SpecificResistance]] that
    * allows conversion into a [[squants.electro.ElectricalConductance]] squants
    * quantity.
    */
  extension (q: ComparableQuantity[SpecificResistance])

    /** @param length
      *   Used to convert [[OHM_PER_KILOMETRE]] into [[OHM]].
      * @return
      *   a quantity with unit [[Ohms]].
      */
    def toResistance(implicit
        length: ComparableQuantity[Length]
    ): squants.electro.ElectricalResistance =
      Ohms(
        q.to(OHM_PER_KILOMETRE)
          .multiply(length.to(KILOMETRE))
          .getValue
          .doubleValue
      )

    /** @return
      *   a quantity with unit [[Ohms]].
      */
    def toResistancePerLength: ElectricalResistancePerLength =
      OhmsPerKilometer(q.to(OHM_PER_KILOMETRE).getValue.doubleValue)

  /** Extension for [[ComparableQuantity]] of type [[ElectricConductance]] that
    * allows conversion into a [[squants.electro.ElectricalConductance]] squants
    * quantity.
    */
  extension (quantity: ComparableQuantity[ElectricConductance]) {

    def toSquants: squants.electro.ElectricalConductance = Siemens(
      quantity.to(SIEMENS).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[SpecificConductance]] that
    * allows conversion into a [[squants.electro.ElectricalConductance]] squants
    * quantity.
    */
  extension (quantity: ComparableQuantity[SpecificConductance]) {

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

  /** Extension for Optional of [[ComparableQuantity]] of type [[Area]] that
    * allows conversion into an Optional of [[squants.Area]] squants quantity.
    */
  extension (quantity: Optional[ComparableQuantity[Area]]) {
    @targetName("areaOptionalToSquants")
    def toSquants: Option[squants.Area] =
      quantity.toScala.map(q =>
        SquareMeters(q.to(SQUARE_METRE).getValue.doubleValue)
      )
  }

  /** Extension for [[ComparableQuantity]] of type [[Area]] that allows
    * conversion into a [[squants.Area]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Area]) {

    def toSquants: squants.Area = SquareMeters(
      quantity.to(SQUARE_METRE).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[Angle]] that allows
    * conversion into a [[squants.Angle]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Angle]) {

    def toSquants: squants.Angle = Radians(
      quantity.to(RADIAN).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[Temperature]] that allows
    * conversion into a [[squants.Temperature]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Temperature]) {

    def toSquants: squants.Temperature = Celsius(
      quantity.to(CELSIUS).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[Volume]] that allows
    * conversion into a [[squants.space.Volume]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Volume]) {

    def toSquants: squants.space.Volume = CubicMeters(
      quantity.to(CUBIC_METRE).getValue.doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[SpecificHeatCapacity]] that
    * allows conversion into a
    * [[edu.ie3.util.scala.quantities.SpecificHeatCapacity]] squants quantity.
    */
  extension (quantity: ComparableQuantity[SpecificHeatCapacity]) {

    def toSquants: edu.ie3.util.scala.quantities.SpecificHeatCapacity =
      KilowattHoursPerKelvinCubicMeters(
        quantity
          .to(KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
          .getValue
          .doubleValue
      )
  }

  /** Extension for [[ComparableQuantity]] of type [[Irradiance]] that allows
    * conversion into a [[Irradiance]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Irradiance]) {

    def toSquants: squants.radio.Irradiance = WattsPerSquareMeter(
      quantity
        .to(WATT_PER_SQUAREMETRE)
        .getValue
        .doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[Speed]] that allows
    * conversion into a [[Velocity]] squants quantity.
    */
  extension (quantity: ComparableQuantity[Speed]) {

    def toSquants: Velocity = MetersPerSecond(
      quantity
        .to(METRE_PER_SECOND)
        .getValue
        .doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[ThermalResistivity]] that
    * allows conversion into a [[ThermalResistivity]] squants quantity.
    */
  extension (
      quantity: ComparableQuantity[
        edu.ie3.util.quantities.interfaces.ThermalResistivity
      ]
  ) {
    def toSquants: ThermalResistivitySquants = KelvinMetersPerWatt(
      quantity
        .to(KELVIN_METRE_PER_WATT)
        .getValue
        .doubleValue
    )
  }

  /** Extension for [[ComparableQuantity]] of type [[ThermalCapacitance]] that
    * allows conversion into a [[ThermalCapacitance]] squants quantity.
    */
  extension (
      quantity: ComparableQuantity[
        edu.ie3.util.quantities.interfaces.ThermalCapacitance
      ]
  ) {
    def toSquants: ThermalCapacitanceSquants =
      JoulesPerMeterKelvin( // FIXME JoulesPerMeter or CubicMeter
        quantity
          .to(JOULE_PER_CUBIC_METRE_KELVIN)
          .getValue
          .doubleValue
      )
  }
}
