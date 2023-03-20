/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import java.util.UUID
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{ThermalBusInput, ThermalHouseInput}
import edu.ie3.util.quantities.interfaces.{HeatCapacity, ThermalConductance}

import javax.measure.quantity.{Energy, Power, Temperature, Time}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units.HOUR
import squants.Kelvin
import squants.energy.{KilowattHours, Kilowatts, MegawattHours, Megawatts}
import squants.thermal.ThermalCapacity
import squants.time.{Hours, Seconds}
import tech.units.indriya.unit.Units
import edu.ie3.simona.model.thermal.ThermalHouse.{ThermalHouseState, temperatureTolerance}
import edu.ie3.util.quantities.PowerSystemUnits

import java.util.UUID

/** A thermal house model including a variable inner temperature <p> *
  * <strong>Important:</strong> The field innerTemperature is a variable.
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human readable id
  * @param operatorInput
  *   Operator input
  * @param operationTime
  *   Operation time
  * @param bus
  *   Thermal bus input
  * @param ethLosses
  *   transmission coefficient of heat storage, usually in [kW/K]
  * @param ethCapa
  *   heat energy storage capability of thermal house, usually in [kWh/K]
  */
final case class ThermalHouse(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
  ethLosses: squants.Power, // FIXME thermal conductance, power per 1K
  ethCapa: ThermalCapacity,
  lowerBoundaryTemperature: squants.Temperature,
  upperBoundaryTemperature: squants.Temperature
) extends ThermalSink(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus
    ) {

  /** Check if inner temperature is higher than preferred maximum temperature
    *
    * @return
    *   true, if inner temperature is too high
    */
  def isInnerTemperatureTooHigh(
      innerTemperature: squants.Temperature
  ): Boolean =
    innerTemperature > Kelvin(
      upperBoundaryTemperature.toKelvinScale - temperatureTolerance.toKelvinScale
    )

  /** Check if inner temperature is lower than preferred minimum temperature
    *
    * @return
    *   true, if inner temperature is too low
    */
  def isInnerTemperatureTooLow(
      innerTemperature: squants.Temperature,
      boundaryTemperature: squants.Temperature = lowerBoundaryTemperature
  ): Boolean =
    innerTemperature < Kelvin(
      boundaryTemperature.toKelvinScale + temperatureTolerance.toKelvinScale
    )

  /** Calculate the new inner temperature of the thermal house.
    *
    * @param thermalPower
    *   added thermal power (e.g. of heat pump)
    * @param duration
    *   time step length
    * @param currentInnerTemperature
    *   current inner temperature
    * @param ambientTemperature
    *   ambient temperature of thermal house
    * @return
    *   new inner temperature
    */
  def newInnerTemperature(
      thermalPower: squants.Power,
      duration: squants.Time,
      currentInnerTemperature: squants.Temperature,
      ambientTemperature: squants.Temperature
  ): squants.Temperature = {
    val thermalEnergyChange = calcThermalEnergyChange(
      calcThermalEnergyGain(thermalPower, duration),
      calcThermalEnergyLoss(
        currentInnerTemperature,
        ambientTemperature,
        duration
      )
    )
    calcNewInnerTemperature(
      currentInnerTemperature,
      calcInnerTemperatureChange(thermalEnergyChange)
    )
  }

  /** Calculate the new inner temperature of the thermal house
    *
    * @param oldInnerTemperature
    *   previous inner temperature
    * @param temperatureChange
    *   temperature change
    * @return
    *   new inner temperature
    */
  private def calcNewInnerTemperature(
      oldInnerTemperature: squants.Temperature,
      temperatureChange: squants.Temperature
  ): squants.Temperature =
    Kelvin(oldInnerTemperature.toKelvinScale + temperatureChange.toKelvinScale)

  /** Calculate the temperature change for the thermal house form the thermal
    * energy change
    *
    * @param thermalEnergyChange
    *   thermal energy change (e.g. through heat pump)
    * @return
    *   temperature change
    */
  private def calcInnerTemperatureChange(
      thermalEnergyChange: squants.Energy
  ): squants.Temperature = {
    thermalEnergyChange / ethCapa
  }

  /** Calculate the thermal energy change combining the added and lost energy
    *
    * @param thermalEnergyGain
    *   thermal energy added
    * @param thermalEnergyLoss
    *   thermal energy lost
    * @return
    *   thermal energy change
    */
  private def calcThermalEnergyChange(
      thermalEnergyGain: squants.Energy,
      thermalEnergyLoss: squants.Energy
  ): squants.Energy =
    thermalEnergyGain - thermalEnergyLoss

  /** Calculate the thermal energy gain, e.g. due to a running heat pump
    *
    * @param pThermal
    *   added thermal power (e.g. of heat pump)
    * @param time
    *   time step length in which thermal power is added
    * @return
    *   resulting thermal energy gain
    */
  private def calcThermalEnergyGain(
      pThermal: squants.Power,
      time: squants.Time
  ): squants.Energy = pThermal * time

  /** Calculate the thermal energy loss due to the temperature deviation over
    * the time step
    *
    * @param innerTemperature
    *   previous inner temperature
    * @param ambientTemperature
    *   ambient temperature of thermal house
    * @param time
    *   time step length
    * @return
    *   resulting thermal energy loss
    */
  private def calcThermalEnergyLoss(
      innerTemperature: squants.Temperature,
      ambientTemperature: squants.Temperature,
      time: squants.Time
  ): squants.Energy = {
    val temperatureDeviation =
      innerTemperature.toKelvinScale - ambientTemperature.toKelvinScale
    (ethLosses * time) * temperatureDeviation
  }

}

case object ThermalHouse {

  /** Internal method to construct a new [[ThermalHouse]] based on a provided
    * [[ThermalHouseInput]]
    *
    * @param input
    *   instance of [[ThermalHouseInput]] this thermal house should be built
    *   from
    * @return
    *   a ready-to-use [[ThermalHouse]] with referenced electric parameters
    */
  def apply(
      input: ThermalHouseInput
  ): ThermalHouse =
    new ThermalHouse(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      Kilowatts(
        input.getEthLosses
          .to(PowerSystemUnits.KILOWATT_PER_KELVIN)
          .getValue
          .doubleValue
      ),
      KilowattHours(
        input.getEthCapa
          .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN)
          .getValue
          .doubleValue
      ) / Kelvin(1d),
      squants.thermal.Kelvin(
        input.getLowerTemperatureLimit.to(Units.KELVIN).getValue.doubleValue
      ),
      squants.thermal.Kelvin(
        input.getUpperTemperatureLimit.to(Units.KELVIN).getValue.doubleValue
      )
    )

}
