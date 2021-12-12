/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import java.util.UUID

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  ThermalBusInput,
  ThermalHouseInput
}
import edu.ie3.util.quantities.interfaces.{HeatCapacity, ThermalConductance}
import javax.measure.quantity.{Energy, Power, Temperature, Time}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units.HOUR

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
    ethLosses: ComparableQuantity[ThermalConductance],
    ethCapa: ComparableQuantity[HeatCapacity],
    lowerBoundaryTemperature: ComparableQuantity[Temperature],
    upperBoundaryTemperature: ComparableQuantity[Temperature]
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
      innerTemperature: ComparableQuantity[Temperature]
  ): Boolean =
    innerTemperature.isGreaterThan(upperBoundaryTemperature)

  /** Check if inner temperature is lower than preferred minimum temperature
    *
    * @return
    *   true, if inner temperature is too low
    */
  def isInnerTemperatureTooLow(
      innerTemperature: ComparableQuantity[Temperature]
  ): Boolean =
    innerTemperature.isLessThan(lowerBoundaryTemperature)

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
      thermalPower: ComparableQuantity[Power],
      duration: ComparableQuantity[Time],
      currentInnerTemperature: ComparableQuantity[Temperature],
      ambientTemperature: ComparableQuantity[Temperature]
  ): ComparableQuantity[Temperature] = {
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
      oldInnerTemperature: ComparableQuantity[Temperature],
      temperatureChange: ComparableQuantity[Temperature]
  ): ComparableQuantity[Temperature] =
    oldInnerTemperature.add(temperatureChange)

  /** Calculate the temperature change for the thermal house form the thermal
    * energy change
    *
    * @param thermalEnergyChange
    *   thermal energy change (e.g. through heat pump)
    * @return
    *   temperature change
    */
  private def calcInnerTemperatureChange(
      thermalEnergyChange: ComparableQuantity[Energy]
  ): ComparableQuantity[Temperature] = {
    thermalEnergyChange
      .divide(ethCapa)
      .asType(classOf[Temperature])
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
      thermalEnergyGain: ComparableQuantity[Energy],
      thermalEnergyLoss: ComparableQuantity[Energy]
  ): ComparableQuantity[Energy] =
    thermalEnergyGain.subtract(thermalEnergyLoss)

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
      pThermal: ComparableQuantity[Power],
      time: ComparableQuantity[Time]
  ): ComparableQuantity[Energy] =
    pThermal.multiply(time).asType(classOf[Energy])

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
      innerTemperature: ComparableQuantity[Temperature],
      ambientTemperature: ComparableQuantity[Temperature],
      time: ComparableQuantity[Time]
  ): ComparableQuantity[Energy] = {
    val temperatureDeviation = innerTemperature.subtract(ambientTemperature)
    temperatureDeviation
      .multiply(ethLosses.multiply(time.to(HOUR)))
      .asType(classOf[Energy])
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
      input.getEthLosses,
      input.getEthCapa,
      input.getLowerTemperatureLimit,
      input.getUpperTemperatureLimit
    )

}
