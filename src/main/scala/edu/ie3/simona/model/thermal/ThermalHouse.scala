/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  ThermalBusInput,
  ThermalHouseInput
}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached
}
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseState,
  temperatureTolerance
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.{
  KilowattHoursPerKelvinCubicMeters,
  SpecificHeatCapacity,
  ThermalConductance,
  WattsPerKelvin
}
import squants.energy.{KilowattHours, MegawattHours, Megawatts}
import squants.space.CubicMeters
import squants.thermal.{Celsius, Kelvin}
import squants.time.Hours
import squants.{Energy, Power, Temperature, Time}
import tech.units.indriya.unit.Units

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
  * @param targetTemperature
  *   Target room temperature [K]
  * @param lowerBoundaryTemperature
  *   Lower temperature boundary [K]
  * @param upperBoundaryTemperature
  *   Upper boundary temperature [K]
  */
final case class ThermalHouse(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    ethLosses: ThermalConductance,
    ethCapa: SpecificHeatCapacity,
    targetTemperature: Temperature,
    targetTemperature: Temperature,
    lowerBoundaryTemperature: Temperature,
    upperBoundaryTemperature: Temperature
) extends ThermalSink(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus
    ) {

  /** Calculate the energy demand at the instance in question. If the inner
    * temperature is at or above the lower boundary temperature, there is no
    * demand. If it is below the target temperature, the demand is the energy
    * needed to heat up the house to the maximum temperature. The current
    * (external) thermal infeed is not accounted for, as we assume, that after
    * determining the thermal demand, a change in external infeed will take
    * place.
    *
    * @param tick
    *   Questionable tick
    * @param ambientTemperature
    *   Ambient temperature in the instance in question
    * @param state
    *   most recent state, that is valid for this model
    * @return
    *   the needed energy in the questioned tick
    */
  def energyDemand(
      tick: Long,
      ambientTemperature: Temperature,
      state: ThermalHouseState
  ): ThermalEnergyDemand = {
    /* Calculate the inner temperature of the house, at the questioned instance in time */
    val duration = state.tick.durationUntil(tick)
    val innerTemperature = newInnerTemperature(
      state.qDot,
      duration,
      state.innerTemperature,
      ambientTemperature
    )

    /* Determine the needed energy */
    val requiredEnergy =
      if (isInnerTemperatureTooLow(innerTemperature)) {
        energy(targetTemperature, innerTemperature)
      } else
        MegawattHours(0d)

    val possibleEnergy =
      if (!isInnerTemperatureTooHigh(innerTemperature)) {
        energy(upperBoundaryTemperature, innerTemperature)
      } else
        MegawattHours(0d)
    ThermalEnergyDemand(requiredEnergy, possibleEnergy)
  }

  /** Calculate the needed energy to change from start temperature to target
    * temperature
    *
    * @param targetTemperature
    *   The target temperature to reach
    * @param startTemperature
    *   The starting temperature
    * @return
    *   The needed energy to change
    */
  private def energy(
      targetTemperature: Temperature,
      startTemperature: Temperature
  ): Energy = {
    ethCapa.calcEnergy(targetTemperature, startTemperature, CubicMeters(1d))
  }

  /** Check if inner temperature is higher than preferred maximum temperature
    *
    * @return
    *   true, if inner temperature is too high
    */
  def isInnerTemperatureTooHigh(
      innerTemperature: Temperature
  ): Boolean =
    innerTemperature > (upperBoundaryTemperature - temperatureTolerance)

  /** Check if inner temperature is lower than preferred minimum temperature
    *
    * @return
    *   true, if inner temperature is too low
    */
  def isInnerTemperatureTooLow(
      innerTemperature: Temperature,
      boundaryTemperature: Temperature = lowerBoundaryTemperature
  ): Boolean =
    innerTemperature < (lowerBoundaryTemperature + temperatureTolerance)

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
  private def newInnerTemperature(
      thermalPower: Power,
      duration: Time,
      currentInnerTemperature: Temperature,
      ambientTemperature: Temperature
  ): Temperature = {
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
      oldInnerTemperature: Temperature,
      temperatureChange: Temperature
  ): Temperature =
    Celsius(
      oldInnerTemperature + temperatureChange
    )

  /** Calculate the temperature change for the thermal house form the thermal
    * energy change
    *
    * @param thermalEnergyChange
    *   thermal energy change (e.g. through heat pump)
    * @return
    *   temperature change
    */
  private def calcInnerTemperatureChange(
      thermalEnergyChange: Energy
  ): Temperature = {
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
      thermalEnergyGain: Energy,
      thermalEnergyLoss: Energy
  ): Energy =
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
      pThermal: Power,
      time: Time
  ): Energy = pThermal * time

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
      innerTemperature: Temperature,
      ambientTemperature: Temperature,
      time: Time
  ): Energy = {
    ethLosses.thermalConductanceToEnergy(
      innerTemperature,
      ambientTemperature,
      time
    )
  }

  /** Update the current state of the house
    * @param tick
    *   current instance in time
    * @param state
    *   currently applicable state
    * @param ambientTemperature
    *   Ambient temperature
    * @param qDot
    *   new thermal influx
    * @return
    *   Updated state and the tick in which the next threshold is reached
    */
  def updateState(
      tick: Long,
      state: ThermalHouseState,
      ambientTemperature: Temperature,
      qDot: Power
  ): (ThermalHouseState, Option[ThermalThreshold]) = {
    val duration = state.tick.durationUntil(tick)
    val updatedInnerTemperature = newInnerTemperature(
      state.qDot,
      duration,
      state.innerTemperature,
      ambientTemperature
    )

    /* Calculate the next given threshold */
    val threshold =
      nextThreshold(tick, qDot, updatedInnerTemperature, ambientTemperature)

    (
      state.copy(
        tick = tick,
        innerTemperature = updatedInnerTemperature,
        qDot = qDot
      ),
      threshold
    )
  }

  /** Determine the next threshold, that will be reached
    * @param tick
    *   The current tick
    * @param qDotExternal
    *   The external influx
    * @param innerTemperature
    *   The inner temperature
    * @param ambientTemperature
    *   The ambient temperature
    * @return
    *   The next threshold, that will be reached
    */

  private def nextThreshold(
      tick: Long,
      qDotExternal: Power,
      innerTemperature: Temperature,
      ambientTemperature: Temperature
  ): Option[ThermalThreshold] = {
    val artificialDuration = Hours(1d)
    val loss = calcThermalEnergyLoss(
      innerTemperature,
      ambientTemperature,
      artificialDuration
    ) / artificialDuration

    val resultingQDot = qDotExternal - loss
    if (
      resultingQDot <
        Megawatts(0d)
    ) {
      /* House has more losses than gain */
      val nextTick = nextActivation(
        tick,
        innerTemperature,
        lowerBoundaryTemperature,
        resultingQDot
      )
      Some(HouseTemperatureLowerBoundaryReached(nextTick))
    } else if (
      resultingQDot > (
        Megawatts(0d)
      )
    ) {
      /* House has more gain than losses */
      val nextTick = nextActivation(
        tick,
        upperBoundaryTemperature,
        innerTemperature,
        resultingQDot
      )
      Some(HouseTemperatureUpperBoundaryReached(nextTick))
    } else {
      /* House is in perfect balance */
      None
    }
  }

  private def nextActivation(
      tick: Long,
      higherTemperature: Temperature,
      lowerTemperature: Temperature,
      qDot: Power
  ): Long = {
    val flexibleEnergy = energy(higherTemperature, lowerTemperature)
    if (
      flexibleEnergy <
        KilowattHours(0d)
    )
      tick
    else {
      val duration =
        flexibleEnergy / (qDot * math.signum(qDot.value.doubleValue()))
      tick + duration.toSeconds.toLong
    }
  }
}

object ThermalHouse {
  protected final def temperatureTolerance: Temperature =
    Kelvin(0.01d)

  def apply(input: ThermalHouseInput): ThermalHouse = new ThermalHouse(
    input.getUuid,
    input.getId,
    input.getOperator,
    input.getOperationTime,
    input.getThermalBus,
    WattsPerKelvin(
      input.getEthLosses
        .to(PowerSystemUnits.KILOWATT_PER_KELVIN)
        .getValue
        .doubleValue * 1000
    ),
    // FIXME in PDSDM? Should be KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE
    KilowattHoursPerKelvinCubicMeters(
      input.getEthCapa
        .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN)
        .getValue
        .doubleValue
    ),
    Celsius(input.getTargetTemperature.to(Units.CELSIUS).getValue.doubleValue),
    Celsius(
      input.getLowerTemperatureLimit.to(Units.CELSIUS).getValue.doubleValue
    ),
    Celsius(
      input.getUpperTemperatureLimit.to(Units.CELSIUS).getValue.doubleValue
    )
  )

  /** State of a thermal house
    *
    * @param tick
    *   Last tick of temperature change
    * @param innerTemperature
    *   Inner temperature of the house
    * @param qDot
    *   Continuous infeed of thermal energy since the given tick
    */
  final case class ThermalHouseState(
      override val tick: Long,
      innerTemperature: Temperature,
      qDot: Power
  ) extends ThermalModelState

  def startingState(house: ThermalHouse): ThermalHouseState =
    ThermalHouseState(
      -1L,
      house.targetTemperature,
      Megawatts(0d)
    )

  object ThermalHouseThreshold {
    final case class HouseTemperatureLowerBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
    final case class HouseTemperatureUpperBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
  }
}
