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
import edu.ie3.util.quantities.PowerSystemUnits
import squants.Kelvin
import squants.energy.{KilowattHours, Kilowatts, MegawattHours, Megawatts}
import squants.thermal.ThermalCapacity
import squants.time.{Hours, Seconds}
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
    ethLosses: squants.Power, // FIXME thermal conductance, power per 1K
    ethCapa: ThermalCapacity,
    targetTemperature: squants.Temperature,
    lowerBoundaryTemperature: squants.Temperature,
    upperBoundaryTemperature: squants.Temperature
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
      ambientTemperature: squants.Temperature,
      state: ThermalHouseState
  ): ThermalEnergyDemand = {
    /* Calculate the inner temperature of the house, at the questioned instance in time */
    val duration = Seconds(tick - state.tick)
    val innerTemperature = newInnerTemperature(
      state.qDot,
      duration,
      state.innerTemperature,
      ambientTemperature
    )

    /* Determine, which temperature boundary triggers a needed energy to reach the temperature constraints */
    val temperatureToTriggerRequiredEnergy =
      if (
        innerTemperature <= state.innerTemperature && state.qDot <= Kilowatts(
          0d
        )
      )
        lowerBoundaryTemperature
      else targetTemperature
    val requiredEnergy =
      if (
        isInnerTemperatureTooLow(
          innerTemperature,
          temperatureToTriggerRequiredEnergy
        )
      ) energy(targetTemperature, innerTemperature)
      else
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
      targetTemperature: squants.Temperature,
      startTemperature: squants.Temperature
  ): squants.Energy = {
    val temperatureDifference =
      targetTemperature - startTemperature
    ethCapa * temperatureDifference
  }

  /** Check if inner temperature is higher than preferred maximum temperature
    *
    * @return
    *   true, if inner temperature is too high
    */
  def isInnerTemperatureTooHigh(
      innerTemperature: squants.Temperature
  ): Boolean =
    innerTemperature > upperBoundaryTemperature - temperatureTolerance

  /** Check if inner temperature is lower than preferred minimum temperature
    *
    * @return
    *   true, if inner temperature is too low
    */
  def isInnerTemperatureTooLow(
      innerTemperature: squants.Temperature,
      boundaryTemperature: squants.Temperature = lowerBoundaryTemperature
  ): Boolean =
    innerTemperature < boundaryTemperature + temperatureTolerance

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
    oldInnerTemperature + temperatureChange

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
    val temperatureDeviation = innerTemperature - ambientTemperature
    temperatureDeviation.toKelvinScale * (ethLosses * time) // TODO ?
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
      ambientTemperature: squants.Temperature,
      qDot: squants.Power
  ): (ThermalHouseState, Option[ThermalThreshold]) = {
    val duration = Seconds(tick - state.tick)
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
      qDotExternal: squants.Power,
      innerTemperature: squants.Temperature,
      ambientTemperature: squants.Temperature
  ): Option[ThermalThreshold] = {
    val artificialDuration = Hours(1d)
    val loss = calcThermalEnergyLoss(
      innerTemperature,
      ambientTemperature,
      artificialDuration
    ) / artificialDuration
    val resultingQDot = qDotExternal - loss
    if (
      resultingQDot < Megawatts(0d) && !isInnerTemperatureTooLow(
        innerTemperature
      )
    ) {
      /* House has more losses than gain */
      nextActivation(
        tick,
        innerTemperature,
        lowerBoundaryTemperature,
        resultingQDot
      ).map(HouseTemperatureLowerBoundaryReached)
    } else if (
      resultingQDot > Megawatts(0d) && !isInnerTemperatureTooHigh(
        innerTemperature
      )
    ) {
      /* House has more gain than losses */
      nextActivation(
        tick,
        upperBoundaryTemperature,
        innerTemperature,
        resultingQDot
      ).map(HouseTemperatureUpperBoundaryReached)
    } else {
      /* House is in perfect balance */
      None
    }
  }

  private def nextActivation(
      tick: Long,
      higherTemperature: squants.Temperature,
      lowerTemperature: squants.Temperature,
      qDot: squants.Power
  ): Option[Long] = {
    val flexibleEnergy = energy(higherTemperature, lowerTemperature)
    if (flexibleEnergy < MegawattHours(0d))
      None
    else {
      val duration = Math.round(
        (flexibleEnergy / (qDot * math.signum(qDot.toWatts))).toSeconds
      )
      Some(tick + duration)
    }
  }
}

object ThermalHouse {
  protected final def temperatureTolerance: squants.Temperature = Kelvin(0.01)

  def apply(input: ThermalHouseInput): ThermalHouse = new ThermalHouse(
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
      input.getTargetTemperature.to(Units.KELVIN).getValue.doubleValue
    ),
    squants.thermal.Kelvin(
      input.getLowerTemperatureLimit.to(Units.KELVIN).getValue.doubleValue
    ),
    squants.thermal.Kelvin(
      input.getUpperTemperatureLimit.to(Units.KELVIN).getValue.doubleValue
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
      innerTemperature: squants.Temperature,
      qDot: squants.Power
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
