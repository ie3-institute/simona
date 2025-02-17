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
  ThermalHouseInput,
}
import edu.ie3.simona.model.participant.HpModel.HpRelevantData
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureTargetOrUpperBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseState,
  temperatureTolerance,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{ThermalConductance, WattsPerKelvin}
import squants.energy.KilowattHours
import squants.thermal.{Kelvin, ThermalCapacity}
import squants.time.{Hours, Seconds}
import squants.{Energy, Power, Temperature, Time}
import tech.units.indriya.unit.Units

import java.util.UUID

/** A thermal house model
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
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
    ethCapa: ThermalCapacity,
    targetTemperature: Temperature,
    lowerBoundaryTemperature: Temperature,
    upperBoundaryTemperature: Temperature,
) extends ThermalSink(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
    ) {

  /** Calculate the energy demand at the instance in question. The amount to
    * reach target temperature of this model is interpreted as required demand.
    * The amount to reach the current @actualTemperatureTarget (could be target
    * temperature or upper temperature boundary of this model) is interpreted as
    * possible demand. The current (external) thermal infeed is not accounted
    * for, as we assume, that after determining the thermal demand, a change in
    * external infeed will take place.
    *
    * @param relevantData
    *   Data of heat pump including state of the heat pump.
    * @param state
    *   most recent state, that is valid for this model
    * @param actualTargetTemperature
    *   the applied target temperature for this model
    * @return
    *   The needed energy in the questioned tick.
    */
  def energyDemand(
      relevantData: HpRelevantData,
      state: ThermalHouseState,
      actualTargetTemperature: Temperature,
  ): ThermalEnergyDemand = {
    /* Calculate the inner temperature of the house, at the questioned instance in time */
    val duration = Seconds(relevantData.currentTick - state.tick)
    val currentInnerTemp = newInnerTemperature(
      state.qDot,
      duration,
      state.innerTemperature,
      relevantData.ambientTemperature,
    )

    val requiredEnergy =
      if (isInnerTemperatureTooLow(currentInnerTemp)) {
        energy(targetTemperature, currentInnerTemp)
      } else
        zeroKWh

    val possibleEnergy =
      if (
        !isInnerTemperatureTooHigh(currentInnerTemp, actualTargetTemperature)
      ) {
        energy(actualTargetTemperature, currentInnerTemp)
      } else zeroKWh

    ThermalEnergyDemand(requiredEnergy, possibleEnergy)
  }

  /** Calculate the needed energy to change from start temperature to target
    * temperature.
    *
    * In edge cases, i.e. within the tolerance margin of target temperatures,
    * the temperature difference can be negative. For these cases we set the
    * temperature difference to zero, resulting in an energy demand of 0 kWh.
    *
    * @param targetTemperature
    *   The target temperature to reach.
    * @param startTemperature
    *   The starting temperature.
    * @return
    *   The needed energy to change.
    */
  private def energy(
      targetTemperature: Temperature,
      startTemperature: Temperature,
  ): Energy = {
    val temperatureDiff =
      Kelvin(targetTemperature.toKelvinScale - startTemperature.toKelvinScale)
        .max(Kelvin(0))

    ethCapa * temperatureDiff
  }

  /** Check if inner temperature is higher than preferred maximum temperature
    * @param innerTemperature
    *   the inner temperature of the house
    * @param boundaryTemperature
    *   the applied boundary temperature to check against
    *
    * @return
    *   true, if inner temperature is too high
    */
  def isInnerTemperatureTooHigh(
      innerTemperature: Temperature,
      boundaryTemperature: Temperature,
  ): Boolean =
    innerTemperature > (
      boundaryTemperature - temperatureTolerance
    )

  /** Check if inner temperature is lower than preferred minimum temperature
    *
    * @return
    *   true, if inner temperature is too low
    */
  def isInnerTemperatureTooLow(
      innerTemperature: Temperature,
      boundaryTemperature: Temperature = lowerBoundaryTemperature,
  ): Boolean =
    innerTemperature < (
      boundaryTemperature + temperatureTolerance
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
      thermalPower: Power,
      duration: Time,
      currentInnerTemperature: Temperature,
      ambientTemperature: Temperature,
  ): Temperature = {
    val thermalEnergyGain = thermalPower * duration

    // thermal energy loss due to the deviation between outside and inside temperature
    val thermalEnergyLoss = ethLosses.calcThermalEnergyChange(
      currentInnerTemperature,
      ambientTemperature,
      duration,
    )

    val energyChange = thermalEnergyGain - thermalEnergyLoss

    // temperature change calculated from energy change(WattHours) and thermal capacity(Joules per Kelvin)
    val temperatureChange = energyChange / ethCapa

    // return value new inner temperature
    currentInnerTemperature + temperatureChange
  }

  /** Update the current state of the house.
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param state
    *   Currently applicable state
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param qDot
    *   New thermal influx
    * @param actualTargetTemperature
    *   the applied target temperature for this model
    * @return
    *   Updated state and the tick in which the next threshold is reached
    */
  def determineState(
      relevantData: HpRelevantData,
      state: ThermalHouseState,
      lastAmbientTemperature: Temperature,
      qDot: Power,
      actualTargetTemperature: Temperature,
  ): (ThermalHouseState, Option[ThermalThreshold]) = {
    val duration = Seconds(relevantData.currentTick - state.tick)
    val updatedInnerTemperature = newInnerTemperature(
      state.qDot,
      duration,
      state.innerTemperature,
      lastAmbientTemperature,
    )

    /* Calculate the next given threshold */
    val threshold =
      nextThreshold(
        relevantData.currentTick,
        qDot,
        updatedInnerTemperature,
        relevantData.ambientTemperature,
        actualTargetTemperature,
      )

    (
      state.copy(
        tick = relevantData.currentTick,
        innerTemperature = updatedInnerTemperature,
        qDot = qDot,
      ),
      threshold,
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
    * @param actualTargetTemperature
    *   the applied target temperature for this model
    * @return
    *   The next threshold, that will be reached
    */
  private def nextThreshold(
      tick: Long,
      qDotExternal: Power,
      innerTemperature: Temperature,
      ambientTemperature: Temperature,
      actualTargetTemperature: Temperature,
  ): Option[ThermalThreshold] = {
    val artificialDuration = Hours(1d)
    val loss = ethLosses.calcThermalEnergyChange(
      innerTemperature,
      ambientTemperature,
      artificialDuration,
    ) / artificialDuration
    val resultingQDot = qDotExternal - loss
    if (
      resultingQDot < zeroMW && !isInnerTemperatureTooLow(
        innerTemperature
      )
    ) {
      /* House has more losses than gain */
      nextActivation(
        tick,
        innerTemperature,
        lowerBoundaryTemperature,
        resultingQDot,
      ).map(HouseTemperatureLowerBoundaryReached)
    } else if (
      resultingQDot > zeroMW && !isInnerTemperatureTooHigh(
        innerTemperature,
        actualTargetTemperature,
      )
    ) {
      /* House has more gain than losses */
      nextActivation(
        tick,
        actualTargetTemperature,
        innerTemperature,
        resultingQDot,
      ).map(HouseTemperatureTargetOrUpperBoundaryReached)
    } else {
      /* House is in perfect balance */
      None
    }
  }

  private def nextActivation(
      tick: Long,
      higherTemperature: Temperature,
      lowerTemperature: Temperature,
      qDot: Power,
  ): Option[Long] = {
    val flexibleEnergy = energy(higherTemperature, lowerTemperature)
    if (flexibleEnergy < zeroMWh)
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
  protected def temperatureTolerance: Temperature = Kelvin(0.01d)

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
        .doubleValue
        * 1000 // kW/K to W/K
    ),
    KilowattHours(
      input.getEthCapa
        .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN)
        .getValue
        .doubleValue
    ) / Kelvin(1d),
    Kelvin(
      input.getTargetTemperature.to(Units.KELVIN).getValue.doubleValue
    ),
    Kelvin(
      input.getLowerTemperatureLimit.to(Units.KELVIN).getValue.doubleValue
    ),
    Kelvin(
      input.getUpperTemperatureLimit.to(Units.KELVIN).getValue.doubleValue
    ),
  )

  /** State of a thermal house
    *
    * @param tick
    *   Last tick of temperature change
    * @param innerTemperature
    *   Inner temperature of the house
    * @param qDot
    *   Continuous external infeed of thermal energy since the given tick
    */
  final case class ThermalHouseState(
      tick: Long,
      innerTemperature: Temperature,
      qDot: Power,
  )

  def startingState(house: ThermalHouse): ThermalHouseState =
    ThermalHouseState(
      -1L,
      house.targetTemperature,
      zeroMW,
    )

  object ThermalHouseThreshold {
    final case class HouseTemperatureLowerBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
    final case class HouseTemperatureTargetOrUpperBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
  }
}
