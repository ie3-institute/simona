/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import java.util.UUID
import edu.ie3.datamodel.models.{OperationTime, StandardUnits}
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  ThermalBusInput,
  ThermalHouseInput
}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseState,
  temperatureTolerance
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.interfaces.{HeatCapacity, ThermalConductance}

import javax.measure.quantity.{Energy, Power, Temperature, Time}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units
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
    ethLosses: ComparableQuantity[ThermalConductance],
    ethCapa: ComparableQuantity[HeatCapacity],
    targetTemperature: ComparableQuantity[Temperature],
    lowerBoundaryTemperature: ComparableQuantity[Temperature],
    upperBoundaryTemperature: ComparableQuantity[Temperature]
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
      ambientTemperature: ComparableQuantity[Temperature],
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
        Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)

    val possibleEnergy =
      if (!isInnerTemperatureTooHigh(innerTemperature)) {
        energy(upperBoundaryTemperature, innerTemperature)
      } else
        Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
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
      targetTemperature: ComparableQuantity[Temperature],
      startTemperature: ComparableQuantity[Temperature]
  ): ComparableQuantity[Energy] = {
    val temperatureDifference =
      targetTemperature
        .to(Units.KELVIN)
        .subtract(startTemperature.to(Units.KELVIN))
    ethCapa
      .multiply(temperatureDifference)
      .asType(classOf[Energy])
      .to(StandardUnits.ENERGY_RESULT)
  }

  /** Check if inner temperature is higher than preferred maximum temperature
    *
    * @return
    *   true, if inner temperature is too high
    */
  def isInnerTemperatureTooHigh(
      innerTemperature: ComparableQuantity[Temperature]
  ): Boolean =
    innerTemperature.isGreaterThan(
      upperBoundaryTemperature.subtract(temperatureTolerance)
    )

  /** Check if inner temperature is lower than preferred minimum temperature
    *
    * @return
    *   true, if inner temperature is too low
    */
  def isInnerTemperatureTooLow(
      innerTemperature: ComparableQuantity[Temperature]
  ): Boolean =
    innerTemperature.isLessThan(
      lowerBoundaryTemperature.add(temperatureTolerance)
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
  private def newInnerTemperature(
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
      ambientTemperature: ComparableQuantity[Temperature],
      qDot: ComparableQuantity[Power]
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
      qDotExternal: ComparableQuantity[Power],
      innerTemperature: ComparableQuantity[Temperature],
      ambientTemperature: ComparableQuantity[Temperature]
  ): Option[ThermalThreshold] = {
    val artificialDuration = Quantities.getQuantity(1d, Units.HOUR)
    val loss = calcThermalEnergyLoss(
      innerTemperature,
      ambientTemperature,
      artificialDuration
    ).divide(artificialDuration)
      .asType(classOf[Power])
      .to(PowerSystemUnits.KILOWATT)
    val resultingQDot = qDotExternal.subtract(loss)
    if (
      resultingQDot.isLessThan(
        Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
      )
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
      resultingQDot.isGreaterThan(
        Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
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
      higherTemperature: ComparableQuantity[Temperature],
      lowerTemperature: ComparableQuantity[Temperature],
      qDot: ComparableQuantity[Power]
  ): Long = {
    val flexibleEnergy = energy(higherTemperature, lowerTemperature)
    if (
      flexibleEnergy.isLessThan(
        Quantities.getQuantity(0d, StandardUnits.ENERGY_IN)
      )
    )
      tick
    else {
      val duration = flexibleEnergy
        .divide(qDot.multiply(math.signum(qDot.getValue.doubleValue())))
        .asType(classOf[Time])
        .to(Units.SECOND)
        .getValue
        .longValue()
      tick + duration
    }
  }
}

object ThermalHouse {
  protected final def temperatureTolerance: ComparableQuantity[Temperature] =
    Quantities.getQuantity(0.01, Units.KELVIN)

  def apply(input: ThermalHouseInput): ThermalHouse = new ThermalHouse(
    input.getUuid,
    input.getId,
    input.getOperator,
    input.getOperationTime,
    input.getThermalBus,
    input.getEthLosses,
    input.getEthCapa,
    input.getTargetTemperature,
    input.getLowerTemperatureLimit,
    input.getUpperTemperatureLimit
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
      innerTemperature: ComparableQuantity[Temperature],
      qDot: ComparableQuantity[Power]
  ) extends ThermalModelState

  def startingState(house: ThermalHouse): ThermalHouseState =
    ThermalHouseState(
      -1L,
      house.targetTemperature,
      Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
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
