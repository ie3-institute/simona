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
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseState,
  temperatureTolerance,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{ThermalConductance, WattsPerKelvin}
import squants.energy.KilowattHours
import squants.space.Litres
import squants.thermal.{Celsius, Kelvin, ThermalCapacity}
import squants.time.{Hours, Seconds}
import squants.{Energy, Power, Temperature, Time, Volume}
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID
import java.time.Duration
import java.time.temporal.ChronoUnit

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
  * @param housingType
  *   type of house, either `house` or `flat`
  * @param houseInhabitants
  *   number of people living in the building
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
    housingType: String,
    houseInhabitants: Double,
) extends ThermalSink(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
    ) {

  /** Calculate the energy demand for heating at the instance in question. If
    * the inner temperature is at or above the lower boundary temperature, there
    * is no demand. If it is below the target temperature, the demand is the
    * energy needed to heat up the house to the maximum temperature. The current
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
    *   the needed energy for heating in the questioned tick
    */
  def energyDemandHeating(
      tick: Long,
      ambientTemperature: Temperature,
      state: ThermalHouseState,
  ): ThermalEnergyDemand = {
    /* Calculate the inner temperature of the house, at the questioned instance in time */
    val duration = Seconds(tick - state.tick)
    val currentInnerTemp = newInnerTemperature(
      state.qDot,
      duration,
      state.innerTemperature,
      ambientTemperature,
    )

    /* Determine, which temperature boundary triggers a needed energy to reach the temperature constraints */
    val temperatureToTriggerRequiredEnergy =
      if (
        currentInnerTemp <= state.innerTemperature &&
        state.qDot <= zeroKW
      ) {
        // temperature has been decreasing and heat source has been turned off
        // => we have reached target temp before and are now targeting lower temp
        lowerBoundaryTemperature
      } else targetTemperature
    val requiredEnergy =
      if (
        isInnerTemperatureTooLow(
          currentInnerTemp,
          temperatureToTriggerRequiredEnergy,
        )
      ) energy(targetTemperature, currentInnerTemp)
      else
        zeroMWh

    val possibleEnergy =
      if (!isInnerTemperatureTooHigh(currentInnerTemp)) {
        // if upper boundary has not been reached,
        // there is an amount of optional energy that could be stored
        energy(upperBoundaryTemperature, currentInnerTemp)
      } else
        zeroMWh
    ThermalEnergyDemand(requiredEnergy, possibleEnergy)
  }

  /** Calculate the energy demand for warm water at the instance in question.
    *
    * @param tick
    *   Questionable tick
    * @param state
    *   most recent state, that is valid for this model
    * @param simulationStart
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   the needed energy for heating in the questioned tick
    */

  def energyDemandDomesticHotWater(
      tick: Long,
      state: Option[ThermalHouseState],
      simulationStart: ZonedDateTime,
      houseInhabitants: Double,
  ): ThermalEnergyDemand = {

    def calculateThermalEnergyOfWaterDemand(tick: Long): Energy = {
      val waterDemand =
        waterDemandOfHour(tick, simulationStart, houseInhabitants, housingType)
      thermalEnergyDemandWater(waterDemand, Celsius(10d), Celsius(55d))
    }

    if (tick == 0) {
      val demand = calculateThermalEnergyOfWaterDemand(tick)
      ThermalEnergyDemand(demand, demand)
    } else {
      state match {
        case Some(state) =>
          val lastStateTime =
            simulationStart.plusSeconds(math.max(state.tick, 0))
          val actualStateTime = simulationStart.plusSeconds(tick)
          val timeDiffSeconds =
            Duration.between(lastStateTime, actualStateTime).toSeconds

          if (timeDiffSeconds > 3600) {
            val hoursDiff = (timeDiffSeconds / 3600).toInt
            val energyDemandWaterPastHours =
              (0 until hoursDiff).foldLeft(zeroKWH) { (acc, i) =>
                val nextFullHourTick = Duration
                  .between(
                    simulationStart,
                    lastStateTime.plusHours(1 + i).truncatedTo(ChronoUnit.HOURS),
                  )
                  .toSeconds
                acc + calculateThermalEnergyOfWaterDemand(nextFullHourTick)
              }
            // also get the demand of the actual hour if tick exactly hits the full hour
            val energyDemandWater = if (tick % 3600 == 0)
              energyDemandWaterPastHours + calculateThermalEnergyOfWaterDemand(
                tick
              )
            else energyDemandWaterPastHours
            ThermalEnergyDemand(energyDemandWater, energyDemandWater)
          } else if (actualStateTime.getHour != lastStateTime.getHour) {
            val demand = calculateThermalEnergyOfWaterDemand(tick)
            ThermalEnergyDemand(demand, demand)
          } else ThermalEnergyDemand(zeroKWH, zeroKWH)

        case None => ThermalEnergyDemand.noDemand
      }
    }
  }

  /** Calculate the energy required to heat up a given volume of water from a
    * start to an end temperature
    *
    * @param waterDemand
    *   water volume to get heated up
    * @param startTemperature
    *   starting Temperature
    * @param endTemperature
    *   end Temperature
    * @return
    *   the needed energy
    */

  private def thermalEnergyDemandWater(
      waterDemand: Volume,
      startTemperature: Temperature,
      endTemperature: Temperature,
  ): Energy = {
    if (endTemperature < startTemperature)
      throw new RuntimeException(
        s"End temperature of $endTemperature is lower than the start temperature $startTemperature for the water heating system."
      )

    waterDemand.toCubicMeters * KilowattHours(
      1.16
    ) * (endTemperature.toCelsiusDegrees - startTemperature.toCelsiusDegrees)
  }

  /** Provides water demand of an building (house or flat) for a given hour of
    * the day based on the housingType and the number of inhabitants
    *
    * @param tick
    *   actual simulation tick
    * @param simulationStart
    *   simulations start time as ZonedDateTime
    * @param noPersonsInHoushold
    *   number of persons living in the building
    * @param housingType
    *   type of the building, either `house` or `flat`
    * @return
    *   the needed energy
    */

  private def waterDemandOfHour(
      tick: Long,
      simulationStart: ZonedDateTime,
      noPersonsInHoushold: Double,
      housingType: String,
  ): Volume = {

    // Volume => VDI 2067 Blatt 12
    // Time series relative => DIN EN 12831-3 Table B.2 for single family houses and for flats
    val waterVolumeRelativeHouse: Map[Int, Double] = Map(
      0 -> 0.018,
      1 -> 0.01,
      2 -> 0.006,
      3 -> 0.003,
      4 -> 0.004,
      5 -> 0.006,
      6 -> 0.024,
      7 -> 0.047,
      8 -> 0.068,
      9 -> 0.057,
      10 -> 0.061,
      11 -> 0.061,
      12 -> 0.063,
      13 -> 0.064,
      14 -> 0.051,
      15 -> 0.044,
      16 -> 0.043,
      17 -> 0.047,
      18 -> 0.057,
      19 -> 0.065,
      20 -> 0.066,
      21 -> 0.058,
      22 -> 0.045,
      23 -> 0.032,
    )
    val waterVolumeRelativeFlat: Map[Int, Double] = Map(
      0 -> 0.01,
      1 -> 0.01,
      2 -> 0.01,
      3 -> 0.0,
      4 -> 0.0,
      5 -> 0.1,
      6 -> 0.03,
      7 -> 0.06,
      8 -> 0.08,
      9 -> 0.06,
      10 -> 0.05,
      11 -> 0.05,
      12 -> 0.06,
      13 -> 0.06,
      14 -> 0.05,
      15 -> 0.04,
      16 -> 0.04,
      17 -> 0.05,
      18 -> 0.06,
      19 -> 0.07,
      20 -> 0.07,
      21 -> 0.06,
      22 -> 0.05,
      23 -> 0.02,
    )

    val waterDemandVolumePerPersonYear =
      // Shower and Bath + washbasin + dish washing per hand (also dish washer in the building)
      Litres(8600 + 4200 + 300)

    val isHouse: Boolean =
      housingType.toLowerCase match {
        case "house" => true
        case "flat"  => false
        case _ =>
          throw new IllegalArgumentException(
            s"Invalid housing type: $housingType. Expected 'house' or 'flat'."
          )
      }

    val currentHour: Int = simulationStart.plusSeconds(tick).getHour

    val waterVolumeRelative: Map[Int, Double] =
      if (isHouse) waterVolumeRelativeHouse else waterVolumeRelativeFlat

    waterVolumeRelative.getOrElse(
      currentHour,
      throw new RuntimeException(
        "Couldn't get the actual hour to determine water demand"
      ),
    ) * noPersonsInHoushold * waterDemandVolumePerPersonYear / 365
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
      startTemperature: Temperature,
  ): Energy = {
    ethCapa * Kelvin(
      targetTemperature.toKelvinScale - startTemperature.toKelvinScale
    )
  }

  /** Check if inner temperature is higher than preferred maximum temperature
    *
    * @return
    *   true, if inner temperature is too high
    */
  def isInnerTemperatureTooHigh(
      innerTemperature: Temperature,
      boundaryTemperature: Temperature = upperBoundaryTemperature,
  ): Boolean =
    innerTemperature > Kelvin(
      boundaryTemperature.toKelvinScale - temperatureTolerance.toKelvinScale
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

  /** Update the current state of the house
    *
    * @param tick
    *   Current instance in time
    * @param state
    *   Currently applicable state
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param qDot
    *   New thermal influx
    * @return
    *   Updated state and the tick in which the next threshold is reached
    */
  def determineState(
      tick: Long,
      state: ThermalHouseState,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      qDot: Power,
  ): (ThermalHouseState, Option[ThermalThreshold]) = {
    val duration = Seconds(tick - state.tick)
    val updatedInnerTemperature = newInnerTemperature(
      state.qDot,
      duration,
      state.innerTemperature,
      lastAmbientTemperature,
    )

    /* Calculate the next given threshold */
    val threshold =
      nextThreshold(tick, qDot, updatedInnerTemperature, ambientTemperature)

    (
      state.copy(
        tick = tick,
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
    * @return
    *   The next threshold, that will be reached
    */
  private def nextThreshold(
      tick: Long,
      qDotExternal: Power,
      innerTemperature: Temperature,
      ambientTemperature: Temperature,
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
        innerTemperature
      )
    ) {
      /* House has more gain than losses */
      nextActivation(
        tick,
        upperBoundaryTemperature,
        innerTemperature,
        resultingQDot,
      ).map(HouseTemperatureUpperBoundaryReached)
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
    input.getHousingType,
    input.getNumberOfInhabitants,
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
    final case class HouseTemperatureUpperBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
  }
}
