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
import edu.ie3.util.scala.quantities.QuantityUtil.*
import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.participant.ParticipantModel.ModelState
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalEnergyDemand
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTargetTemperatureReached,
  HouseTemperatureLowerBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseState,
  temperatureTolerance,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.SquantsUtils.RichThermalCapacity
import edu.ie3.util.scala.quantities.{ThermalConductance, WattsPerKelvin}
import squants.energy.KilowattHours
import squants.space.Litres
import squants.thermal.{Celsius, Kelvin, ThermalCapacity}
import squants.time.Seconds
import squants.{Energy, Power, Temperature, Time, Volume}
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
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

  /** Calculates the energy demand for heating at the instance in question by
    * calculating the [[ThermalEnergyDemand]] to reach target temperature from
    * actual inner temperature. Since [[ThermalEnergyDemand]] is two parted,
    * requiredEnergy and possibleEnergy, both have to be determined.
    * RequiredEnergy: In case the inner temperature is at or below the lower
    * boundary temperature, the energy demand till target temperature is
    * interpreted as requiredEnergy. Otherwise, the required energy will be
    * zero. PossibleEnergy: In case the inner temperature is not at or above the
    * target temperature, the energy demand to reach targetTemperature is
    * interpreted as possible energy. Otherwise, it will be zero. The current
    * (external) thermal feed in is not accounted for, as we assume, that after
    * determining the thermal demand, a change in external feed in will take
    * place.
    *
    * @param thermalHouseState
    *   Current state, that is valid for this model.
    * @return
    *   The needed energy for heating in the questioned tick.
    */
  def energyDemandHeating(
      thermalHouseState: ThermalHouseState
  ): ThermalEnergyDemand = {
    // Since we updated the state before, we can directly take the innerTemperature
    val currentInnerTemp = thermalHouseState.innerTemperature

    val requiredEnergy =
      if (isInnerTemperatureTooLow(currentInnerTemp)) {
        energy(targetTemperature, currentInnerTemp)
      } else
        zeroKWh

    val possibleEnergy =
      if (!isInnerTemperatureTooHigh(currentInnerTemp)) {
        energy(targetTemperature, currentInnerTemp)
      } else zeroKWh

    ThermalEnergyDemand(requiredEnergy, possibleEnergy)
  }

  /** Checks if the simulation reached a new full hour in the simulation time.
    * If we already simulated a tick within the current simulation time hour, it
    * returns None. Else a sequence of the hours since the tick of state will be
    * returned.
    *
    * @param tick
    *   The actual simulation tick.
    * @param simulationTime
    *   The actual time of the simulation.
    * @param state
    *   The state of the heat pump.
    * @return
    *   A sequence of the absolute hour in simulation time. Or None, if we
    *   already simulated a tick within the actual hour.
    */

  def checkIfNeedToDetermineDomesticHotWaterDemand(
      tick: Long,
      simulationTime: ZonedDateTime,
      state: HpState,
  ): Option[Seq[Int]] = {
    if (tick == 0) {
      Some(Seq(simulationTime.getHour))
    } else {
      val timeDiffSeconds = tick - state.tick
      val lastStateTime = simulationTime.minusSeconds(timeDiffSeconds)
      if (timeDiffSeconds > 3600) {
        val hoursDiff = (timeDiffSeconds / 3600).toInt

        Some(
          (1 to hoursDiff + 1).map(hour => (lastStateTime.getHour + hour) % 24)
        )

      } else if (simulationTime.getHour != lastStateTime.getHour) {
        Some(Seq(simulationTime.getHour))
      } else {
        None
      }
    }
  }

  /** Calculate the energy demand for warm water at the instance in question.
    *
    * @param hoursWaterDemandToDetermine
    *   Sequence of hours for which the energy demand should be determined.
    * @param thermalHouseState
    *   Actual state, that is valid for this model.
    * @return
    *   The needed energy for hot domestic water for the questioned hours.
    */

  def energyDemandDomesticHotWater(
      hoursWaterDemandToDetermine: Option[Seq[Int]],
      thermalHouseState: Option[ThermalHouseState],
  ): ThermalEnergyDemand = {
    val totalEnergyDemand = hoursWaterDemandToDetermine match {
      case Some(hours) =>
        hours.foldLeft(zeroKWh) { (accumulator, hour) =>
          accumulator + calculateThermalEnergyOfWaterDemand(hour)
        }
      case None =>
        zeroKWh
    }
    ThermalEnergyDemand(totalEnergyDemand, totalEnergyDemand)
  }

  /** Calculate the energy demand for warm water for some specific hour at the
    * instance in question.
    *
    * @param hour
    *   The hour for that the energy demand should be determined.
    * @return
    *   The needed energy for domestic hot water in the questioned hour.
    */

  private def calculateThermalEnergyOfWaterDemand(hour: Int): Energy = {
    val waterDemand =
      waterDemandOfHour(hour, houseInhabitants, housingType)
    thermalEnergyDemandWater(waterDemand, Celsius(10d), Celsius(55d))
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
    * @param hour
    *   The hour for which the demand is to be calculated.
    * @param noPersonsInHoushold
    *   number of persons living in the building
    * @param housingType
    *   type of the building, either `house` or `flat`
    * @return
    *   the needed energy
    */

  private def waterDemandOfHour(
      hour: Int,
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
      // Shower and Bath + bathroom sink + dish washing per hand (also dishwasher in the building)
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

    val waterVolumeRelative: Map[Int, Double] =
      if (isHouse) waterVolumeRelativeHouse else waterVolumeRelativeFlat

    waterVolumeRelative.getOrElse(
      hour,
      throw new RuntimeException(
        "Couldn't get the actual hour to determine water demand"
      ),
    ) * noPersonsInHoushold * waterDemandVolumePerPersonYear / 365
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

  /** Check if inner temperature is higher than preferred maximum temperature.
    * @param innerTemperature
    *   The inner temperature of the house.
    * @param boundaryTemperature
    *   The applied boundary temperature to check against.
    * @return
    *   True, if inner temperature is too high.
    */
  def isInnerTemperatureTooHigh(
      innerTemperature: Temperature,
      boundaryTemperature: Temperature = targetTemperature,
  ): Boolean =
    innerTemperature > (
      boundaryTemperature - temperatureTolerance
    )

  /** Check if inner temperature is lower than preferred minimum temperature.
    *
    * @param innerTemperature
    *   The inner temperature of the house.
    * @param boundaryTemperature
    *   The applied boundary temperature to check against.
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
  def newInnerTemperatureRecursive(
      thermalPower: Power,
      duration: Time,
      currentInnerTemperature: Temperature,
      ambientTemperature: Temperature,
  ): Temperature = {
    val (k1, k2) = getFactorsK1AndK2(thermalPower, ambientTemperature)
    // k1/k2 represents the temperature for limes t -> infinity
    val longTermTemperature = k1 / k2
    val exponent_k2 = -1 * k2 * duration.toSeconds

    val temperatureValue =
      (currentInnerTemperature.toKelvinScale - longTermTemperature) * Math.exp(
        exponent_k2
      ) + longTermTemperature

    Kelvin(temperatureValue)
  }

  /** Update the current state of the house.
    *
    * @param tick
    *   The tick that the houseState should be updated to.
    * @param lastThermalHouseState
    *   The applicable state of thermalHouse until this tick.
    * @param qDot
    *   The thermal feed in to the thermal house.
    * @return
    *   Updated state of the instance.
    */
  def determineState(
      tick: Long,
      lastThermalHouseState: ThermalHouseState,
      qDot: Power,
  ): ThermalHouseState = {
    val duration = Seconds(tick - lastThermalHouseState.tick)
    val updatedInnerTemperature = newInnerTemperatureRecursive(
      qDot,
      duration,
      lastThermalHouseState.innerTemperature,
      lastThermalHouseState.ambientTemperature,
    )

    lastThermalHouseState.copy(
      tick = tick,
      innerTemperature = updatedInnerTemperature,
    )
  }

  /** Determine the next threshold, that will be reached.
    *
    * @param thermalHouseState
    *   The applicable state of thermalHouse until this tick.
    * @param qDot
    *   The thermal feed in to the thermal house.
    * @param nextTryForPossibleThreshold
    *   Next or initial possible threshold value that should be find by
    *   recursive search.
    * @return
    *   The next threshold, that will be reached.
    */
  def determineNextThreshold(
      thermalHouseState: ThermalHouseState,
      qDot: Power,
      nextTryForPossibleThreshold: Long = 1,
  ): Option[ThermalThreshold] = {
    val limitTemperature = Kelvin(
      qDot.toWatts / ethLosses.toWattsPerKelvin
    ) + Kelvin(thermalHouseState.ambientTemperature.toKelvinScale)

    if (isInnerTemperatureTooLow(limitTemperature + temperatureTolerance))
      /* Losses and gain of house are not in balance, thus temperature will reach some limit sooner or later */
      /* House has more losses than gain */
      {
        nextActivation(
          thermalHouseState.tick,
          lowerBoundaryTemperature,
          thermalHouseState.innerTemperature,
          thermalHouseState.ambientTemperature,
          qDot,
        ).map(HouseTemperatureLowerBoundaryReached)
      } else if (
      isInnerTemperatureTooHigh(limitTemperature - temperatureTolerance)
    ) { /* House has more gain than losses */
      nextActivation(
        thermalHouseState.tick,
        targetTemperature,
        thermalHouseState.innerTemperature,
        thermalHouseState.ambientTemperature,
        qDot,
      ).map(HouseTargetTemperatureReached)
    } else {
      /* House is in perfect balance */
      None
    }
  }

  private def nextActivation(
      tick: Long,
      nextInnerTemperatureToReach: Temperature,
      currentInnerTemperature: Temperature,
      ambientTemperature: Temperature,
      qDot: Power,
  ): Option[Long] = {
    val (k1, k2) = getFactorsK1AndK2(qDot, ambientTemperature)
    // k1/k2 represents the temperature for limes t -> infinity
    val longTermTemperature = Kelvin(
      k1 / k2
    )

    val durationValue = Math.log(
      (nextInnerTemperatureToReach - longTermTemperature) / (currentInnerTemperature - longTermTemperature)
    ) / (k2 * -1)

    val duration = Math.floor(durationValue).toLong
    Some(tick + duration)
  }

  private def getFactorsK1AndK2(
      qDot: Power,
      ambientTemperature: Temperature,
  ): (Double, Double) = {
    val ethCapaValue = ethCapa.toWattSecondsPerKelvin
    val ethLossesValue = ethLosses.toWattsPerKelvin

    val k1 =
      qDot.toWatts / ethCapaValue + ethLossesValue * ambientTemperature.toKelvinScale / ethCapaValue // in K/Sec
    val k2 = ethLossesValue / ethCapaValue // in 1/Sec

    (k1, k2)
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

  /** State of a thermal house.
    *
    * @param tick
    *   Last tick of temperature change.
    * @param ambientTemperature
    *   The current ambient temperature.
    * @param innerTemperature
    *   Inner temperature of the house.
    */
  final case class ThermalHouseState(
      override val tick: Long,
      ambientTemperature: Temperature,
      innerTemperature: Temperature,
  ) extends ModelState

  def startingState(
      house: ThermalHouse,
      ambientTemperature: Temperature,
  ): ThermalHouseState =
    ThermalHouseState(
      0L,
      ambientTemperature,
      house.targetTemperature,
    )

  object ThermalHouseThreshold {
    final case class HouseTemperatureLowerBoundaryReached(
        override val tick: Long
    ) extends ThermalThreshold
    final case class HouseTargetTemperatureReached(
        override val tick: Long
    ) extends ThermalThreshold
  }
}
