/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  ThermalBusInput
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.SquantsUtils.RichEnergy
import edu.ie3.util.scala.quantities.{
  DefaultQuantities,
  KilowattHoursPerKelvinCubicMeters,
  SpecificHeatCapacity
}
import squants.energy.{Kilowatts, Megawatts}
import squants.space.{CubicMeters, Volume}
import squants.thermal.Celsius
import squants.time.{Hours, Seconds}
import squants.{Energy, Power, Temperature}
import tech.units.indriya.unit.Units

import java.util.UUID

/** A cylindrical thermal storage used for implementations, which require a
  * mutable storage. <p> <strong>Important:</strong> The field storageLvl is a
  * variable.
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
  * @param minEnergyThreshold
  *   Minimum permissible energy stored in the storage
  * @param maxEnergyThreshold
  *   Maximum permissible energy stored in the storage
  * @param chargingPower
  *   Thermal power, that can be charged / discharged
  */
final case class CylindricalThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    minEnergyThreshold: Energy,
    maxEnergyThreshold: Energy,
    chargingPower: Power,
    override protected var _storedEnergy: Energy
) extends ThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
      minEnergyThreshold,
      maxEnergyThreshold,
      chargingPower
    )
    with MutableStorage {

  /** Updates the given last state. Based on the then set thermal influx, the
    * current state is calculated. Positive values of influx are consider to
    * flow into the storage. Additionally, the tick, when the next threshold is
    * reached, is calculated as well.
    *
    * @param tick
    *   Tick, where this change happens
    * @param qDot
    *   Influx
    * @param lastState
    *   Last known state
    * @return
    *   The updated state as well as the tick, when a threshold is reached
    */
  override def updateState(
      tick: Long,
      qDot: Power,
      lastState: ThermalStorageState
  ): (ThermalStorageState, Option[ThermalThreshold]) = {
    /* Determine new state based on time difference and given state */
    val energyBalance = lastState.qDot * Seconds(tick - lastState.tick)

    val newEnergy = lastState.storedEnergy + energyBalance
    val updatedEnergy = newEnergy match {
      case energy if energy > maxEnergyThreshold => maxEnergyThreshold
      case energy if energy < minEnergyThreshold => minEnergyThreshold
      case energy                                => energy
    }

    /* Determine, when a threshold is reached */
    val nextThreshold = {
      qDot match {
        case positive if qDot > Megawatts(0d) => {
          val duration = (maxEnergyThreshold - updatedEnergy) / qDot
          Some(StorageFull(tick + Math.max(duration.toSeconds.toLong, 0L)))
        }
        case negative if qDot < Megawatts(0d) => {
          val duration = ((updatedEnergy - minEnergyThreshold) / qDot * (-1))
          Some(StorageEmpty(tick + Math.max(duration.toSeconds.toLong, 0L)))
        }

        case equal if qDot == Megawatts(0d) => {

          ThermalStorageState(tick, updatedEnergy, qDot)
          None
        }

      }

    }
    (ThermalStorageState(tick, updatedEnergy, qDot), nextThreshold)
  }

  override def usableThermalEnergy: Energy =
    _storedEnergy - minEnergyThreshold

  override def tryToStoreAndReturnRemainder(
      addedEnergy: Energy
  ): Option[Energy] = {
    if (addedEnergy > zeroEnergy) {
      _storedEnergy = _storedEnergy + addedEnergy
      if (_storedEnergy > maxEnergyThreshold) {
        val surplus = _storedEnergy - maxEnergyThreshold
        _storedEnergy = maxEnergyThreshold
        return Option(surplus)
      }
    }
    Option.empty
  }

  override def tryToTakeAndReturnLack(
      takenEnergy: Energy
  ): Option[Energy] = {
    if (takenEnergy > zeroEnergy) {
      _storedEnergy = _storedEnergy - takenEnergy
      if (_storedEnergy < minEnergyThreshold) {
        val lack = minEnergyThreshold - _storedEnergy
        _storedEnergy = minEnergyThreshold
        return Option(lack)
      }
    }
    Option.empty
  }

  override def startingState: ThermalStorageState = ThermalStorageState(
    -1L,
    getMinEnergyThreshold,
    Kilowatts(0d)
  )
}

case object CylindricalThermalStorage {

  /** Function to construct a new [[CylindricalThermalStorage]] based on a
    * provided [[CylindricalStorageInput]]
    *
    * @param input
    *   instance of [[CylindricalStorageInput]] this storage should be built
    *   from
    * @return
    *   a ready-to-use [[CylindricalThermalStorage]] with referenced electric
    *   parameters
    */
  def apply(
      input: CylindricalStorageInput,
      initialStoredEnergy: Energy = DefaultQuantities.zeroKWH
  ): CylindricalThermalStorage = {
    val minEnergyThreshold: Energy =
      CylindricalThermalStorage.volumeToEnergy(
        CubicMeters(
          input.getStorageVolumeLvlMin
            .to(Units.CUBIC_METRE)
            .getValue
            .doubleValue
        ),
        KilowattHoursPerKelvinCubicMeters(
          input.getC
            .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
            .getValue
            .doubleValue
        ),
        Celsius(input.getInletTemp.to(Units.CELSIUS).getValue.doubleValue),
        Celsius(input.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue)
      )

    val maxEnergyThreshold: Energy =
      CylindricalThermalStorage.volumeToEnergy(
        CubicMeters(
          input.getStorageVolumeLvl.to(Units.CUBIC_METRE).getValue.doubleValue
        ),
        KilowattHoursPerKelvinCubicMeters(
          input.getC
            .to(PowerSystemUnits.KILOWATTHOUR_PER_KELVIN_TIMES_CUBICMETRE)
            .getValue
            .doubleValue
        ),
        Celsius(input.getInletTemp.to(Units.CELSIUS).getValue.doubleValue),
        Celsius(input.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue)
      )

    /* TODO: Currently, the input model does not define any maximum charge power. Assume, that the usable energy can
     *   be charged / discharged within the interval of an hour */
    val chargingPower = (maxEnergyThreshold - minEnergyThreshold) / Hours(1d)
    new CylindricalThermalStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      minEnergyThreshold,
      maxEnergyThreshold,
      chargingPower,
      initialStoredEnergy
    )
  }

  /** Equation from docs for the relation between needed volume and energy.
    *
    * @param volume
    *   needed/available volume
    * @param c
    *   Specific heat capacity
    * @param inletTemp
    *   Inlet temperature
    * @param returnTemp
    *   Return temperature
    * @return
    *   energy
    */
  def volumeToEnergy(
      volume: Volume,
      c: SpecificHeatCapacity,
      inletTemp: Temperature,
      returnTemp: Temperature
  ): Energy = {
    c.calcEnergy(returnTemp, inletTemp, volume)
  }

  /** Equation from docs for the relation between stored heat and volume change.
    *
    * @param energy
    *   available energy
    * @param c
    *   Specific heat capacity
    * @param inletTemp
    *   Inlet temperature
    * @param returnTemp
    *   Return temperature
    * @return
    *   volume
    */
  def energyToVolume(
      energy: Energy,
      c: SpecificHeatCapacity,
      inletTemp: Temperature,
      returnTemp: Temperature
  ): Volume = {
    val energyDensity = c.calcEnergyDensity(returnTemp, inletTemp)

    energy.calcVolume(energyDensity)
  }
}
