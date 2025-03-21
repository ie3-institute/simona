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
  ThermalBusInput,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull,
}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.SquantsUtils.RichEnergy
import edu.ie3.util.scala.quantities.{
  KilowattHoursPerKelvinCubicMeters,
  SpecificHeatCapacity,
}
import squants.energy.Kilowatts
import squants.space.{CubicMeters, Volume}
import squants.thermal.Celsius
import squants.time.Seconds
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
  *   the element's human-readable id
  * @param operatorInput
  *   Operator input
  * @param operationTime
  *   Operation time
  * @param bus
  *   Thermal bus input
  * @param maxEnergyThreshold
  *   Maximum permissible energy stored in the storage
  * @param pThermalMax
  *   Thermal power, that can be charged / discharged
  */
final case class CylindricalThermalStorage(
    uuid: UUID,
    id: String,
    operatorInput: OperatorInput,
    operationTime: OperationTime,
    bus: ThermalBusInput,
    maxEnergyThreshold: Energy,
    pThermalMax: Power,
    storedEnergy: Energy,
) extends ThermalStorage(
      uuid,
      id,
      operatorInput,
      operationTime,
      bus,
      maxEnergyThreshold,
      pThermalMax,
    ) {

  /** Updates the given last state. Based on the then set thermal influx, the
    * current state is calculated. Positive values of influx are consider to
    * flow into the storage. Additionally, the tick, when the next threshold is
    * reached, is calculated as well.
    *
    * @param tick
    *   Tick, where this change happens
    * @param qDot
    *   Influx
    * @param lastHeatStorageState
    *   Last state of the heat storage.
    * @return
    *   The updated state as well as the tick, when a threshold is reached
    */
  override def updateState(
      tick: Long,
      qDot: Power,
      lastHeatStorageState: ThermalStorageState,
  ): (ThermalStorageState, Option[ThermalThreshold]) = {
    /* Determine new state based on time difference and given state */
    val energyBalance =
      lastHeatStorageState.qDot * Seconds(tick - lastHeatStorageState.tick)
    val newEnergy = lastHeatStorageState.storedEnergy + energyBalance
    val updatedEnergy =
      if (isFull(newEnergy))
        maxEnergyThreshold
      else if (isEmpty(newEnergy))
        zeroKWh
      else
        newEnergy

    /* Determine, when a threshold is reached */
    val nextThreshold =
      if (qDot > zeroKW) {
        val duration = (maxEnergyThreshold - updatedEnergy) / qDot
        val durationInTicks = Math.floor(duration.toSeconds).toLong
        if (durationInTicks <= 0L)
          None
        else
          Some(StorageFull(tick + durationInTicks))
      } else if (qDot < zeroKW) {
        val duration = updatedEnergy / qDot * (-1)
        val durationInTicks = Math.floor(duration.toSeconds).toLong
        if (durationInTicks <= 0L)
          None
        else
          Some(StorageEmpty(tick + durationInTicks))
      } else {
        return (ThermalStorageState(tick, updatedEnergy, qDot), None)
      }

    (ThermalStorageState(tick, updatedEnergy, qDot), nextThreshold)
  }

  override def startingState: ThermalStorageState = ThermalStorageState(
    -1L,
    zeroKWh,
    zeroKW,
  )
}

object CylindricalThermalStorage {

  /** Function to construct a new [[CylindricalThermalStorage]] based on a
    * provided [[CylindricalStorageInput]]
    *
    * @param input
    *   instance of [[CylindricalStorageInput]] this storage should be built
    *   from
    * @param initialStoredEnergy
    *   initial stored energy
    * @return
    *   a ready-to-use [[CylindricalThermalStorage]] with referenced electric
    *   parameters
    */

  def apply(
      input: CylindricalStorageInput,
      initialStoredEnergy: Energy = zeroKWh,
  ): CylindricalThermalStorage = {

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
        Celsius(input.getInletTemp.to(Units.CELSIUS).getValue.doubleValue()),
        Celsius(input.getReturnTemp.to(Units.CELSIUS).getValue.doubleValue()),
      )

    val pThermalMax = Kilowatts(
      input
        .getpThermalMax()
        .to(PowerSystemUnits.KILOWATT)
        .getValue
        .doubleValue()
    )

    new CylindricalThermalStorage(
      input.getUuid,
      input.getId,
      input.getOperator,
      input.getOperationTime,
      input.getThermalBus,
      maxEnergyThreshold,
      pThermalMax,
      initialStoredEnergy,
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
      returnTemp: Temperature,
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
      returnTemp: Temperature,
  ): Volume = {
    val energyDensity = c.calcEnergyDensity(returnTemp, inletTemp)

    energy.calcVolume(energyDensity)
  }
}
