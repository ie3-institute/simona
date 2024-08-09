package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{StorageEmpty, StorageFull}
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.SpecificHeatCapacity
import edu.ie3.util.scala.quantities.SquantsUtils.RichEnergy
import squants.space.Volume
import squants.time.Seconds
import squants.{Energy, Power, Temperature}

trait ThermalStorageCalculations {

  def isFull(energy: Energy): Boolean
  def isEmpty(energy: Energy): Boolean
  def maxEnergyThreshold: Energy
  def minEnergyThreshold: Energy
  def zeroMW: Power

  /** Updates the given last state. Based on the then set thermal influx, the
   * current state is calculated. Positive values of influx are considered to
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
  def updateState(
    tick: Long,
    qDot: Power,
    lastState: ThermalStorageState
  ): (ThermalStorageState, Option[ThermalThreshold]) = {
    /* Determine new state based on time difference and given state */
    val energyBalance = lastState.qDot * Seconds(tick - lastState.tick)
    val newEnergy = lastState.storedEnergy + energyBalance
    val updatedEnergy =
      if (isFull(newEnergy)) maxEnergyThreshold
      else if (isEmpty(newEnergy)) minEnergyThreshold
      else newEnergy

    /* Determine, when a threshold is reached */
    val nextThreshold: Option[ThermalThreshold] =
      if (qDot > zeroMW) {
        val duration = (maxEnergyThreshold - updatedEnergy) / qDot
        val durationInTicks = Math.round(duration.toSeconds)
        if (durationInTicks <= 0L) None
        else Some(ThermalStorageThreshold.StorageFull(tick + durationInTicks))
      } else if (qDot < zeroMW) {
        val duration = (updatedEnergy - minEnergyThreshold) / qDot * (-1)
        val durationInTicks = Math.round(duration.toSeconds)
        if (durationInTicks <= 0L) None
        else Some(ThermalStorageThreshold.StorageEmpty(tick + durationInTicks))
      } else {
        None
      }

    (ThermalStorageState(tick, updatedEnergy, qDot), nextThreshold)
  }


  def startingState: ThermalStorageState = ThermalStorageState(
    -1L,
    minEnergyThreshold,
    zeroMW
  )

  @deprecated("Use thermal storage state instead")
  def usableThermalEnergy(state: ThermalStorageState): Energy =
    state.storedEnergy - minEnergyThreshold

  @deprecated("Use thermal storage state instead")
  def tryToStoreAndReturnRemainder(
    addedEnergy: Energy,
    state: ThermalStorageState
  ): (ThermalStorageState, Option[Energy]) = {
    if (addedEnergy > zeroKWH) {
      val newStoredEnergy = state.storedEnergy + addedEnergy
      if (newStoredEnergy > maxEnergyThreshold) {
        val surplus = newStoredEnergy - maxEnergyThreshold
        val updatedState = state.copy(storedEnergy = maxEnergyThreshold)
        (updatedState, Some(surplus))
      } else {
        val updatedState = state.copy(storedEnergy = newStoredEnergy)
        (updatedState, None)
      }
    } else {
      (state, None)
    }
  }

  @deprecated("Use thermal storage state instead")
  def tryToTakeAndReturnLack(
    takenEnergy: Energy,
    state: ThermalStorageState
  ): (ThermalStorageState, Option[Energy]) = {
    if (takenEnergy > zeroKWH) {
      val newStoredEnergy = state.storedEnergy - takenEnergy
      if (newStoredEnergy < minEnergyThreshold) {
        val lack = minEnergyThreshold - newStoredEnergy
        val updatedState = state.copy(storedEnergy = minEnergyThreshold)
        (updatedState, Some(lack))
      } else {
        val updatedState = state.copy(storedEnergy = newStoredEnergy)
        (updatedState, None)
      }
    } else {
      (state, None)
    }
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
