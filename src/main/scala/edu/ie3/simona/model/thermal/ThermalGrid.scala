/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.thermal.{
  CylindricalStorageInput,
  DomesticHotWaterStorageInput,
}
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  DomesticHotWaterStorageResult,
  ThermalHouseResult,
}
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.Kilowatts
import squants.{Energy, Power, Seconds, Temperature}

import java.time.ZonedDateTime
import scala.jdk.CollectionConverters.SetHasAsScala

/** Calculation model for a thermal grid. It is assumed, that all elements are
  * connected directly with exactly one thermal bus
  *
  * @param house
  *   Thermal houses connected to the bus
  * @param heatStorage
  *   Thermal storages
  * @param domesticHotWaterStorage
  *   Storages for domestic hot water / tap water
  */
final case class ThermalGrid(
    house: Option[ThermalHouse],
    heatStorage: Option[ThermalStorage],
    domesticHotWaterStorage: Option[ThermalStorage],
) extends LazyLogging {

  /** Determine the energy demand of the total grid at the given instance in
    * time and returns it including the updatedState
    *
    * @param tick
    *   Questioned instance in time
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Currently applicable state of the thermal grid
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   The total energy demand of the house and the storage and an updated
    *   [[ThermalGridState]]
    */
  def energyDemandAndUpdatedState(
      tick: Long,
      // FIXME this is also in state
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ): (
      ThermalEnergyDemand,
      ThermalEnergyDemand,
      ThermalEnergyDemand,
      ThermalGridState,
  ) = {
    /* First get the energy demand of the houses but only if inner temperature is below target temperature */

    val (houseDemand, updatedHouseState, demandHotDomesticWater) =
      house.zip(state.houseState) match {
        case Some((thermalHouse, lastHouseState)) => {
          val (updatedHouseState, updatedStorageState) =
            thermalHouse.determineState(
              tick,
              lastHouseState,
              lastAmbientTemperature,
              ambientTemperature,
              lastHouseState.qDot,
            )
          val (heatDemand, newHouseState) = if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature | (lastHouseState.qDot > zeroKW && updatedHouseState.innerTemperature < thermalHouse.upperBoundaryTemperature)
          ) {
            (
              thermalHouse.energyDemandHeating(
                tick,
                ambientTemperature,
                updatedHouseState,
              ),
              Some(updatedHouseState),
            )

          } else {
            (ThermalEnergyDemand.noDemand, Some(updatedHouseState))
          }

          val energyDemandDomesticHotWater =
            thermalHouse.energyDemandDomesticHotWater(
              tick,
              Some(lastHouseState),
              simulationStartTime,
              houseInhabitants,
            )
          (heatDemand, newHouseState, energyDemandDomesticHotWater)
        }
        case None =>
          (ThermalEnergyDemand.noDemand, None, ThermalEnergyDemand.noDemand)
      }

    /* Then go over the storages, see what they can provide and what they might be able to charge */
    // Heat storages first
    val (storageDemand, updatedStorageState) = {

      heatStorage
        .zip(state.storageState)
        .map { case (storage, state) =>
          val updatedStorageState =
            storage.updateState(tick, state.qDot, state)._1
          val storedEnergy = updatedStorageState.storedEnergy
          val soc = storedEnergy / storage.getMaxEnergyThreshold
          val storageRequired = {
            if (soc == 0d) {
              storage.getMaxEnergyThreshold - storedEnergy

            } else {
              zeroMWH
            }
          }

          val storagePossible = storage.getMaxEnergyThreshold - storedEnergy
          (
            ThermalEnergyDemand(
              storageRequired,
              storagePossible,
            ),
            Some(updatedStorageState),
          )

        }
        .getOrElse(
          ThermalEnergyDemand(zeroMWH, zeroMWH),
          None,
        )
    }

    // Domestic hot water storages
    val (domesticHotWaterStorageDemand, updatedDomesticHotWaterStorageState) = {
      val domesticHotWaterDemand: ThermalEnergyDemand = house
        .map(
          _.energyDemandDomesticHotWater(
            tick,
            state.houseState,
            simulationStartTime,
            houseInhabitants,
          )
        )
        .getOrElse(ThermalEnergyDemand(zeroKWH, zeroKWH))
      val applicableqDotDomesticStorage =
        identifyApplicableQDot(tick, domesticHotWaterDemand)._1

      domesticHotWaterStorage
        .zip(state.domesticHotWaterStorageState)
        .map { case (storage, state) =>
          val updatedStorageState =
            storage
              .updateState(
                tick,
                state.qDot.plus(applicableqDotDomesticStorage),
                state,
              )
              ._1
          val storedEnergy = updatedStorageState.storedEnergy

          val demandOfStorage =
            if (storedEnergy < demandHotDomesticWater.required)
              demandHotDomesticWater.required.minus(storedEnergy)
            else zeroMWH
          (
            ThermalEnergyDemand(demandOfStorage, demandOfStorage),
            Some(updatedStorageState),
          )
        }
        .getOrElse(
          ThermalEnergyDemand(zeroMWH, zeroMWH),
          None,
        )
    }

    (
      ThermalEnergyDemand(
        houseDemand.required,
        houseDemand.possible,
      ),
      ThermalEnergyDemand(
        storageDemand.required,
        storageDemand.possible,
      ),
      ThermalEnergyDemand(
        domesticHotWaterStorageDemand.required,
        domesticHotWaterStorageDemand.possible,
      ),
      ThermalGridState(
        updatedHouseState,
        updatedStorageState,
        updatedDomesticHotWaterStorageState,
      ),
    )
  }

  /** Update the current state of the grid
    *
    * @param tick
    *   Instance in time
    * @param state
    *   Currently applicable state
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param isRunning
    *   determines whether the heat pump is running or not
    * @param qDot
    *   Thermal energy balance
    * @param houseDemand
    *   determines if the thermal house has heat demand
    * @param storageDemand
    *   determines if the thermal storage has heat demand
    * @param domesticHotWaterStorageDemand
    *   determines if the domestic hot water storage has heat demand
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   The updated state of the grid
    */
  def updateState(
      tick: Long,
      state: ThermalGridState,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      isRunning: Boolean,
      qDot: Power,
      houseDemand: ThermalEnergyDemand,
      storageDemand: ThermalEnergyDemand,
      domesticHotWaterStorageDemand: ThermalEnergyDemand,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ): (ThermalGridState, Option[ThermalThreshold]) = if (qDot > zeroKW)
    handleInfeed(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      isRunning,
      qDot,
      houseDemand,
      storageDemand,
      domesticHotWaterStorageDemand,
      simulationStartTime,
      houseInhabitants,
    )
  else
    handleConsumption(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      qDot,
      simulationStartTime,
      houseInhabitants,
    )

  /** Handles the case, when a grid has infeed. Depending which entity has some
    * heat demand the house or the storage will be heated up / filled up.
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the houses
    * @param isRunning
    *   determines whether the heat pump is running or not
    * @param qDot
    *   Infeed to the grid
    * @param houseDemand
    *   determines if the thermal house has heat demand
    * @param heatStorageDemand
    *   determines if the thermal storage has heat demand
    * @param domesticHotWaterStorageDemand
    *   determines if the domestic hot water storage has heat demand
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   Updated thermal grid state
    */
  private def handleInfeed(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      isRunning: Boolean,
      qDot: Power,
      houseDemand: ThermalEnergyDemand,
      heatStorageDemand: ThermalEnergyDemand,
      domesticHotWaterStorageDemand: ThermalEnergyDemand,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // TODO: We would need to issue a storage result model here...

    /* Consider the action in the last state and if it's possible to continue*/
    val (_, qDotHouseLastState, houseReachedBoundary, houseLeftBoundary) =
      updateStateGetLastThermalActionAndCheckIfCanContinueThermalHouse(
        tick,
        state,
        lastAmbientTemperature,
        ambientTemperature,
      )
    val (
      updatedHeatStorageState,
      qDotStorageLastState,
      thermalStorageReachedBoundary,
      thermalStorageLeftBoundary,
    ) =
      updateStateGetLastThermalActionAndCheckIfCanContinueThermalStorage(
        tick,
        state,
      )
    val (
      _,
      qDotDomesticWaterStorageLastState,
      domesticHotWaterStorageReachedBoundary,
      domesticHotWaterStorageLeftBoundary,
    ) =
      updateStateGetLastThermalActionAndCheckIfCanContinueDomesticHotWaterStorage(
        tick,
        state,
      )

    if (
      !houseReachedBoundary && !houseLeftBoundary &&
      !thermalStorageReachedBoundary && !thermalStorageLeftBoundary &&
      !domesticHotWaterStorageReachedBoundary && !domesticHotWaterStorageLeftBoundary
    ) {
      handleCases(
        tick,
        lastAmbientTemperature,
        ambientTemperature,
        state,
        qDotHouseLastState.getOrElse(zeroKW),
        qDotStorageLastState.getOrElse(zeroKW),
        qDotDomesticWaterStorageLastState.getOrElse(zeroKW),
        simulationStartTime,
        houseInhabitants,
      )
    } else {

      (
        houseDemand.hasRequiredDemand,
        houseDemand.hasAdditionalDemand,
        heatStorageDemand.hasRequiredDemand,
        heatStorageDemand.hasAdditionalDemand,
        domesticHotWaterStorageDemand.hasRequiredDemand,
        domesticHotWaterStorageDemand.hasAdditionalDemand,
      ) match {
        case (true, _, _, _, true, _) =>
          updatedHeatStorageState match {
            // if heatStorage is not empty, house and hot water storage can handleDemand
            // take qDot to recharge domesticHotWaterStorage and
            // cover thermal demand of house by heatStorage
            case Some(storageState) if storageState.storedEnergy > zeroKWH =>
              handleCases(
                tick,
                lastAmbientTemperature,
                ambientTemperature,
                state,
                heatStorage.map(_.getChargingPower).getOrElse(zeroKW),
                heatStorage.map(_.getChargingPower).getOrElse(zeroKW) * (-1),
                qDot,
                simulationStartTime,
                houseInhabitants,
              )
            case _ =>
              splitThermalHeatAndPushIntoHouseAndDomesticStorage(
                tick,
                lastAmbientTemperature,
                ambientTemperature,
                state,
                qDotHouseLastState,
                qDotDomesticWaterStorageLastState,
                qDot,
                simulationStartTime,
                houseInhabitants,
              )
          }

        // Same cases if there is Some(heatStorageDemand) or not
        case (true, _, _, _, false, _) =>
          pushThermalHeatIntoHouseOnly(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            qDot,
            simulationStartTime,
            houseInhabitants,
          )

        // Prioritize domestic hot water storage
        // Same case if there is Some(heatStorageDemand) or not
        case (false, _, _, _, true, _) =>
          pushThermalHeatIntoDomesticHotWaterStorageOnly(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            qDot,
            simulationStartTime,
            houseInhabitants,
          )

        case (false, _, true, _, false, _) =>
          pushThermalHeatIntoThermalStorageOnly(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            qDot,
            simulationStartTime,
            houseInhabitants,
          )
        //  all cases for required demands are now handled
        // now take last action into account
        case _ =>
          // House and domestic hot water storage can only have additional demand now
          domesticHotWaterStorageLeftBoundary match {
            case true =>
              handleCases(
                tick,
                lastAmbientTemperature,
                ambientTemperature,
                state,
                qDotHouseLastState.getOrElse(zeroKW),
                qDotStorageLastState.getOrElse(zeroKW),
                zeroKW,
                simulationStartTime,
                houseInhabitants,
              )
            // if storage has additional demand charge it before heating the house
            case _ =>
              if (heatStorageDemand.hasAdditionalDemand)
                handleCases(
                  tick,
                  lastAmbientTemperature,
                  ambientTemperature,
                  state,
                  zeroKW,
                  qDot,
                  zeroKW,
                  simulationStartTime,
                  houseInhabitants,
                )
              else {
                handleCases(
                  tick,
                  lastAmbientTemperature,
                  ambientTemperature,
                  state,
                  qDot,
                  zeroKW,
                  zeroKW,
                  simulationStartTime,
                  houseInhabitants,
                )
              }
          }

      }
    }
  }

  /** Method that updates the state of thermal house, get its last thermal
    * action (qDot) and if it reached or left any boundaries this tick.
    *
    * @param tick
    *   Current tick
    * @param state
    *   Current state of the houses
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @return
    *   Option of the updated house state, option of the last qDot and Booleans
    *   if some boundaries are reached or left.
    */

  private def updateStateGetLastThermalActionAndCheckIfCanContinueThermalHouse(
      tick: Long,
      state: ThermalGridState,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
  ): (Option[ThermalHouseState], Option[Power], Boolean, Boolean) = {
    state match {
      case ThermalGridState(
            Some(lastHouseState),
            _,
            _,
          ) =>
        (house, state.houseState) match {
          case (Some(thermalHouse), Some(lastHouseState)) =>
            val (newState, _) = thermalHouse.determineState(
              tick,
              lastHouseState,
              lastAmbientTemperature,
              ambientTemperature,
              lastHouseState.qDot,
            )
            val houseReachedBoundary = thermalHouse.isInnerTemperatureTooHigh(
              newState.innerTemperature
            ) || thermalHouse.isInnerTemperatureTooLow(
              newState.innerTemperature
            )
            val houseLeftBoundary = (thermalHouse.isInnerTemperatureTooHigh(
              lastHouseState.innerTemperature
            ) && !thermalHouse.isInnerTemperatureTooHigh(
              newState.innerTemperature
            )) || (thermalHouse.isInnerTemperatureTooLow(
              newState.innerTemperature
            ) && !thermalHouse.isInnerTemperatureTooLow(
              newState.innerTemperature
            ))
            (
              Some(newState),
              Some(lastHouseState.qDot),
              houseReachedBoundary,
              houseLeftBoundary,
            )
        }
      case ThermalGridState(
            None,
            _,
            _,
          ) =>
        (None, None, false, false)
    }
  }

  /** Method that updates the state of thermal storage, get its last thermal
    * action (qDot) and if it reached or left any boundaries this tick.
    *
    * @param tick
    *   Current tick
    * @param state
    *   Current state of the houses
    * @return
    *   Option of the updated storage state, option of the last qDot and
    *   Booleans if some boundaries are reached or left.
    */

  private def updateStateGetLastThermalActionAndCheckIfCanContinueThermalStorage(
      tick: Long,
      state: ThermalGridState,
  ): (Option[ThermalStorageState], Option[Power], Boolean, Boolean) = {
    getLastThermalActionAndCheckIfCanContinueStorage(
      tick,
      state,
      heatStorage,
      state.storageState,
    )
  }

  /** Method that updates the state of domestic hot water storage, get its last
    * thermal action (qDot) and if it reached or left any boundaries this tick.
    *
    * @param tick
    *   Current tick
    * @param state
    *   Current state of the houses
    * @return
    *   Option of the updated storage state, option of the last qDot and
    *   Booleans if some boundaries are reached or left.
    */

  private def updateStateGetLastThermalActionAndCheckIfCanContinueDomesticHotWaterStorage(
      tick: Long,
      state: ThermalGridState,
  ): (Option[ThermalStorageState], Option[Power], Boolean, Boolean) = {
    getLastThermalActionAndCheckIfCanContinueStorage(
      tick,
      state,
      domesticHotWaterStorage,
      state.domesticHotWaterStorageState,
    )
  }

  /** Abstract method that updates the state of thermal storage, get its last
    * thermal action (qDot) and if it reached or left any boundaries this tick.
    * @param tick
    *   Current tick
    * @param state
    *   Current state of the houses
    * @param storage
    *   The storage handled here
    * @param lastStorageState
    *   Last state of the storage
    * @return
    *   Option of the updated storage state, option of the last qDot and
    *   Booleans if some boundaries are reached or left.
    */

  private def getLastThermalActionAndCheckIfCanContinueStorage(
      tick: Long,
      state: ThermalGridState,
      storage: Option[ThermalStorage],
      lastStorageState: Option[ThermalStorageState],
  ): (Option[ThermalStorageState], Option[Power], Boolean, Boolean) = {
    (storage, lastStorageState) match {
      case (Some(storage), Some(lastStorageState)) =>
        val (newState, _) = storage.updateState(
          tick,
          lastStorageState.qDot,
          lastStorageState,
        )

        val storageReachedBorder =
          (newState.storedEnergy == storage.getMaxEnergyThreshold) ||
            (newState.storedEnergy == zeroKWH)

        val storageLeftBorder =
          (lastStorageState.storedEnergy == storage.getMaxEnergyThreshold && newState.storedEnergy != zeroKWH) ||
            (lastStorageState.storedEnergy == zeroKWH && newState.storedEnergy != zeroKWH)
        (
          Some(newState),
          Some(lastStorageState.qDot),
          storageReachedBorder,
          storageLeftBorder,
        )
      case _ =>
        (None, None, false, false)
    }
  }

  /** Helper method to push energy directly into the thermal house
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    */

  private def pushThermalHeatIntoHouseOnly(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ) = {
    handleCases(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      qDot,
      zeroKW,
      zeroKW,
      simulationStartTime,
      houseInhabitants,
    )
  }

  /** Helper method to push energy directly into the thermal heat storage
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    */

  private def pushThermalHeatIntoThermalStorageOnly(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ) = {
    handleCases(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      zeroKW,
      qDot,
      zeroKW,
      simulationStartTime,
      houseInhabitants,
    )
  }

  /** Helper method to push energy directly into the domestic hot water storage
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    */

  private def pushThermalHeatIntoDomesticHotWaterStorageOnly(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ) = {
    handleCases(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      zeroKW,
      zeroKW,
      qDot,
      simulationStartTime,
      houseInhabitants,
    )
  }

  /** Helper method to split qDot of the heat pump and push energy directly into
    * house and the domestic hot water storage
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDotHouseLastState
    *   The infeed into the thermal house in the last state
    * @param qDotDomesticHotWaterStorageLastState
    *   The infeed into the domestic hot water storage in the last state
    * @param qDot
    *   Infeed to the grid
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    */

  private def splitThermalHeatAndPushIntoHouseAndDomesticStorage(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouseLastState: Option[Power],
      qDotDomesticHotWaterStorageLastState: Option[Power],
      qDot: Power,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ) = {
    val qDotDomesticHotWaterStorage: Power =
      if (
        (qDot / 2) > domesticHotWaterStorage
          .map(_.getChargingPower)
          .getOrElse(Kilowatts(0d))
      ) {
        domesticHotWaterStorage
          .map(_.getChargingPower)
          .getOrElse(Kilowatts(0d))
      } else {
        qDot / 2
      }

    handleCases(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      qDot - qDotDomesticHotWaterStorage,
      zeroKW,
      qDotDomesticHotWaterStorage,
      simulationStartTime,
      houseInhabitants,
    )
  }

  /** Handles the different cases, of thermal flows from and into the thermal
    * grid.
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the thermal grid
    * @param qDotHouse
    *   Infeed to the house
    * @param qDotHeatStorage
    *   Infeed to the heat storage
    * @param qDotDomesticHotWaterStorage
    *   Infeed to the domestic hot water storage
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   Updated thermal grid state and the next threshold if there is one
    */
  private def handleCases(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
      qDotDomesticHotWaterStorage: Power,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // FIXME: Is there any case where we get back some remainingQDotHouse?
    val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
      handleInfeedHouse(
        tick,
        lastAmbientTemperature,
        ambientTemperature,
        state,
        qDotHouse,
      )

    val (updatedStorageState, thermalStorageThreshold) =
      handleInfeedStorage(
        tick,
        state,
        qDotHeatStorage,
        heatStorage,
        simulationStartTime,
        houseInhabitants,
      )

    val (
      updatedDomesticHotWaterStorageState,
      domesticHotWaterStorageThreshold,
    ) = handleInfeedStorage(
      tick,
      state,
      qDotDomesticHotWaterStorage,
      domesticHotWaterStorage,
      simulationStartTime,
      houseInhabitants,
    )

    val nextThreshold = determineMostRecentThreshold(
      Seq(
        thermalHouseThreshold,
        thermalStorageThreshold,
        domesticHotWaterStorageThreshold,
      )
    )

    (
      state.copy(
        houseState = updatedHouseState,
        storageState = updatedStorageState,
        domesticHotWaterStorageState = updatedDomesticHotWaterStorageState,
      ),
      nextThreshold,
    )
  }

  /** Handles the case, when the house has heat demand and will be heated up
    * here.
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    *   Updated thermal house state, a ThermalThreshold and the remaining qDot
    */
  private def handleInfeedHouse(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ): (Option[ThermalHouseState], Option[ThermalThreshold], Power) = {
    (house, state.houseState) match {
      case (Some(thermalHouse), Some(lastHouseState)) =>
        val (newState, threshold) = thermalHouse.determineState(
          tick,
          lastHouseState,
          lastAmbientTemperature,
          ambientTemperature,
          qDot,
        )
        /* Check if house can handle the thermal feed in */
        if (
          thermalHouse.isInnerTemperatureTooHigh(
            newState.innerTemperature
          )
        ) {
          val (fullHouseState, maybeFullHouseThreshold) =
            thermalHouse.determineState(
              tick,
              lastHouseState,
              lastAmbientTemperature,
              ambientTemperature,
              zeroKW,
            )
          (Some(fullHouseState), maybeFullHouseThreshold, qDot)
        } else {
          (Some(newState), threshold, zeroKW)
        }
      case _ => (None, None, zeroKW)
    }
  }

  /** Handles the cases, when the storage has heat demand and will be filled up
    * here (positive qDot) or will be return its stored energy into the thermal
    * grid (negative qDot). Same if the storage is used for domestic hot water.
    * Positive qDot will fill the storage, negative will cover the demand.
    *
    * @param tick
    *   Current tick
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @param storage
    *   the storage that should be handled
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   Updated thermal grid state
    */
  private def handleInfeedStorage(
      tick: Long,
      state: ThermalGridState,
      qDot: Power,
      storage: Option[ThermalStorage],
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ): (Option[ThermalStorageState], Option[ThermalThreshold]) = {
    // FIXME: We should somewhere check that pThermalMax of Storage is always capable for qDot pThermalMax >= pThermal of Hp
    val selectedState = storage match {
      case Some(_: CylindricalThermalStorage) => state.storageState
      case Some(_: DomesticHotWaterStorage) =>
        state.domesticHotWaterStorageState
      case _ => None
    }

    (storage, selectedState) match {
      case (
            Some(thermalStorage: CylindricalThermalStorage),
            Some(lastStorageState),
          ) =>
        val (newState, threshold) = thermalStorage.updateState(
          tick,
          qDot,
          lastStorageState,
        )
        (Some(newState), threshold)

      case (
            Some(domesticHotWaterStorage: DomesticHotWaterStorage),
            Some(lastDomesticHotWaterStorageState),
          ) =>
        val storedEnergy = domesticHotWaterStorage
          .updateState(
            tick,
            lastDomesticHotWaterStorageState.qDot,
            lastDomesticHotWaterStorageState,
          )
          ._1
          .storedEnergy

        val domesticHotWaterDemand = house
          .map(
            _.energyDemandDomesticHotWater(
              tick,
              state.houseState,
              simulationStartTime,
              houseInhabitants,
            )
          )
          .getOrElse(ThermalEnergyDemand(zeroKWH, zeroKWH))

        val (applicableQDotDomesticStorage, thresholdToCoverDemand) =
          domesticHotWaterDemand match {
            case demand
                if demand.required > zeroKWH && storedEnergy == zeroKWH =>
              // Use qDot from Hp directly to cover hot water demand
              val threshold = Some(
                SimpleThermalThreshold(
                  (domesticHotWaterDemand.required / qDot).toSeconds.toLong
                )
              )
              (zeroKW, threshold)
            case demand
                if demand.required > zeroKWH && storedEnergy > zeroKWH =>
              // Use storage to cover hot water demand
              identifyApplicableQDot(tick, demand)

            case demand
                if demand.required == zeroKWH && lastDomesticHotWaterStorageState.qDot > zeroKW && storedEnergy != domesticHotWaterStorage.maxEnergyThreshold =>
              // Storage got recharged in the last state
              // Threshold will be calculated later
              (lastDomesticHotWaterStorageState.qDot, None)
            case demand if demand.required == zeroKWH & qDot == zeroKW =>
              // Don't do anything with domestic hot water storage
              (zeroKW, None)
            case demand if demand.required == zeroKWH & qDot > zeroKW =>
              // Use qDot from Hp to recharge domestic hot water storage
              // Threshold will be calculated later
              (qDot, None)

            case _ =>
              throw new RuntimeException(
                s"Unexpected case occur when try to handle infeed into domestic hot water storage ${domesticHotWaterStorage.uuid}."
              )
          }

        val (updatedStorageState, updatedThreshold) =
          domesticHotWaterStorage
            .updateState(
              tick,
              applicableQDotDomesticStorage,
              lastDomesticHotWaterStorageState,
            )

        val nextThreshold = determineMostRecentThreshold(
          Seq(updatedThreshold, thresholdToCoverDemand)
        )
        (
          Some(updatedStorageState),
          nextThreshold,
        )
      case _ => (None, None)
    }
  }

  /** Returns the very next threshold or None if there isn't any.
    *
    * @param thresholds
    *   A sequence of thresholds
    * @return
    *   The next [[ThermalThreshold]] or [[None]].
    */

  private def determineMostRecentThreshold(
      thresholds: Seq[Option[ThermalThreshold]]
  ): Option[ThermalThreshold] = {

    @annotation.tailrec
    def findMostRecent(
        remaining: Seq[ThermalThreshold],
        currentMin: Option[ThermalThreshold],
    ): Option[ThermalThreshold] = {
      remaining match {
        case Nil => currentMin
        case head :: tail =>
          val newMin = currentMin match {
            case None => Some(head)
            case Some(minThreshold) =>
              if (head.tick < minThreshold.tick) Some(head) else currentMin
          }
          findMostRecent(tail, newMin)
      }
    }

    findMostRecent(thresholds.flatten, None)
  }

  /** Handle consumption (or no infeed) from thermal grid
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   Updated thermal grid state
    */
  private def handleConsumption(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    /* House will be left with no influx in all cases. Determine if and when a threshold is reached */
    val maybeUpdatedHouseState =
      house.zip(state.houseState).map { case (house, houseState) =>
        house.determineState(
          tick,
          houseState,
          lastAmbientTemperature,
          ambientTemperature,
          zeroMW,
        )
      }

    /* Update the state of the storage */
    val maybeUpdatedStorageState =
      heatStorage.zip(state.storageState).map { case (storage, storageState) =>
        storage.updateState(tick, qDot, storageState)
      }

    val (revisedHouseState, revisedStorageState) =
      reviseInfeedFromStorage(
        tick,
        maybeUpdatedHouseState,
        maybeUpdatedStorageState,
        state.houseState,
        state.storageState,
        lastAmbientTemperature,
        ambientTemperature,
        qDot,
      )

    heatStorage.zip(state.storageState).map { case (storage, storageState) =>
      storage.updateState(tick, qDot, storageState)
    }

    val domesticHotWaterDemand = house
      .map(
        _.energyDemandDomesticHotWater(
          tick,
          state.houseState,
          simulationStartTime,
          houseInhabitants,
        )
      )
      .getOrElse(ThermalEnergyDemand(zeroKWH, zeroKWH))

    val (
      qDotDomesticHotWaterDemand,
      tickWhenStorageDemandEnds,
    ) =
      identifyApplicableQDot(tick, domesticHotWaterDemand)

    val (
      updatedDomesticHotWaterStorageState,
      domesticHotWaterStorageThreshold,
    ) = handleInfeedStorage(
      tick,
      state,
      qDotDomesticHotWaterDemand,
      domesticHotWaterStorage,
      simulationStartTime,
      houseInhabitants,
    )

    val nextThresholdHotWaterStorage = determineMostRecentThreshold(
      Seq(tickWhenStorageDemandEnds, domesticHotWaterStorageThreshold)
    )

    val nextThreshold = determineMostRecentThreshold(
      Seq(
        revisedHouseState.flatMap(_._2),
        revisedStorageState.flatMap(_._2),
        nextThresholdHotWaterStorage,
      )
    )

    (
      state.copy(
        houseState = revisedHouseState.map(_._1),
        storageState = revisedStorageState.map(_._1),
        domesticHotWaterStorageState = updatedDomesticHotWaterStorageState,
      ),
      nextThreshold,
    )
  }

  private def identifyApplicableQDot(
      tick: Long,
      domesticHotWaterDemand: ThermalEnergyDemand,
  ): (Power, Option[SimpleThermalThreshold]) = {

    if (domesticHotWaterDemand.required > zeroKWH) {
      val chargingPower = domesticHotWaterStorage
        .map(_.getChargingPower)
        .getOrElse(
          throw new RuntimeException(
            s"Trying to get the chargingPower of domesticHotWaterStorage was not possible"
          )
        )

      val approxDurationAtFullPower =
        domesticHotWaterDemand.required / chargingPower

      if (approxDurationAtFullPower > Seconds(1)) {
        val preciseChargingPower =
          -1 * domesticHotWaterDemand.required / (Seconds(
            approxDurationAtFullPower.toSeconds.toLong + 1
          ))
        val threshold =
          -1 * domesticHotWaterDemand.required / preciseChargingPower

        (
          preciseChargingPower,
          Some(
            SimpleThermalThreshold(tick + math.round(threshold.toSeconds))
          ),
        )
      } else {
        (
          (-1) * domesticHotWaterDemand.required / Seconds(1d),
          Some(SimpleThermalThreshold(tick + 1)),
        )
      }
    } else {
      (zeroKW, None)
    }
  }

  /** Check, if the storage can heat the house. This is only done, if <ul>
    * <li>the house has reached it's lower temperature boundary,</li> <li>there
    * is no infeed from external and</li> <li>the storage is not empty
    * itself</li> </ul>
    * @param tick
    *   The current tick
    * @param maybeHouseState
    *   Optional thermal house state
    * @param maybeStorageState
    *   Optional thermal storage state
    * @param formerHouseState
    *   Previous thermal house state before a first update was performed
    * @param formerStorageState
    *   Previous thermal storage state before a first update was performed
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param ambientTemperature
    *   Current ambient temperature
    * @param qDot
    *   Thermal influx
    * @return
    *   Options to revised thermal house and storage state
    */
  def reviseInfeedFromStorage(
      tick: Long,
      maybeHouseState: Option[(ThermalHouseState, Option[ThermalThreshold])],
      maybeStorageState: Option[
        (ThermalStorageState, Option[ThermalThreshold])
      ],
      formerHouseState: Option[ThermalHouseState],
      formerStorageState: Option[ThermalStorageState],
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      qDot: Power,
  ): (
      Option[(ThermalHouseState, Option[ThermalThreshold])],
      Option[(ThermalStorageState, Option[ThermalThreshold])],
  ) = house.zip(maybeHouseState).zip(heatStorage.zip(maybeStorageState)) match {
    case Some(
          (
            (thermalHouse, (houseState, _)),
            (thermalStorage, (storageState, _)),
          )
        )
        if qDot.~=(zeroKW)(Kilowatts(10e-3)) &&
          thermalHouse.isInnerTemperatureTooLow(
            houseState.innerTemperature
          ) && !thermalStorage.isEmpty(storageState.storedEnergy) =>
      /* Storage is meant to heat the house only, if there is no infeed from external (+/- 10 W) and the house is cold */
      val revisedStorageState = thermalStorage.updateState(
        tick,
        thermalStorage.getChargingPower * -1,
        formerStorageState.getOrElse(
          throw new InconsistentStateException(
            "Impossible to find no storage state"
          )
        ),
      )
      val revisedHouseState = thermalHouse.determineState(
        tick,
        formerHouseState.getOrElse(
          throw new InconsistentStateException(
            "Impossible to find no house state"
          )
        ),
        lastAmbientTemperature,
        ambientTemperature,
        thermalStorage.getChargingPower,
      )
      (Some(revisedHouseState), Some(revisedStorageState))
    case _ => (maybeHouseState, maybeStorageState)
  }

  /** Convert the given state of the thermal grid into result models of it's
    * constituent models
    * @param state
    *   State to be converted
    * @param startDateTime
    *   Start date time of the simulation
    * @return
    *   A [[Seq]] of results of the constituent thermal model
    */
  def results(
      state: ThermalGridState
  )(implicit startDateTime: ZonedDateTime): Seq[ResultEntity] = {
    /* FIXME: We only want to write results when there is a change within the participant.
     * At the moment we write an storage result when the house result gets updated and vice versa.
     * */

    val houseResultTick: Option[Long] = house
      .zip(state.houseState)
      .flatMap {
        case (
              _,
              ThermalHouseState(tick, _, _),
            ) =>
          Some(tick)
        case _ => None
      }

    val storageResultTick: Option[Long] = heatStorage
      .zip(state.storageState)
      .flatMap {
        case (
              _,
              ThermalStorageState(tick, _, _),
            ) =>
          Some(tick)
        case _ => None
      }

    val domesticHotWaterStorageResultTick: Option[Long] =
      domesticHotWaterStorage
        .zip(state.domesticHotWaterStorageState)
        .flatMap {
          case (
                _,
                ThermalStorageState(tick, _, _),
              ) =>
            Some(tick)
          case _ => None
        }

    val actualResultTick = Seq(
      houseResultTick,
      storageResultTick,
      domesticHotWaterStorageResultTick,
    ).flatten.maxOption.getOrElse(
      throw new InconsistentStateException(
        s"Was not able to get tick for thermal result. Result tick of thermal house: $houseResultTick," +
          s" Result tick of thermal heat storage: $storageResultTick, Result tick of domestic hot water storage: $domesticHotWaterStorageResultTick."
      )
    )

    val houseResults = house
      .zip(state.houseState)
      .map {
        case (
              thermalHouse,
              ThermalHouseState(_, innerTemperature, thermalInfeed),
            ) =>
          Seq(
            new ThermalHouseResult(
              actualResultTick.toDateTime,
              thermalHouse.uuid,
              thermalInfeed.toMegawatts.asMegaWatt,
              innerTemperature.toKelvinScale.asKelvin,
            )
          )
      }
      .getOrElse(Seq.empty[ResultEntity])

    val storageResults = heatStorage
      .zip(state.storageState)
      .map {
        case (
              storage: CylindricalThermalStorage,
              ThermalStorageState(_, storedEnergy, qDot),
            ) =>
          houseResults :+ new CylindricalStorageResult(
            actualResultTick.toDateTime,
            storage.uuid,
            storedEnergy.toMegawattHours.asMegaWattHour,
            qDot.toMegawatts.asMegaWatt,
            (storedEnergy / storage.maxEnergyThreshold).asPu,
          )
        case _ =>
          throw new NotImplementedError(
            s"Result handling for storage type '${heatStorage.getClass.getSimpleName}' not supported."
          )
      }
      .getOrElse(houseResults)

    val finalResults = domesticHotWaterStorage
      .zip(state.domesticHotWaterStorageState)
      .map {
        case (
              storage: DomesticHotWaterStorage,
              ThermalStorageState(_, storedEnergy, qDot),
            ) =>
          storageResults :+ new DomesticHotWaterStorageResult(
            actualResultTick.toDateTime,
            storage.uuid,
            storedEnergy.toMegawattHours.asMegaWattHour,
            qDot.toMegawatts.asMegaWatt,
            (storedEnergy / storage.maxEnergyThreshold).asPu,
          )
        case _ =>
          throw new NotImplementedError(
            s"Result handling for storage type '${heatStorage.getClass.getSimpleName}' not supported."
          )
      }
      .getOrElse(storageResults)

    finalResults
  }
}

object ThermalGrid {
  def apply(
      input: edu.ie3.datamodel.models.input.container.ThermalGrid
  ): ThermalGrid = {
    val houses = input.houses().asScala.map(ThermalHouse(_)).toSet
    val storages: Set[CylindricalThermalStorage] = input
      .heatStorages()
      .asScala
      .flatMap {
        case cylindricalInput: CylindricalStorageInput =>
          Some(CylindricalThermalStorage(cylindricalInput))
        case _ => None
      }
      .toSet
    val domesticHotWaterStorage: Set[DomesticHotWaterStorage] = input
      .domesticHotWaterStorages()
      .asScala
      .flatMap {

        case domesticHotWaterInput: DomesticHotWaterStorageInput =>
          Some(DomesticHotWaterStorage(domesticHotWaterInput))
        case _ => None
      }
      .toSet
    new ThermalGrid(
      houses.headOption,
      storages.headOption,
      domesticHotWaterStorage.headOption,
    )
  }

  /** Current state of a grid
    * @param houseState
    *   State of the thermal house
    * @param storageState
    *   State of the thermal storage
    */
  final case class ThermalGridState(
      houseState: Option[ThermalHouseState],
      storageState: Option[ThermalStorageState],
      domesticHotWaterStorageState: Option[ThermalStorageState],
  )

  def startingState(thermalGrid: ThermalGrid): ThermalGridState =
    ThermalGridState(
      thermalGrid.house.map(house => ThermalHouse.startingState(house)),
      thermalGrid.heatStorage.map(_.startingState),
      thermalGrid.domesticHotWaterStorage.map(_.startingState),
    )

  /** Defines the thermal energy demand of a thermal grid. It comprises the
    * absolutely required energy demand to reach the target state as well as an
    * energy, that can be handled. The possible energy always has to be greater
    * than or equal to the absolutely required energy. Thus, this class can only
    * be instantiated via factory.
    * @param required
    *   The absolutely required energy to reach target state
    * @param possible
    *   The maximum possible energy, that can be handled
    */
  final case class ThermalEnergyDemand private (
      required: Energy,
      possible: Energy,
  ) {
    def +(rhs: ThermalEnergyDemand): ThermalEnergyDemand = ThermalEnergyDemand(
      required + rhs.required,
      possible + rhs.possible,
    )

    def hasRequiredDemand: Boolean = required > zeroMWH

    def hasAdditionalDemand: Boolean = possible > zeroMWH
  }
  object ThermalEnergyDemand {

    /** Builds a new instance of [[ThermalEnergyDemand]]. If the possible energy
      * is less than the required energy, this is considered to be a bad state.
      * @param required
      *   The absolutely required energy to reach target state
      * @param possible
      *   The maximum possible energy, that can be handled
      * @return
      *   Thermal energy demand container class, that meets all specifications
      */
    def apply(
        required: Energy,
        possible: Energy,
    ): ThermalEnergyDemand = {
      if (
        math.abs(possible.toKilowattHours) < math.abs(required.toKilowattHours)
      )
        throw new InvalidParameterException(
          s"The possible amount of energy {$possible} is smaller than the required amount of energy {$required}. This is not supported."
        )
      else
        new ThermalEnergyDemand(required, possible)
    }

    def noDemand: ThermalEnergyDemand = ThermalEnergyDemand(
      zeroMWH,
      zeroMWH,
    )
  }
}
