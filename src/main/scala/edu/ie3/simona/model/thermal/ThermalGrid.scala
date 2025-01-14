/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.thermal.{CylindricalStorageInput, DomesticHotWaterStorageInput}
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{CylindricalStorageResult, DomesticHotWaterStorageResult, ThermalHouseResult}
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.{ThermalDemandWrapper, ThermalEnergyDemand, ThermalGridState}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.{KilowattHours, Kilowatts}
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
    * @param lastHpState
    *   Last state of the heat pump
    * @param relevantData
    *   data of heat pump including
    * @return
    *   The total energy demand of the house and the storage and an updated
    *   [[ThermalGridState]]
    */
  def energyDemandAndUpdatedState(
      relevantData: HpRelevantData,
      lastHpState: HpState,
  ): (ThermalDemandWrapper, ThermalGridState) = {
    /* First get the energy demand of the houses but only if inner temperature is below target temperature */
    val (houseDemand, updatedHouseState, demandHotDomesticWater) =
      house.zip(lastHpState.thermalGridState.houseState) match {
        case Some((thermalHouse, lastHouseState)) => {
          val (updatedHouseState, _) =
            thermalHouse.determineState(
              relevantData,
              lastHouseState,
              lastHpState.ambientTemperature.getOrElse(
                relevantData.ambientTemperature
              ),
              lastHouseState.qDot,
            )
          val (heatDemand, newHouseState) = if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature | (lastHouseState.qDot > zeroKW && updatedHouseState.innerTemperature < thermalHouse.upperBoundaryTemperature)
          ) {
            (
              thermalHouse.energyDemandHeating(
                relevantData,
                updatedHouseState,
              ),
              Some(updatedHouseState),
            )

          } else {
            (ThermalEnergyDemand.noDemand, Some(updatedHouseState))
          }

          val energyDemandDomesticHotWater =
            thermalHouse.energyDemandDomesticHotWater(
              relevantData,
              Some(lastHouseState),
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
        .zip(lastHpState.thermalGridState.storageState)
        .map { case (storage, state) =>
          val (updatedStorageState, _) =
            storage.updateState(relevantData.currentTick, state.qDot, state)
          val storedEnergy = updatedStorageState.storedEnergy
          val soc = storedEnergy / storage.getMaxEnergyThreshold
          val storageRequired = {
            if (soc == 0d) {
              storage.getMaxEnergyThreshold - storedEnergy

            } else {
              zeroKWh
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
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          None,
        )
    }

    // Domestic hot water storages
    val (domesticHotWaterStorageDemand, updatedDomesticHotWaterStorageState) = {
      val domesticHotWaterDemand: ThermalEnergyDemand = house
        .map(
          _.energyDemandDomesticHotWater(
            relevantData,
            lastHpState.thermalGridState.houseState,
          )
        )
        .getOrElse(ThermalEnergyDemand(zeroKWh, zeroKWh))
      val (applicableqDotDomesticStorage, _) =
        identifyApplicableQDot(relevantData.currentTick, domesticHotWaterDemand)

      domesticHotWaterStorage
        .zip(lastHpState.thermalGridState.domesticHotWaterStorageState)
        .map { case (storage, state) =>
          val updatedStorageState =
            storage
              .updateState(
                relevantData.currentTick,
                state.qDot.plus(applicableqDotDomesticStorage),
                state,
              )
              ._1
          val storedEnergy = updatedStorageState.storedEnergy

          // Declare demand of domestic hot water storage if demand is higher than stored energy or stored energy is less than 20% of capacity
          val demandOfStorage =
            if (
              storedEnergy < demandHotDomesticWater.required || storedEnergy < storage.getMaxEnergyThreshold * 0.2
            ) {

              storage.getMaxEnergyThreshold + demandHotDomesticWater.required - storedEnergy

            } else zeroKWh
          (
            ThermalEnergyDemand(demandOfStorage, demandOfStorage),
            Some(updatedStorageState),
          )
        }
        .getOrElse(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          None,
        )
    }

    (
      ThermalDemandWrapper(
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
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastThermalGridState
    *   state of the thermalGrid until this tick
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param isRunning
    *   determines whether the heat pump is running or not
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump)
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage)
    * @return
    *   The updated state of the grid
    */
  def updateState(
      relevantData: HpRelevantData,
      lastThermalGridState: ThermalGridState,
      lastAmbientTemperature: Temperature,
      isRunning: Boolean,
      qDot: Power,
      thermalDemands: ThermalDemandWrapper,
  ): (ThermalGridState, Option[ThermalThreshold]) = if (qDot > zeroKW)
    handleInfeed(
      relevantData,
      lastAmbientTemperature,
      lastThermalGridState,
      isRunning,
      qDot,
      thermalDemands,
    )
  else
    handleConsumption(
      relevantData,
      lastAmbientTemperature,
      lastThermalGridState,
      qDot,
    )

  /** Handles the case, when a grid has infeed. Depending on which entity has
    * some heat demand the house or the storage will be heated up / filled up.
    * First the actions from lastState will be considered and checked if the
    * behaviour should be continued. This might be the case, if we got activated
    * by updated weather data. If this is not the case, all other cases will be
    * handled by [[ThermalGrid.handleFinalInfeedCases]]
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param lastThermalGridState
    *   state of the thermalGrid until this tick
    * @param isRunning
    *   determines whether the heat pump is running or not
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump)
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage)
    * @return
    *   Updated thermal grid state and the thermalThreshold if there is one
    */
  private def handleInfeed(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      lastThermalGridState: ThermalGridState,
      isRunning: Boolean,
      qDot: Power,
      thermalDemands: ThermalDemandWrapper,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // TODO: We would need to issue a storage result model here...

    /* Consider the action in the last state and if it's possible to continue*/
    val (_, qDotHouseLastState, houseReachedBoundary, houseLeftBoundary) =
      updateStateGetLastThermalActionAndCheckIfCanContinueThermalHouse(
        relevantData,
        lastThermalGridState,
        lastAmbientTemperature,
      )
    val (
      updatedHeatStorageState,
      qDotStorageLastState,
      thermalStorageReachedBoundary,
      thermalStorageLeftBoundary,
    ) =
      updateStateGetLastThermalActionAndCheckIfCanContinueThermalStorage(
        relevantData.currentTick,
        lastThermalGridState,
      )
    val (
      _,
      qDotDomesticWaterStorageLastState,
      domesticHotWaterStorageReachedBoundary,
      domesticHotWaterStorageLeftBoundary,
    ) =
      updateStateGetLastThermalActionAndCheckIfCanContinueDomesticHotWaterStorage(
        relevantData.currentTick,
        lastThermalGridState,
      )
    val domesticHotWaterStorageDemand =
      thermalDemands.domesticHotWaterStorageDemand

    if (
      !houseReachedBoundary && !houseLeftBoundary &&
      !thermalStorageReachedBoundary && !thermalStorageLeftBoundary &&
      !domesticHotWaterStorageReachedBoundary && !domesticHotWaterStorageLeftBoundary
    ) {
      handleCases(
        relevantData,
        lastAmbientTemperature,
        lastThermalGridState,
        qDotHouseLastState.getOrElse(zeroKW),
        qDotStorageLastState.getOrElse(zeroKW),
        if (domesticHotWaterStorageDemand.hasRequiredDemand)
          qDotDomesticWaterStorageLastState.getOrElse(zeroKW)
        else zeroKW,
      )
    } else {
      handleFinalInfeedCases(
        thermalDemands,
        relevantData,
        lastAmbientTemperature,
        lastThermalGridState,
        qDot,
        updatedHeatStorageState,
        qDotHouseLastState,
        qDotStorageLastState,
        qDotDomesticWaterStorageLastState,
        domesticHotWaterStorageLeftBoundary,
        isRunning,
      )
    }
  }

  /** Handles the last cases of [[ThermalGrid.handleInfeed]], where the thermal
    * infeed should be determined. FIXME adapt whole table
    * | house req. demand | house add. demand | storage req. demand | storage add. demand | qDot to house | qDot to storage |
    * |:------------------|:------------------|:--------------------|:--------------------|:--------------|:----------------|
    * | true              | true              | true                | true                | true          | false           |
    * | true              | true              | true                | false               | true          | false           |
    * | true              | true              | false               | true                | true          | false           |
    * | true              | true              | false               | false               | true          | false           |
    * | true              | false             | true                | true                | true          | false           |
    * | true              | false             | true                | false               | true          | false           |
    * | true              | false             | false               | true                | true          | false           |
    * | true              | false             | false               | false               | true          | false           |
    * | false             | true              | true                | true                | false         | true            |
    * | false             | true              | true                | false               | false         | true            |
    * | false             | true              | false               | true                | false         | true            |
    * | false             | true              | false               | false               | true          | false           |
    * | false             | false             | true                | true                | false         | true            |
    * | false             | false             | true                | false               | false         | true            |
    * | false             | false             | false               | true                | false         | true            |
    * | false             | false             | false               | false               | false         | false           |
    *
    * This can be simplified to five cases FIXME adapt whole table
    * | No | Conditions                                                               | Result    |
    * |:---|:-------------------------------------------------------------------------|:----------|
    * | 1  | if(house.reqD) => house                                                  | house     |
    * | 2  | if(!house.reqD && !storage.reqD) => storage                              | storage   |
    * | 3  | if(!house.reqD && !storage.reqD && storage.addD) => storage              | storage   |
    * | 4  | if(!house.reqD && house.addD && !storage.reqD && !storage.addD) => house | house     |
    * | 5  | if(all == false) => no output                                            | no output |
    *
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage)
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param gridState
    *   Current state of the thermalGrid
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump)
    * @return
    *   Updated thermal grid state and the thermalThreshold if there is one
    */
  private def handleFinalInfeedCases(
      thermalDemands: ThermalDemandWrapper,
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      gridState: ThermalGridState,
      qDot: Power,
      updatedHeatStorageState: Option[ThermalStorageState],
      qDotHouseLastState: Option[Power],
      qDotStorageLastState: Option[Power],
      qDotDomesticWaterStorageLastState: Option[Power],
      domesticHotWaterStorageLeftBoundary: Boolean,
      isRunning: Boolean,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    (
      thermalDemands.houseDemand.hasRequiredDemand,
      thermalDemands.houseDemand.hasAdditionalDemand,
      thermalDemands.heatStorageDemand.hasRequiredDemand,
      thermalDemands.heatStorageDemand.hasAdditionalDemand,
      thermalDemands.domesticHotWaterStorageDemand.hasRequiredDemand,
      thermalDemands.domesticHotWaterStorageDemand.hasAdditionalDemand,
    ) match {
      case (true, _, _, _, true, _) =>
        updatedHeatStorageState match {
          // if heatStorage is not empty, house and hot water storage can handleDemand
          // take qDot to recharge domesticHotWaterStorage and
          // cover thermal demand of house by heatStorage
          case Some(storageState) if storageState.storedEnergy > zeroKWh =>
            handleCases(
              relevantData,
              lastAmbientTemperature,
              gridState,
              heatStorage.map(_.getChargingPower).getOrElse(zeroKW),
              heatStorage.map(_.getChargingPower).getOrElse(zeroKW) * (-1),
              qDot,
            )
          case _ =>
            splitThermalHeatAndPushIntoHouseAndDomesticStorage(
              relevantData,
              lastAmbientTemperature,
              gridState,
              qDotHouseLastState,
              qDotDomesticWaterStorageLastState,
              qDot,
            )
        }

      case (true, _, _, _, false, _) =>
        // if there is a heatStorage that isn't empty, take energy from storage
        updatedHeatStorageState match {
          case Some(storageState)
              if (storageState.storedEnergy > zeroKWh && !isRunning) =>
            handleCases(
              relevantData,
              lastAmbientTemperature,
              gridState,
              heatStorage.map(_.getChargingPower).getOrElse(zeroKW),
              heatStorage.map(_.getChargingPower).getOrElse(zeroKW) * (-1),
              zeroKW,
            )
          case _ =>
            pushThermalHeatIntoHouseOnly(
              relevantData,
              lastAmbientTemperature,
              gridState,
              qDot,
            )
        }

      // Prioritize domestic hot water storage
      // Same case if there is Some(heatStorageDemand) or not
      case (false, _, _, _, true, _) =>
        pushThermalHeatIntoDomesticHotWaterStorageOnly(
          relevantData,
          lastAmbientTemperature,
          gridState,
          qDot,
        )

      case (false, _, true, _, false, _) =>
        pushThermalHeatIntoThermalStorageOnly(
          relevantData,
          lastAmbientTemperature,
          gridState,
          qDot,
        )
      //  all cases for required demands are handled now
      // now take last action into account
      case _ =>
        // House and domestic hot water storage can only have additional demand now
        domesticHotWaterStorageLeftBoundary match {
          case true =>
            handleCases(
              relevantData,
              lastAmbientTemperature,
              gridState,
              qDotHouseLastState.getOrElse(zeroKW),
              qDotStorageLastState.getOrElse(zeroKW),
              zeroKW,
            )
          // if storage has additional demand charge it before heating the house
          case _ =>
            if (thermalDemands.heatStorageDemand.hasAdditionalDemand)
              handleCases(
                relevantData,
                lastAmbientTemperature,
                gridState,
                zeroKW,
                qDot,
                zeroKW,
              )
            else {
              handleCases(
                relevantData,
                lastAmbientTemperature,
                gridState,
                qDot,
                zeroKW,
                zeroKW,
              )
            }
        }

    }
  }

  /** Method that updates the state of thermal house, get its last thermal
    * action (qDot) and if it reached or left any boundaries this tick.
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param state
    *   Current state of the houses
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @return
    *   Option of the updated house state, option of the last qDot and Booleans
    *   if some boundaries are reached or left.
    */

  private def updateStateGetLastThermalActionAndCheckIfCanContinueThermalHouse(
      relevantData: HpRelevantData,
      state: ThermalGridState,
      lastAmbientTemperature: Temperature,
  ): (Option[ThermalHouseState], Option[Power], Boolean, Boolean) = {
    state match {
      case ThermalGridState(
            Some(_),
            _,
            _,
          ) =>
        (house, state.houseState) match {
          case (Some(thermalHouse), Some(lastHouseState)) =>
            val (newState, _) = thermalHouse.determineState(
              relevantData,
              lastHouseState,
              lastAmbientTemperature,
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
            (newState.storedEnergy == zeroKWh)

        val storageLeftBorder =
          (lastStorageState.storedEnergy == storage.getMaxEnergyThreshold && newState.storedEnergy != zeroKWh) ||
            (lastStorageState.storedEnergy == zeroKWh && newState.storedEnergy != zeroKWh)
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
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    */

  private def pushThermalHeatIntoHouseOnly(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ) = {
    handleCases(
      relevantData,
      lastAmbientTemperature,
      state,
      qDot,
      zeroKW,
      zeroKW,
    )
  }

  /** Helper method to push energy directly into the thermal heat storage
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    */

  private def pushThermalHeatIntoThermalStorageOnly(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ) = {
    handleCases(
      relevantData,
      lastAmbientTemperature,
      state,
      zeroKW,
      qDot,
      zeroKW,
    )
  }

  /** Helper method to push energy directly into the domestic hot water storage
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    */

  private def pushThermalHeatIntoDomesticHotWaterStorageOnly(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ) = {
    handleCases(
      relevantData,
      lastAmbientTemperature,
      state,
      zeroKW,
      zeroKW,
      qDot,
    )
  }

  /** Helper method to split qDot of the heat pump and push energy directly into
    * house and the domestic hot water storage
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param state
    *   Current state of the houses
    * @param qDotHouseLastState
    *   The infeed into the thermal house in the last state
    * @param qDotDomesticHotWaterStorageLastState
    *   The infeed into the domestic hot water storage in the last state
    * @param qDot
    *   Infeed to the grid
    * @return
    */

  private def splitThermalHeatAndPushIntoHouseAndDomesticStorage(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouseLastState: Option[Power],
      qDotDomesticHotWaterStorageLastState: Option[Power],
      qDot: Power,
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
      relevantData,
      lastAmbientTemperature,
      state,
      qDot - qDotDomesticHotWaterStorage,
      zeroKW,
      qDotDomesticHotWaterStorage,
    )
  }

  /** Handles the different cases, of thermal flows from and into the thermal
    * grid.
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param state
    *   Current state of the thermal grid
    * @param qDotHouse
    *   Infeed to the house
    * @param qDotHeatStorage
   *    Infeed to the heat storage (positive: Storage is charging, negative:
   *    Storage is discharging)
    * @param qDotDomesticHotWaterStorage
    *   Infeed to the domestic hot water storage
    * @return
    *   Updated thermal grid state and the next threshold if there is one
    */
  private def handleCases(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
      qDotDomesticHotWaterStorage: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // FIXME: Is there any case where we get back some remainingQDotHouse?
    val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
      handleInfeedHouse(
        relevantData,
        lastAmbientTemperature,
        state,
        qDotHouse,
      )

    val (updatedStorageState, thermalStorageThreshold) =
      handleInfeedStorage(
        relevantData,
        state,
        qDotHeatStorage,
        heatStorage,
      )

    val (
      updatedDomesticHotWaterStorageState,
      domesticHotWaterStorageThreshold,
    ) = handleInfeedStorage(
      relevantData,
      state,
      qDotDomesticHotWaterStorage,
      domesticHotWaterStorage,
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
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param state
    *   Current state of the houses
    * @param qDotHouse
    *   Infeed into the house
    * @return
    *   Updated thermal house state, a ThermalThreshold and the remaining qDot
    */
  private def handleInfeedHouse(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouse: Power,
  ): (Option[ThermalHouseState], Option[ThermalThreshold], Power) = {
    (house, state.houseState) match {
      case (Some(thermalHouse), Some(lastHouseState)) =>
        val (newState, threshold) = thermalHouse.determineState(
          relevantData,
          lastHouseState,
          lastAmbientTemperature,
          qDotHouse,
        )
        /* Check if house can handle the thermal feed in */
        if (
          thermalHouse.isInnerTemperatureTooHigh(
            newState.innerTemperature
          )
        ) {
          val (fullHouseState, maybeFullHouseThreshold) =
            thermalHouse.determineState(
              relevantData,
              lastHouseState,
              lastAmbientTemperature,
              zeroKW,
            )
          (Some(fullHouseState), maybeFullHouseThreshold, qDotHouse)
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
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param state
    *   Current state of the houses
   * @param qDotStorage
   *   Infeed to the storage (positive: Storage is charging, negative: Storage
   *   is discharging)
    * @param storage
    *   the storage that should be handled
    * @return
    *   Updated thermal grid state
    */
  private def handleInfeedStorage(
      relevantData: HpRelevantData,
      state: ThermalGridState,
      qDotStorage: Power,
      storage: Option[ThermalStorage],
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
            Some(domesticHotWaterStorage: DomesticHotWaterStorage),
            Some(lastDomesticHotWaterStorageState),
          ) =>
        val storedEnergy = domesticHotWaterStorage
          .updateState(
            relevantData.currentTick,
            lastDomesticHotWaterStorageState.qDot,
            lastDomesticHotWaterStorageState,
          )
          ._1
          .storedEnergy

        val domesticHotWaterDemand = house
          .map(
            _.energyDemandDomesticHotWater(
              relevantData,
              state.houseState,
            )
          )
          .getOrElse(ThermalEnergyDemand(zeroKWh, zeroKWh))

        val (applicableQDotDomesticStorage, thresholdToCoverDemand) =
          domesticHotWaterDemand match {
            case demand
                if demand.required > zeroKWh && storedEnergy == zeroKWh =>
              // Use qDot from Hp directly to cover hot water demand
              val threshold = Some(
                SimpleThermalThreshold(
                  (domesticHotWaterDemand.required / qDotStorage).toSeconds.toLong
                )
              )
              (zeroKW, threshold)
            case demand
                if demand.required > zeroKWh && storedEnergy > zeroKWh =>
              // Use storage to cover hot water demand
              identifyApplicableQDot(relevantData.currentTick, demand)

            case demand
                if demand.required == zeroKWh && lastDomesticHotWaterStorageState.qDot > zeroKW && storedEnergy != domesticHotWaterStorage.maxEnergyThreshold =>
              // Storage got recharged in the last state
              // Threshold will be calculated later
              (lastDomesticHotWaterStorageState.qDot, None)
            case demand if demand.required == zeroKWh & qDotStorage == zeroKW =>
              // Don't do anything with domestic hot water storage
              (zeroKW, None)
            case demand if demand.required == zeroKWh & qDotStorage > zeroKW =>
              // Use qDot from Hp to recharge domestic hot water storage
              // Threshold will be calculated later
              (qDotStorage, None)
            case demand if demand.required == zeroKWh & qDotStorage < zeroKW =>
              // Storage was discharging but demand is now covered. Set output to zeroKw
              (zeroKW, None)

            case _ =>
              throw new RuntimeException(
                s"Unexpected case occur when try to handle infeed into domestic hot water storage ${domesticHotWaterStorage.uuid}."
              )
          }

        val (updatedStorageState, updatedThreshold) =
          domesticHotWaterStorage
            .updateState(
              relevantData.currentTick,
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

      case (
            Some(thermalStorage: CylindricalThermalStorage),
            Some(lastStorageState),
          ) =>
        val (newState, threshold) = thermalStorage.updateState(
          relevantData.currentTick,
          qDotStorage,
          lastStorageState,
        )
        (Some(newState), threshold)

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
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param lastThermalGridState
    *   state of the thermalGrid until this tick
    * @param qDot
    *     Infeed to the grid from thermal generation (e.g. heat pump)
    * @param simulationStartTime
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   Updated thermal grid state
    */
  private def handleConsumption(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      lastThermalGridState: ThermalGridState,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    /* House will be left with no influx in all cases. Determine if and when a threshold is reached */
    val maybeUpdatedHouseState =
      house.zip(lastThermalGridState.houseState).map {
        case (house, houseState) =>
          house.determineState(
            relevantData,
            houseState,
            lastAmbientTemperature,
            zeroKW,
          )
      }

    /* Update the state of the storage */
    val maybeUpdatedStorageState =
      heatStorage.zip(lastThermalGridState.storageState).map {
        case (storage, storageState) =>
          storage.updateState(relevantData.currentTick, qDot, storageState)
      }

    val (revisedHouseState, revisedStorageState) =
      reviseInfeedFromStorage(
        relevantData,
        maybeUpdatedHouseState,
        maybeUpdatedStorageState,
        lastThermalGridState.houseState,
        lastThermalGridState.storageState,
        lastAmbientTemperature,
        qDot,
      )

    heatStorage.zip(lastThermalGridState.storageState).map { case (storage, storageState) =>
      storage.updateState(relevantData.currentTick, qDot, storageState)
    }

    val domesticHotWaterDemand = house
      .map(
        _.energyDemandDomesticHotWater(
          relevantData,
          lastThermalGridState.houseState,
        )
      )
      .getOrElse(ThermalEnergyDemand(zeroKWh, zeroKWh))

    val (
      qDotDomesticHotWaterDemand,
      tickWhenStorageDemandEnds,
    ) =
      identifyApplicableQDot(relevantData.currentTick, domesticHotWaterDemand)

    val (
      updatedDomesticHotWaterStorageState,
      domesticHotWaterStorageThreshold,
    ) = handleInfeedStorage(
      relevantData,
      lastThermalGridState,
      qDotDomesticHotWaterDemand,
      domesticHotWaterStorage,
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
      lastThermalGridState.copy(
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

    if (domesticHotWaterDemand.required > zeroKWh) {
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
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
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
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump)
    * @return
    *   Options to revised thermal house and storage state
    */
  def reviseInfeedFromStorage(
      relevantData: HpRelevantData,
      maybeHouseState: Option[(ThermalHouseState, Option[ThermalThreshold])],
      maybeStorageState: Option[
        (ThermalStorageState, Option[ThermalThreshold])
      ],
      formerHouseState: Option[ThermalHouseState],
      formerStorageState: Option[ThermalStorageState],
      lastAmbientTemperature: Temperature,
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
        relevantData.currentTick,
        thermalStorage.getChargingPower * -1,
        formerStorageState.getOrElse(
          throw new InconsistentStateException(
            "Impossible to find no storage state"
          )
        ),
      )
      val revisedHouseState = thermalHouse.determineState(
        relevantData,
        formerHouseState.getOrElse(
          throw new InconsistentStateException(
            "Impossible to find no house state"
          )
        ),
        lastAmbientTemperature,
        thermalStorage.getChargingPower,
      )
      (Some(revisedHouseState), Some(revisedStorageState))
    case _ => (maybeHouseState, maybeStorageState)
  }

  /** Convert the given state of the thermal grid into result models of its
    * constituent models
    *
    * @param currentTick
    *   Actual simulation tick
    * @param state
    *   State to be converted
    * @param startDateTime
    *   Start date time of the simulation
    * @return
    *   A [[Seq]] of results of the constituent thermal model
    */
  def results(currentTick: Long, state: ThermalGridState)(implicit
      startDateTime: ZonedDateTime
  ): Seq[ResultEntity] = {

    val maybeHouseResult = house
      .zip(state.houseState)
      .filter { case (_, state) => state.tick == currentTick }
      .map {
        case (
              thermalHouse,
              ThermalHouseState(tick, innerTemperature, thermalInfeed),
            ) =>
          new ThermalHouseResult(
            tick.toDateTime,
            thermalHouse.uuid,
            thermalInfeed.toMegawatts.asMegaWatt,
            innerTemperature.toKelvinScale.asKelvin,
          )
      }

    val maybeThermalStorageResult = heatStorage
      .zip(state.storageState)
      .filter { case (_, state) => state.tick == currentTick }
      .map {
        case (
              storage: CylindricalThermalStorage,
              ThermalStorageState(tick, storedEnergy, qDot),
            ) =>
          new CylindricalStorageResult(
            tick.toDateTime,
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

    val maybeDomesticHotWaterStorageResult = domesticHotWaterStorage
      .zip(state.domesticHotWaterStorageState)
      .filter { case (_, state) => state.tick == currentTick }
      .map {
        case (
              storage: DomesticHotWaterStorage,
              ThermalStorageState(tick, storedEnergy, qDot),
            ) =>
          new DomesticHotWaterStorageResult(
            tick.toDateTime,
            storage.uuid,
            storedEnergy.toMegawattHours.asMegaWattHour,
            qDot.toMegawatts.asMegaWatt,
            (storedEnergy / storage.maxEnergyThreshold).asPu,
          )
        case _ =>
          throw new NotImplementedError(
            s"Result handling for storage type '${domesticHotWaterStorage.getClass.getSimpleName}' not supported."
          )
      }

    Seq(
      maybeHouseResult,
      maybeThermalStorageResult,
      maybeDomesticHotWaterStorageResult,
    ).flatten

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
        case _: DomesticHotWaterStorageInput =>
          None
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
  ) {

    /** This method will return booleans whether there is a heat demand of house
      * or thermal storage as well as a boolean indicating if there is no
      * thermal storage, or it is empty.
      *
      * @return
      *   boolean which is true, if there is no thermalStorage, or it's empty.
      */
    def isThermalStorageEmpty: Boolean = {
      implicit val tolerance: Energy = KilowattHours(1e-3)
      storageState.isEmpty || storageState
        .exists(
          _.storedEnergy =~ zeroKWh
        )
    }
  }

  def startingState(thermalGrid: ThermalGrid): ThermalGridState =
    ThermalGridState(
      thermalGrid.house.map(house => ThermalHouse.startingState(house)),
      thermalGrid.heatStorage.map(_.startingState),
      thermalGrid.domesticHotWaterStorage.map(_.startingState),
    )

  /** Wraps the demand of thermal units (thermal house, thermal storage).
    *
    * @param houseDemand
    *   the demand of the thermal house
    * @param heatStorageDemand
    *   the demand of the thermal heat storage
    */
  final case class ThermalDemandWrapper private (
      houseDemand: ThermalEnergyDemand,
      heatStorageDemand: ThermalEnergyDemand,
      domesticHotWaterStorageDemand: ThermalEnergyDemand,
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

    def hasRequiredDemand: Boolean = required > zeroKWh

    def hasAdditionalDemand: Boolean = possible > zeroKWh
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
      zeroKWh,
      zeroKWh,
    )
  }
}
