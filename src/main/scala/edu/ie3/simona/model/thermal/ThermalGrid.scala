/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  ThermalHouseResult,
}
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power, Temperature}

import java.time.ZonedDateTime
import scala.jdk.CollectionConverters.SetHasAsScala

/** Calculation model for a thermal grid. It is assumed, that all elements are
  * connected directly with exactly one thermal bus
  *
  * @param house
  *   Thermal houses connected to the bus
  * @param storage
  *   Thermal storages
  */
final case class ThermalGrid(
    house: Option[ThermalHouse],
    storage: Option[ThermalStorage],
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

    val (houseDemand, updatedHouseState) =
      house.zip(lastHpState.thermalGridState.houseState) match {
        case Some((thermalHouse, lastHouseState)) =>
          val (updatedHouseState, _) =
            thermalHouse.determineState(
              relevantData,
              lastHouseState,
              lastHpState.ambientTemperature.getOrElse(
                relevantData.ambientTemperature
              ),
              lastHouseState.qDot,
            )
          if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature |
              (lastHouseState.qDot > zeroKW && updatedHouseState.innerTemperature < thermalHouse.upperBoundaryTemperature)
          ) {
            (
              thermalHouse.energyDemand(
                relevantData,
                updatedHouseState,
              ),
              Some(updatedHouseState),
            )

          } else {
            (ThermalEnergyDemand.noDemand, Some(updatedHouseState))
          }

        case None =>
          (ThermalEnergyDemand.noDemand, None)
      }

    /* Then go over the storages, see what they can provide and what they might be able to charge */
    val (storageDemand, updatedStorageState) = {

      storage
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
              zeroMWh
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
          ThermalEnergyDemand(zeroMWh, zeroMWh),
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
      ),
      ThermalGridState(updatedHouseState, updatedStorageState),
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
    *   Thermal energy balance
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

  /** Handles the case, when a grid has infeed. Depending on which entity has some
    * heat demand the house or the storage will be heated up / filled up.
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
    *   Infeed to the grid
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

    /* Consider the action in the last state */
    val qDotHouseLastState =
      lastThermalGridState.houseState.map(_.qDot).getOrElse(zeroKW)
    val qDotStorageLastState =
      lastThermalGridState.storageState.map(_.qDot).getOrElse(zeroKW)

    // We can use the qDots from lastState to keep continuity. If...
    if (
      // ... house was heated in lastState but not from Storage and has still some demand.
      ((qDotHouseLastState > zeroKW && (qDotStorageLastState >= zeroKW) && thermalDemands.houseDemand.hasAdditionalDemand) ||
      // ... storage was filled up in the lastState and has still additional demand
      // But only if the house not reached some requiredDemand.
      qDotStorageLastState > zeroKW && thermalDemands.heatStorageDemand.hasAdditionalDemand && !thermalDemands.houseDemand.hasRequiredDemand)
    ) {
      // We can continue for the house
      val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
        handleInfeedHouse(
          relevantData,
          lastAmbientTemperature,
          lastThermalGridState,
          qDotHouseLastState,
        )

      // ...and for the storage
      val (updatedStorageState, thermalStorageThreshold) = {
        // In case the ThermalHouse could not handle the infeed it will be used for the storage.
        if (remainingQDotHouse > qDotStorageLastState) {
          handleInfeedStorage(
            relevantData.currentTick,
            lastThermalGridState,
            remainingQDotHouse,
          )
        } else {
          handleInfeedStorage(
            relevantData.currentTick,
            lastThermalGridState,
            qDotStorageLastState,
          )
        }
      }

      val nextThreshold = determineMostRecentThreshold(
        thermalHouseThreshold,
        thermalStorageThreshold,
      )
      (
        lastThermalGridState.copy(
          houseState = updatedHouseState,
          storageState = updatedStorageState,
        ),
        nextThreshold,
      )
    }
    // Handle edge case where house was heated from storage and HP will be activated in between
    else if (qDotHouseLastState > zeroKW && qDotStorageLastState < zeroKW) {
      if (isRunning) {
        handleCases(
          relevantData,
          lastAmbientTemperature,
          lastThermalGridState,
          qDot,
          zeroKW,
        )
      } else {

        handleCases(
          relevantData,
          lastAmbientTemperature,
          lastThermalGridState,
          qDotHouseLastState,
          qDotStorageLastState,
        )
      }
    }
    // Handle edge case where house should be heated from storage
    else if (!isRunning && qDot > zeroKW) {
      handleCases(
        relevantData,
        lastAmbientTemperature,
        lastThermalGridState,
        qDot,
        -qDot,
      )
    } else
      handleFinalInfeedCases(
        thermalDemands,
        relevantData,
        lastAmbientTemperature,
        lastThermalGridState,
        qDot,
      )
  }

  /** Handles the last cases of [[ThermalGrid.handleInfeed]], where the thermal
    * infeed should be determined.
    *
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
    * This can be simplified to five cases
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
    *   Infeed to the grid
    * @return
    *   Updated thermal grid state and the thermalThreshold if there is one
    */
  private def handleFinalInfeedCases(
      thermalDemands: ThermalDemandWrapper,
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      gridState: ThermalGridState,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    (
      thermalDemands.houseDemand.hasRequiredDemand,
      thermalDemands.houseDemand.hasAdditionalDemand,
      thermalDemands.heatStorageDemand.hasRequiredDemand,
      thermalDemands.heatStorageDemand.hasAdditionalDemand,
    ) match {

      case (true, _, _, _) =>
        // house first then heatStorage after heating House
        handleCases(
          relevantData,
          lastAmbientTemperature,
          gridState,
          qDot,
          zeroKW,
        )

      case (_, _, true, _) =>
        handleCases(
          relevantData,
          lastAmbientTemperature,
          gridState,
          zeroKW,
          qDot,
        )

      case (false, _, false, true) =>
        handleCases(
          relevantData,
          lastAmbientTemperature,
          gridState,
          zeroKW,
          qDot,
        )

      case (_, true, false, false) =>
        handleCases(
          relevantData,
          lastAmbientTemperature,
          gridState,
          qDot,
          zeroKW,
        )

      case (false, false, false, false) =>
        handleCases(
          relevantData,
          lastAmbientTemperature,
          gridState,
          zeroKW,
          zeroKW,
        )
      case _ =>
        throw new InconsistentStateException(
          "There should be at least a house or a storage state."
        )
    }
  }

  /** Handles the different cases, of thermal flows from and into the thermal
    * grid.
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param lastAmbientTemperature
    *   Ambient temperature until this tick
    * @param state
    *   Current state of the thermal grid
    * @param qDotHouse
    *   Infeed to the house
    * @param qDotHeatStorage
    *   Infeed to the heat storage
    * @return
    *   Updated thermal grid state and the next threshold if there is one
    */
  private def handleCases(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    val (updatedHouseState, thermalHouseThreshold, _) =
      handleInfeedHouse(
        relevantData,
        lastAmbientTemperature,
        state,
        qDotHouse,
      )

    val (updatedStorageState, thermalStorageThreshold) =
      handleInfeedStorage(relevantData.currentTick, state, qDotHeatStorage)

    val nextThreshold = determineMostRecentThreshold(
      thermalHouseThreshold,
      thermalStorageThreshold,
    )

    (
      state.copy(
        houseState = updatedHouseState,
        storageState = updatedStorageState,
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
    *   Ambient temperature until this tick
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    *   Updated thermal house state, a ThermalThreshold and the remaining qDot
    */
  private def handleInfeedHouse(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ): (Option[ThermalHouseState], Option[ThermalThreshold], Power) = {
    (house, state.houseState) match {
      case (Some(thermalHouse), Some(lastHouseState)) =>
        val (newState, threshold) = thermalHouse.determineState(
          relevantData,
          lastHouseState,
          lastAmbientTemperature,
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
              relevantData,
              lastHouseState,
              lastAmbientTemperature,
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
    * grid (negative qDot).
    * @param tick
    *   Current tick
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    *   Updated thermal grid state
    */
  private def handleInfeedStorage(
      tick: Long,
      state: ThermalGridState,
      qDot: Power,
  ): (Option[ThermalStorageState], Option[ThermalThreshold]) = {
    (storage, state.storageState) match {
      case (Some(thermalStorage), Some(lastStorageState)) =>
        val (newState, threshold) = thermalStorage.updateState(
          tick,
          qDot,
          lastStorageState,
        )
        (Some(newState), threshold)
      case _ => (None, None)
    }
  }

  /** Determines the most recent threshold of two given input thresholds
    *
    * @param maybeHouseThreshold
    *   Option of a possible next threshold of the thermal house
    * @param maybeStorageThreshold
    *   Option of a possible next threshold of the thermal storage
    * @return
    *   The next threshold
    */
  private def determineMostRecentThreshold(
      maybeHouseThreshold: Option[ThermalThreshold],
      maybeStorageThreshold: Option[ThermalThreshold],
  ): Option[ThermalThreshold] =
    (maybeHouseThreshold, maybeStorageThreshold) match {
      case (Some(houseThreshold), Some(storageThreshold)) =>
        if (houseThreshold.tick <= storageThreshold.tick)
          maybeHouseThreshold
        else
          maybeStorageThreshold
      case (None, Some(_)) => maybeStorageThreshold
      case (Some(_), None) => maybeHouseThreshold
      case _               => None
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
    *   Infeed to the grid
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
            zeroMW,
          )
      }

    /* Update the state of the storage */
    val maybeUpdatedStorageState =
      storage.zip(lastThermalGridState.storageState).map {
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

    val nextThreshold = determineMostRecentThreshold(
      revisedHouseState.flatMap(_._2),
      revisedStorageState.flatMap(_._2),
    )

    (
      lastThermalGridState.copy(
        houseState = revisedHouseState.map(_._1),
        storageState = revisedStorageState.map(_._1),
      ),
      nextThreshold,
    )
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
    *   Thermal influx
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
  ) = house.zip(maybeHouseState).zip(storage.zip(maybeStorageState)) match {
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

    val maybeStorageResult = storage
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
            s"Result handling for storage type '${storage.getClass.getSimpleName}' not supported."
          )
      }

    Seq(maybeHouseResult, maybeStorageResult).flatten
  }
}

object ThermalGrid {
  def apply(
      input: edu.ie3.datamodel.models.input.container.ThermalGrid
  ): ThermalGrid = {
    val houses = input.houses().asScala.map(ThermalHouse(_)).toSet
    val storages: Set[ThermalStorage] = input
      .storages()
      .asScala
      .flatMap {
        case cylindricalInput: CylindricalStorageInput =>
          Some(CylindricalThermalStorage(cylindricalInput))
        case _ => None
      }
      .toSet
    new ThermalGrid(
      houses.headOption,
      storages.headOption,
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
      thermalGrid.storage.map(_.startingState),
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

    def hasRequiredDemand: Boolean = required > zeroMWh

    def hasAdditionalDemand: Boolean = possible > required
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
      if (possible < required)
        new ThermalEnergyDemand(possible, possible)
      else
        new ThermalEnergyDemand(required, possible)
    }

    def noDemand: ThermalEnergyDemand = ThermalEnergyDemand(
      zeroMWh,
      zeroMWh,
    )
  }
}
