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
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.participant2.HpModel.{HpOperatingPoint, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}

import java.time.ZonedDateTime
import scala.jdk.CollectionConverters.SetHasAsScala
import scala.language.postfixOps

/** Calculation model for a thermal grid. It is assumed, that all elements are
  * connected directly with exactly one thermal bus.
  *
  * @param house
  *   Thermal houses connected to the bus.
  * @param heatStorage
  *   Thermal storages connected to the bus.
  */
final case class ThermalGrid(
    house: Option[ThermalHouse],
    heatStorage: Option[ThermalStorage],
) extends LazyLogging {

  /** Determine the energy demand of the total grid at the given instance in
    * time and returns it including the updatedState.
    *
    * @param state
    *   Required data for calculation.
    * @return
    *   The total energy demand of the house and the storage and an updated
    *   [[ThermalGridState]].
    */
  def energyDemandAndUpdatedState(
      tick: Long,
      state: HpState,
      operatingPoint: HpOperatingPoint,
  ): (ThermalDemandWrapper, ThermalGridState) = {

    val (_, lastHouseQDot, lastHeatStorageQDot) =
      operatingPoint.thermalOps match {
        case Some(thermalOp) =>
          (thermalOp.qDotHp, thermalOp.qDotHouse, thermalOp.qDotHeatStorage)
        case None => (zeroKW, zeroKW, zeroKW)
      }

    /* First get the energy demand of the houses but only if inner temperature is below target temperature */
    val (houseDemand, updatedHouseState) =
      house.zip(state.thermalGridState.houseState) match {
        case Some((thermalHouse, lastHouseState)) =>
          val (updatedHouseState, _) =
            thermalHouse.updateState(
              tick,
              lastHouseState,
              state.ambientTemperature,
              state.lastAmbientTemperature,
              lastHouseQDot,
            )
          if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature
          ) {
            (
              thermalHouse.energyDemand(
                state,
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

      heatStorage
        .zip(state.thermalGridState.storageState)
        .map { case (storage, heatStorageState) =>
          val (updatedStorageState, _) =
            storage.updateState(
              tick,
              state.lastThermalFlows.heatStorage,
              heatStorageState,
            )
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

  /** Update the current state of the grid. Whether there are two cases to
    * handle, external infeed into the thermal grid and no infeed.
    *
    * @param state
    *   Data of heat pump including state of the heat pump.
    * @param lastThermalGridState
    *   State of the thermalGrid until this tick.
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick.
    * @param isRunning
    *   Determines whether the heat pump is running or not.
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @param thermalDemands
    *   Holds the thermal demands of the thermal units (house, storage).
    * @return
    *   The updated state of the grid.
    */
  def updateState(
      tick: Long,
      state: HpState,
      isRunning: Boolean,
      qDot: Power,
      thermalDemands: ThermalDemandWrapper,
  ): (ThermalGridState, Option[ThermalThreshold]) = if (qDot > zeroKW)
    handleInfeed(
      tick,
      state,
      isRunning,
      qDot,
      thermalDemands,
    )
  else
    handleConsumption(
      tick,
      state,
      qDot,
    )

  /** Handles the case, when a grid has infeed. Depending on which entity has
    * some heat demand the house or the storage will be heated up / filled up.
    * First the actions from lastState will be considered and checked if the
    * behaviour should be continued. This might be the case, if we got activated
    * by updated weather data. If this is not the case, all other cases will be
    * handled by [[ThermalGrid.handleFinalInfeedCases]].
    *
    * @param state
    *   data of heat pump including state of the heat pump.
    * @param isRunning
    *   determines whether the heat pump is running or not.
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage).
    * @return
    *   Updated thermal grid state and the thermalThreshold if there is one.
    */
  private def handleInfeed(
      tick: Long,
      state: HpState,
      isRunning: Boolean,
      qDot: Power,
      thermalDemands: ThermalDemandWrapper,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // TODO: We would need to issue a storage result model here...

    /* Consider the action in the last state */
    val qDotHouseLastState =
      state.thermalGridState.houseState.map(_.qDot).getOrElse(zeroKW)
    val qDotStorageLastState =
      state.thermalGridState.storageState.map(_.qDot).getOrElse(zeroKW)

    // We can use the qDots from lastState to keep continuity. If...
    if (
      // ... house was heated in lastState but not from Storage and has still some demand. Hp must still run for this.
      ((qDotHouseLastState > zeroKW && (qDotStorageLastState >= zeroKW) && thermalDemands.houseDemand.hasAdditionalDemand) && isRunning ||
      // ... storage was filled up in the lastState and has still additional demand
      // But only if the house not reached some requiredDemand. Hp must still run for this.
      qDotStorageLastState > zeroKW && thermalDemands.heatStorageDemand.hasAdditionalDemand && !thermalDemands.houseDemand.hasRequiredDemand && isRunning)
    ) {
      // We can continue for the house
      val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
        handleInfeedHouse(tick, state, qDotHouseLastState)

      // ...and for the storage
      val (updatedStorageState, thermalStorageThreshold) = {
        // In case the ThermalHouse could not handle the infeed it will be used for the storage.
        if (remainingQDotHouse > qDotStorageLastState) {
          handleStorageCases(
            state,
            remainingQDotHouse,
          )
        } else {
          handleStorageCases(
            state,
            qDotStorageLastState,
          )
        }
      }

      val nextThreshold = determineMostRecentThreshold(
        thermalHouseThreshold,
        thermalStorageThreshold,
      )
      (
        state.thermalGridState.copy(
          houseState = updatedHouseState,
          storageState = updatedStorageState,
        ),
        nextThreshold,
      )
    }
    // Handle edge case where house was heated from storage...
    else if (qDotHouseLastState > zeroKW && qDotStorageLastState < zeroKW) {
      // ...and HP gets activated in current tick
      if (isRunning) {
        handleCases(
          tick,
          state,
          qDot,
          zeroKW,
        )
      } else {
        // ... or continue lastState's behaviour
        handleCases(tick, state, qDotHouseLastState, qDotStorageLastState)
      }
    }
    // Handle edge case where house should be heated from storage
    else if (!isRunning && qDot > zeroKW) {
      handleCases(
        tick,
        state,
        qDot,
        -qDot,
      )
    }
    // or finally check for all other cases.
    else
      handleFinalInfeedCases(tick, state, thermalDemands, qDot)
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
    * This can be simplified to four cases
    * | No | Conditions                           | Result    |
    * |:---|:-------------------------------------|:----------|
    * | 1  | if house.reqD                        | house     |
    * | 2  | else if storage.reqD OR storage.addD | storage   |
    * | 3  | else if house.addD                   | house     |
    * | 4  | else                                 | no output |
    *
    * @param state
    *   data of heat pump including state of the heat pump.
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage).
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @return
    *   Updated thermal grid state and the thermalThreshold if there is one.
    */
  private def handleFinalInfeedCases(
      tick: Long,
      state: HpState,
      thermalDemands: ThermalDemandWrapper,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {

    if (thermalDemands.houseDemand.hasRequiredDemand)
      handleCases(
        tick,
        state,
        qDot,
        zeroKW,
      )
    else if (
      thermalDemands.heatStorageDemand.hasRequiredDemand || thermalDemands.heatStorageDemand.hasAdditionalDemand
    )
      handleCases(
        tick,
        state,
        zeroKW,
        qDot,
      )
    else if (thermalDemands.houseDemand.hasAdditionalDemand)
      handleCases(
        tick,
        state,
        qDot,
        zeroKW,
      )
    else
      handleCases(tick, state, zeroKW, zeroKW)

  }

  /** Handles the different cases, of thermal flows from and into the thermal
    * grid.
    *
    * @param state
    *   data of heat pump including state of the heat pump.
    * @param qDotHouse
    *   Infeed to the house.
    * @param qDotHeatStorage
    *   Infeed to the heat storage (positive: Storage is charging, negative:
    *   Storage is discharging).
    * @return
    *   Updated thermal grid state and the next threshold if there is one.
    */
  private def handleCases(
      tick: Long,
      state: HpState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    val (updatedHouseState, thermalHouseThreshold, _) =
      handleInfeedHouse(
        tick,
        state,
        qDotHouse,
      )

    val (updatedStorageState, thermalStorageThreshold) =
      handleStorageCases(state, qDotHeatStorage)

    val nextThreshold = determineMostRecentThreshold(
      thermalHouseThreshold,
      thermalStorageThreshold,
    )

    (
      state.thermalGridState.copy(
        houseState = updatedHouseState,
        storageState = updatedStorageState,
      ),
      nextThreshold,
    )
  }

  /** Handles the case, when the house has heat demand and will be heated up
    * here.
    *
    * @param state
    *   Data of heat pump including state of the heat pump.
    * @param lastAmbientTemperature
    *   Ambient temperature until this tick.
    * @param qDotHouse
    *   Infeed into the house.
    * @return
    *   Updated thermal house state, a ThermalThreshold and the remaining qDot.
    */
  private def handleInfeedHouse(
      tick: Long,
      state: HpState,
      qDotHouse: Power,
  ): (Option[ThermalHouseState], Option[ThermalThreshold], Power) = {
    (house, state.thermalGridState.houseState) match {
      case (Some(thermalHouse), Some(lastHouseState)) =>
        val (newState, threshold) = thermalHouse.updateState(
          tick,
          lastHouseState,
          state.ambientTemperature,
          state.lastAmbientTemperature,
          qDotHouse,
        )
        /* Check if house can handle the thermal feed in */
        if (
          thermalHouse.isInnerTemperatureTooHigh(
            newState.innerTemperature
          )
        ) {
          val (fullHouseState, maybeFullHouseThreshold) =
            thermalHouse.updateState(
              tick,
              lastHouseState,
              state.ambientTemperature,
              state.lastAmbientTemperature,
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
    * here (positive qDot) or will return its stored energy into the thermal
    * grid (negative qDot).
    * @param state
    *   FIXME
    * @param qDotStorage
    *   Infeed to the storage (positive: Storage is charging, negative: Storage
    *   is discharging).
    * @return
    *   Updated thermal grid state.
    */
  private def handleStorageCases(
      state: HpState,
      qDotStorage: Power,
  ): (Option[ThermalStorageState], Option[ThermalThreshold]) = {
    (heatStorage, state.thermalGridState.storageState) match {
      case (Some(thermalStorage), Some(lastStorageState)) =>
        val (newState, threshold) = thermalStorage.updateState(
          state.tick,
          qDotStorage,
          lastStorageState,
        )
        (Some(newState), threshold)
      case _ => (None, None)
    }
  }

  /** Determines the most recent threshold of two given input thresholds.
    *
    * @param maybeHouseThreshold
    *   Option of a possible next threshold of the thermal house.
    * @param maybeStorageThreshold
    *   Option of a possible next threshold of the thermal storage.
    * @return
    *   The next threshold.
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

  /** Handle consumption (or no infeed) from thermal grid.
    *
    * @param state
    *   data of heat pump including state of the heat pump.
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @return
    *   Updated thermal grid state.
    */
  private def handleConsumption(
      tick: Long,
      state: HpState,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    /* House will be left with no influx in all cases. Determine if and when a threshold is reached */
    val maybeUpdatedHouseState =
      house.zip(state.thermalGridState.houseState).map {
        case (thermalHouse, houseState) =>
          thermalHouse.updateState(
            tick,
            houseState,
            state.ambientTemperature,
            state.lastAmbientTemperature,
            zeroMW,
          )
      }

    /* Update the state of the storage */
    val maybeUpdatedStorageState =
      heatStorage.zip(state.thermalGridState.storageState).map {
        case (storage, storageState) =>
          storage.updateState(state.tick, qDot, storageState)
      }

    val (revisedHouseState, revisedStorageState) =
      reviseInfeedFromStorage(
        tick,
        state,
        maybeUpdatedHouseState,
        maybeUpdatedStorageState,
        qDot,
      )

    val nextThreshold = determineMostRecentThreshold(
      revisedHouseState.flatMap(_._2),
      revisedStorageState.flatMap(_._2),
    )

    (
      state.thermalGridState.copy(
        houseState = revisedHouseState.map(_._1),
        storageState = revisedStorageState.map(_._1),
      ),
      nextThreshold,
    )
  }

  /** Check, if the storage can heat the house. This is only done, if <ul>
    * <li>there is no infeed from external and</li> <li>the storage is not empty
    * itself.</li> </ul>
    *
    * @param state
    *   data of heat pump including state of the heat pump.
    * @param maybeHouseState
    *   Optional thermal house state.
    * @param maybeStorageState
    *   Optional thermal storage state.
    * @param qDot
    *   Infeed to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @return
    *   Options to revised thermal house and storage state.
    */
  def reviseInfeedFromStorage(
      tick: Long,
      state: HpState,
      maybeHouseState: Option[(ThermalHouseState, Option[ThermalThreshold])],
      maybeStorageState: Option[
        (ThermalStorageState, Option[ThermalThreshold])
      ],
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
        state.tick,
        thermalStorage.getpThermalMax * -1,
        state.thermalGridState.storageState.getOrElse(
          throw new InconsistentStateException(
            "Impossible to find no storage state"
          )
        ),
      )
      val revisedHouseState = thermalHouse.updateState(
        tick,
        state.thermalGridState.houseState.getOrElse(
          throw new InconsistentStateException(
            "Impossible to find no house state"
          )
        ),
        state.ambientTemperature,
        state.lastAmbientTemperature,
        thermalStorage.getpThermalMax,
      )
      (Some(revisedHouseState), Some(revisedStorageState))
    case _ => (maybeHouseState, maybeStorageState)
  }

  /** Convert the given state of the thermal grid into result models of its
    * constituent models.
    *
    * @param state
    *   FIXME
    * @param startDateTime
    *   Start date time of the simulation.
    * @return
    *   A [[Seq]] of results of the constituent thermal model.
    */
  def results(
      state: HpState,
      currentOperatingPoint: HpOperatingPoint,
      dateTime: ZonedDateTime,
  ): Seq[ResultEntity] = {

    val (_, currentThermalGridState) =
      energyDemandAndUpdatedState(state.tick, state, currentOperatingPoint)

    // FIXME? We would only write results if there is a change, which would only be the case if there is an actual Op.
    val currentOp = currentOperatingPoint.thermalOps match {
      case Some(op) => op
      case _ =>
        throw new CriticalFailureException(
          s"There should be an OperatingPoint at this step, but there isn't one: $currentOperatingPoint"
        )
    }

    val maybeHouseResult = house
      .zip(currentThermalGridState.houseState)
      .map {
        case (
              thermalHouse,
              ThermalHouseState(_, innerTemperature, _),
            ) =>
          new ThermalHouseResult(
            dateTime,
            thermalHouse.uuid,
            currentOp.qDotHouse.toMegawatts.asMegaWatt,
            innerTemperature.toKelvinScale.asKelvin,
          )
      }

    val maybeStorageResult = heatStorage
      .zip(currentThermalGridState.storageState)
      .map {
        case (
              storage: CylindricalThermalStorage,
              ThermalStorageState(_, storedEnergy, _),
            ) =>
          new CylindricalStorageResult(
            dateTime,
            storage.uuid,
            storedEnergy.toMegawattHours.asMegaWattHour,
            currentOp.qDotHeatStorage.toMegawatts.asMegaWatt,
            (storedEnergy / storage.maxEnergyThreshold).asPu,
          )
        case _ =>
          throw new NotImplementedError(
            s"Result handling for storage type '${heatStorage.getClass.getSimpleName}' not supported."
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
      .heatStorages()
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

  /** Current state of a grid.
    * @param houseState
    *   State of the thermal house.
    * @param storageState
    *   State of the thermal storage.
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
      thermalGrid.heatStorage.map(_.startingState),
    )

  /** Wraps the demand of thermal units (thermal house, thermal storage).
    *
    * @param houseDemand
    *   The demand of the thermal house.
    * @param heatStorageDemand
    *   The demand of the thermal heat storage.
    */
  final case class ThermalDemandWrapper private (
      houseDemand: ThermalEnergyDemand,
      heatStorageDemand: ThermalEnergyDemand,
  )

  // FIXME
  /** Wraps the demand of thermal units (thermal house, thermal storage).
    *
    * @param houseDemand
    *   The demand of the thermal house.
    * @param heatStorageDemand
    *   The demand of the thermal heat storage.
    */
  final case class ThermalFlowWrapper private (
      house: Power,
      heatStorage: Power,
  )

  /** Defines the thermal energy demand of a thermal grid. It comprises the
    * absolutely required energy demand to reach the target state as well as an
    * energy, that can be handled. The possible energy always has to be greater
    * than or equal to the absolutely required energy. Thus, this class can only
    * be instantiated via factory.
    * @param required
    *   The absolutely required energy to reach target state. For
    *   [[ThermalHouse]] this would be the energy demand to reach the boundary
    *   or targetTemperature. For [[ThermalStorage]] this would be the amount of
    *   Energy to get fully charged when empty. If the [[ThermalStorage]] is not
    *   empty, the required energy is zero.
    * @param possible
    *   The maximum possible energy, that can be handled.
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
      *   The absolutely required energy to reach target state.
      * @param possible
      *   The maximum possible energy, that can be handled.
      * @return
      *   Thermal energy demand container class, that meets all specifications.
      */
    def apply(
        required: Energy,
        possible: Energy,
    ): ThermalEnergyDemand = {
      if (
        math.abs(possible.toKilowattHours) < math.abs(required.toKilowattHours)
      )
        throw new InvalidParameterException(
          s"The possible amount of energy $possible is smaller than the required amount of energy $required. This is not supported."
        )

      if (possible.toKilowattHours < 0 || required.toKilowattHours < 0)
        throw new InvalidParameterException(
          s"The possible $possible or required $required amount of energy cannot be negative. This is not supported."
        )

      new ThermalEnergyDemand(required, possible)
    }

    def noDemand: ThermalEnergyDemand = ThermalEnergyDemand(
      zeroMWh,
      zeroMWh,
    )
  }
}
