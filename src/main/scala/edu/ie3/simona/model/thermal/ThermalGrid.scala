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
import squants.energy.Kilowatts
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
    * @param tick
    *   Questioned instance in time
    * @param lastAmbientTemperature
    *   Ambient temperature until this tick
    * @param ambientTemperature
    *   Current ambient temperature in the instance in question
    * @param state
    *   Currently applicable state of the thermal grid
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
  ): (ThermalDemandWrapper, ThermalGridState) = {
    /* First get the energy demand of the houses but only if inner temperature is below target temperature */

    val (houseDemand, updatedHouseState) =
      house.zip(state.houseState) match {
        case Some((thermalHouse, lastHouseState)) =>
          val (updatedHouseState, _) =
            thermalHouse.determineState(
              tick,
              lastHouseState,
              lastAmbientTemperature,
              ambientTemperature,
              lastHouseState.qDot,
            )
          if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature |
              (lastHouseState.qDot > zeroKW && updatedHouseState.innerTemperature < thermalHouse.upperBoundaryTemperature)
          ) {
            (
              thermalHouse.energyDemand(
                tick,
                ambientTemperature,
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
        .zip(state.storageState)
        .map { case (storage, state) =>
          val (updatedStorageState, _) =
            storage.updateState(tick, state.qDot, state)
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
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage)
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
      thermalDemands: ThermalDemandWrapper,
  ): (ThermalGridState, Option[ThermalThreshold]) = if (qDot > zeroKW)
    handleInfeed(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      isRunning,
      qDot,
      thermalDemands,
    )
  else
    handleConsumption(
      tick,
      lastAmbientTemperature,
      ambientTemperature,
      state,
      qDot,
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
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage)
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
      thermalDemands: ThermalDemandWrapper,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // TODO: We would need to issue a storage result model here...

    /* Consider the action in the last state */
    val (qDotHouseLastState, qDotStorageLastState) = state match {
      case ThermalGridState(Some(houseState), Some(storageState)) =>
        (houseState.qDot, storageState.qDot)
      case ThermalGridState(Some(houseState), None) => (houseState.qDot, zeroKW)
      case ThermalGridState(None, Some(storageState)) =>
        (zeroKW, storageState.qDot)
      case _ =>
        throw new InconsistentStateException(
          "There should be at least a house or a storage state."
        )
    }

    if (
      (qDotHouseLastState > zeroKW && (qDotStorageLastState >= zeroKW)) | (qDotStorageLastState > zeroKW && thermalDemands.heatStorageDemand.hasAdditionalDemand)
    ) {
      val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
        handleInfeedHouse(
          tick,
          lastAmbientTemperature,
          ambientTemperature,
          state,
          qDotHouseLastState,
        )
      val (updatedStorageState, thermalStorageThreshold) =
        if (
          qDotStorageLastState >= zeroKW && remainingQDotHouse > qDotStorageLastState
        ) {
          handleInfeedStorage(
            tick,
            state,
            remainingQDotHouse,
          )
        } else {
          handleInfeedStorage(
            tick,
            state,
            qDotStorageLastState,
          )
        }

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
    // Handle edge case where house was heated from storage and HP will be activated in between
    else if ((qDotHouseLastState > zeroKW && qDotStorageLastState < zeroKW)) {
      if (isRunning) {
        handleCases(
          tick,
          lastAmbientTemperature,
          ambientTemperature,
          state,
          qDot,
          zeroKW,
        )
      } else {

        handleCases(
          tick,
          lastAmbientTemperature,
          ambientTemperature,
          state,
          qDotHouseLastState,
          qDotStorageLastState,
        )
      }
    }
    // Handle edge case where house should be heated from storage
    else if ((!isRunning && qDot > zeroKW)) {
      handleCases(
        tick,
        lastAmbientTemperature,
        ambientTemperature,
        state,
        qDot,
        -qDot,
      )
    } else {
      (
        thermalDemands.houseDemand.hasRequiredDemand,
        thermalDemands.houseDemand.hasAdditionalDemand,
        thermalDemands.heatStorageDemand.hasAdditionalDemand,
      ) match {

        case (true, _, _) =>
          // house first then heatStorage after heating House
          handleCases(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            qDot,
            zeroKW,
          )

        case (false, _, true) =>
          handleCases(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            zeroKW,
            qDot,
          )

        case (false, true, false) =>
          handleCases(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            qDot,
            zeroKW,
          )

        case (false, false, false) =>
          handleCases(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            zeroKW,
            zeroKW,
          )
        case _ =>
          throw new InconsistentStateException(
            "There should be at least a house or a storage state."
          )
      }
    }
  }

  /** Handles the different cases, of thermal flows from and into the thermal
    * grid.
    *
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature until this tick
    * @param ambientTemperature
    *   actual ambient temperature
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
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    val (updatedHouseState, thermalHouseThreshold, _) =
      handleInfeedHouse(
        tick,
        lastAmbientTemperature,
        ambientTemperature,
        state,
        qDotHouse,
      )

    val (updatedStorageState, thermalStorageThreshold) =
      handleInfeedStorage(tick, state, qDotHeatStorage)

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
    * @param tick
    *   Current tick
    * @param lastAmbientTemperature
    *   Ambient temperature until this tick
    * @param ambientTemperature
    *   actual ambient temperature
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
    *   Updated thermal grid state
    */
  private def handleConsumption(
      tick: Long,
      lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
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
      storage.zip(state.storageState).map { case (storage, storageState) =>
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

    val nextThreshold = determineMostRecentThreshold(
      revisedHouseState.flatMap(_._2),
      revisedStorageState.flatMap(_._2),
    )

    (
      state.copy(
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
  )

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

    def hasAdditionalDemand: Boolean = possible > zeroMWh
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
      zeroMWh,
      zeroMWh,
    )
  }
}
