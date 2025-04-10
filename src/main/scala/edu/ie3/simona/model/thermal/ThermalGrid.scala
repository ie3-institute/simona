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
import edu.ie3.simona.model.participant2.HpModel.{
  HpOperatingPoint,
  HpState,
  ThermalGridOperatingPoint,
}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.KilowattHours
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

  /** Updates the state of the ThermalGrid by using the actual HpOperatingPoint.
    * @param tick
    *   The actual tick of simulation.
    * @param state
    *   Last state of the heat pump.
    * @param operatingPoint
    *   The operating point of the heat pump.
    * @return
    *   an updated [[ThermalGridState]].
    */
  def updateThermalGridState(
      tick: Long,
      state: HpState,
      operatingPoint: HpOperatingPoint,
  ): ThermalGridState = {
    val houseQDot = operatingPoint.thermalOps.qDotHouse
    val heatStorageQDot = operatingPoint.thermalOps.qDotHeatStorage

    val updatedHouseState: Option[ThermalHouseState] =
      house.zip(state.thermalGridState.houseState) match {
        case Some((thermalHouse, houseState)) =>
          Some(
            thermalHouse
              .determineState(
                tick,
                houseState,
                houseQDot,
              )
          )
        case _ => None
      }

    val updatedStorageState: Option[ThermalStorageState] = {
      heatStorage.zip(state.thermalGridState.storageState) match {
        case Some((storage, heatStorageState)) =>
          Some(
            storage.determineState(
              tick,
              heatStorageState,
              heatStorageQDot,
            )
          )
        case _ => None
      }
    }

    ThermalGridState(updatedHouseState, updatedStorageState)
  }

  /** Determine the energy demand of the thermalGrid.
    * @param thermalGridState
    *   Last state of the thermal grid.
    * @return
    *   The energy demand of elements of thermalGrid.
    */

  def determineEnergyDemand(
      thermalGridState: ThermalGridState
  ): ThermalDemandWrapper = {

    val houseDemand = house.zip(thermalGridState.houseState) match {
      case Some((thermalHouse, houseState)) =>
        if (houseState.innerTemperature < thermalHouse.targetTemperature) {
          thermalHouse.energyDemand(houseState)
        } else {
          ThermalEnergyDemand.noDemand
        }
      case None => ThermalEnergyDemand.noDemand
    }

    val storageDemand = heatStorage.zip(thermalGridState.storageState) match {
      case Some((storage, storageState)) =>
        val storedEnergy = storageState.storedEnergy
        val storageRequired = {
          if (storedEnergy == zeroKWh)
            storage.getMaxEnergyThreshold
          else
            zeroMWh
        }

        val storagePossible = storage.getMaxEnergyThreshold - storedEnergy
        ThermalEnergyDemand(
          storageRequired,
          storagePossible,
        )
      case None => ThermalEnergyDemand.noDemand
    }

    ThermalDemandWrapper(
      ThermalEnergyDemand(
        houseDemand.required,
        houseDemand.possible,
      ),
      ThermalEnergyDemand(
        storageDemand.required,
        storageDemand.possible,
      ),
    )
  }

  /** Handles the case, when a grid has feed in. Depending on which entity has
    * some heat demand the house or the storage will be heated up / filled up.
    * First the actions from lastState will be considered and checked if the
    * behaviour should be continued. This might be the case, if we got activated
    * by updated weather data. If this is not the case, all other cases will be
    * handled by [[ThermalGrid.handleFinalFeedInCases]].
    *
    * @param state
    *   Last state of the heat pump.
    * @param isRunning
    *   determines whether the heat pump is running or not.
    * @param qDot
    *   Feed in to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage).
    * @return
    *   Updated thermal grid state and the thermalThreshold if there is one.
    */
  def handleFeedIn(
      state: HpState,
      isRunning: Boolean,
      qDot: Power,
      thermalDemands: ThermalDemandWrapper,
  ): (ThermalGridState, Option[ThermalThreshold], ThermalGridOperatingPoint) = {
    // TODO: We would need to issue a storage result model here...

    /* Consider the action in the last state */
    val lastHouseQDot = state.lastHpOperatingPoint.thermalOps.qDotHouse
    val lastHeatStorageQDot =
      state.lastHpOperatingPoint.thermalOps.qDotHeatStorage

    // We can use the qDots from lastState to keep continuity. If...
    if (
      // ... house was heated in lastState but not from Storage and has still some demand. Hp must still run for this.
      lastHouseQDot > zeroKW && lastHeatStorageQDot >= zeroKW && thermalDemands.houseDemand.hasPossibleDemand && isRunning ||
      // ... storage was filled up in the lastState and has still possible demand
      // But only if the house not reached some requiredDemand. Hp must still run for this.
      lastHeatStorageQDot > zeroKW && thermalDemands.heatStorageDemand.hasPossibleDemand && !thermalDemands.houseDemand.hasRequiredDemand && isRunning
    ) {
      // We can continue for the house
      val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
        handleFeedInHouse(state, lastHouseQDot)

      // ...and for the storage
      val (updatedStorageState, thermalStorageThreshold) = {
        // In case the ThermalHouse could not handle the feed in it will be used for the storage.
        if (remainingQDotHouse > lastHeatStorageQDot) {
          handleStorageCases(state, remainingQDotHouse)
        } else {
          handleStorageCases(state, lastHeatStorageQDot)
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
        ThermalGridOperatingPoint(
          lastHouseQDot + lastHeatStorageQDot,
          lastHouseQDot,
          lastHeatStorageQDot,
        ),
      )
    }
    // Handle edge case where house was heated from storage...
    else if (lastHouseQDot > zeroKW && lastHeatStorageQDot < zeroKW) {
      // ...and HP gets activated in current tick
      if (isRunning) {
        handleCases(state, qDot, zeroKW)
      } else {
        // ... or continue lastState's behaviour
        handleCases(state, lastHouseQDot, lastHeatStorageQDot)
      }
    }
    // Handle edge case where house should be heated from storage
    // FIXME Is there a case where we got here?
    else if (!isRunning && qDot > zeroKW) {
      handleCases(state, qDot, -qDot)
    }
    // or finally check for all other cases.
    else
      handleFinalFeedInCases(state, thermalDemands, qDot)
  }

  /** Handles the last cases of [[ThermalGrid.handleFeedIn]], where the thermal
    * feed in should be determined.
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
    *   Last state of the heat pump.
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, storage).
    * @param qDot
    *   Feed in to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @return
    *   Updated thermal grid state and the thermalThreshold if there is one.
    */
  private def handleFinalFeedInCases(
      state: HpState,
      thermalDemands: ThermalDemandWrapper,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold], ThermalGridOperatingPoint) = {

    if (thermalDemands.houseDemand.hasRequiredDemand)
      handleCases(state, qDot, zeroKW)
    else if (
      thermalDemands.heatStorageDemand.hasRequiredDemand || thermalDemands.heatStorageDemand.hasPossibleDemand
    )
      handleCases(state, zeroKW, qDot)
    else if (thermalDemands.houseDemand.hasPossibleDemand)
      handleCases(state, qDot, zeroKW)
    else
      handleCases(state, zeroKW, zeroKW)
  }

  /** Handles the different cases, of thermal flows from and into the thermal
    * grid.
    *
    * @param state
    *   Last state of the heat pump.
    * @param qDotHouse
    *   Feed in to the house.
    * @param qDotHeatStorage
    *   Feed in to the heat storage (positive: Storage is charging, negative:
    *   Storage is discharging).
    * @return
    *   Updated thermal grid state and the next threshold if there is one.
    */
  private def handleCases(
      state: HpState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
  ): (ThermalGridState, Option[ThermalThreshold], ThermalGridOperatingPoint) = {
    val (updatedHouseState, thermalHouseThreshold, _) =
      handleFeedInHouse(state, qDotHouse)

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
      ThermalGridOperatingPoint(
        qDotHouse + qDotHeatStorage,
        qDotHouse,
        qDotHeatStorage,
      ),
    )
  }

  /** Handles the case, when the house has heat demand and will be heated up
    * here.
    *
    * @param state
    *   Last state of the heat pump.
    * @param qDotHouse
    *   Feed in into the house.
    * @return
    *   Updated thermal house state, a ThermalThreshold and the thermal feed in
    *   into the thermal house.
    */
  private def handleFeedInHouse(
      state: HpState,
      qDotHouse: Power,
  ): (
      Option[ThermalHouseState],
      Option[ThermalThreshold],
      Power,
  ) = {
    (house, state.thermalGridState.houseState) match {
      case (Some(thermalHouse), Some(lastHouseState)) =>
        val newState = thermalHouse.determineState(
          state.tick,
          lastHouseState,
          qDotHouse,
        )
        /* Check if house can handle the thermal feed in */
        if (
          thermalHouse.isInnerTemperatureTooHigh(
            newState.innerTemperature
          )
        ) {
          val fullHouseState =
            thermalHouse.determineState(
              state.tick,
              lastHouseState,
              zeroKW,
            )

          val maybeFullHouseThreshold =
            thermalHouse.determineNextThreshold(newState, zeroKW)

          (Some(fullHouseState), maybeFullHouseThreshold, qDotHouse)

        } else {
          val threshold = thermalHouse.determineNextThreshold(
            newState,
            qDotHouse,
          )
          (Some(newState), threshold, zeroKW)
        }
      case _ => (None, None, zeroKW)
    }
  }

  /** Handles the cases, when the storage has heat demand and will be filled up
    * here (positive qDot) or will return its stored energy into the thermal
    * grid (negative qDot).
    *
    * @param state
    *   Last state of the heat pump.
    * @param qDotStorage
    *   Feed in to the storage (positive: Storage is charging, negative: Storage
    *   is discharging).
    * @return
    *   Updated thermal grid state. Updated thermal storage state amd the
    *   ThermalThreshold.
    */
  private def handleStorageCases(
      state: HpState,
      qDotStorage: Power,
  ): (Option[ThermalStorageState], Option[ThermalThreshold]) = {
    (heatStorage, state.thermalGridState.storageState) match {
      case (Some(thermalStorage), Some(lastStorageState)) =>
        val newState = thermalStorage.determineState(
          state.tick,
          lastStorageState,
          qDotStorage,
        )
        val threshold = thermalStorage.determineNextThreshold(
          newState,
          qDotStorage,
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

  /** Handle consumption (or no feed in) from thermal grid.
    *
    * @param state
    *   Last state of the heat pump.
    * @return
    *   Updated thermal grid state, the ThermalThreshold, and the
    *   ThermalGridOperatingPoint.
    */
  def handleConsumption(
      state: HpState
  ): (ThermalGridState, Option[ThermalThreshold], ThermalGridOperatingPoint) = {
    /* House will be left with no influx in all cases. Determine if and when a threshold is reached */
    val maybeUpdatedHouseState =
      house.zip(state.thermalGridState.houseState).map {
        case (thermalHouse, houseState) =>
          thermalHouse.determineState(
            state.tick,
            houseState,
            zeroKW,
          )
      }

    /* Update the state of the storage */
    val maybeUpdatedStorageState =
      heatStorage.zip(state.thermalGridState.storageState).map {
        case (storage, storageState) =>
          storage.determineState(state.tick, storageState, zeroKW)
      }

    val (
      revisedHouseState,
      revisedQDotHouse,
      revisedStorageState,
      revisedQDotStorage,
    ) =
      reviseFeedInFromStorage(
        state,
        maybeUpdatedHouseState,
        maybeUpdatedStorageState,
      )

    val revisedThermalGridOperatingPoint = ThermalGridOperatingPoint(
      zeroKW,
      revisedQDotHouse,
      revisedQDotStorage,
    )

    val revisedHouseThreshold =
      house.zip(revisedHouseState).flatMap { case (thermalHouse, houseState) =>
        thermalHouse
          .determineNextThreshold(
            houseState,
            revisedThermalGridOperatingPoint.qDotHouse,
          )
      }

    val revisedStorageThreshold =
      heatStorage.zip(revisedStorageState).flatMap {
        case (thermalStorage: ThermalStorage, storageState) =>
          thermalStorage
            .determineNextThreshold(
              storageState,
              revisedThermalGridOperatingPoint.qDotHeatStorage,
            )
      }

    val nextThreshold = determineMostRecentThreshold(
      revisedHouseThreshold,
      revisedStorageThreshold,
    )

    (
      state.thermalGridState.copy(
        houseState = revisedHouseState,
        storageState = revisedStorageState,
      ),
      nextThreshold,
      revisedThermalGridOperatingPoint,
    )
  }

  /** Check, if the storage can heat the house. This is only done, if <ul>
    * <li>the house has reached it's lower temperature boundary,</li> <li>there
    * is no feed in from external and</li> <li>the storage is not empty
    * itself</li> </ul>
    *
    * @param state
    *   Last state of the heat pump.
    * @param maybeHouseState
    *   Optional thermal house state.
    * @param maybeStorageState
    *   Optional thermal storage state.
    * @return
    *   Options to revised thermal house and storage state and the updated qDot
    *   of house and storage.
    */
  def reviseFeedInFromStorage(
      state: HpState,
      maybeHouseState: Option[ThermalHouseState],
      maybeStorageState: Option[ThermalStorageState],
  ): (
      Option[ThermalHouseState],
      Power,
      Option[ThermalStorageState],
      Power,
  ) = house.zip(maybeHouseState).zip(heatStorage.zip(maybeStorageState)) match {
    case Some(
          (
            (thermalHouse, houseState),
            (thermalStorage, storageState),
          )
        )
        if thermalHouse.isInnerTemperatureTooLow(
          houseState.innerTemperature
        ) && !thermalStorage.isEmpty(storageState.storedEnergy) =>
      /* Storage is meant to heat the house only, if there is no feed in from external and the house is cold */
      val updatedQDotHouse = thermalStorage.getpThermalMax

      val revisedHouseState = Some(
        thermalHouse.determineState(
          state.tick,
          houseState,
          updatedQDotHouse,
        )
      )

      val revisedStorageState = Some(
        thermalStorage.determineState(
          state.tick,
          storageState,
          updatedQDotHouse * -1,
        )
      )

      (
        revisedHouseState,
        updatedQDotHouse,
        revisedStorageState,
        updatedQDotHouse * -1,
      )
    case _ => (maybeHouseState, zeroKW, maybeStorageState, zeroKW)
  }

  /** Convert the given state of the thermal grid into result models of its
    * constituent models.
    *
    * @param state
    *   Last state of the heat pump.
    * @param lastOperatingPoint
    *   The last operating point of the heat pump.
    * @param currentOperatingPoint
    *   The actual operating point of the heat pump.
    * @param dateTime
    *   The actual date and time of the actual simulation tick.
    * @return
    *   A [[Seq]] of results of the constituent thermal model.
    */
  def results(
      state: HpState,
      lastOperatingPoint: Option[HpOperatingPoint],
      currentOperatingPoint: HpOperatingPoint,
      dateTime: ZonedDateTime,
  ): Seq[ResultEntity] = {
    val currentOpThermals = currentOperatingPoint.thermalOps

    val lastOpThermals = lastOperatingPoint.map(_.thermalOps)

    def createThermalHouseResult(
        thermalHouse: ThermalHouse
    ): Option[ThermalHouseResult] = {
      state.thermalGridState.houseState
        .collectFirst { case ThermalHouseState(_, _, innerTemperature) =>
          new ThermalHouseResult(
            dateTime,
            thermalHouse.uuid,
            currentOpThermals.qDotHouse.toMegawatts.asMegaWatt,
            innerTemperature.toKelvinScale.asKelvin,
          )
        }
        .orElse(
          throw new NotImplementedError(
            s"Result handling for thermalHouse type '${thermalHouse.getClass.getSimpleName}' not supported."
          )
        )
    }

    def createCylindricalStorageResult(
        storage: CylindricalThermalStorage
    ): Option[CylindricalStorageResult] = {
      state.thermalGridState.storageState
        .collectFirst { case ThermalStorageState(_, storedEnergy) =>
          new CylindricalStorageResult(
            dateTime,
            storage.uuid,
            storedEnergy.toMegawattHours.asMegaWattHour,
            currentOpThermals.qDotHeatStorage.toMegawatts.asMegaWatt,
            (storedEnergy / storage.maxEnergyThreshold).asPu,
          )
        }
        .orElse(
          throw new NotImplementedError(
            s"Result handling for storage type '${storage.getClass.getSimpleName}' not supported."
          )
        )
    }

    val maybeHouseResult = {
      (
        house,
        lastOpThermals.forall(_.qDotHouse != currentOpThermals.qDotHouse),
        state.tick != 0,
      ) match {
        case (Some(house: ThermalHouse), true, true) =>
          createThermalHouseResult(house)
        // We always want the results of the first tick
        case (Some(house: ThermalHouse), _, false) =>
          createThermalHouseResult(house)
        case _ => None
      }
    }

    val maybeStorageResult = {
      (
        heatStorage,
        lastOpThermals.forall(
          _.qDotHeatStorage != currentOpThermals.qDotHeatStorage
        ),
        state.tick != 0,
      ) match {
        case (Some(storage: CylindricalThermalStorage), true, true) =>
          createCylindricalStorageResult(storage)
        // We always want the results of the first tick
        case (Some(storage: CylindricalThermalStorage), _, false) =>
          createCylindricalStorageResult(storage)
        case _ => None
      }
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

  def startingState(
      thermalGrid: ThermalGrid,
      ambientTemperature: Temperature,
  ): ThermalGridState =
    ThermalGridState(
      thermalGrid.house.map(house =>
        ThermalHouse.startingState(house, ambientTemperature)
      ),
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

    def hasPossibleDemand: Boolean = possible > zeroMWh
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
