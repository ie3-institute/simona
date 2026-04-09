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
import edu.ie3.simona.model.participant.hp.HpModel.{
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
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.quantities.QuantityUtils.{
  asKelvin,
  asMegaWatt,
  asMegaWattHour,
  asPu,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import edu.ie3.util.scala.quantities.QuantityUtil.*
import squants.energy.KilowattHours
import squants.{Energy, Power, Seconds, Temperature}

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
  * @param domesticHotWaterStorage
  *   Storages for domestic hot water / tap water connected to the bus.
  */
final case class ThermalGrid(
    house: Option[ThermalHouse],
    heatStorage: Option[CylindricalThermalStorage],
    domesticHotWaterStorage: Option[DomesticHotWaterStorage],
) extends LazyLogging {

  /** Determines the state of the ThermalGrid by using the HpOperatingPoint.
    *
    * @param tick
    *   The current tick of simulation.
    * @param lastState
    *   Last state of the thermal grid.
    * @param operatingPoint
    *   The operating point of the heat pump.
    * @return
    *   The updated [[ThermalGridState]].
    */
  def determineState(
      tick: Long,
      lastState: ThermalGridState,
      operatingPoint: HpOperatingPoint,
  ): ThermalGridState = {
    val houseQDot = operatingPoint.thermalOps.qDotHouse
    val heatStorageQDot = operatingPoint.thermalOps.qDotHeatStorage
    val waterStorageQDot = operatingPoint.thermalOps.qDotDomesticHotWaterStorage

    val updatedHouseState = house.zip(lastState.houseState) match {
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

    val updatedHeatStorageState =
      heatStorage.zip(lastState.heatStorageState) match {
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

    val updatedDomesticHotWaterStorageState = domesticHotWaterStorage
      .zip(lastState.domesticHotWaterStorageState)
      .map { case (storage, waterStorageState) =>
        storage.determineState(
          tick,
          waterStorageState,
          waterStorageQDot,
        )
      }

    ThermalGridState(
      updatedHouseState,
      updatedHeatStorageState,
      updatedDomesticHotWaterStorageState,
    )
  }

  /* ENERGY DEMAND */

  /** Determine the energy demand of the thermalGrid.
    *
    * @param thermalGridState
    *   Last state of the thermal grid.
    * @param hoursWaterDemandToDetermine
    *   The hours of which the energy demand for domestic hot water will have to
    *   be determined.
    * @return
    *   The energy demand of elements of thermalGrid.
    */
  def determineEnergyDemand(
      thermalGridState: ThermalGridState,
      hoursWaterDemandToDetermine: Option[Seq[Int]],
  ): ThermalDemandWrapper = {

    val (houseDemandHeating, houseDemandWater) =
      calculateHouseDemand(thermalGridState, hoursWaterDemandToDetermine)
    val domesticHotWaterStorageDemand = calculateDomesticStorageDemand(
      thermalGridState
    )
    val heatStorageDemand = calculateHeatStorageDemand(thermalGridState)

    ThermalDemandWrapper(
      houseDemandHeating,
      heatStorageDemand,
      houseDemandWater,
      domesticHotWaterStorageDemand,
    )
  }

  /** Determine the energy demand for heating and the water demand of the house.
    *
    * @param thermalGridState
    *   Last state of the thermal grid.
    * @param hoursWaterDemandToDetermine
    *   The hours of which the energy demand for domestic hot water will have to
    *   be determined.
    * @return
    *   The energy and water demand of the house.
    */
  private def calculateHouseDemand(
      thermalGridState: ThermalGridState,
      hoursWaterDemandToDetermine: Option[Seq[Int]],
  ): (ThermalEnergyDemand, ThermalEnergyDemand) = {
    house.zip(thermalGridState.houseState) match {
      case Some((thermalHouse, houseState)) =>
        // Calculate domestic hot water demand
        val domesticHotWaterDemand =
          thermalHouse.energyDemandDomesticHotWater(
            hoursWaterDemandToDetermine
          )
        // Calculate heating demand of house
        val heatingDemand = {
          if houseState.innerTemperature < thermalHouse.targetTemperature
          then {
            thermalHouse.energyDemandHeating(houseState)
          } else {
            ThermalEnergyDemand.noDemand
          }
        }
        (heatingDemand, domesticHotWaterDemand)

      case None =>
        (ThermalEnergyDemand.noDemand, ThermalEnergyDemand.noDemand)
    }
  }

  /** Determine the energy demand of the HeatStorage.
    *
    * @param thermalGridState
    *   Last state of the thermal grid.
    * @return
    *   The energy demand of the HeatStorage.
    */
  private def calculateHeatStorageDemand(
      thermalGridState: ThermalGridState
  ): ThermalEnergyDemand = {
    heatStorage.zip(thermalGridState.heatStorageState) match {
      case Some((storage, storageState)) =>
        val storedEnergy = storageState.storedEnergy
        val storageRequired = {
          if storedEnergy == zeroKWh then storage.getMaxEnergyThreshold
          else zeroMWh
        }

        val storagePossible = storage.getMaxEnergyThreshold - storedEnergy
        ThermalEnergyDemand(
          storageRequired,
          storagePossible,
        )
      case None => ThermalEnergyDemand.noDemand
    }
  }

  /** Determine the energy demand of the DomesticHotWaterStorage.
    *
    * @param thermalGridState
    *   Last state of the thermal grid.
    * @return
    *   The energy demand of the domestic hot water storage.
    */
  private def calculateDomesticStorageDemand(
      thermalGridState: ThermalGridState
  ): ThermalEnergyDemand = {
    domesticHotWaterStorage.zip(
      thermalGridState.domesticHotWaterStorageState
    ) match {
      case Some((storage, storageState)) =>
        val storedEnergy = storageState.storedEnergy
        val storageRequired = {
          if storedEnergy == zeroKWh then storage.getMaxEnergyThreshold
          else zeroMWh
        }

        val storagePossible = storage.getMaxEnergyThreshold - storedEnergy
        ThermalEnergyDemand(
          storageRequired,
          storagePossible,
        )
      case None => ThermalEnergyDemand.noDemand
    }
  }

  /* OPERATING POINT */

  /** Handles the case, when a grid has feed in. To do so, first the conditions
    * of all grid elements are evaluated if there is demand for heating. Based
    * on these, the distribution strategy for the heating power (qDot) is
    * chosen.
    *
    * @param state
    *   State of the heat pump.
    * @param qDot
    *   Feed in to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @return
    *   The operating point of the thermal grid and the thermalThreshold if
    *   there is one.
    */
  def handleFeedIn(
      state: HpState,
      qDot: Power,
  ): (ThermalGridOperatingPoint, Option[ThermalThreshold]) = {
    // TODO: We would need to issue a storage result model here...
    val conditions = ThermalDemandConditions.from(state)
    val strategy = selectFeedInStrategy(conditions)
    val (qDotHouse, qDotHeatStorage, qDotWaterStorage) =
      strategy(qDot, heatStorage, domesticHotWaterStorage)

    val operatingPoint =
      handleFeedInCase(state, qDotHouse, qDotHeatStorage, qDotWaterStorage)
    val nextThreshold = getThreshold(state, operatingPoint)

    (operatingPoint, nextThreshold)
  }

  /** Selects the strategy how to distribute the thermal power (qDot) from the
    * heat source to the elements within this ThermalGrid.
    *
    * | No | Conditions                                   | Result                          |
    * |:---|:---------------------------------------------|:--------------------------------|
    * | 1  | if house.reqD AND waterStorage.reqD          | split to house and waterStorage |
    * | 2  | else if house.reqD                           | house                           |
    * | 3  | else if waterStorage.reqD                    | waterStorage                    |
    * | 4  | else if heatStorage.reqD OR heatStorage.posD | heatStorage                     |
    * | 5  | else if waterStorage.posD                    | waterStorage                    |
    * | 6  | else if house.posD                           | house                           |
    * | 7  | else                                         | no output                       |
    *
    * @param conditions
    *   The ThermalDemandConditions, describing the current status of heat
    *   demand of the grid elements.
    * @return
    *   The FeedInStrategy how to distribute the qDot from the heat source.
    */
  private def selectFeedInStrategy(
      conditions: ThermalDemandConditions
  ): FeedInStrategy = {
    if conditions.shouldContinueHouseHeating then {
      HouseOnlyStrategy
    } else if conditions.waterStorageDemand &&
      (conditions.houseDemand || conditions.houseHeatedLastState)
    then {
      SplitHouseWaterStrategy
    } else if conditions.houseDemand then {
      HouseOnlyStrategy
    } else if conditions.waterStorageDemand then {
      WaterStorageFirstStrategy
    } else if conditions.heatStorageDemand then {
      HeatStorageFirstStrategy
    } else if conditions.housePossible then {
      HouseOnlyStrategy
    } else {
      NoOperationStrategy
    }
  }

  /** Handles the different thermal flows from and into the thermal grid.
    *
    * @param state
    *   State of the heat pump.
    * @param qDotHouse
    *   Feed in to the house.
    * @param qDotHeatStorage
    *   Feed in to the heat storage (positive: Storage is charging, negative:
    *   Storage is discharging).
    * @param qDotDomesticHotWaterStorage
    *   In-feed to the domestic hot water storage.
    * @return
    *   The operating point of the thermal grid.
    */
  private def handleFeedInCase(
      state: HpState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
      qDotDomesticHotWaterStorage: Power,
  ): ThermalGridOperatingPoint = {
    // Handle domestic hot water demand
    val resultingQDotHotWaterStorage =
      // There only can be consumption, if there isn't feed in into the storage.
      if qDotDomesticHotWaterStorage == zeroKW then
        handleHotWaterConsumption(state)
      else qDotDomesticHotWaterStorage

    ThermalGridOperatingPoint(
      qDotHouse + qDotHeatStorage + qDotDomesticHotWaterStorage,
      qDotHouse,
      qDotHeatStorage,
      resultingQDotHotWaterStorage,
    )
  }

  /** Handle consumption (or no feed in) from thermal grid.
    *
    * @param state
    *   State of the heat pump.
    * @return
    *   The operating point of the thermal grid and the ThermalThreshold if
    *   there is one.
    */
  def handleConsumption(
      state: HpState
  ): (ThermalGridOperatingPoint, Option[ThermalThreshold]) = {
    /* Check if house can be heated from storage */
    val operatingPoint =
      maybeReviseFeedInFromStorage(state)
        /* House will be left with no influx in all cases */
        .getOrElse(ThermalGridOperatingPoint.zero)

    // handle hot water demand
    val qDotHotWaterStorage = handleHotWaterConsumption(state)
    val adaptedOperatingPoint =
      operatingPoint.copy(qDotDomesticHotWaterStorage = qDotHotWaterStorage)

    val nextThreshold = getThreshold(state, adaptedOperatingPoint)

    (adaptedOperatingPoint, nextThreshold)
  }

  /** returns negative or zero qdot */
  private def handleHotWaterConsumption(
      state: HpState
  ): Power =
    getHotWaterEnergyDemand(state)
      .map { domesticHotWaterDemand =>
        val minimumOperationDuration = Seconds(1)

        if domesticHotWaterDemand.required > zeroKWh then {
          val chargingPower = domesticHotWaterStorage
            .map(_.getpThermalMax)
            .getOrElse(
              throw new RuntimeException(
                s"Trying to get the chargingPower of domesticHotWaterStorage was not possible"
              )
            )

          val approxDurationAtFullPower =
            domesticHotWaterDemand.required / chargingPower

          if approxDurationAtFullPower > minimumOperationDuration then {
            -1 * domesticHotWaterDemand.required / Seconds(
              approxDurationAtFullPower.toSeconds.ceil
            )
          } else {
            -1 * domesticHotWaterDemand.required / minimumOperationDuration
          }
        } else zeroKW
      }
      .getOrElse(zeroKW)

  private def getHotWaterEnergyDemand(
      state: HpState
  ): Option[ThermalEnergyDemand] = {
    domesticHotWaterStorage
      .zip(
        state.thermalGridState.domesticHotWaterStorageState
      )
      .map { case (_, storageState) =>
        // Check if storage can handle the demand
        val domesticHotWaterDemand =
          state.thermalDemands.domesticWaterDemandOfHouse

        if storageState.storedEnergy < domesticHotWaterDemand.required then
          // if it can't, take max qDot that empties the storage asap
          ThermalEnergyDemand(
            storageState.storedEnergy,
            storageState.storedEnergy,
          )
        else domesticHotWaterDemand
      }
  }

  /** Check, if the storage can heat the house. This is only done, if <ul>
    * <li>the house has reached it's lower temperature boundary,</li> <li>there
    * is no feed in from external and</li> <li>the storage is not empty
    * itself</li> </ul>.
    *
    * @param state
    *   State of the heat pump.
    * @return
    *   Operating point of the thermal grid, if house is heated from storage.
    */
  def maybeReviseFeedInFromStorage(
      state: HpState
  ): Option[ThermalGridOperatingPoint] = house
    .zip(state.thermalGridState.houseState)
    .zip(heatStorage.zip(state.thermalGridState.heatStorageState))
    .filter {
      case (
            (thermalHouse, houseState),
            (thermalStorage, storageState),
          ) =>
        // In case the storage isn't empty
        // First OR-Condition: If the house has req. demand (innerTempTooLow), we can heat the house from storage.
        // Second OR-Condition: Edge case when em controlled: If the house was heated last state by Hp and setPower is below turnOn condition now,
        // but house didn't reach target or boundary temperature yet, then house can be heated from storage.
        !thermalStorage.isEmpty(storageState.storedEnergy) &&
        (thermalHouse.isInnerTemperatureTooLow(houseState.innerTemperature) ||
          (state.thermalDemands.houseDemand.hasPossibleDemand && state.lastHpOperatingPoint.thermalOps.qDotHouse > zeroKW))
    }
    .map {
      case (
            (_, _),
            (thermalStorage, _),
          ) =>
        ThermalGridOperatingPoint(
          zeroKW,
          thermalStorage.getpThermalMax,
          thermalStorage.getpThermalMax * -1,
          zeroKW,
        )
    }

  /* THRESHOLDS */

  private def getThreshold(
      state: HpState,
      operatingPoint: ThermalGridOperatingPoint,
  ): Option[ThermalThreshold] = {
    val thresholdThermalHouse =
      getHouseThreshold(state, operatingPoint.qDotHouse)
    val thresholdHeatStorage =
      getStorageThreshold(state, operatingPoint.qDotHeatStorage)
    val thresholdHotWaterStorage =
      if operatingPoint.qDotDomesticHotWaterStorage <= zeroKW then
        // consumption
        determineHotWaterConsumptionThreshold(
          state,
          operatingPoint.qDotDomesticHotWaterStorage,
        )
      else
        // feed-in
        getHotWaterStorageThreshold(
          state,
          operatingPoint.qDotDomesticHotWaterStorage,
        )
    determineNextThreshold(
      Seq(
        thresholdThermalHouse,
        thresholdHeatStorage,
        thresholdHotWaterStorage,
      )
    )
  }

  private def determineHotWaterConsumptionThreshold(
      state: HpState,
      qDotDomesticHotWaterStorage: Power,
  ): Option[ThermalThreshold] = {
    if qDotDomesticHotWaterStorage == zeroKW then {
      if domesticHotWaterStorage
          .zip(
            state.thermalGridState.domesticHotWaterStorageState
          )
          .isDefined
      then Some(SimpleThermalThreshold(calculateNextHourThreshold(state)))
      else None
    } else {
      getHotWaterEnergyDemand(state).map { domesticHotWaterDemand =>
        val ticksToFull =
          math.round(
            (-1 * domesticHotWaterDemand.required / qDotDomesticHotWaterStorage).toSeconds
          )
        SimpleThermalThreshold(state.tick + ticksToFull)
      }
    }
  }

  /** Calculates the tick value for the next full hour threshold based on the
    * current simulation state.
    *
    * @param state
    *   State of the heat pump.
    * @return
    *   The tick of the next full hour.
    */
  private def calculateNextHourThreshold(state: HpState): Long = {
    val time = state.simulationTime
    val nextFullHour: ZonedDateTime =
      time.plusHours(1).withMinute(0).withSecond(0).withNano(0)
    val simulationStartTime = time.minusSeconds(state.tick)
    nextFullHour.toTick(using simulationStartTime)
  }

  private def getHouseThreshold(
      state: HpState,
      qDotHouse: Power,
  ): Option[ThermalThreshold] =
    house.zip(state.thermalGridState.houseState).flatMap {
      case (thermalHouse, houseState) =>
        thermalHouse.determineNextThreshold(houseState, qDotHouse)
    }

  private def getStorageThreshold(
      state: HpState,
      qDotStorage: Power,
  ): Option[ThermalThreshold] =
    heatStorage.zip(state.thermalGridState.heatStorageState).flatMap {
      case (storage, storageState) =>
        storage.determineNextThreshold(storageState, qDotStorage)
    }

  private def getHotWaterStorageThreshold(
      state: HpState,
      qDotStorage: Power,
  ): Option[ThermalThreshold] =
    domesticHotWaterStorage
      .zip(state.thermalGridState.domesticHotWaterStorageState)
      .flatMap { case (storage, storageState) =>
        storage.determineNextThreshold(storageState, qDotStorage)
      }

  /** Determines the next threshold of a given input sequence of thresholds.
    *
    * @param thresholds
    *   Sequence of Options of possible next thresholds from the thermal house
    *   or storage.
    *
    * @return
    *   The next [[ThermalThreshold]] or [[None]].
    */
  private def determineNextThreshold(
      thresholds: Seq[Option[ThermalThreshold]]
  ): Option[ThermalThreshold] =
    thresholds.flatten.reduceOption { case (currentMin, threshold) =>
      if threshold.tick < currentMin.tick then threshold
      else currentMin
    }

  /* RESULTS */

  /** Convert the given state of the thermal grid into result models of its
    * constituent models.
    *
    * @param state
    *   State of the heat pump.
    * @param lastOperatingPoint
    *   The last operating point of the heat pump.
    * @param currentOperatingPoint
    *   The current operating point of the heat pump.
    * @param dateTime
    *   The current date and time of this simulation tick.
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
      state.thermalGridState.heatStorageState
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

    def createDomesticHotWaterStorageResult(
        storage: DomesticHotWaterStorage
    ): Option[DomesticHotWaterStorageResult] = {
      state.thermalGridState.domesticHotWaterStorageState
        .collectFirst { case ThermalStorageState(_, storedEnergy) =>
          new DomesticHotWaterStorageResult(
            dateTime,
            storage.uuid,
            storedEnergy.toMegawattHours.asMegaWattHour,
            currentOpThermals.qDotDomesticHotWaterStorage.toMegawatts.asMegaWatt,
            (storedEnergy / storage.maxEnergyThreshold).asPu,
          )
        }
        .orElse(
          throw new NotImplementedError(
            s"Result handling for storage type '${storage.getClass.getSimpleName}' not supported."
          )
        )
    }

    // We always want the results if there are changes, or it's the first tick
    val maybeHouseResult = {
      (
        house,
        lastOpThermals.forall(
          _.qDotHouse != currentOpThermals.qDotHouse
        ) || state.tick == 0,
      ) match {
        case (Some(house: ThermalHouse), true) =>
          createThermalHouseResult(house)
        case _ => None
      }
    }

    // We always want the results if there are changes, or it's the first tick
    val maybeHeatStorageResult = {
      (
        heatStorage,
        lastOpThermals.forall(
          _.qDotHeatStorage != currentOpThermals.qDotHeatStorage
        ) || state.tick == 0,
      ) match {
        case (Some(storage: CylindricalThermalStorage), true) =>
          createCylindricalStorageResult(storage)
        case _ => None
      }
    }

    // We always want the results if there are changes, or it's the first tick
    val maybeDomesticHotWaterStorageResult = {
      (
        domesticHotWaterStorage,
        lastOpThermals.forall(
          _.qDotDomesticHotWaterStorage != currentOpThermals.qDotDomesticHotWaterStorage
        ) || state.tick == 0,
      ) match {
        case (Some(storage: DomesticHotWaterStorage), true) =>
          createDomesticHotWaterStorageResult(storage)
        case _ => None
      }
    }

    Seq(
      maybeHouseResult,
      maybeHeatStorageResult,
      maybeDomesticHotWaterStorageResult,
    ).flatten
  }
}

object ThermalGrid {
  def apply(
      input: edu.ie3.datamodel.models.input.container.ThermalGrid
  ): ThermalGrid = {
    val houses = input.houses().asScala.map(ThermalHouse(_)).toSet
    val heatStorages = input
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
    val domesticHotWaterStorage = input
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
      heatStorages.headOption,
      domesticHotWaterStorage.headOption,
    )
  }

  /** Current state of a grid.
    *
    * @param houseState
    *   State of the thermal house.
    * @param heatStorageState
    *   State of the thermal heat storage.
    * @param domesticHotWaterStorageState
    *   State of the domestic hot water storage.
    */
  final case class ThermalGridState(
      houseState: Option[ThermalHouseState],
      heatStorageState: Option[ThermalStorageState],
      domesticHotWaterStorageState: Option[ThermalStorageState],
  ) {

    /** This method will return boolean indicating if there is no heat storage,
      * or it is empty.
      *
      * @return
      *   boolean which is true, if there is no heat storage, or it's empty.
      */
    def isHeatStorageEmpty: Boolean = {
      implicit val tolerance: Energy = KilowattHours(1e-3)
      heatStorageState.isEmpty || heatStorageState
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
      thermalGrid.domesticHotWaterStorage.map(_.startingState),
    )

  /** Wraps the demand of thermal units (thermal house, thermal storage).
    *
    * @param houseDemand
    *   The demand of the thermal house.
    * @param heatStorageDemand
    *   The demand of the thermal heat storage.
    * @param domesticWaterDemandOfHouse
    *   The actual demand for domestic hot water by the house that needs to get
    *   covered from domesticHotWaterStorage.
    * @param domesticHotWaterStorageDemand
    *   The demand of the domestic hot water storage.
    */
  final case class ThermalDemandWrapper(
      houseDemand: ThermalEnergyDemand,
      heatStorageDemand: ThermalEnergyDemand,
      domesticWaterDemandOfHouse: ThermalEnergyDemand,
      domesticHotWaterStorageDemand: ThermalEnergyDemand,
  )

  /** Defines the thermal energy demand of a thermal grid. It comprises the
    * absolutely required energy demand to reach the target state as well as an
    * energy, that can be handled. The possible energy always has to be greater
    * than or equal to the absolutely required energy. Thus, this class can only
    * be instantiated via factory.
    *
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

    def hasRequiredDemand: Boolean = required > zeroKWh

    def hasPossibleDemand: Boolean = possible > zeroKWh
  }

  object ThermalEnergyDemand {

    /** Builds a new instance of [[ThermalEnergyDemand]]. If the possible energy
      * is less than the required energy, this is considered to be a bad state.
      *
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
      if math.abs(possible.toKilowattHours) < math.abs(required.toKilowattHours)
      then
        throw new InvalidParameterException(
          s"The possible amount of energy $possible is smaller than the required amount of energy $required. This is not supported."
        )

      if possible.toKilowattHours < 0 || required.toKilowattHours < 0 then
        throw new InvalidParameterException(
          s"The possible $possible or required $required amount of energy cannot be negative. This is not supported."
        )

      new ThermalEnergyDemand(required, possible)
    }

    lazy val noDemand: ThermalEnergyDemand = ThermalEnergyDemand(
      zeroMWh,
      zeroMWh,
    )
  }
}
