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
  ThermalHouseResult,
}
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
    * @param ambientTemperature
    *   Ambient temperature in the instance in question
    * @param state
    *   Currently applicable state of the thermal grid
    * @param simulationStart
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   The total energy demand of the house and the storage and an updated
    *   [[ThermalGridState]]
    */
  def energyDemandAndUpdatedState(
      tick: Long,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      simulationStart: ZonedDateTime,
      houseInhabitants: Double,
  ): (
      ThermalEnergyDemand,
      ThermalEnergyDemand,
      ThermalEnergyDemand,
      ThermalGridState,
  ) = {
    /* First get the energy demand of the houses but only if inner temperature is below target temperature */

    val (houseDemand, updatedHouseState, demandHotDomesticWater) =
      house.zip(state.houseState).headOption match {
        case Some((thermalHouse, lastHouseState)) => {
          val (updatedHouseState, updatedStorageState) =
            thermalHouse.determineState(
              tick,
              lastHouseState,
              ambientTemperature,
              lastHouseState.qDot,
            )
          val (heatDemand, newHouseState) = if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature
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

          val energyDemandDomesticHotWater = thermalHouse.energyDemandWater(
            tick,
            Some(lastHouseState),
            simulationStart,
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
      val domesticHotWaterDemand: ThermalEnergyDemand = house.map(_.energyDemandWater(tick,state.houseState, simulationStart,houseInhabitants)).getOrElse(ThermalEnergyDemand(zeroKWH,zeroKWH))
      val applicableqDotDomesticStorage =
      identifyApplicableQDot(tick, domesticHotWaterDemand)._1


      domesticHotWaterStorage
        .zip(state.domesticHotWaterStorageState)
        .map { case (storage, state) =>
          val updatedStorageState =
            storage.updateState(tick, state.qDot.plus(applicableqDotDomesticStorage), state)._1
          val storedEnergy = updatedStorageState.storedEnergy

          val demandOfStorage =
            if (storedEnergy < demandHotDomesticWater.required)
              demandHotDomesticWater.required.minus(storedEnergy)
            else zeroMWH
          (ThermalEnergyDemand(demandOfStorage, demandOfStorage), Some(updatedStorageState))
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
    * @param ambientTemperature
    *   Ambient temperature
    * @param qDot
    *   Thermal energy balance
    * @param houseDemand
    *   determines if the thermal house has heat demand
    * @param storageDemand
    *   determines if the thermal storage has heat demand
    * @param domesticHotWaterStorageDemand
    *   determines if the domestic hot water storage has heat demand
    * @param simulationStart
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   The updated state of the grid
    */
  def updateState(
      tick: Long,
      state: ThermalGridState,
      ambientTemperature: Temperature,
      qDot: Power,
      houseDemand: Boolean,
      storageDemand: Boolean,
      domesticHotWaterStorageDemand: Boolean,
      simulationStartTime: ZonedDateTime,
      houseInhabitants: Double,
  ): (ThermalGridState, Option[ThermalThreshold]) = if (qDot > zeroKW)
    handleInfeed(
      tick,
      ambientTemperature,
      state,
      qDot,
      houseDemand,
      storageDemand,
      domesticHotWaterStorageDemand,
    )
  else
    handleConsumption(
      tick,
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
    * @param ambientTemperature
    *   Ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @param houseDemand
    *   determines if the thermal house has heat demand
    * @param heatStorageDemand
    *   determines if the thermal storage has heat demand
    * @param domesticHotWaterStorageDemand
    *   determines if the domestic hot water storage has heat demand
    * @return
    *   Updated thermal grid state
    */
  private def handleInfeed(
      tick: Long,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
      houseDemand: Boolean,
      heatStorageDemand: Boolean,
      domesticHotWaterStorageDemand: Boolean,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // TODO: We would need to issue a storage result model here...

    /* Consider the action in the last state */
    val (
      qDotHouseLastState,
      qDotStorageLastState,
      qDotDomesticWaterStorageLastState,
    ) = state match {
      case ThermalGridState(
            Some(houseState),
            Some(storageState),
            Some(domesticWaterState),
          ) =>
        (houseState.qDot, storageState.qDot, domesticWaterState.qDot)
      case ThermalGridState(Some(houseState), Some(storageState), None) =>
        (houseState.qDot, storageState.qDot, zeroKW)
      case ThermalGridState(Some(houseState), None, Some(domesticWaterState)) =>
        (houseState.qDot, zeroKW, domesticWaterState.qDot)
      case ThermalGridState(Some(houseState), None, None) =>
        (houseState.qDot, zeroKW, zeroKW)
      case ThermalGridState(None, Some(storageState), None) =>
        (zeroKW, storageState.qDot, zeroKW)
      case _ =>
        throw new InconsistentStateException(
          "There should be at least a house or a storage state."
        )
    }

    if (
      (qDotHouseLastState > zeroKW && qDotHouseLastState == qDot) | qDotStorageLastState > zeroKW | qDotDomesticWaterStorageLastState > zeroKW
    ) {
      val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
        handleInfeedHouse(tick, ambientTemperature, state, qDotHouseLastState)
      val (updatedStorageState, thermalStorageThreshold) =
        if (
          qDotStorageLastState >= zeroKW && remainingQDotHouse > qDotStorageLastState
        ) {
          handleInfeedStorage(
            tick,
            state,
            remainingQDotHouse,
            heatStorage,
          )
        } else {
          handleInfeedStorage(
            tick,
            state,
            qDotStorageLastState,
            heatStorage,
          )
        }

      val (
        updatedDomesticHotWaterStorageState,
        domesticHotWaterStorageThreshold,
      ) = handleInfeedStorage(
        tick,
        state,
        qDotDomesticWaterStorageLastState,
        domesticHotWaterStorage,
      )

      val nextThreshold = determineMostRecentThreshold(
        thermalHouseThreshold,
        thermalStorageThreshold,
        domesticHotWaterStorageThreshold,
      )
      (
        state.copy(
          houseState = updatedHouseState,
          storageState = updatedStorageState,
          domesticHotWaterStorageState = updatedDomesticHotWaterStorageState,
        ),
        nextThreshold,
      )
    } else {

      (houseDemand, heatStorageDemand, domesticHotWaterStorageDemand) match {

        case (true, _, true) =>
          // hot water and house first then heatStorage after heating House
          // Split qDot but max. ChargingPower of Storage
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
            ambientTemperature,
            state,
            qDot - qDotDomesticHotWaterStorage,
            zeroKW,
            qDotDomesticHotWaterStorage,
          )

        // Same if there is Some(heatStorageDemand) or not
        case (true, _, false) =>
          handleCases(tick, ambientTemperature, state, qDot, zeroKW, zeroKW)

        // Same if there is Some(heatStorageDemand) or not
        case (false, _, true) =>
          handleCases(tick, ambientTemperature, state, zeroKW, zeroKW, qDot)

        case (false, true, false) =>
          handleCases(tick, ambientTemperature, state, zeroKW, qDot, zeroKW)
        case _ =>
          throw new InconsistentStateException(
            "There should be at least a house or a storage state."
          )
      }

    }

  }

  private def handleCases(
      tick: Long,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
      qDotDomesticHotWaterStorage: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    // FIXME: Is there any case where we get back some remainingQDotHouse?
    val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
      handleInfeedHouse(tick, ambientTemperature, state, qDotHouse)

    val (updatedStorageState, thermalStorageThreshold) =
      handleInfeedStorage(tick, state, qDotHeatStorage, heatStorage)

    val (
      updatedDomesticHotWaterStorageState,
      domesticHotWaterStorageThreshold,
    ) = handleInfeedStorage(
      tick,
      state,
      qDotDomesticHotWaterStorage,
      domesticHotWaterStorage,
    )

    val nextThreshold = determineMostRecentThreshold(
      thermalHouseThreshold,
      thermalStorageThreshold,
      domesticHotWaterStorageThreshold,
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
    * @param ambientTemperature
    *   Ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    *   Updated thermal house state, a ThermalThreshold and the remaining qDot
    */
  private def handleInfeedHouse(
      tick: Long,
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ): (Option[ThermalHouseState], Option[ThermalThreshold], Power) = {
    (house, state.houseState) match {
      case (Some(thermalHouse), Some(lastHouseState)) =>
        val (newState, threshold) = thermalHouse.determineState(
          tick,
          lastHouseState,
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
    * @return
    *   Updated thermal grid state
    */
  private def handleInfeedStorage(
      tick: Long,
      state: ThermalGridState,
      qDot: Power,
      storage: Option[ThermalStorage],
  ): (Option[ThermalStorageState], Option[ThermalThreshold]) = {

    val selectedState = storage match {
      case Some(_: CylindricalThermalStorage) => state.storageState
      case Some(_: DomesticHotWaterStorage) =>
        state.domesticHotWaterStorageState
      case _ => None
    }

    (storage, selectedState) match {
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

  private def determineMostRecentThreshold(
      maybeHouseThreshold: Option[ThermalThreshold],
      maybeStorageThreshold: Option[ThermalThreshold],
      maybeDomesticHotWaterStorageThreshold: Option[ThermalThreshold],
  ): Option[ThermalThreshold] = {
    val thresholds = List(
      maybeHouseThreshold,
      maybeStorageThreshold,
      maybeDomesticHotWaterStorageThreshold,
    ).flatten

    thresholds.foldLeft(Option.empty[ThermalThreshold]) { (noTick, threshold) =>
      noTick match {
        case None => Some(threshold)
        case Some(minThreshold) =>
          if (threshold.tick < minThreshold.tick) Some(threshold) else noTick
      }
    }
  }

  /** Handle consumption (or no infeed) from thermal grid
    *
    * @param tick
    *   Current tick
    * @param ambientTemperature
    *   Ambient temperature
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @param simulationStart
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    * @return
    *   Updated thermal grid state
    */
  private def handleConsumption(
      tick: Long,
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
        ambientTemperature,
        qDot,
      )

    heatStorage.zip(state.storageState).map { case (storage, storageState) =>
      storage.updateState(tick, qDot, storageState)
    }

    val domesticHotWaterDemand = house
      .map(
        _.energyDemandWater(
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
    )

    val nextThresholdHotWaterStorage = determineMostRecentThreshold(
      tickWhenStorageDemandEnds,
      domesticHotWaterStorageThreshold,
      None,
    )

    val nextThreshold = determineMostRecentThreshold(
      revisedHouseState.flatMap(_._2),
      revisedStorageState.flatMap(_._2),
      nextThresholdHotWaterStorage,
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

  private def identifyApplicableQDot(tick: Long, domesticHotWaterDemand: ThermalEnergyDemand):(Power,Option[SimpleThermalThreshold])={

    if (domesticHotWaterDemand.required > zeroKWH) {
      val chargingPower = domesticHotWaterStorage
        .map(_.getChargingPower)
        .getOrElse(
          throw new RuntimeException(
            s"Trying to get the chargingPower of domesticHotWaterStorage was not possible"
          )
        )

      val durationAtFullPower =
        domesticHotWaterDemand.required / chargingPower

      if (durationAtFullPower > Seconds(1)) {
        (
          chargingPower * (-1),
          Some(
            SimpleThermalThreshold(
              tick + Math.round(durationAtFullPower.toSeconds)
            )
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
    * @param ambientTemperature
    *   Ambient temperature
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
      .headOption
      .flatMap {
        case (
              thermalHouse,
              ThermalHouseState(tick, _, _),
            ) =>
          Some(tick)
        case _ => None
      }

    val storageResultTick: Option[Long] = heatStorage
      .zip(state.storageState)
      .headOption
      .flatMap {
        case (
              thermalStorage,
              ThermalStorageState(tick, _, _),
            ) =>
          Some(tick)
        case _ => None
      }

    val actualResultTick: Long = (houseResultTick, storageResultTick) match {
      case (Some(hTick), Some(sTick)) => math.max(hTick, sTick)
      case (Some(hTick), None)        => hTick
      case (None, Some(sTick))        => sTick
      case (None, None) =>
        throw new RuntimeException(
          "ThermalGrid result should be carried out but it was not possible to get the tick for the result"
        )
    }

    val houseResults = house
      .zip(state.houseState)
      .map {
        case (
              thermalHouse,
              ThermalHouseState(tick, innerTemperature, thermalInfeed),
            ) =>
          Seq.empty[ResultEntity] :+ new ThermalHouseResult(
            actualResultTick.toDateTime,
            thermalHouse.uuid,
            thermalInfeed.toMegawatts.asMegaWatt,
            innerTemperature.toKelvinScale.asKelvin,
          )
      }
      .getOrElse(Seq.empty[ResultEntity])

    heatStorage
      .zip(state.storageState)
      .map {
        case (
              storage: CylindricalThermalStorage,
              ThermalStorageState(tick, storedEnergy, qDot),
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

    def hasAdditionalDemand: Boolean = possible > required
  }
  object ThermalEnergyDemand {

    /** Builds a new instance of [[ThermalEnergyDemand]]. If the possible energy
      * is less than the required energy, this is considered to be a bad state
      * and the required energy is curtailed to the possible energy.
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
      zeroMWH,
      zeroMWH,
    )
  }
}
