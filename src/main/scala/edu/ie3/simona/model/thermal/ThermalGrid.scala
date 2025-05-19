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
import edu.ie3.util.scala.quantities.QuantityUtil.*
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  DomesticHotWaterStorageResult,
  ThermalHouseResult,
}
import edu.ie3.simona.exceptions.InvalidParameterException
import edu.ie3.simona.model.participant.HpModel.{
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
import edu.ie3.util.quantities.QuantityUtils.{
  asMegaWattHour,
  asKelvin,
  asPu,
  asMegaWatt,
}
import edu.ie3.util.scala.quantities.DefaultQuantities._
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
    heatStorage: Option[ThermalStorage],
    domesticHotWaterStorage: Option[ThermalStorage],
) extends LazyLogging {

  /** Determines the state of the ThermalGrid by using the HpOperatingPoint.
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

    val updatedDomesticHotWaterStorageState =
      domesticHotWaterStorage
        .zip(lastState.domesticHotWaterStorageState)
        .map {
          case (
                storage: DomesticHotWaterStorage,
                waterStorageState: ThermalStorageState,
              ) =>
            storage.determineState(
              tick,
              waterStorageState,
              waterStorageQDot,
            )
          case _ =>
            throw new IllegalStateException(
              "Could not find state of domestic hot water storage."
            )
        }

    ThermalGridState(
      updatedHouseState,
      updatedHeatStorageState,
      updatedDomesticHotWaterStorageState,
    )
  }

  /** Determine the energy demand of the thermalGrid.
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
      house.zip(thermalGridState.houseState) match {
        case Some((thermalHouse, houseState)) =>
          // Calculate domestic hot water demand
          val domesticHotWaterDemand =
            thermalHouse.energyDemandDomesticHotWater(
              hoursWaterDemandToDetermine,
              thermalGridState.houseState,
            )
          // Calculate heating demand of house
          val heatingDemand = {
            if (houseState.innerTemperature < thermalHouse.targetTemperature) {
              thermalHouse.energyDemandHeating(houseState)
            } else {
              ThermalEnergyDemand.noDemand
            }
          }
          (heatingDemand, domesticHotWaterDemand)

        case None =>
          (ThermalEnergyDemand.noDemand, ThermalEnergyDemand.noDemand)
      }

    val heatStorageDemand =
      heatStorage.zip(thermalGridState.heatStorageState) match {
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

    val domesticHotWaterStorageDemand =
      domesticHotWaterStorage.zip(
        thermalGridState.domesticHotWaterStorageState
      ) match {
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
        houseDemandHeating.required,
        houseDemandHeating.possible,
      ),
      ThermalEnergyDemand(
        heatStorageDemand.required,
        heatStorageDemand.possible,
      ),
      ThermalEnergyDemand(
        houseDemandWater.required,
        houseDemandWater.possible,
      ),
      ThermalEnergyDemand(
        domesticHotWaterStorageDemand.required,
        domesticHotWaterStorageDemand.possible,
      ),
    )
  }

  /** Handles the case, when a grid has feed in. Depending on which entity has
    * some heat demand the house or the storage will be heated up / filled up.
    * First the actions from last operating point will be considered and checked
    * if the behaviour should be continued. This might be the case, if we got
    * activated by updated weather data. If this is not the case, all other
    * cases will be handled by [[ThermalGrid.handleFinalFeedInCases]].
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

    /* Consider the action in the last state
    We can continue using the qDots from last operating point to keep continuity.
    If the house was heated in lastState and has still some demand and the domestic
    hot water storage as no demand. */
    if (
      state.lastHpOperatingPoint.thermalOps.qDotHouse > zeroKW &&
      state.thermalDemands.houseDemand.hasPossibleDemand &&
      !state.thermalDemands.domesticHotWaterStorageDemand.hasRequiredDemand
    )
      handleCase(state, qDot, zeroKW, zeroKW)
    // or finally check for all other cases.
    else
      handleFinalFeedInCases(state, qDot)
  }

  /** Handles the last cases of [[ThermalGrid.handleFeedIn]], where the thermal
    * feed in should be determined.
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
    * @param state
    *   State of the heat pump.
    * @param qDot
    *   Feed in to the grid from thermal generation (e.g. heat pump) or thermal
    *   storages.
    * @return
    *   The operating point of the thermal grid and the thermalThreshold if
    *   there is one.
    */
  private def handleFinalFeedInCases(
      state: HpState,
      qDot: Power,
  ): (ThermalGridOperatingPoint, Option[ThermalThreshold]) = {

    if (
      state.thermalDemands.domesticHotWaterStorageDemand.hasRequiredDemand && (state.thermalDemands.houseDemand.hasRequiredDemand ||
        (state.lastHpOperatingPoint.thermalOps.qDotHouse > zeroKW && state.lastHpOperatingPoint.thermalOps.qDotHp > zeroKW))
    )
      // if the DomesticHotWaterStorage has reqDemand AND house has reqDemand or was heated in lastState by Hp, we would like to split the qDot between house and waterStorage
      handleCase(state, qDot / 2, zeroKW, qDot / 2)
    else if (state.thermalDemands.houseDemand.hasRequiredDemand)
      handleCase(state, qDot, zeroKW, zeroKW)
    else if (
      state.thermalDemands.domesticHotWaterStorageDemand.hasRequiredDemand
    )
      handleCase(state, zeroKW, zeroKW, qDot)
    else if (
      state.thermalDemands.heatStorageDemand.hasRequiredDemand || state.thermalDemands.heatStorageDemand.hasPossibleDemand
    )
      handleCase(state, zeroKW, qDot, zeroKW)
    else if (state.thermalDemands.houseDemand.hasPossibleDemand)
      handleCase(state, qDot, zeroKW, zeroKW)
    else
      handleCase(state, zeroKW, zeroKW, zeroKW)
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
    *   The operating point of the thermal grid and the next threshold if there
    *   is one.
    */
  private def handleCase(
      state: HpState,
      qDotHouse: Power,
      qDotHeatStorage: Power,
      qDotDomesticHotWaterStorage: Power,
  ): (ThermalGridOperatingPoint, Option[ThermalThreshold]) = {
    val (_, thresholdThermalHouse) =
      handleFeedInHouse(state, qDotHouse)

    val thresholdThermalStorage =
      handleFeedInStorage(state, qDotHeatStorage, heatStorage)

    // Handle domestic hot water demand
    val (resultingQDotHotWaterStorage, thresholdHotWaterStorage) =
      // There only can be consumption, if there isn't feed in into the storage.
      if (qDotDomesticHotWaterStorage == zeroKW)
        handleHotWaterConsumption(state)
      else {
        val threshold = handleFeedInStorage(
          state,
          qDotDomesticHotWaterStorage,
          domesticHotWaterStorage,
        )
        (qDotDomesticHotWaterStorage, threshold)
      }

    val nextThreshold = determineMostRecentThreshold(
      Seq(
        thresholdThermalHouse,
        thresholdThermalStorage,
        thresholdHotWaterStorage,
      )
    )

    (
      ThermalGridOperatingPoint(
        qDotHouse + qDotHeatStorage + qDotDomesticHotWaterStorage,
        qDotHouse,
        qDotHeatStorage,
        resultingQDotHotWaterStorage,
      ),
      nextThreshold,
    )
  }

  /** Handles the case, when the house has heat demand and will be heated up
    * here.
    *
    * @param state
    *   State of the heat pump.
    * @param qDotHouse
    *   Feed in into the house.
    * @return
    *   The thermal feed in into the thermal house and the ThermalThreshold.
    */
  private def handleFeedInHouse(
      state: HpState,
      qDotHouse: Power,
  ): (Power, Option[ThermalThreshold]) = {
    house.zip(state.thermalGridState.houseState) match {
      case Some((thermalHouse, houseState)) =>
        /* Check if house can handle the thermal feed in */
        if (
          thermalHouse.isInnerTemperatureTooHigh(
            houseState.innerTemperature
          )
        ) {

          val maybeFullHouseThreshold =
            thermalHouse.determineNextThreshold(houseState, zeroKW)

          (qDotHouse, maybeFullHouseThreshold)

        } else {
          val threshold = thermalHouse.determineNextThreshold(
            houseState,
            qDotHouse,
          )
          (zeroKW, threshold)
        }
      case _ => (zeroKW, None)
    }
  }

  /** Handles the case, when the storage has heat demand and will be filled up
    * here (positive qDot). It will return the next thermal threshold of the
    * storage.
    * @param state
    *   State of the heat pump.
    * @param qDotStorage
    *   Feed in to the storage (positive: Storage is charging, negative: Storage
    *   is discharging).
    * @return
    *   Updated thermal storage state and the ThermalThreshold.
    */
  private def handleFeedInStorage(
      state: HpState,
      qDotStorage: Power,
      storage: Option[ThermalStorage],
  ): Option[ThermalThreshold] = {
    // TODO: We should somewhere check that pThermalMax of Storage is always capable for qDot pThermalMax >= pThermal of Hp
    val selectedState = storage match {
      case Some(_: CylindricalThermalStorage) =>
        state.thermalGridState.heatStorageState
      case Some(_: DomesticHotWaterStorage) =>
        state.thermalGridState.domesticHotWaterStorageState
      case _ => None
    }

    (storage, selectedState) match {
      case (
            Some(domesticHotWaterStorage: DomesticHotWaterStorage),
            Some(domesticHotWaterStorageState),
          ) =>
        domesticHotWaterStorage
          .determineNextThreshold(domesticHotWaterStorageState, qDotStorage)

      case (
            Some(thermalStorage: CylindricalThermalStorage),
            Some(heatStorageState),
          ) =>
        thermalStorage.determineNextThreshold(heatStorageState, qDotStorage)
      case _ => None
    }
  }

  /** Returns the very next threshold or None if there isn't any.
    *
    * @param thresholds
    *   A sequence of thresholds.
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

    // handle hot water demand
    val (qDotHotWaterStorage, thresholdWaterStorage) =
      handleHotWaterConsumption(state)
    /* House will be left with no influx in all cases. Determine if and when a threshold is reached */
    val houseThreshold = house.zip(state.thermalGridState.houseState) match {
      case Some((thermalHouse, houseState)) =>
        thermalHouse.determineNextThreshold(houseState, zeroKW)
      case _ => None
    }

    /* Check if house can be heated from storage */
    val (revisedOp, revisedThreshold) =
      reviseFeedInFromStorage(state, houseThreshold)

    val operatingPoint =
      revisedOp.copy(qDotDomesticHotWaterStorage = qDotHotWaterStorage)
    val nextThreshold = determineMostRecentThreshold(
      Seq(revisedThreshold, thresholdWaterStorage)
    )

    (operatingPoint, nextThreshold)
  }

  private def handleHotWaterConsumption(
      state: HpState
  ): (Power, Option[ThermalThreshold]) = {

    val domesticHotWaterDemand =
      state.thermalDemands.domesticWaterDemandOfHouse

    val (qDot, threshold) = domesticHotWaterStorage.zip(
      state.thermalGridState.domesticHotWaterStorageState
    ) match {
      case Some((_, storageState)) =>
        // Check if storage can handle the demand
        if (storageState.storedEnergy < domesticHotWaterDemand.required) {
          // if it can't, take max qDot that empties the storage asap, return the according threshold
          {
            identifyApplicableQDot(
              state,
              ThermalEnergyDemand(
                storageState.storedEnergy,
                storageState.storedEnergy,
              ),
            )
          }
        } else {
          // else, choose qDot to fit demand and return the according threshold
          identifyApplicableQDot(state, domesticHotWaterDemand)
        }
      case _ => (zeroKW, None)
    }

    (qDot, threshold)
  }

  private def identifyApplicableQDot(
      state: HpState,
      domesticHotWaterDemand: ThermalEnergyDemand,
  ): (Power, Option[ThermalThreshold]) = {
    if (domesticHotWaterDemand.required > zeroKWh) {
      val chargingPower = domesticHotWaterStorage
        .map(_.getpThermalMax)
        .getOrElse(
          throw new RuntimeException(
            s"Trying to get the chargingPower of domesticHotWaterStorage was not possible"
          )
        )

      val approxDurationAtFullPower =
        domesticHotWaterDemand.required / chargingPower

      if (approxDurationAtFullPower > Seconds(1)) {
        val preciseChargingPower =
          -1 * domesticHotWaterDemand.required / Seconds(
            approxDurationAtFullPower.toSeconds.toLong + 1
          )
        val threshold =
          -1 * domesticHotWaterDemand.required / preciseChargingPower

        (
          preciseChargingPower,
          Some(
            SimpleThermalThreshold(state.tick + math.round(threshold.toSeconds))
          ),
        )
      } else {
        (
          -1 * domesticHotWaterDemand.required / Seconds(1d),
          Some(SimpleThermalThreshold(state.tick + 1)),
        )
      }
    } else {

      val time = state.simulationTime
      val nextFullHour: ZonedDateTime =
        time.plusHours(1).withMinute(0).withSecond(0).withNano(0)
      val simulationStartTime = time.minusSeconds(state.tick)
      val nextThreshold = nextFullHour.toTick(simulationStartTime)

      (zeroKW, Some(SimpleThermalThreshold(nextThreshold)))
    }
  }

  /** Check, if the storage can heat the house. This is only done, if <ul>
    * <li>the house has reached it's lower temperature boundary,</li> <li>there
    * is no feed in from external and</li> <li>the storage is not empty
    * itself</li> </ul>.
    *
    * @param state
    *   State of the heat pump.
    * @param maybeHouseThreshold
    *   Optional thermal house threshold.
    * @return
    *   Operating point of the thermal grid and the next thermal threshold, if
    *   there is one.
    */
  def reviseFeedInFromStorage(
      state: HpState,
      maybeHouseThreshold: Option[ThermalThreshold],
  ): (ThermalGridOperatingPoint, Option[ThermalThreshold]) = house
    .zip(state.thermalGridState.houseState)
    .zip(heatStorage.zip(state.thermalGridState.heatStorageState)) match {
    case Some(
          (
            (thermalHouse, houseState),
            (thermalStorage, storageState),
          )
        )
        // In case the storage isn't empty
        // First OR-Condition: If the house has req. demand (innerTempTooLow), we can heat the house from storage.
        // Second OR-Condition: Edge case when em controlled: If the house was heated last state by Hp and setPower is below turnOn condition now,
        // but house didn't reach target or boundary temperature yet, then house can be heated from storage.
        if !thermalStorage.isEmpty(storageState.storedEnergy) &&
          (thermalHouse.isInnerTemperatureTooLow(houseState.innerTemperature) ||
            (state.thermalDemands.houseDemand.hasPossibleDemand && state.lastHpOperatingPoint.thermalOps.qDotHouse > zeroKW)) =>
      /* Storage is meant to heat the house only, if there is no feed in from external (+/- 10 W) and the house is cold */
      val revisedHouseThreshold = thermalHouse.determineNextThreshold(
        houseState,
        thermalStorage.getpThermalMax,
      )
      val revisedStorageThreshold = thermalStorage.determineNextThreshold(
        storageState,
        thermalStorage.getpThermalMax * -1,
      )

      val nextThreshold = determineMostRecentThreshold(
        Seq(
          revisedHouseThreshold,
          revisedStorageThreshold,
        )
      )
      (
        ThermalGridOperatingPoint(
          zeroKW,
          thermalStorage.getpThermalMax,
          thermalStorage.getpThermalMax * -1,
          zeroKW,
        ),
        nextThreshold,
      )
    case _ => (ThermalGridOperatingPoint.zero, maybeHouseThreshold)
  }

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

    val maybeHouseResult = {
      (
        house,
        lastOpThermals.forall(_.qDotHouse != currentOpThermals.qDotHouse),
        state.tick == 0,
      ) match {
        case (Some(house: ThermalHouse), true, _) =>
          createThermalHouseResult(house)
        // We always want the results of the first tick
        case (Some(house: ThermalHouse), _, true) =>
          createThermalHouseResult(house)
        case _ => None
      }
    }

    val maybeHeatStorageResult = {
      (
        heatStorage,
        lastOpThermals.forall(
          _.qDotHeatStorage != currentOpThermals.qDotHeatStorage
        ),
        state.tick == 0,
      ) match {
        case (Some(storage: CylindricalThermalStorage), true, _) =>
          createCylindricalStorageResult(storage)
        // We always want the results of the first tick
        case (Some(storage: CylindricalThermalStorage), _, true) =>
          createCylindricalStorageResult(storage)
        case _ => None
      }
    }

    val maybeDomesticHotStorageResult = {
      (
        domesticHotWaterStorage,
        lastOpThermals.forall(
          _.qDotDomesticHotWaterStorage != currentOpThermals.qDotDomesticHotWaterStorage
        ),
        state.tick == 0,
      ) match {
        case (Some(storage: DomesticHotWaterStorage), true, _) =>
          createDomesticHotWaterStorageResult(storage)
        // We always want the results of the first tick
        case (Some(storage: DomesticHotWaterStorage), _, true) =>
          createDomesticHotWaterStorageResult(storage)
        case _ => None
      }
    }

    Seq(
      maybeHouseResult,
      maybeHeatStorageResult,
      maybeDomesticHotStorageResult,
    ).flatten
  }
}

object ThermalGrid {
  def apply(
      input: edu.ie3.datamodel.models.input.container.ThermalGrid
  ): ThermalGrid = {
    val houses = input.houses().asScala.map(ThermalHouse(_)).toSet
    val heatStorages: Set[CylindricalThermalStorage] = input
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

    /** This method will return booleans whether there is a heat demand of house
      * or thermal storage as well as a boolean indicating if there is no
      * thermal storage, or it is empty.
      *
      * @return
      *   boolean which is true, if there is no thermalStorage, or it's empty.
      */
    def isThermalStorageEmpty: Boolean = {
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
