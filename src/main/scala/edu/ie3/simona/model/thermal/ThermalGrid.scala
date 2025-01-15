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
    * @param state
    *   Currently applicable state
    * @param lastAmbientTemperature
    *   Ambient temperature valid up until (not including) the current tick
    * @param qDot
    *   Thermal energy balance
    * @return
    *   The updated state of the grid
    */
  def updateState(
      relevantData: HpRelevantData,
      state: ThermalGridState,
      lastAmbientTemperature: Temperature,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = if (qDot > zeroKW)
    handleInfeed(relevantData, lastAmbientTemperature, state, qDot)
  else
    handleConsumption(
      relevantData,
      lastAmbientTemperature,
      state,
      qDot,
    )

  /** Handles the case, when a grid has infeed. First, heat up all the houses to
    * their maximum temperature, then fill up the storages
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
    *   Updated thermal grid state
    */
  private def handleInfeed(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) =
    house.zip(state.houseState) match {
      case Some((thermalHouse, lastHouseState)) =>
        /* Set thermal power exchange with storage to zero */
        // TODO: We would need to issue a storage result model here...
        val updatedStorageState = storage.zip(state.storageState) match {
          case Some((thermalStorage, storageState)) =>
            Some(
              thermalStorage
                .updateState(
                  relevantData.currentTick,
                  zeroKW,
                  storageState,
                )
                ._1
            )
          case _ => state.storageState
        }

        val (updatedHouseState, maybeHouseThreshold) =
          thermalHouse.determineState(
            relevantData,
            lastHouseState,
            lastAmbientTemperature,
            qDot,
          )

        if (
          thermalHouse.isInnerTemperatureTooHigh(
            updatedHouseState.innerTemperature
          )
        ) {
          /* The house is already heated up fully, set back the infeed and put it into storage, if available */
          val (fullHouseState, maybeFullHouseThreshold) =
            thermalHouse.determineState(
              relevantData,
              lastHouseState,
              lastAmbientTemperature,
              zeroKW,
            )
          storage.zip(updatedStorageState) match {
            case Some((thermalStorage, storageState)) =>
              val (updatedStorageState, maybeStorageThreshold) =
                thermalStorage.updateState(
                  relevantData.currentTick,
                  qDot,
                  storageState,
                )

              /* Both house and storage are updated. Determine what reaches the next threshold */
              val nextThreshold = determineMostRecentThreshold(
                maybeFullHouseThreshold,
                maybeStorageThreshold,
              )

              (
                state.copy(
                  houseState = Some(fullHouseState),
                  storageState = Some(updatedStorageState),
                ),
                nextThreshold,
              )
            case None =>
              /* There is no storage, house determines the next activation */
              (
                state.copy(houseState = Some(fullHouseState)),
                maybeFullHouseThreshold,
              )
          }
        } else {
          /* The house can handle the infeed */
          (
            state.copy(houseState = Some(updatedHouseState)),
            maybeHouseThreshold,
          )
        }

      case None =>
        storage.zip(state.storageState) match {
          case Some((thermalStorage, storageState)) =>
            val (updatedStorageState, maybeStorageThreshold) =
              thermalStorage.updateState(
                relevantData.currentTick,
                qDot,
                storageState,
              )
            (
              state.copy(storageState = Some(updatedStorageState)),
              maybeStorageThreshold,
            )
          case None =>
            throw new InconsistentStateException(
              "A thermal grid has to contain either at least a house or a storage."
            )
        }
    }

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
    * @param state
    *   Current state of the houses
    * @param qDot
    *   Infeed to the grid
    * @return
    *   Updated thermal grid state
    */
  private def handleConsumption(
      relevantData: HpRelevantData,
      lastAmbientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    /* House will be left with no influx in all cases. Determine if and when a threshold is reached */
    val maybeUpdatedHouseState =
      house.zip(state.houseState).map { case (house, houseState) =>
        house.determineState(
          relevantData,
          houseState,
          lastAmbientTemperature,
          zeroMW,
        )
      }

    /* Update the state of the storage */
    val maybeUpdatedStorageState =
      storage.zip(state.storageState).map { case (storage, storageState) =>
        storage.updateState(relevantData.currentTick, qDot, storageState)
      }

    val (revisedHouseState, revisedStorageState) =
      reviseInfeedFromStorage(
        relevantData,
        maybeUpdatedHouseState,
        maybeUpdatedStorageState,
        state.houseState,
        state.storageState,
        lastAmbientTemperature,
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
  final case class ThermalDemandWrapper(
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
      zeroMWh,
      zeroMWh,
    )
  }
}
