/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{CylindricalStorageResult, ThermalHouseResult}
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.thermal.ThermalGrid.{ThermalEnergyDemand, ThermalGridState}
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
    * @param ambientTemperature
    *   Ambient temperature in the instance in question
    * @param state
    *   Currently applicable state of the thermal grid
    * @return
    *   The total energy demand of the house and the storage and an updated
    *   [[ThermalGridState]]
    */
  def energyDemandAndUpdatedState(
      tick: Long,
    //FIXME this is also in state
    lastAmbientTemperature: Temperature,
      ambientTemperature: Temperature,
      state: ThermalGridState,
  ): (ThermalEnergyDemand, ThermalEnergyDemand, ThermalGridState) = {
    /* First get the energy demand of the houses but only if inner temperature is below target temperature */

    val (houseDemand, updatedHouseState) =
      house.zip(state.houseState).headOption match {
        case Some((thermalHouse, lastHouseState)) =>
          val (updatedHouseState, updatedStorageState) =
            thermalHouse.determineState(
              tick,
              lastHouseState,
              lastAmbientTemperature,
              ambientTemperature,
              lastHouseState.qDot,
            )
          if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature | (lastHouseState.qDot > zeroKW && updatedHouseState.innerTemperature < thermalHouse.upperBoundaryTemperature)
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

    (
      ThermalEnergyDemand(
        houseDemand.required,
        houseDemand.possible,
      ),
      ThermalEnergyDemand(
        storageDemand.required,
        storageDemand.possible,
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
    * @param ambientTemperature
    *   Ambient temperature
   * @param isRunning
   *   determines whether the heat pump is running or not
    * @param qDot
    *   Thermal energy balance
    * @param houseDemand
    *   determines if the thermal house has heat demand
    * @param storageDemand
    *   determines if the thermal storage has heat demand
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
      houseDemand: Boolean,
      storageDemand: Boolean,
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
    )
  else
    handleConsumption(tick,    lastAmbientTemperature, ambientTemperature, state, qDot)

  /** Handles the case, when a grid has infeed. Depending which entity has some
    * heat demand the house or the storage will be heated up / filled up.
    *
    * @param tick
    *   Current tick
    * @param ambientTemperature
    *   Ambient temperature
    * @param state
    *   Current state of the houses
   * @param isRunning
   *   determines whether the heat pump is running or not
    * @param qDot
    *   Infeed to the grid
    * @param houseDemand
    *   determines if the thermal house has heat demand
    * @param storageDemand
    *   determines if the thermal storage has heat demand
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
      houseDemand: Boolean,
      heatStorageDemand: Boolean,
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
      (qDotHouseLastState > zeroKW && !(qDotStorageLastState < zeroKW)) | (qDotStorageLastState > zeroKW & heatStorageDemand)
    ) {
      val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
        handleInfeedHouse(tick, lastAmbientTemperature, ambientTemperature, state, qDotHouseLastState)
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
      // Handle edge case where house get heated from storage and HP will be activated in between
    else if(
      (qDotHouseLastState > zeroKW && qDotStorageLastState < zeroKW)
    ){
      if (isRunning){
        handleCases(
          tick,
          lastAmbientTemperature,
          ambientTemperature,
          state,
          qDot,
          zeroKW,
        )}
        else{

      handleCases(
        tick,
        lastAmbientTemperature,
        ambientTemperature,
        state,
        qDotHouseLastState,
        qDotStorageLastState,
      )
        }}


    else {

      (houseDemand, heatStorageDemand) match {

        case (true, _) =>
          // house first then heatStorage after heating House
          handleCases(
            tick,
            lastAmbientTemperature,
            ambientTemperature,
            state,
            qDot,
            zeroKW,
          )

        case (false, true) =>
          handleCases(tick,    lastAmbientTemperature, ambientTemperature, state, zeroKW, qDot)

        case (false, false) =>
          handleCases(tick,    lastAmbientTemperature, ambientTemperature, state, zeroKW, zeroKW)
        case _ =>
          throw new InconsistentStateException(
            "There should be at least a house or a storage state."
          )
      }
    }
  }

  /** Handles the different cases, of thermal flows from and into the thermal grid.
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
   * @param qDotStorage
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
    // FIXME: Is there any case where we get back some remainingQDotHouse?
    val (updatedHouseState, thermalHouseThreshold, remainingQDotHouse) =
      handleInfeedHouse(tick, lastAmbientTemperature, ambientTemperature, state, qDotHouse)

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
    * @param ambientTemperature
    *   Ambient temperature
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
   *   Ambient temperature until this tick
   * @param ambientTemperature
   *   actual ambient temperature
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
   *   Ambient temperature until this tick
   * @param ambientTemperature
   *   actual ambient temperature
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

    val storageResultTick: Option[Long] = storage
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

    storage
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
            s"Result handling for storage type '${storage.getClass.getSimpleName}' not supported."
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
