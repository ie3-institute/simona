/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import breeze.linalg.min
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  ThermalHouseResult
}
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageThreshold.{
  StorageEmpty,
  StorageFull
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.util.TickUtil.TickLong
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import javax.measure.quantity.{Dimensionless, Energy, Power, Temperature, Time}
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
    storage: Option[ThermalStorage]
) extends LazyLogging {

  /** Determine the energy demand of the total grid at the given instance in
    * time
    * @param tick
    *   Questioned instance in time
    * @param ambientTemperature
    *   Ambient temperature in the instance in question
    * @param state
    *   Currently applicable state of the thermal grid
    * @return
    *   The total energy demand of the grid
    */
  def energyDemand(
      tick: Long,
      ambientTemperature: ComparableQuantity[Temperature],
      state: ThermalGridState
  ): ThermalEnergyDemand = {
    /* First get the energy demand of the houses */
    val houseDemand = house
      .zip(state.houseState)
      .map { case (house, state) =>
        house.energyDemand(
          tick,
          ambientTemperature,
          state
        )
      }
      .getOrElse(ThermalEnergyDemand.noDemand)

    /* Then go over the storages, see what they can provide and what they might be able to charge */
    val (storedEnergy, remainingCapacity) = {
      storage
        .zip(state.storageState)
        .map { case (storage, state) =>
          val usableEnergy = state.storedEnergy
          val remaining = storage.getMaxEnergyThreshold
            .subtract(usableEnergy)
          (
            usableEnergy,
            remaining
          )
        }
        .getOrElse(
          (
            Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT),
            Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
          )
        )
    }

    val usedEnergy =
      if (storedEnergy.isGreaterThanOrEqualTo(houseDemand.required))
        houseDemand.required
      else
        storedEnergy
    val finallyRemaining =
      remainingCapacity.add(usedEnergy)

    ThermalEnergyDemand(
      houseDemand.required.subtract(usedEnergy),
      houseDemand.possible.add(finallyRemaining)
    )
  }

  /** Update the current state of the grid
    * @param tick
    *   Instance in time
    * @param state
    *   Currently applicable state
    * @param ambientTemperature
    *   Ambient temperature
    * @param qDot
    *   Thermal energy balance
    * @return
    *   The updated state of the grid
    */
  def updateState(
      tick: Long,
      state: ThermalGridState,
      ambientTemperature: ComparableQuantity[Temperature],
      qDot: ComparableQuantity[Power]
  ): (ThermalGridState, Option[ThermalThreshold]) = if (
    qDot.isGreaterThan(
      Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
    )
  )
    handleInfeed(tick, ambientTemperature, state, qDot)
  else
    handleConsumption(tick, ambientTemperature, state, qDot)

  /** Handles the case, when a grid has infeed. First, heat up all the houses to
    * their maximum temperature, then fill up the storages
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
  private def handleInfeed(
      tick: Long,
      ambientTemperature: ComparableQuantity[Temperature],
      state: ThermalGridState,
      qDot: ComparableQuantity[Power]
  ): (ThermalGridState, Option[ThermalThreshold]) =
    house.zip(state.houseState) match {
      case Some((thermalHouse, lastHouseState)) =>
        thermalHouse.updateState(
          tick,
          lastHouseState,
          ambientTemperature,
          qDot
        ) match {
          case (_, Some(HouseTemperatureUpperBoundaryReached(thresholdTick)))
              if thresholdTick == tick =>
            /* The house is already heated up fully, set back the infeed and put it into storage, if available */
            val (updatedHouseState, maybeHouseThreshold) =
              thermalHouse.updateState(
                tick,
                lastHouseState,
                ambientTemperature,
                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
              )
            storage.zip(state.storageState) match {
              case Some((thermalStorage, storageState)) =>
                val (updatedStorageState, maybeStorageThreshold) =
                  thermalStorage.updateState(tick, qDot, storageState)

                /* Both house and storage are updated. Determine what reaches the next threshold */
                val nextThreshold = determineMostRecentThreshold(
                  maybeHouseThreshold,
                  maybeStorageThreshold
                )

                (
                  state.copy(
                    houseState = Some(updatedHouseState),
                    storageState = Some(updatedStorageState)
                  ),
                  nextThreshold
                )
              case None =>
                /* There is no storage, house determines the next activation */
                (
                  state.copy(houseState = Some(updatedHouseState)),
                  maybeHouseThreshold
                )
            }
          case (updatedState, maybeThreshold) =>
            /* The house can handle the infeed */
            (state.copy(houseState = Some(updatedState)), maybeThreshold)
        }
      case None =>
        storage.zip(state.storageState) match {
          case Some((thermalStorage, storageState)) =>
            val (updatedStorageState, maybeStorageThreshold) =
              thermalStorage.updateState(tick, qDot, storageState)
            (
              state.copy(storageState = Some(updatedStorageState)),
              maybeStorageThreshold
            )
          case None =>
            throw new InconsistentStateException(
              "A thermal grid has to contain either at least a house or a storage."
            )
        }
    }

  private def determineMostRecentThreshold(
      maybeHouseThreshold: Option[ThermalThreshold],
      maybeStorageThreshold: Option[ThermalThreshold]
  ): Option[ThermalThreshold] =
    (maybeHouseThreshold, maybeStorageThreshold) match {
      case (Some(houseThreshold), Some(storageThreshold)) =>
        if (houseThreshold.tick <= storageThreshold.tick)
          maybeHouseThreshold
        else
          maybeStorageThreshold
      case (None, Some(_)) => maybeStorageThreshold
      case (Some(_), None) => maybeHouseThreshold
    }

  /** Handle consumption (or no infeed) from thermal grid
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
  private def handleConsumption(
      tick: Long,
      ambientTemperature: ComparableQuantity[Temperature],
      state: ThermalGridState,
      qDot: ComparableQuantity[Power]
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    /* House will be left with no influx in all cases. Determine if and when a threshold is reached */
    val maybeUpdatedHouseState =
      house.zip(state.houseState).map { case (house, houseState) =>
        house.updateState(
          tick,
          houseState,
          ambientTemperature,
          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT)
        )
      }

    /* Update the state of the storage */
    val maybeUpdatedStorageState =
      storage.zip(state.storageState).map { case (storage, storageState) =>
        storage.updateState(tick, qDot, storageState)
      }

    val nextThreshold = determineMostRecentThreshold(
      maybeUpdatedHouseState.flatMap(_._2),
      maybeUpdatedStorageState.flatMap(_._2)
    )

    (
      state.copy(
        houseState = maybeUpdatedHouseState.map(_._1),
        storageState = maybeUpdatedStorageState.map(_._1)
      ),
      nextThreshold
    )
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
    val houseResults = house
      .zip(state.houseState)
      .map {
        case (
              thermalHouse,
              ThermalHouseState(tick, innerTemperature, thermalInfeed)
            ) =>
          Seq.empty[ResultEntity] :+ new ThermalHouseResult(
            tick.toDateTime,
            thermalHouse.uuid,
            thermalInfeed,
            innerTemperature
          )
      }
      .getOrElse(Seq.empty[ResultEntity])

    storage
      .zip(state.storageState)
      .map {
        case (
              storage: CylindricalThermalStorage,
              ThermalStorageState(tick, storedEnergy, qDot)
            ) =>
          houseResults :+ new CylindricalStorageResult(
            tick.toDateTime,
            storage.uuid,
            storedEnergy,
            qDot,
            storage.maxEnergyThreshold
              .divide(storedEnergy)
              .asType(classOf[Dimensionless])
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
      storages.headOption
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
      storageState: Option[ThermalStorageState]
  )

  def startingState(thermalGrid: ThermalGrid): ThermalGridState =
    ThermalGridState(
      thermalGrid.house.map(house => ThermalHouse.startingState(house)),
      thermalGrid.storage.map(_.startingState)
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
      required: ComparableQuantity[Energy],
      possible: ComparableQuantity[Energy]
  ) {
    def +(rhs: ThermalEnergyDemand): ThermalEnergyDemand = ThermalEnergyDemand(
      required.add(rhs.required),
      possible.add(rhs.possible)
    )

    def hasRequiredDemand: Boolean = required.isGreaterThan(
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
    )
    def hasAdditionalDemand: Boolean = possible.isGreaterThan(required)
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
        required: ComparableQuantity[Energy],
        possible: ComparableQuantity[Energy]
    ): ThermalEnergyDemand = {
      if (possible.isLessThan(required))
        new ThermalEnergyDemand(
          possible.to(StandardUnits.ENERGY_RESULT),
          possible.to(StandardUnits.ENERGY_RESULT)
        )
      else
        new ThermalEnergyDemand(
          required.to(StandardUnits.ENERGY_RESULT),
          possible.to(StandardUnits.ENERGY_RESULT)
        )
    }

    def noDemand: ThermalEnergyDemand = ThermalEnergyDemand(
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT),
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
    )
  }
}
