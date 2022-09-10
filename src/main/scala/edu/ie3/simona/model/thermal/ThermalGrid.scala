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
import edu.ie3.datamodel.models.result.thermal.ThermalHouseResult
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  LowerTemperatureReached,
  UpperTemperatureReached
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Energy, Power, Temperature}
import scala.annotation.tailrec
import scala.jdk.CollectionConverters.SetHasAsScala

/** Calculation model for a thermal grid. It is assumed, that all elements are
  * connected directly with exactly one thermal bus
  *
  * @param houses
  *   Collection of all thermal houses connected to the bus
  * @param storages
  *   Collection of thermal storages
  */
final case class ThermalGrid(
    houses: Set[ThermalHouse],
    storages: Set[ThermalStorage]
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
    val houseDemand =
      houses.foldLeft(ThermalEnergyDemand.noDemand) {
        case (currentDemand, house) =>
          state.partState.get(house.uuid) match {
            case Some(houseState: ThermalHouseState) =>
              currentDemand + house.energyDemand(
                tick,
                ambientTemperature,
                houseState
              )
            case _ =>
              throw new InconsistentStateException(
                s"Unable to find state for thermal house with uuid '${house.uuid}'."
              )
          }
      }

    /* Then go over the storages, see what they can provide and what they might be able to charge */
    val (storedEnergy, remainingCapacity) = storages.foldLeft(
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT),
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
    ) { case ((currentStoredEnergy, currentRemainingCapacity), storage) =>
      storage match {
        case cylindricalStorage @ CylindricalThermalStorage(
              _,
              _,
              _,
              _,
              _,
              storageVolumeLvlMax,
              _,
              inletTemp,
              returnTemp,
              c,
              _
            ) =>
          val usableEnergy = cylindricalStorage.usableThermalEnergy
          val remaining = CylindricalThermalStorage
            .volumeToEnergy(storageVolumeLvlMax, c, inletTemp, returnTemp)
            .subtract(usableEnergy)
          (
            currentStoredEnergy.add(usableEnergy),
            currentRemainingCapacity.add(remaining)
          )
        case _ => (currentStoredEnergy, currentRemainingCapacity)
      }
    }
    val usedEnergy =
      if (storedEnergy.isGreaterThanOrEqualTo(houseDemand.required))
        houseDemand.required
      else
        storedEnergy
    val finallyRemaining =
      remainingCapacity.add(storedEnergy.subtract(usedEnergy))

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
  ): ThermalGridState = if (
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
  ): ThermalGridState = {
    /* There is energy fed into the grid. Disperse it along the appliances */
    // TODO: qDot = 0 for all storages in first iteration

    /* Get the demand of the houses and calculate shares */
    val (updatedHouseState, nextTick) =
      heatUpHouses(tick, ambientTemperature, state.partState, qDot)

    /* Fill up the storages */
    // TODO

    ThermalGridState(updatedHouseState)
  }

  /** Recursively heat up all the houses, until all upper boundaries are reached
    *
    * @param tick
    *   Current tick
    * @param ambientTemperature
    *   Ambient temperature
    * @param houseState
    *   Current states of the houses
    * @param qDot
    *   Thermal infeed to the grid
    * @param updatedState
    *   Optional already updated state (only the first update needs to be handed
    *   back)
    * @return
    *   Updated state and tick of next threshold match
    */
  @tailrec
  private def heatUpHouses(
      tick: Long,
      ambientTemperature: ComparableQuantity[Temperature],
      houseState: Map[UUID, ThermalModelState],
      qDot: ComparableQuantity[Power],
      updatedState: Option[Map[UUID, ThermalModelState]] = None
  ): (Map[UUID, ThermalModelState], Long) = {
    val shares = houseShares(tick, ambientTemperature, houseState)
    houses.foldLeft(
      (Map.empty[UUID, ThermalHouseState], Long.MaxValue, Set.empty[UUID])
    ) { case ((updatedHouseStates, shortestTick, fullHouses), house) =>
      houseState.get(house.uuid) match {
        case Some(houseState: ThermalHouseState) =>
          val share = shares.getOrElse(house.uuid, 0)
          house.updateState(
            tick,
            houseState,
            ambientTemperature,
            qDot.multiply(share)
          ) match {
            case (updatedState, LowerTemperatureReached(_)) =>
              (
                updatedHouseStates + (house.uuid -> updatedState
                  .asInstanceOf[ThermalHouseState]),
                shortestTick,
                fullHouses + house.uuid
              )
            case (updatedState, UpperTemperatureReached(nextTick)) =>
              (
                updatedHouseStates + (house.uuid -> updatedState
                  .asInstanceOf[ThermalHouseState]),
                min(shortestTick, nextTick),
                fullHouses
              )
          }
        case _ =>
          throw new InconsistentStateException(
            s"Unable to find state for thermal house with uuid '${house.uuid}'."
          )
      }
    } match {
      case (updatedHouseStates, shortestTick, fullHouses) =>
        val housesWithCapacity =
          houses.filter(house => !fullHouses.contains(house.uuid))
        if (housesWithCapacity.isEmpty) {
          /* All houses are filled up, return the first updated state and the tick, where the last house is full */
          updatedState match {
            case Some(state) => (state, shortestTick)
            case None        => (updatedHouseStates, shortestTick)
          }
        } else {
          heatUpHouses(
            shortestTick,
            ambientTemperature,
            updatedHouseStates,
            qDot,
            if (updatedState.isDefined) updatedState
            else Some(updatedHouseStates)
          )
        }
    }
  }

  /** Determine the share of each house to the total energy demand of all houses
    * @param tick
    *   The questioned tick
    * @param ambientTemperature
    *   The ambient temperature at questioned tick
    * @param state
    *   The current state of the houses
    * @return
    *   The shares of each single house
    */
  private def houseShares(
      tick: Long,
      ambientTemperature: ComparableQuantity[Temperature],
      state: Map[UUID, ThermalModelState]
  ): Map[UUID, Double] = {
    val houseDemands = houses.map { house =>
      state.get(house.uuid) match {
        case Some(houseState: ThermalHouseState) =>
          house.uuid -> house.energyDemand(
            tick,
            ambientTemperature,
            houseState
          )
        case _ =>
          throw new InconsistentStateException(
            s"Unable to find state for thermal house with uuid '${house.uuid}'."
          )
      }
    }.toMap
    val totalDemand = houseDemands.values.reduce(_ + _)
    houseDemands.map { case (uuid, ThermalEnergyDemand(required, _)) =>
      uuid -> required
        .divide(totalDemand.required)
        .asType(classOf[Dimensionless])
        .to(PowerSystemUnits.PU)
        .getValue
        .doubleValue()
    }
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
  ): ThermalGridState = state

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
    val results = Seq.empty[ResultEntity]

    state.partState.foreach {
      case (uuid, ThermalHouseState(tick, innerTemperature, thermalInfeed)) =>
        results :+ new ThermalHouseResult(
          tick.toDateTime,
          uuid,
          thermalInfeed,
          innerTemperature
        )
      case (uuid, unsupported) =>
        logger.debug(
          s"The result handling for thermal state '${unsupported.getClass.getSimpleName}' of model '$uuid' is not handled yet."
        )
    }

    results
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
      houses,
      storages
    )
  }

  /** Current state of a grid
    * @param partState
    *   Mapping from model uuid to it's state
    */
  final case class ThermalGridState(partState: Map[UUID, ThermalModelState])

  def startingState(thermalGrid: ThermalGrid): ThermalGridState =
    ThermalGridState(
      thermalGrid.houses
        .map(house => house.uuid -> ThermalHouse.startingState(house))
        .toMap
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
        new ThermalEnergyDemand(possible, possible)
      else
        new ThermalEnergyDemand(required, possible)
    }

    def noDemand: ThermalEnergyDemand = ThermalEnergyDemand(
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT),
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
    )
  }
}
