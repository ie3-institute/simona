/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

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
import edu.ie3.simona.util.TickUtil.TickLong
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Energy, Power, Temperature}
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

    houseDemand
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
  ): ThermalGridState = {
    val updatedHouseStates = houses.map { house =>
      state.partState.get(house.uuid) match {
        case Some(houseState: ThermalHouseState) =>
          house.uuid -> house.updateState(
            tick,
            houseState,
            ambientTemperature,
            qDot
          )
        case _ =>
          throw new InconsistentStateException(
            s"Unable to find state for thermal house with uuid '${house.uuid}'."
          )
      }
    }.toMap

    ThermalGridState(updatedHouseStates)
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
