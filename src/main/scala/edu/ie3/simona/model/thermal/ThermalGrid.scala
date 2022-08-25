/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.util.UUID
import javax.measure.quantity.{Energy, Temperature}
import scala.jdk.CollectionConverters.SetHasAsScala

/** Calculation model for a thermal grid. It is assumed, that all elements are
  * connected directly with exactly one thermal bus
  *
  * @param houses
  *   Collection of all thermal houses connected to the bus
  * @param storages
  *   Collection of thermal storages
  */
case class ThermalGrid(
    houses: Set[ThermalHouse],
    storages: Set[ThermalStorage]
) {

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
  ): ComparableQuantity[Energy] = {
    /* First get the energy demand of the houses */
    val houseDemand =
      houses.foldLeft(Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)) {
        case (currentEnergy, house) =>
          val houseDemand = state.partState.get(house.uuid) match {
            case Some(houseState: ThermalHouseState) =>
              house.energyDemand(tick, ambientTemperature, houseState)
            case Some(_) || None =>
              throw new InconsistentStateException(
                s"Unable to find state for thermal house with uuid '${house.uuid}'."
              )
          }
          currentEnergy.add(houseDemand)
      }

    houseDemand
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
}
