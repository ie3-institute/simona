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
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridThreshold.{
  ThermalGridEmpty,
  ThermalGridFilledUp
}
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState,
  ThermalGridThreshold
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  LowerTemperatureReached,
  UpperTemperatureReached
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalHouseThreshold.{
  StorageEmpty,
  StorageFull
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
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
        .map {
          case (cylindricalStorage: CylindricalThermalStorage, state) =>
            val usableEnergy = cylindricalStorage.usableThermalEnergy
            val remaining = cylindricalStorage.maxEnergyThreshold
              .subtract(usableEnergy)
            (
              usableEnergy,
              remaining
            )
          case _ =>
            (
              Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT),
              Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
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
  ): (ThermalGridState, Option[ThermalGridThreshold]) = if (
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
  ): (ThermalGridState, Option[ThermalGridThreshold]) = {
    /* There is energy fed into the grid. Disperse it along the appliances. Priority is on the houses, therefore shut
     * off storages first */
    val shutOffStorageState =
      storage.zip(state.storageState).map { case (storage, previousState) =>
        storage
          .updateState(
            tick,
            Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN),
            previousState
          )
          ._1
      }

    /* Get the demand of the houses and calculate shares */
    house.zip(state.houseState).map { case (house, houseState) =>
      house.updateState(
        tick,
        houseState,
        ambientTemperature,
        qDot
      )
    } match {
      case Some(
            (coldHouseState, Some(LowerTemperatureReached(houseThresholdTick)))
          ) =>
        /* There is a house apparent, but the thermal influx is not enough to compensate the losses. Take energy from
         * storage */
        storage
          .zip(shutOffStorageState)
          .map { case (storage, previousState) =>
            val dischargeQDot = qDot.multiply(
              -1
            ) // TODO: Define maximum charge / discharge power
            storage.updateState(
              houseThresholdTick,
              dischargeQDot,
              previousState
            ) match {
              case (
                    emptiedStorageState,
                    Some(StorageEmpty(storageEmptyTick))
                  ) =>
                /* Update house state with added energy from storage */
                house.zip(state.houseState).map { case (house, houseState) =>
                  house.updateState(
                    tick,
                    houseState,
                    ambientTemperature,
                    dischargeQDot.multiply(-1)
                  )
                } match {
                  case Some(
                        (
                          refreshedHouseState,
                          Some(UpperTemperatureReached(veryNextTick))
                        )
                      ) =>
                    /* The house is will be all heated up. Wait until it is cold again. */
                    house.map(
                      _.updateState(
                        veryNextTick,
                        refreshedHouseState,
                        ambientTemperature,
                        qDot
                      )
                    ) match {
                      case Some(
                            (
                              refreshedHouseState,
                              Some(LowerTemperatureReached(veryNextTick))
                            )
                          ) =>
                        /* The house is all heated up. Wait until it is cold again. */
                        (
                          ThermalGridState(
                            Some(refreshedHouseState),
                            Some(emptiedStorageState)
                          ),
                          Some(ThermalGridEmpty(veryNextTick))
                        )
                      case _ =>
                        throw new InconsistentStateException(
                          "Exceptional state found."
                        )
                    }

                  case Some(
                        (
                          refreshedHouseState,
                          Some(LowerTemperatureReached(houseColdTick))
                        )
                      ) =>
                    /* Storage will be empty, house will be cold. So grid in total is empty */
                    (
                      ThermalGridState(
                        Some(refreshedHouseState),
                        Some(emptiedStorageState)
                      ),
                      Some(
                        ThermalGridEmpty(min(houseColdTick, storageEmptyTick))
                      )
                    )
                  case Some((refreshedHouseState, None)) =>
                    /* Storage is empty, but the house is in perfect balance. Next tick is when the storage is empty. */
                    (
                      ThermalGridState(
                        Some(refreshedHouseState),
                        Some(emptiedStorageState)
                      ),
                      Some(ThermalGridEmpty(storageEmptyTick))
                    )
                }
            }
          }
          .getOrElse {
            /* There is no storage and the house is cold, so the grid is empty in total */
            (
              ThermalGridState(
                Some(coldHouseState),
                state.storageState
              ),
              Some(ThermalGridEmpty(houseThresholdTick))
            )
          }
      case Some(
            (
              updatedHouseState,
              Some(UpperTemperatureReached(houseThresholdTick))
            )
          ) =>
        /* There is a house and the house is heated up to the maximum temperature. Fill the storage */
        val updatedStorageState = storage.zip(shutOffStorageState).map {
          case (storage, previousState) =>
            storage.updateState(
              houseThresholdTick,
              qDot,
              previousState
            )
        }
        val reason: Option[ThermalGridThreshold] = updatedStorageState match {
          case Some((_, Some(thermalStorageThreshold))) =>
            /* Some storage threshold is reached */
            Some(ThermalGridFilledUp(thermalStorageThreshold.tick))
          case Some((_, None)) =>
            /* Storage can load until eternity */
            None
          case None =>
            /* There is no storage. Grid is fully filled up, when the house is filled up */
            Some(ThermalGridFilledUp(houseThresholdTick))
        }
        (
          ThermalGridState(
            Some(updatedHouseState),
            updatedStorageState.map(_._1)
          ),
          reason
        )
      case Some((updatedHouseState, None)) =>
        /* The house neither does heat up nor cool down, will stay in this state forever */
        (ThermalGridState(Some(updatedHouseState), state.storageState), None)
      case None =>
        /* There is no house at all. Fill up the storage directly, if one is apparent. */
        val updatedStorageState = storage.zip(shutOffStorageState).map {
          case (storage, previousState) =>
            storage.updateState(
              tick,
              qDot,
              previousState
            )
        }
        (ThermalGridState(None, updatedStorageState.map(_._1)), None)
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
  ): (ThermalGridState, Option[ThermalGridThreshold]) = ???

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

    house.zip(state.houseState).map {
      case (
            thermalHouse,
            ThermalHouseState(tick, innerTemperature, thermalInfeed)
          ) =>
        results :+ new ThermalHouseResult(
          tick.toDateTime,
          thermalHouse.uuid,
          thermalInfeed,
          innerTemperature
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
        new ThermalEnergyDemand(possible, possible)
      else
        new ThermalEnergyDemand(required, possible)
    }

    def noDemand: ThermalEnergyDemand = ThermalEnergyDemand(
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT),
      Quantities.getQuantity(0d, StandardUnits.ENERGY_RESULT)
    )
  }

  sealed trait ThermalGridThreshold {
    val tick: Long
  }

  object ThermalGridThreshold {
    final case class ThermalGridFilledUp(override val tick: Long)
        extends ThermalGridThreshold
    final case class ThermalGridEmpty(override val tick: Long)
        extends ThermalGridThreshold
  }
}
