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
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalHouseThreshold.StorageEmpty
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.util.TickUtil.TickLong
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
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

    /* Heat up the house, until any threshold is met */
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
        HandleTooLowInfeed(
          tick,
          houseThresholdTick,
          state.houseState.getOrElse(
            throw new InconsistentStateException(
              "Unable to get house state, although this place can only be reached, if state was found earlier."
            )
          ),
          shutOffStorageState,
          coldHouseState,
          ambientTemperature,
          qDot
        )
      case Some(
            (
              updatedHouseState,
              Some(UpperTemperatureReached(houseThresholdTick))
            )
          ) =>
        /* There is a house and the house is heated up to the maximum temperature. Fill the storage */
        handleSurplusInfeed(
          shutOffStorageState,
          houseThresholdTick,
          updatedHouseState,
          qDot
        )
      case Some((updatedHouseState, None)) =>
        /* The house neither does heat up nor cool down, will stay in this state forever */
        (ThermalGridState(Some(updatedHouseState), state.storageState), None)
      case None =>
        /* There is no house at all. Fill up the storage directly, if one is apparent. */
        val updatedStorageState =
          takeOrPushToStorage(tick, shutOffStorageState, qDot)
        (ThermalGridState(None, updatedStorageState.map(_._1)), None)
    }
  }

  private def takeOrPushToStorage(
      tick: Long,
      storageState: Option[ThermalStorageState],
      qDot: ComparableQuantity[Power]
  ): Option[
    (ThermalStorageState, Option[ThermalStorage.ThermalStorageThreshold])
  ] = storage.zip(storageState).map { case (storage, previousState) =>
    storage.updateState(
      tick,
      qDot,
      previousState
    )
  }

  /** Handle too low infeed into the thermal grid resulting in a house, that
    * reaches it's lowest temperature boundary. Try to heat up the use, by
    * topping up the infeed of the grid with the maximum discharge power of the
    * thermal storage if one is apparent.
    *
    * @param initialTick
    *   Initial tick, when the new too low infeed has been set
    * @param coldHouseTick
    *   Tick in which the houses reaches it's lowest temperature boundary
    * @param initialHouseState
    *   State of the house, when too low infeed started
    * @param initialStorageState
    *   State of the storage, when too low infeed started
    * @param coldHouseState
    *   State of the cold house
    * @param ambientTemperature
    *   Ambient temperature
    * @param qDot
    *   (Too low) thermal influx
    * @return
    *   The thermal grid state after all countermeasures as well as the
    *   threshold, that is reached then
    */
  private def HandleTooLowInfeed(
      initialTick: Long,
      coldHouseTick: Long,
      initialHouseState: ThermalHouseState,
      initialStorageState: Option[ThermalStorageState],
      coldHouseState: ThermalHouseState,
      ambientTemperature: ComparableQuantity[Temperature],
      qDot: ComparableQuantity[Power]
  ): (ThermalGridState, Some[ThermalGridThreshold]) = storage
    .zip(initialStorageState)
    .map { case (storage, previousState) =>
      val dischargeQDot = storage.getChargingPower.multiply(
        -1
      )

      /* If a storage is apparent, discharge it with the maximum possible power to top up the external influx */
      storage.updateState(
        coldHouseTick,
        dischargeQDot,
        previousState
      ) match {
        case (
              emptiedStorageState,
              Some(StorageEmpty(storageEmptyTick))
            ) =>
          /* Update house state with added energy from storage */
          heatHouseWithAdditionalInfeedFromStorage(
            initialTick,
            initialHouseState,
            storageEmptyTick,
            emptiedStorageState,
            ambientTemperature,
            qDot,
            dischargeQDot.multiply(-1)
          )
        case (_, unsupportedThreshold) =>
          throw new IllegalStateException(
            s"The given threshold '$unsupportedThreshold' is not supported after discharging the thermal storage."
          )
      }
    }
    .getOrElse {
      /* There is no storage and the house is cold, so the grid is empty in total */
      (
        ThermalGridState(
          Some(coldHouseState),
          initialStorageState
        ),
        Some(ThermalGridEmpty(coldHouseTick))
      )
    }

  /** Heat up the house with the additional help of discharging the storage. If
    * discharging helped to heat up the house to the maximum level, wait until
    * the house has reached the lowest boundary once again.
    * @param initialTick
    *   Initial tick, when too low external influx started
    * @param initialHouseState
    *   State of the house at that tick
    * @param emptyStorageTick
    *   Tick in which the storage will be empty
    * @param emptyStorageState
    *   State of the empty storage
    * @param ambientTemperature
    *   Ambient temperature
    * @param qDot
    *   (Too low) thermal influx
    * @param qDotStorage
    *   Thermal influx from storage
    * @return
    *   The state of the grid and the threshold, that has been reached
    */
  private def heatHouseWithAdditionalInfeedFromStorage(
      initialTick: Long,
      initialHouseState: ThermalHouseState,
      emptyStorageTick: Long,
      emptyStorageState: ThermalStorageState,
      ambientTemperature: ComparableQuantity[Temperature],
      qDot: ComparableQuantity[Power],
      qDotStorage: ComparableQuantity[Power]
  ): (ThermalGridState, Some[ThermalGridThreshold]) =
    house.map(_ -> initialHouseState).map { case (house, houseState) =>
      house.updateState(
        initialTick,
        houseState,
        ambientTemperature,
        qDotStorage.add(qDot)
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
                Some(emptyStorageState)
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
            Some(emptyStorageState)
          ),
          Some(
            ThermalGridEmpty(min(houseColdTick, emptyStorageTick))
          )
        )
      case Some((refreshedHouseState, None)) =>
        /* Storage is empty, but the house is in perfect balance. Next tick is when the storage is empty. */
        (
          ThermalGridState(
            Some(refreshedHouseState),
            Some(emptyStorageState)
          ),
          Some(ThermalGridEmpty(emptyStorageTick))
        )
      case None =>
        throw new InconsistentStateException(
          "Error while handling too low thermal infeed. No house found, although this place can only be reached with a house."
        )
    }

  /** Handle the additional infeed, after the house has been heated up until the
    * maximum temperature has been reached
    * @param initialStorageState
    *   Initial state of the storage
    * @param hotHouseTick
    *   Tick, in which the house has reached it's maximum temperature
    * @param hotHouseState
    *   State of the heated up house
    * @param qDot
    *   Thermal influx
    * @return
    *   State of the grid after storing the remainder energy and the threshold
    *   that has been reached
    */
  private def handleSurplusInfeed(
      initialStorageState: Option[ThermalStorageState],
      hotHouseTick: Long,
      hotHouseState: ThermalHouseState,
      qDot: ComparableQuantity[Power]
  ): (ThermalGridState, Option[ThermalGridThreshold]) = {
    val updatedStorageState =
      takeOrPushToStorage(hotHouseTick, initialStorageState, qDot)
    val reason: Option[ThermalGridThreshold] = updatedStorageState match {
      case Some((_, Some(thermalStorageThreshold))) =>
        /* Some storage threshold is reached */
        Some(ThermalGridFilledUp(thermalStorageThreshold.tick))
      case Some((_, None)) =>
        /* Storage can load until eternity */
        None
      case None =>
        /* There is no storage. Grid is fully filled up, when the house is filled up */
        Some(ThermalGridFilledUp(hotHouseTick))
    }
    (
      ThermalGridState(
        Some(hotHouseState),
        updatedStorageState.map(_._1)
      ),
      reason
    )
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
