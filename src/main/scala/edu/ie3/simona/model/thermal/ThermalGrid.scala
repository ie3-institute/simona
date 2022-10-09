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
        handleTooLowInfeed(
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
        takeOrPushToStorage(tick, shutOffStorageState, qDot) match {
          case Some((updatedStorageState, Some(StorageFull(storageTick)))) =>
            (
              ThermalGridState(None, Some(updatedStorageState)),
              Some(ThermalGridFilledUp(storageTick))
            )
          case _ =>
            throw new InconsistentStateException(
              "Found inconsistent state after filling storage"
            )
        }
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
  def handleTooLowInfeed(
      initialTick: Long,
      coldHouseTick: Long,
      initialHouseState: ThermalHouseState,
      initialStorageState: Option[ThermalStorageState],
      coldHouseState: ThermalHouseState,
      ambientTemperature: ComparableQuantity[Temperature],
      qDot: ComparableQuantity[Power]
  ): (ThermalGridState, Option[ThermalGridThreshold]) = {
    /* Determine the needed additional power from storage */
    house
      .map { house =>
        /* Determine which energy was needed to heat up to maximum limit at the beginning and at the end */
        val initialDemand = house
          .energyDemand(initialTick, ambientTemperature, initialHouseState)
          .possible
        val resultingDemand = house
          .energyDemand(coldHouseTick, ambientTemperature, coldHouseState)
          .required

        /* Determine, how the energy did evolve and how high the thermal losses are */
        val energyBalance = initialDemand.subtract(resultingDemand)
        val duration =
          Quantities.getQuantity(coldHouseTick - initialTick, Units.SECOND)
        val externalEnergy = qDot.multiply(duration).asType(classOf[Energy])
        val loss = energyBalance
          .subtract(externalEnergy)
          .divide(duration)
          .asType(classOf[Power])

        storage.zip(initialStorageState) match {
          case Some((str, strState)) =>
            /* Determine the available energy in the storage and the duration, when the needed energy matches exactly
             * the available energy */
            val availableEnergy = strState.storedEnergy
            val targetDuration = availableEnergy
              .subtract(initialDemand)
              .divide(loss.subtract(qDot))
              .asType(classOf[Time])
            val targetDischargePower =
              availableEnergy.divide(targetDuration).asType(classOf[Power])

            /* Check, if this is possible and curtail power if needed */
            val dischargeQDot =
              if (targetDischargePower.isGreaterThan(str.getChargingPower))
                str.getChargingPower
              else targetDischargePower

            /* Re-calculate the house state with the additional influx from storage */
            val (reCalculatedHouseState, houseThreshold) = house.updateState(
              initialTick,
              initialHouseState,
              ambientTemperature,
              qDot.add(dischargeQDot)
            )
            val (reCalculatedStorageState, storageThreshold) =
              str.updateState(initialTick, qDot.multiply(-1), strState)

            val gridThreshold = (houseThreshold, storageThreshold) match {
              case (
                    Some(LowerTemperatureReached(houseTick)),
                    Some(StorageEmpty(storageTick))
                  ) =>
                /* Still insufficient feed in */
                Some(ThermalGridEmpty(min(houseTick, storageTick)))
              case (
                    Some(UpperTemperatureReached(houseTick)),
                    Some(StorageEmpty(storageTick))
                  ) =>
                /* House can be heated up. The next threshold is reached, when the house is cold again. */
                if (storageTick < houseTick)
                  throw new InconsistentStateException(
                    "The storage is not meant to be empty before the house is hot."
                  )

                house
                  .updateState(
                    houseTick,
                    reCalculatedHouseState,
                    ambientTemperature,
                    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
                  )
                  ._2 match {
                  case Some(LowerTemperatureReached(houseColdTick)) =>
                    Some(ThermalGridEmpty(houseColdTick))
                  case a =>
                    throw new InconsistentStateException(
                      s"The house cannot reach state $a if it only cools down."
                    )
                }
              case (a, b) =>
                throw new InconsistentStateException(
                  s"Found an inconsistent state: $a, $b"
                )
            }

            (
              ThermalGridState(
                Some(reCalculatedHouseState),
                Some(reCalculatedStorageState)
              ),
              gridThreshold
            )
          case None =>
            /* There is no storage, we cannot add any power from there */
            (
              ThermalGridState(Some(coldHouseState), None),
              Some(ThermalGridEmpty(coldHouseTick))
            )
        }
      }
      .getOrElse(
        throw new InconsistentStateException(
          "This place can only be reached, if a house has been found earlier."
        )
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
  ): (ThermalGridState, Option[ThermalGridThreshold]) = {
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

    (
      maybeUpdatedHouseState.flatMap(_._2),
      maybeUpdatedStorageState.flatMap(_._2)
    ) match {
      case (None, None) =>
        /* Neither house, nor storage will reach any boundary */
        (
          ThermalGridState(
            maybeUpdatedHouseState.map(_._1),
            maybeUpdatedStorageState.map(_._1)
          ),
          None
        )
      case (_, Some(StorageFull(_))) =>
        throw new IllegalStateException(
          "When discharging from storage, upper threshold cannot be reached!"
        )
      case (Some(LowerTemperatureReached(coldHouseTick)), None) =>
        /* The house is cold sometime, but the storage is in perfect balance. Take energy from the storage to heat up the house */
        handleTooLowInfeed(
          tick,
          coldHouseTick,
          state.houseState.getOrElse(
            throw new IllegalStateException(
              "This place can only be reached if a house state has been found earlier"
            )
          ),
          maybeUpdatedStorageState.map(_._1),
          maybeUpdatedHouseState
            .map(_._1)
            .getOrElse(
              throw new IllegalStateException(
                "This place can only be reached if a house state has been found earlier"
              )
            ),
          ambientTemperature,
          Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
        )
      case (
            Some(LowerTemperatureReached(houseTick)),
            Some(StorageEmpty(storageTick))
          ) =>
        /* Storage and house will be empty */
        (
          ThermalGridState(
            maybeUpdatedHouseState.map(_._1),
            maybeUpdatedStorageState.map(_._1)
          ),
          Some(ThermalGridEmpty(min(houseTick, storageTick)))
        )
      case (None, Some(StorageEmpty(storageEmptyTick))) =>
        /* There is only a storage in this grid */
        (
          ThermalGridState(None, maybeUpdatedStorageState.map(_._1)),
          Some(ThermalGridEmpty(storageEmptyTick))
        )
      case _ =>
        /* Irrelevant case, that won't lead to any additional triggering */
        (
          ThermalGridState(
            maybeUpdatedHouseState.map(_._1),
            maybeUpdatedStorageState.map(_._1)
          ),
          None
        )
    }
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
