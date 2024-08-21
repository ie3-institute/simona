/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.thermal.CylindricalStorageInput
import squants.Dimensionless
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  ThermalHouseResult,
}
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalEnergyDemand,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseState
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.Kilowatts
import squants.{Each, Energy, Power, Temperature}

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
    * time
    * @param tick
    *   Questioned instance in time
    * @param lastAmbientTemperature
    *   Ambient temperature in the instance in question until actual tick
    * @param actualAmbientTemperature
    *   Ambient temperature in the instance in question at tick
    * @param state
    *   Currently applicable state of the thermal grid
    * @return
    *   The total energy demand of the grid
    */
  def energyDemand(
      tick: Long,
      // FIXME this is also in state -> refactoring by HiWi
      lastAmbientTemperature: Temperature,
      actualAmbientTemperature: Temperature,
      state: ThermalGridState,
  ): ThermalEnergyDemand = {
    /* First get the energy demand of the houses but only if inner temperature is below target temperature */

    val houseDemand =
      house.zip(state.houseState).headOption match {
        case Some((thermalHouse, lastHouseState)) =>
          val (updatedHouseState, updatedStorageState) =
            thermalHouse.determineState(
              tick,
              lastHouseState,
              lastAmbientTemperature,
              lastHouseState.qDot,
            )
          if (
            updatedHouseState.innerTemperature < thermalHouse.targetTemperature
          ) {
            thermalHouse.energyDemand(
              tick,
              actualAmbientTemperature,
              updatedHouseState,
            )

          } else {
            ThermalEnergyDemand.noDemand
          }

        case None =>
          ThermalEnergyDemand.noDemand
      }

    /* Then go over the storages, see what they can provide and what they might be able to charge */
    val (storedEnergy, remainingCapacity) = {
      storage
        .zip(state.storageState)
        .map { case (storage, state) =>
          val updatedStorageState =
            storage.updateState(tick, state.qDot, state)._1
          val usableEnergy = updatedStorageState.storedEnergy
          val remaining = storage.getMaxEnergyThreshold - usableEnergy
          (
            usableEnergy,
            remaining,
          )
        }
        .getOrElse(
          (zeroMWH, zeroMWH)
        )
    }

    val usedEnergy =
      if (storedEnergy >= houseDemand.required)
        houseDemand.required
      else
        storedEnergy
    val finallyRemaining = remainingCapacity + usedEnergy

    ThermalEnergyDemand(
      houseDemand.required - usedEnergy,
      houseDemand.possible + finallyRemaining,
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
      ambientTemperature: Temperature,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = if (qDot > zeroKW)
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
      ambientTemperature: Temperature,
      state: ThermalGridState,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {

    val (qDotHouseLastState, qDotStorageLastState) = determineLastStateQDots(
      state
    )

    val actualThermalStorageSoc = determineThermalStorageSoc(tick, state, qDot)

    // Storage was charged in the lastState and is not full
    if (qDotStorageLastState > zeroKW && actualThermalStorageSoc < Each(1.0)) {
      continuePushingInfeedIntoStorage(tick, state, ambientTemperature, qDot)
    }
    // Storage was not charged in the last state or is full
    else {
      pushInfeedIntoHouseFirst(tick, state, ambientTemperature, qDot)
    }
  }

  /** Determines the power infeed or withdraw of house and storage in the last
    * state
    *
    * @param lastState
    *   The last state of the ThermalGrid
    * @return
    *   The qDot of house and storage in the last state
    */

  private def determineLastStateQDots(
      lastState: ThermalGridState
  ): (Power, Power) = {

    (lastState.houseState, lastState.storageState) match {
      case (Some(house), Some(storage)) => (house.qDot, storage.qDot)
      case (Some(house), None)          => (house.qDot, zeroKW)
      case (None, Some(storage))        => (zeroKW, storage.qDot)
      case (None, None)                 => (zeroKW, zeroKW)
    }
  }

  /** Determines the actual state of charge (SoC) of the thermal storage.
    *
    * @param tick
    *   Instance in time
    * @param lastState
    *   The last applicable state
    * @param qDot
    *   Thermal infeed (+) or withdraw (-)
    * @return
    *   The relative state of charge
    */

  private def determineThermalStorageSoc(
      tick: Long,
      lastState: ThermalGridState,
      qDot: Power,
  ): Dimensionless = {

    if (storage.nonEmpty && lastState.storageState.nonEmpty) {
      val (thermalStorage, lastStorageState) =
        storage.zip(lastState.storageState).head
      val (updatedStorageState, maybeStorageThreshold) =
        thermalStorage.updateState(
          tick,
          qDot,
          lastStorageState,
        )
      Each(
        updatedStorageState.storedEnergy / thermalStorage.getMaxEnergyThreshold
      )
    } else {
      Each(1.0)
    }
  }

  /** This can be used to continue pushing infeed into the storage. Based on the
    * lastState the same qDot will be used to charge the thermal storage, update
    * its state and retunr the next threshold.
    *
    * @param tick
    *   Instance in time
    * @param lastState
    *   The last applicable state
    * @param ambientTemperature
    *   Ambient temperature
    * @param qDot
    *   Thermal infeed (+) or withdraw (-)
    * @return
    *   Updated ThermalGridState and the next thermal threshold
    */

  private def continuePushingInfeedIntoStorage(
      tick: Long,
      lastState: ThermalGridState,
      ambientTemperature: Temperature,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    storage.zip(lastState.storageState) match {
      case Some((thermalStorage, lastStorageState)) =>
        val (
          updatedHouseState: Option[ThermalHouseState],
          maybeHouseThreshold: Option[ThermalThreshold],
        ) =
          /* Set thermal power exchange with house to zero */
          house.zip(lastState.houseState) match {
            case Some((thermalHouse, houseState)) =>
              val nextHouseState =
                thermalHouse.determineState(
                  tick,
                  houseState,
                  ambientTemperature,
                  zeroKW,
                )
              (Some(nextHouseState._1), nextHouseState._2)

            case _ => lastState.houseState
          }

        val (updatedStorageState, maybeStorageThreshold) =
          thermalStorage.updateState(
            tick,
            qDot,
            lastStorageState,
          )

        /* Both house and storage are updated. Determine what reaches the next threshold */
        val nextThreshold = determineMostRecentThreshold(
          maybeHouseThreshold,
          maybeStorageThreshold,
        )

        (
          lastState.copy(
            houseState = updatedHouseState,
            storageState = Some(updatedStorageState),
          ),
          nextThreshold,
        )

      case None =>
        storage.zip(lastState.storageState) match {
          case Some((thermalStorage, storageState)) =>
            val (updatedStorageState, maybeStorageThreshold) =
              thermalStorage.updateState(tick, qDot, storageState)
            (
              lastState.copy(storageState = Some(updatedStorageState)),
              maybeStorageThreshold,
            )
          case None =>
            throw new InconsistentStateException(
              "A thermal grid has to contain either at least a house or a storage."
            )
        }
    }
  }

  /** Manages the thermal infeed into the house (first) or in case the house is
    * already heated up fully, into the thermal storage.
    *
    * @param tick
    *   Instance in time
    * @param lastState
    *   The last applicable state
    * @param ambientTemperature
    *   Ambient temperature
    * @param qDot
    *   Thermal infeed (+) or withdraw (-)
    * @return
    *   Updated ThermalGridState and the next thermal threshold
    */

  private def pushInfeedIntoHouseFirst(
      tick: Long,
      lastState: ThermalGridState,
      ambientTemperature: Temperature,
      qDot: Power,
  ): (ThermalGridState, Option[ThermalThreshold]) = {
    house.zip(lastState.houseState) match {
      case Some((thermalHouse, lastHouseState)) =>
        /* Set thermal power exchange with storage to zero */
        // TODO: We would need to issue a storage result model here...
        val updatedStorageState = storage.zip(lastState.storageState) match {
          case Some((thermalStorage, storageState)) =>
            Some(
              thermalStorage
                .updateState(
                  tick,
                  zeroKW,
                  storageState,
                )
                ._1
            )
          case _ => lastState.storageState
        }

        val (updatedHouseState, maybeHouseThreshold) =
          thermalHouse.determineState(
            tick,
            lastHouseState,
            ambientTemperature,
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
              tick,
              lastHouseState,
              ambientTemperature,
              zeroKW,
            )
          storage.zip(updatedStorageState) match {
            case Some((thermalStorage, storageState)) =>
              val (updatedStorageState, maybeStorageThreshold) =
                thermalStorage.updateState(tick, qDot, storageState)

              /* Both house and storage are updated. Determine what reaches the next threshold */
              val nextThreshold = determineMostRecentThreshold(
                maybeFullHouseThreshold,
                maybeStorageThreshold,
              )

              (
                lastState.copy(
                  houseState = Some(fullHouseState),
                  storageState = Some(updatedStorageState),
                ),
                nextThreshold,
              )
            case None =>
              /* There is no storage, house determines the next activation */
              (
                lastState.copy(houseState = Some(fullHouseState)),
                maybeFullHouseThreshold,
              )
          }
        } else {
          /* The house can handle the infeed */
          (
            lastState.copy(houseState = Some(updatedHouseState)),
            maybeHouseThreshold,
          )
        }

      case None =>
        storage.zip(lastState.storageState) match {
          case Some((thermalStorage, storageState)) =>
            val (updatedStorageState, maybeStorageThreshold) =
              thermalStorage.updateState(tick, qDot, storageState)
            (
              lastState.copy(storageState = Some(updatedStorageState)),
              maybeStorageThreshold,
            )
          case None =>
            throw new InconsistentStateException(
              "A thermal grid has to contain either at least a house or a storage."
            )
        }
    }
  }

  /** Determines the most recent threshold of two given input thresholds
    *
    * @param maybeHouseThreshold
    *   Option of a possible next threshold of the thermal house
    * @param maybeStorageThreshold
    *   Option of a possible next threshold of the thermal storage
    * @return
    *   The next threshold
    */

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
    * @param ambientTemperature
    *   Ambient temperature
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
       At the moment we write an storage result when the house result gets updated and vice versa.
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
