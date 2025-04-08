/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.container.{ThermalGrid => PsdmThermalGrid}
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.{
  HpResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  ComplexPowerAndHeat,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.HpModel.{
  HpOperatingPoint,
  HpState,
  ThermalOpWrapper,
}
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.thermal.ThermalGrid
import edu.ie3.simona.model.thermal.ThermalGrid._
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.{
  zeroCelsius,
  zeroKW,
  zeroKWh,
}
import edu.ie3.util.scala.quantities._
import squants._
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util.UUID

class HpModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val pThermal: Power,
    private val thermalGrid: ThermalGrid,
) extends ParticipantModel[
      HpOperatingPoint,
      HpState,
    ]
    with LazyLogging {

  override def determineState(
      state: HpState,
      operatingPoint: HpOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): HpState = {

    // state.lastStateAmbientTemperature is now the temperature from over lastState, thus we have to update here
    val updatedHpState =
      state.copy(lastStateAmbientTemperature = state.ambientTemperature)

    val thermalGridState =
      thermalGrid.updatedThermalGridState(
        tick,
        updatedHpState,
        operatingPoint,
      )

    val thermalDemands = thermalGrid.determineEnergyDemand(thermalGridState)

    updatedHpState.copy(
      tick = tick,
      thermalGridState = thermalGridState,
      lastHpOperatingPoint = operatingPoint,
      thermalDemands = thermalDemands,
    )
  }

  override def handleInput(
      state: HpState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): HpState = {
    receivedData
      .collectFirst { case weatherData: WeatherData =>
        weatherData
      }
      .map(newData =>
        state.copy(
          ambientTemperature = newData.temp,
          lastStateAmbientTemperature = state.ambientTemperature,
        )
      )
      .getOrElse(state)
  }

  override def determineFlexOptions(
      state: HpState
  ): FlexOptions = {
    val wasRunningLastOp = state.lastHpOperatingPoint.activePower > zeroKW

    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) =
      operatesInNextState(
        state.thermalGridState,
        state.thermalDemands,
        wasRunningLastOp,
      )

    MinMaxFlexOptions(
      if (turnOn) sRated.toActivePower(cosPhiRated) else zeroKW,
      if (canBeOutOfOperation) zeroKW else sRated.toActivePower(cosPhiRated),
      if (canOperate) sRated.toActivePower(cosPhiRated) else zeroKW,
    )
  }

  override def zeroPowerOperatingPoint: HpOperatingPoint =
    HpOperatingPoint.zero

  /** Depending on the input, this function decides whether the heat pump will
    * run in the next state or not. The heat pump is foreseen to operate in the
    * next interval, if the thermal grid either has a demand that needs to be
    * met or the heat pump currently is in operation and the grid is able to
    * handle additional energy.
    *
    * @param thermalGridState
    *   State of the thermalGrid.
    * @param thermalDemands
    *   ThermalEnergyDemand of the house and the thermal storage.
    * @param wasRunningLastPeriod
    *   Indicates if the Hp was running till this tick.
    * @return
    *   Boolean defining if heat pump runs in next time step, if it can be in
    *   operation and can be out of operation.
    */
  private def operatesInNextState(
      thermalGridState: ThermalGridState,
      thermalDemands: ThermalDemandWrapper,
      wasRunningLastPeriod: Boolean,
  ): (Boolean, Boolean, Boolean) = {

    val demandHouse = thermalDemands.houseDemand
    val demandThermalStorage = thermalDemands.heatStorageDemand
    val noThermalStorageOrEmpty = thermalGridState.isThermalStorageEmpty

    val turnHpOn =
      (demandHouse.hasRequiredDemand && noThermalStorageOrEmpty) ||
        (demandHouse.hasPossibleDemand && wasRunningLastPeriod ||
          demandThermalStorage.hasRequiredDemand ||
          (demandThermalStorage.hasPossibleDemand && wasRunningLastPeriod))

    val canOperate =
      demandHouse.hasRequiredDemand || demandHouse.hasPossibleDemand ||
        demandThermalStorage.hasRequiredDemand || demandThermalStorage.hasPossibleDemand
    val canBeOutOfOperation =
      !(demandHouse.hasRequiredDemand && noThermalStorageOrEmpty)

    (
      turnHpOn,
      canOperate,
      canBeOutOfOperation,
    )
  }

  /** Depending on the input, this function calculates the next operating point
    * of the heat pump by determine the active power and thermal power (qDot)
    * provided by the heat pump.
    *
    * @param state
    *   Currently applicable HpState.
    * @param setPower
    *   The setPower from Em, if there is some.
    * @return
    *   The new active power of the heat pump and the thermal power (qDot) from
    *   the heat pump, feed into the thermal grid.
    */

  private def nextOperatingPoint(
      state: HpState,
      setPower: Option[Power],
  ): (Power, Power) = {
    val wasRunningLastOp = state.lastHpOperatingPoint.activePower > zeroKW

    val currentStorageEnergy =
      state.thermalGridState.storageState.map(_.storedEnergy).getOrElse(zeroKWh)
    val storagePThermal =
      thermalGrid.heatStorage.map(_.getpThermalMax).getOrElse(zeroKW)

    val (turnOn, _, _) =
      setPower match {
        case Some(value) =>
          /* If the set point value is above 50 % of the electrical power, turn on the heat pump otherwise turn it off */
          (
            value > (sRated.toActivePower(cosPhiRated) * 0.5),
            None,
            None,
          )
        case None =>
          operatesInNextState(
            state.thermalGridState,
            state.thermalDemands,
            wasRunningLastOp,
          )
      }

    val (newHpActivePower, _, qDotIntoGrid) = {
      if (turnOn)
        (pRated, pThermal, pThermal)
      else if (
        currentStorageEnergy > zeroKWh && state.thermalDemands.houseDemand.hasRequiredDemand
      ) {
        // If the house has req. demand and storage isn't empty, we can heat the house from storage.
        (zeroKW, zeroKW, storagePThermal)
      } else if (
        currentStorageEnergy > zeroKWh && state.thermalDemands.houseDemand.hasPossibleDemand && state.lastHpOperatingPoint.thermalOps.qDotHouse > zeroKW
      )
        // Edge case when em controlled: If the house was heated last state by Hp and setPower is below turnOn condition now,
        // but house didn't reach target or boundary temperature yet. House can be heated from storage, if this one is not empty.
        (zeroKW, zeroKW, storagePThermal)
      else (zeroKW, zeroKW, zeroKW)
    }

    (newHpActivePower, qDotIntoGrid)
  }

  override def createResults(
      state: HpState,
      lastOperatingPoint: Option[HpOperatingPoint],
      currentOperatingPoint: HpOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[ResultEntity] = {
    Iterable(
      new HpResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
        currentOperatingPoint.thermalOps.qDotHp.toMegawatts.asMegaWatt,
      )
    ) ++ thermalGrid.results(
      state,
      lastOperatingPoint,
      currentOperatingPoint,
      dateTime,
    )
  }

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult = {
    data match {
      case result: ComplexPowerAndHeat =>
        new HpResult(
          dateTime,
          uuid,
          result.p.toMegawatts.asMegaWatt,
          result.q.toMegavars.asMegaVar,
          result.qDot.toMegawatts.asMegaWatt,
        )
    }
  }

  /** Calculate the active power behaviour of the model.
    *
    * @param state
    *   The current state including weather data.
    * @return
    *   The active power.
    */

  override def determineOperatingPoint(
      state: HpState
  ): (HpOperatingPoint, Option[Long]) = {

    val (newActivePowerHp, qDotIntoGrid) = nextOperatingPoint(state, None)

    val (updateState, maybeThreshold) =
      /* Determine how qDot is used in thermalGrid and get threshold*/
      if (qDotIntoGrid > zeroKW) {
        thermalGrid.handleInfeed(
          state,
          newActivePowerHp > zeroKW,
          qDotIntoGrid,
          state.thermalDemands,
        )
      } else
        thermalGrid.handleConsumption(state)

    val operatingPoint =
      HpOperatingPoint(
        newActivePowerHp,
        ThermalOpWrapper(
          qDotIntoGrid,
          updateState.houseState.map(_.qDot).getOrElse(zeroKW),
          updateState.storageState.map(_.qDot).getOrElse(zeroKW),
        ),
      )

    val nextTick = maybeThreshold match {
      case Some(threshold) => Some(threshold.tick)
      case None            => None
    }

    (operatingPoint, nextTick)
  }

  override def determineOperatingPoint(
      state: HpState,
      setPower: Power,
  ): (HpOperatingPoint, OperationChangeIndicator) = {

    val (newActivePowerHp, qDotIntoGrid) =
      nextOperatingPoint(state, Some(setPower))

    val (updateState, maybeThreshold) =
      /* Determine how qDot is used in thermalGrid and get threshold*/
      if (qDotIntoGrid > zeroKW) {
        thermalGrid.handleInfeed(
          state,
          newActivePowerHp > zeroKW,
          qDotIntoGrid,
          state.thermalDemands,
        )
      } else
        thermalGrid.handleConsumption(state)

    val operatingPoint =
      HpOperatingPoint(
        newActivePowerHp,
        ThermalOpWrapper(
          qDotIntoGrid,
          updateState.houseState.map(_.qDot).getOrElse(zeroKW),
          updateState.storageState.map(_.qDot).getOrElse(zeroKW),
        ),
      )

    val nextTick = maybeThreshold match {
      case Some(threshold) => Some(threshold.tick)
      case None            => None
    }

    (
      operatingPoint,
      OperationChangeIndicator(
        changesAtNextActivation = true,
        changesAtTick = nextTick,
      ),
    )
  }
}

object HpModel {

  final case class HpOperatingPoint(
      override val activePower: Power,
      thermalOps: ThermalOpWrapper,
  ) extends OperatingPoint {
    override val reactivePower: Option[ReactivePower] = None
  }

  object HpOperatingPoint {
    def zero: HpOperatingPoint =
      HpOperatingPoint(zeroKW, ThermalOpWrapper.zero)
  }

  /** Wraps the thermal powers of the [[HpOperatingPoint]].
    *
    * @param qDotHp
    *   The thermal power output of the heat pump.
    * @param qDotHouse
    *   The thermal power input of the
    *   [[edu.ie3.simona.model.thermal.ThermalHouse]].
    * @param qDotHeatStorage
    *   The thermal power input of the
    *   [[edu.ie3.simona.model.thermal.ThermalStorage]].
    */
  final case class ThermalOpWrapper private (
      qDotHp: Power,
      qDotHouse: Power,
      qDotHeatStorage: Power,
  )
  object ThermalOpWrapper {
    def zero: ThermalOpWrapper = ThermalOpWrapper(zeroKW, zeroKW, zeroKW)
  }

  /** Holds all relevant data for a hp model calculation.
    *
    * @param tick
    *   The current tick.
    * @param ambientTemperature
    *   The actual outside temperature.
    * @param thermalGridState
    *   The applicable state of the [[ThermalGrid]].
    * @param lastHpOperatingPoint
    *   The last [[HpOperatingPoint]] of the heat pump.
    * @param lastStateAmbientTemperature
    *   The outside temperature at the lastState.
    * @param thermalDemands
    *   The actual thermal demands of the thermal grid elements (house,
    *   storage).
    */
  final case class HpState(
      override val tick: Long,
      ambientTemperature: Temperature,
      thermalGridState: ThermalGridState,
      lastHpOperatingPoint: HpOperatingPoint,
      lastStateAmbientTemperature: Temperature,
      thermalDemands: ThermalDemandWrapper,
  ) extends ModelState

  final case class Factory(
      input: HpInput,
      thermalGrid: PsdmThermalGrid,
  ) extends ParticipantModelFactory[HpState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.WeatherService)

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): HpState = {

      val therGrid = ThermalGrid(thermalGrid)
      val initialState = ThermalGrid.startingState(therGrid)
      val thermalDemand = therGrid.determineEnergyDemand(initialState)

      HpState(
        tick,
        zeroCelsius,
        initialState,
        HpOperatingPoint.zero,
        zeroCelsius,
        thermalDemand,
      )
    }

    override def create(): HpModel =
      new HpModel(
        input.getUuid,
        input.getId,
        Kilovoltamperes(
          input.getType.getsRated
            .to(PowerSystemUnits.KILOVOLTAMPERE)
            .getValue
            .doubleValue
        ),
        input.getType.getCosPhiRated,
        QControl(input.getqCharacteristics),
        Kilowatts(
          input.getType
            .getpThermal()
            .to(PowerSystemUnits.KILOWATT)
            .getValue
            .doubleValue
        ),
        ThermalGrid(thermalGrid),
      )
  }
}
