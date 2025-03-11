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
import edu.ie3.simona.model.participant2.HpModel.HpState
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerAndHeatOperatingPoint,
  ModelState,
  OperationChangeIndicator,
}
import edu.ie3.simona.model.thermal.ThermalGrid._
import edu.ie3.simona.model.thermal.{ThermalGrid, ThermalThreshold}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import edu.ie3.util.scala.quantities._
import squants._
import squants.energy.Kilowatts
import squants.space.Degrees
import squants.thermal.Celsius

import java.time.ZonedDateTime
import java.util.UUID

class HpModel private (
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val lat: Angle,
    private val lon: Angle,
    private val pThermal: Power,
    private val thermalGrid: ThermalGrid,
) extends ParticipantModel[
      ActivePowerAndHeatOperatingPoint,
      HpState,
    ]
    with LazyLogging {

  override val initialState: (Long, ZonedDateTime) => HpState = { (tick, _) =>
    val preHpState = HpState(
      tick,
      Celsius(0d),
      ThermalFlowWrapper(zeroKW, zeroKW),
        ThermalGridState(
          startingState(thermalGrid).houseState,
          startingState(thermalGrid).storageState,
        ),
        Celsius(0d),
        ThermalDemandWrapper(
          ThermalEnergyDemand(zeroKWh, zeroKWh),
          ThermalEnergyDemand(zeroKWh, zeroKWh),
        ),
      )

    val (thermalDemands, _) =
      thermalGrid.energyDemandAndUpdatedState(tick, preHpState)

    preHpState.copy(thermalDemands = thermalDemands)
  }

  override def determineState(
      state: HpState,
      operatingPoint: ActivePowerAndHeatOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): HpState = {
    val (thermalDemands, thermalGridState) =
      thermalGrid.energyDemandAndUpdatedState(tick, state)

    state.copy(
      tick = tick,
      thermalGridState = thermalGridState,
      lastAmbientTemperature = state.ambientTemperature,
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
          ambientTemperature = newData.temp
        )
      )
      .getOrElse(state)
  }

  override def determineFlexOptions(
      state: HpState
  ): FlexibilityMessage.ProvideFlexOptions = {
    // Use state to update thermalGrid to the current tick

    val lastHouseQDot =
      state.thermalGridState.houseState.map(_.qDot).getOrElse(zeroKW)
    val lastStorageQDot =
      state.thermalGridState.storageState.map(_.qDot).getOrElse(zeroKW)
    val wasRunningLastState = ((lastHouseQDot + lastStorageQDot) > zeroKW)

    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) =
      operatesInNextState(
        state.thermalGridState,
        state.thermalDemands,
        wasRunningLastState,
      )

    ProvideMinMaxFlexOptions(
      uuid,
      if (turnOn) sRated.toActivePower(cosPhiRated) else zeroKW,
      if (canBeOutOfOperation) zeroKW else sRated.toActivePower(cosPhiRated),
      if (canOperate) sRated.toActivePower(cosPhiRated) else zeroKW,
    )
  }

  override def zeroPowerOperatingPoint: ActivePowerAndHeatOperatingPoint =
    ActivePowerAndHeatOperatingPoint.zero

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
    * @return
    *   Boolean defining if heat pump runs in next time step, if it can be in
    *   operation and can be out of operation.
    */
  private def operatesInNextState(
      thermalGridState: ThermalGridState,
      thermalDemands: ThermalDemandWrapper,
      wasRunningLastState: Boolean,
  ): (Boolean, Boolean, Boolean) = {

    val demandHouse = thermalDemands.houseDemand
    val demandThermalStorage = thermalDemands.heatStorageDemand
    val noThermalStorageOrEmpty = thermalGridState.isThermalStorageEmpty

    val turnHpOn =
      (demandHouse.hasRequiredDemand && noThermalStorageOrEmpty) ||
        (demandHouse.hasAdditionalDemand && wasRunningLastState ||
          demandThermalStorage.hasRequiredDemand ||
          (demandThermalStorage.hasAdditionalDemand && wasRunningLastState))

    val canOperate =
      demandHouse.hasRequiredDemand || demandHouse.hasAdditionalDemand ||
        demandThermalStorage.hasRequiredDemand || demandThermalStorage.hasAdditionalDemand
    val canBeOutOfOperation =
      !(demandHouse.hasRequiredDemand && noThermalStorageOrEmpty)

    (
      turnHpOn,
      canOperate,
      canBeOutOfOperation,
    )
  }

  /** Some Scala Doc
    */

  private def nextOperatingPoint(
      state: HpState,
      setPower: Option[Power],
  ): (Power, Power) = {

    val lastHouseQDot =
      state.thermalGridState.houseState.map(_.qDot).getOrElse(zeroKW)
    val lastStorageQDot =
      state.thermalGridState.storageState.map(_.qDot).getOrElse(zeroKW)
    val wasRunningLastState = (lastHouseQDot + lastStorageQDot) > zeroKW

    val (turnOn, _, _) =
      if (setPower.isDefined) {
        /* If the set point value is above 50 % of the electrical power, turn on the heat pump otherwise turn it off */
        (
          setPower.getOrElse(
            throw new RuntimeException(
              s"setPower $setPower should be defined but couldn't accessed."
            )
          ) > (sRated.toActivePower(cosPhiRated) * 0.5),
          None,
          None,
        )
      } else {
        operatesInNextState(
          state.thermalGridState,
          state.thermalDemands,
          wasRunningLastState,
        )
      }

    val (newActivePowerHp, _, qDotIntoGrid) = {
      if (turnOn)
        (pRated, pThermal, pThermal)
      else if (lastStorageQDot < zeroKW)
        (zeroKW, zeroKW, lastStorageQDot * -1)
      else if (
        lastStorageQDot == zeroKW && (state.thermalDemands.houseDemand.hasRequiredDemand || state.thermalDemands.heatStorageDemand.hasRequiredDemand)
      )
        (zeroKW, zeroKW, thermalGrid.heatStorage.map(_.getpThermalMax).get)
      else (zeroKW, zeroKW, zeroKW)
    }

    (newActivePowerHp, qDotIntoGrid)

  }

  /** Some Scala Doc
    */

  private def pushQDotIntoThermalGrid(
      state: HpState,
      newActivePowerHp: Power,
      qDotIntoGrid: Power,
  ): (HpState, Option[ThermalThreshold]) = {
    val (thermalGridState, maybeThreshold) =
      thermalGrid.updateState(
        state.tick,
        state,
        newActivePowerHp > zeroKW,
        qDotIntoGrid,
        state.thermalDemands,
      )

    (state.copy(thermalGridState = thermalGridState), maybeThreshold)
  }

  override def createResults(
      state: HpState,
      lastOperatingPoint: Option[ActivePowerAndHeatOperatingPoint],
      currentOperatingPoint: ActivePowerAndHeatOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[ResultEntity] = {
    Iterable(
      new HpResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
        currentOperatingPoint.qDot
          .getOrElse(
            throw new RuntimeException(
              s"currentOperatingPoint $currentOperatingPoint does not contain heat value but one is expected."
            )
          )
          .toMegawatts
          .asMegaWatt,
      )
    ) ++ thermalGrid.results(state, dateTime)
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

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable(ServiceType.WeatherService)

  /** Calculate the active power behaviour of the model.
    *
    * @param state
    *   The current state including weather data.
    * @return
    *   The active power.
    */

  override def determineOperatingPoint(
      state: HpState
  ): (ActivePowerAndHeatOperatingPoint, Option[Long]) = {

    val (newActivePowerHp, qDotIntoGrid) = nextOperatingPoint(state, None)

    /* Push thermal energy to the thermal grid and get its updated state in return */
    val (updateState, maybeThreshold) =
      pushQDotIntoThermalGrid(state, newActivePowerHp, qDotIntoGrid)

    val operatingPoint =
      ActivePowerAndHeatOperatingPoint(newActivePowerHp, Some(pThermal))

    val nextTick = maybeThreshold match {
      case Some(threshold) => Some(threshold.tick)
      case None            => None
    }

    (operatingPoint, nextTick)
  }

  override def determineOperatingPoint(
      state: HpState,
      setPower: Power,
  ): (ActivePowerAndHeatOperatingPoint, OperationChangeIndicator) = {

    val (newActivePowerHp, qDotIntoGrid) =
      nextOperatingPoint(state, Some(setPower))

    /* Push thermal energy to the thermal grid and get its updated state in return */
    val (updateState, maybeThreshold) =
      pushQDotIntoThermalGrid(state, newActivePowerHp, qDotIntoGrid)

    val operatingPoint =
      ActivePowerAndHeatOperatingPoint(newActivePowerHp, Some(pThermal))

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

  /** Holds all relevant data for a hp model calculation.
    *
    * @param tick
    *   The current tick.
    * @param dateTime
    *   The date and time of the <b>ending</b> of time frame to calculate.
    * @param ambientTemperature
    *   The outside temperature.
    * @param thermalGridState
    *   FIXME
    * @param lastAmbientTemperature
    *   The outside temperature of the lastState.
    */
  final case class HpState(
      override val tick: Long,
      ambientTemperature: Temperature,
      lastThermalFlows: ThermalFlowWrapper,
      thermalGridState: ThermalGridState,
      lastAmbientTemperature: Temperature,
      thermalDemands: ThermalDemandWrapper,
  ) extends ModelState

  def apply(
      hpInput: HpInput,
      thermalGrid: PsdmThermalGrid,
  ): HpModel =
    new HpModel(
      hpInput.getUuid,
      hpInput.getId,
      Kilovoltamperes(
        hpInput.getType.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      hpInput.getType.getCosPhiRated,
      QControl(hpInput.getqCharacteristics),
      Degrees(hpInput.getNode.getGeoPosition.getY),
      Degrees(hpInput.getNode.getGeoPosition.getX),
      Kilowatts(
        hpInput.getType
          .getpThermal()
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      ThermalGrid(thermalGrid),
    )

}
