/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.HpInput
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
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.HpModel.HpState
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerAndHeatOperatingPoint,
  ModelState,
  OperationChangeIndicator,
}
import edu.ie3.simona.model.thermal.ThermalGrid
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
  ThermalGridState,
  startingState,
}
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

  override val initialState: (Long, ZonedDateTime) => HpState = {
    // FIXME
    val thermalDemands = ThermalDemandWrapper(
      ThermalEnergyDemand(zeroKWh, zeroKWh),
      ThermalEnergyDemand(zeroKWh, zeroKWh),
    )

    (tick, _) =>
      HpState(
        tick,
        Celsius(0d),
        ThermalGridState(
          startingState(thermalGrid).houseState,
          startingState(thermalGrid).storageState,
        ),
        Celsius(0d),
        thermalDemands,
      )
  }

  override def determineState(
      state: HpState,
      operatingPoint: ActivePowerAndHeatOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): HpState = {
    val (_, currentThermalGridState) =
      thermalGrid.energyDemandAndUpdatedState(state)

    state.copy(
      tick = tick,
      thermalGridState = currentThermalGridState,
      lastAmbientTemperature = state.ambientTemperature,
    )
  }

  override def handleInput(
      state: HpState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): HpState = {
    val weatherData = receivedData
      .collectFirst { case weatherData: WeatherData =>
        weatherData
      }
      .getOrElse {
        throw new CriticalFailureException(
          s"Expected WeatherData, got $receivedData"
        )
      }

    state.copy(
      ambientTemperature = weatherData.temp
    )
  }

  override def determineFlexOptions(
      state: HpState
  ): FlexibilityMessage.ProvideFlexOptions = {
    // Use state to update thermalGrid to the current tick
    val (thermalDemandWrapper, currentThermalGridState) =
      thermalGrid.energyDemandAndUpdatedState(
        state
      )

    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) =
      operatesInNextState(
        currentThermalGridState,
        thermalDemandWrapper,
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
    * handle additional energy
    *
    * @param currentThermalGridState
    *   to current tick updated state of the thermalGrid
    * @param thermalDemands
    *   ThermalEnergyDemand of the house and the thermal storage
    * @return
    *   boolean defining if heat pump runs in next time step, if it can be in
    *   operation and can be out of operation
    */
  private def operatesInNextState(
      currentThermalGridState: ThermalGridState,
      thermalDemands: ThermalDemandWrapper,
  ): (Boolean, Boolean, Boolean) = {

    val demandHouse = thermalDemands.houseDemand
    val demandThermalStorage = thermalDemands.heatStorageDemand
    val noThermalStorageOrThermalStorageIsEmpty =
      currentThermalGridState.isThermalStorageEmpty

    val turnHpOn =
      (demandHouse.hasRequiredDemand && noThermalStorageOrThermalStorageIsEmpty) ||
        (demandHouse.hasAdditionalDemand &&
          // Fixme this should be something like state.lastOp.activePower > zeroKW)
          zeroKW > zeroKW ||
          demandThermalStorage.hasRequiredDemand ||
          (demandThermalStorage.hasAdditionalDemand &&
            // Fixme this should be something like state.lastOp.activePower > zeroKW)
            zeroKW > zeroKW))

    val canOperate =
      demandHouse.hasRequiredDemand || demandHouse.hasAdditionalDemand ||
        demandThermalStorage.hasRequiredDemand || demandThermalStorage.hasAdditionalDemand
    val canBeOutOfOperation =
      !(demandHouse.hasRequiredDemand && noThermalStorageOrThermalStorageIsEmpty)

    (
      turnHpOn,
      canOperate,
      canBeOutOfOperation,
    )
  }

  override def createResults(
      state: HpState,
      lastOperatingPoint: Option[ActivePowerAndHeatOperatingPoint],
      currentOperatingPoint: ActivePowerAndHeatOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
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
    )

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

    // These are basically the flexOptions, should / can we calc them also when we're not em controlled?
    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) =
      operatesInNextState(
        state.thermalGridState,
        state.thermalDemands,
      )

    val lastStateStorageQDot = state.thermalGridState.storageState
      .map(_.qDot)
      .getOrElse(zeroKW)

    val (newActivePowerHp, _, qDotIntoGrid) = {
      if (turnOn)
        (pRated, pThermal, pThermal)
      else if (lastStateStorageQDot < zeroKW)
        (zeroKW, zeroKW, lastStateStorageQDot * (-1))
      else if (
        lastStateStorageQDot == zeroKW && (state.thermalDemands.houseDemand.hasRequiredDemand || state.thermalDemands.heatStorageDemand.hasRequiredDemand)
      )
        (
          zeroKW,
          zeroKW,
          thermalGrid.heatStorage.map(_.getpThermalMax: squants.Power).get,
        )
      else (zeroKW, zeroKW, zeroKW)
    }

    /* Push thermal energy to the thermal grid and get its updated state in return */
    val (thermalGridState, maybeThreshold) = {
      thermalGrid.updateState(state, turnOn, qDotIntoGrid, state.thermalDemands)
      // FIXME we need to update state here as well
    }

    val nextTick = maybeThreshold match {
      case Some(threshold) => Some(threshold.tick)
      case None            => None
    }

    (
      ActivePowerAndHeatOperatingPoint(newActivePowerHp, Some(pThermal)),
      nextTick,
    )
  }

  override def determineOperatingPoint(
      state: HpState,
      setPower: Power,
  ): (ActivePowerAndHeatOperatingPoint, OperationChangeIndicator) = ???
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
      thermalGridState: ThermalGridState,
      lastAmbientTemperature: Temperature,
      thermalDemands: ThermalDemandWrapper,
  ) extends ModelState

  def apply(
      hpInput: HpInput,
      // FIXME?
      thermalGrid: ThermalGrid,
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
      thermalGrid,
    )

}
