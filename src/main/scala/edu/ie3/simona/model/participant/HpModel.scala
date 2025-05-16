/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.container.ThermalGrid as PsdmThermalGrid
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.{
  HpResult,
  SystemParticipantResult,
}
import edu.ie3.simona.model.participant.HpModel.{HpOperatingPoint, HpState}
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.ThermalGrid
import edu.ie3.simona.model.thermal.ThermalGrid.*
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  ComplexPowerAndHeat,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.quantities.QuantityUtils.{asMegaVar, asMegaWatt}
import edu.ie3.util.scala.quantities.DefaultQuantities.{
  zeroCelsius,
  zeroKW,
  zeroKWh,
}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.PowerConversionSimona
import edu.ie3.util.scala.quantities.*
import squants.*

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
      lastState: HpState,
      operatingPoint: HpOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): HpState = {

    val thermalGridState =
      thermalGrid.determineState(
        tick,
        lastState.thermalGridState,
        operatingPoint,
      )

    val hoursWaterDemandToDetermine = thermalGrid.house match {
      case Some(house) =>
        house.checkIfNeedToDetermineDomesticHotWaterDemand(
          tick,
          simulationTime,
          lastState,
        )
      case None => None
    }

    val thermalDemands =
      thermalGrid.determineEnergyDemand(
        thermalGridState,
        hoursWaterDemandToDetermine,
      )

    lastState.copy(
      tick = tick,
      simulationTime = simulationTime,
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
          thermalGridState = state.thermalGridState.copy(houseState =
            state.thermalGridState.houseState
              .map(_.copy(ambientTemperature = newData.temp))
          )
        )
      )
      .getOrElse(state)
  }

  override def determineFlexOptions(
      state: HpState
  ): FlexOptions = {
    val wasRunningLastOp = state.lastHpOperatingPoint.activePower > zeroKW
    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) = determineHpOperatingOptions(
      state.thermalGridState,
      state.thermalDemands,
      wasRunningLastOp,
    )

    val (refPower, minPower) = (turnOn, canBeOutOfOperation) match {
      case (true, true) =>
        if (
          state.lastHpOperatingPoint.activePower > zeroKW &&
          state.thermalDemands.houseDemand.hasPossibleDemand &&
          state.thermalGridState.heatStorageState
            .map(_.storedEnergy)
            .getOrElse(zeroKWh) == zeroKWh
        )
          // if Hp was running last state AND there is demand from the house AND the storage is empty,
          // we would like to keep that behaviour even in strict interpretation of flexibility we could
          // be out of operation for flex reasons. Thus, we force Hp to run.
          (sRated.toActivePower(cosPhiRated), sRated.toActivePower(cosPhiRated))
        else {
          (sRated.toActivePower(cosPhiRated), zeroKW)
        }
      case (true, false) =>
        (sRated.toActivePower(cosPhiRated), sRated.toActivePower(cosPhiRated))
      case (false, true) =>
        (
          zeroKW,
          zeroKW,
        )
      case (false, false) =>
        (
          sRated.toActivePower(cosPhiRated),
          sRated.toActivePower(cosPhiRated),
        ) // should not be possible to reach
    }

    val maxPower = if (canOperate) sRated.toActivePower(cosPhiRated) else zeroKW

    MinMaxFlexOptions(refPower, minPower, maxPower)
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
  ): (HpOperatingPoint, Option[Long]) =
    findOperatingPointAndNextThreshold(state, None)

  /** Calculate the active power behaviour of the model.
    *
    * @param state
    *   The current state including weather data.
    * @param setPower
    *   The power set by the energy management.
    * @return
    *   The active power.
    */
  override def determineOperatingPoint(
      state: HpState,
      setPower: Power,
  ): (HpOperatingPoint, OperationChangeIndicator) = {
    val (operatingPoint, nextTick) =
      findOperatingPointAndNextThreshold(state, Some(setPower))

    (
      operatingPoint,
      OperationChangeIndicator(
        changesAtNextActivation = true,
        changesAtTick = nextTick,
      ),
    )
  }

  override def zeroPowerOperatingPoint: HpOperatingPoint =
    HpOperatingPoint.zero

  /** Depending on the input, this function calculates the next operating point
    * of the heat pump and the next threshold.
    *
    * @param state
    *   Currently applicable HpState.
    * @param setPower
    *   The setPower from Em, if there is some.
    * @return
    *   The operating point of the Hp and the next threshold if there is one.
    */
  private def findOperatingPointAndNextThreshold(
      state: HpState,
      setPower: Option[Power],
  ): (HpOperatingPoint, Option[Long]) = {

    /* Determine active and thermal power of the Hp */
    val (newActivePowerHp, qDotIntoGrid) = determineHpOperation(state, setPower)

    /* Determine how qDot is used in thermalGrid and get threshold */
    val (thermalGridOperatingPoint, maybeThreshold) =
      if (qDotIntoGrid > zeroKW) {
        thermalGrid.handleFeedIn(
          state,
          qDotIntoGrid,
        )
      } else
        thermalGrid.handleConsumption(state)

    val operatingPoint =
      HpOperatingPoint(
        newActivePowerHp,
        thermalGridOperatingPoint,
      )

    val nextTick = maybeThreshold match {
      case Some(threshold) => Some(threshold.tick)
      case None            => None
    }

    (operatingPoint, nextTick)
  }

  /** Depending on the input, this function calculates the active power and
    * thermal power (qDot) of the heat pump.
    *
    * @param state
    *   Currently applicable HpState.
    * @param setPower
    *   The setPower from Em, if there is some.
    * @return
    *   The new active power of the heat pump and the thermal power (qDot) from
    *   the heat pump, feed into the thermal grid.
    */
  private def determineHpOperation(
      state: HpState,
      setPower: Option[Power],
  ): (Power, Power) = {
    val wasRunningLastOp = state.lastHpOperatingPoint.activePower > zeroKW
    val turnOn = setPower match {
      case Some(value) =>
        /* If the set point value is above 50 % of the electrical power, turn on the heat pump otherwise turn it off */
        value > (sRated.toActivePower(cosPhiRated) * 0.5)
      case None =>
        determineHpOperatingOptions(
          state.thermalGridState,
          state.thermalDemands,
          wasRunningLastOp,
        )._1
    }

    if (turnOn) (pRated, pThermal)
    else (zeroKW, zeroKW)
  }

  /** Depending on the input, this function determines the different operating
    * options of the heat pump. The heat pump is foreseen to operate, if the
    * thermal grid either has a demand that needs to be met or the heat pump
    * currently is in operation and the grid is able to handle some possible
    * energy demand.
    *
    * @param thermalGridState
    *   State of the thermalGrid.
    * @param thermalDemands
    *   ThermalEnergyDemand of the house and the thermal storage.
    * @param wasRunningLastPeriod
    *   Indicates if the Hp was running till this tick.
    * @return
    *   Boolean defining if the heat pump will run as default behaviour, if it
    *   can be in operation and can be out of operation as flexibility options.
    */
  private def determineHpOperatingOptions(
      thermalGridState: ThermalGridState,
      thermalDemands: ThermalDemandWrapper,
      wasRunningLastPeriod: Boolean,
  ): (Boolean, Boolean, Boolean) = {

    val demandHouse = thermalDemands.houseDemand
    val demandThermalStorage = thermalDemands.heatStorageDemand
    val demandDomesticHotWaterStorage =
      thermalDemands.domesticHotWaterStorageDemand
    val noThermalStorageOrEmpty = thermalGridState.isThermalStorageEmpty

    val turnHpOn =
      (demandHouse.hasRequiredDemand && noThermalStorageOrEmpty) ||
        (demandHouse.hasPossibleDemand && wasRunningLastPeriod ||
          demandThermalStorage.hasRequiredDemand ||
          (demandThermalStorage.hasPossibleDemand && wasRunningLastPeriod)) ||
        demandDomesticHotWaterStorage.hasRequiredDemand // ||
    // (demandDomesticHotWaterStorage.hasPossibleDemand && wasRunningLastPeriod) FIXME

    val canOperate =
      demandHouse.hasRequiredDemand || demandHouse.hasPossibleDemand ||
        demandThermalStorage.hasRequiredDemand || demandThermalStorage.hasPossibleDemand ||
        demandDomesticHotWaterStorage.hasRequiredDemand // || demandDomesticHotWaterStorage.hasPossibleDemand FIXME
    val canBeOutOfOperation =
      !(demandHouse.hasRequiredDemand && noThermalStorageOrEmpty) && !demandDomesticHotWaterStorage.hasRequiredDemand

    (
      turnHpOn,
      canOperate,
      canBeOutOfOperation,
    )
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
      case unknown =>
        throw new IllegalArgumentException(
          s"Unknown data type when matching for primary data results $unknown!"
        )
    }
  }
}

object HpModel {
  final case class HpOperatingPoint(
      override val activePower: Power,
      thermalOps: ThermalGridOperatingPoint,
  ) extends OperatingPoint {
    override val reactivePower: Option[ReactivePower] = None
  }

  object HpOperatingPoint {
    def zero: HpOperatingPoint =
      HpOperatingPoint(zeroKW, ThermalGridOperatingPoint.zero)
  }

  /** Operating point of the thermal grid.
    *
    * @param qDotHp
    *   The thermal power output of the heat pump.
    * @param qDotHouse
    *   The thermal power input of the
    *   [[edu.ie3.simona.model.thermal.ThermalHouse]] used for space heating.
    * @param qDotDomesticHotWaterStorage
    *   The thermal power input of the
    *   [[edu.ie3.simona.model.thermal.ThermalHouse]] used for domestic hot
    *   water / tap water.
    * @param qDotHeatStorage
    *   The thermal power input of the
    *   [[edu.ie3.simona.model.thermal.ThermalStorage]].
    */
  final case class ThermalGridOperatingPoint(
      qDotHp: Power,
      qDotHouse: Power,
      qDotHeatStorage: Power,
      qDotDomesticHotWaterStorage: Power,
  )
  object ThermalGridOperatingPoint {
    def zero: ThermalGridOperatingPoint =
      ThermalGridOperatingPoint(zeroKW, zeroKW, zeroKW, zeroKW)
  }

  /** Holds all relevant data for a hp model calculation.
    *
    * @param tick
    *   The current tick.
    * @param simulationTime
    *   The current simulation time
    * @param thermalGridState
    *   The applicable state of the [[ThermalGrid]].
    * @param lastHpOperatingPoint
    *   The last [[HpOperatingPoint]] of the heat pump.
    * @param thermalDemands
    *   The current thermal demands of the thermal grid elements (house,
    *   storage).
    */
  final case class HpState(
      override val tick: Long,
      simulationTime: ZonedDateTime,
      thermalGridState: ThermalGridState,
      lastHpOperatingPoint: HpOperatingPoint,
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
      val initialState = ThermalGrid.startingState(therGrid, zeroCelsius)
      val thermalDemand =
        therGrid.determineEnergyDemand(
          initialState,
          Some(Seq(simulationTime.getHour)),
        )

      HpState(
        tick,
        simulationTime,
        initialState,
        HpOperatingPoint.zero,
        thermalDemand,
      )
    }

    override def create(): HpModel = {
      val bmType = input.getType

      new HpModel(
        input.getUuid,
        input.getId,
        bmType.getsRated.toApparent,
        input.getType.getCosPhiRated,
        QControl(input.getqCharacteristics),
        bmType.getpThermal.toSquants,
        ThermalGrid(thermalGrid),
      )
    }
  }
}
