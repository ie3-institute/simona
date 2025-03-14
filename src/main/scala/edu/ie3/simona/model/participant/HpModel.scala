/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPowerAndHeat
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalGridState,
}
import edu.ie3.simona.model.thermal.{ThermalGrid, ThermalThreshold}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.energy.Kilowatts
import squants.{Power, Temperature}

import java.time.ZonedDateTime
import java.util.UUID

/** Model of a heat pump (HP) with a
  * [[edu.ie3.simona.model.thermal.ThermalHouse]] medium and its current
  * [[HpState]].
  *
  * @param uuid
  *   the element's uuid
  * @param id
  *   the element's human-readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  * @param pThermal
  *   Thermal output of heat pump
  * @param thermalGrid
  *   Thermal grid attached to this heat pump. Attention!!! This model assumes,
  *   that the grid is only attached to this asset as a source. Shared grids
  *   across electrical models is currently not supported
  */
final case class HpModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: ApparentPower,
    cosPhiRated: Double,
    pThermal: Power,
    thermalGrid: ThermalGrid,
) extends SystemParticipant[
      HpRelevantData,
      ComplexPowerAndHeat,
      HpState,
    ](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    )
    with ApparentPowerAndHeatParticipant[HpRelevantData, HpState] {

  private val pRated: Power = sRated.toActivePower(cosPhiRated)

  /** As this is a state-full model (with respect to the current operation
    * condition and inner temperature), the power calculation operates on the
    * current state of the model, which has to be calculated beforehand by
    * [[HpModel.determineState]]. This state then is fed into the power
    * calculation logic by [[HpState]].
    *
    * @param currentState
    *   Current state of the heat pump
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @return
    *   active power
    */
  override protected def calculateActivePower(
      currentState: HpState,
      relevantData: HpRelevantData,
  ): Power = currentState.activePower

  /** "Calculate" the heat output of the heat pump. The hp's state is already
    * updated, because the calculation of apparent power in
    * [[ApparentPowerAndHeatParticipant]] did trigger it. So we only need to
    * extract the information
    *
    * @param tick
    *   Current simulation time for the calculation
    * @param currentState
    *   Current state of the heat pump
    * @param data
    *   Relevant (external) data for calculation
    * @return
    *   The heat, that is produced in that instant
    */
  override def calculateHeat(
      tick: Long,
      currentState: HpState,
      data: HpRelevantData,
  ): Power = currentState.qDot

  /** Given a [[HpRelevantData]] object and the last [[HpState]], this function
    * calculates the heat pump's next state to get the actual active power of
    * this state use [[calculateActivePower]] with the generated state
    *
    * @param lastHpState
    *   Last state of the heat pump
    * @param relevantData
    *   data of heat pump including
    * @return
    *   Booleans if Hp can operate and can be out of operation plus the updated
    *   [[HpState]]
    */
  def determineState(
      lastHpState: HpState,
      relevantData: HpRelevantData,
  ): (Boolean, Boolean, HpState) = {

    // Use lastHpState and relevantData to update state of thermalGrid to the current tick
    val (thermalDemandWrapper, currentThermalGridState) =
      thermalGrid.energyDemandAndUpdatedState(
        relevantData,
        lastHpState,
      )

    // Determining the operation point and limitations at this tick
    val (turnOn, canOperate, canBeOutOfOperation) =
      operatesInNextState(
        lastHpState,
        currentThermalGridState,
        relevantData,
        thermalDemandWrapper,
      )

    // Updating the HpState
    val updatedHpState =
      calcState(
        lastHpState,
        relevantData,
        turnOn,
        thermalDemandWrapper,
        currentThermalGridState,
      )
    (canOperate, canBeOutOfOperation, updatedHpState)
  }

  /** Depending on the input, this function decides whether the heat pump will
    * run in the next state or not. The heat pump is foreseen to operate in the
    * next interval, if the thermal grid either has a demand that needs to be
    * met or the heat pump currently is in operation and the grid is able to
    * handle additional energy
    *
    * @param lastState
    *   last state of the heat pump
    * @param currentThermalGridState
    *   to current tick updated state of the thermalGrid
    * @param relevantData
    *   Relevant (external) data
    * @param thermalDemands
    *   ThermalEnergyDemand of the house and the thermal storage
    * @return
    *   boolean defining if heat pump runs in next time step, if it can be in
    *   operation and out of operation plus the [[ThermalEnergyDemand]] of
    *   house, heat storage, domestic hot water storage
    */
  private def operatesInNextState(
      lastState: HpState,
      currentThermalGridState: ThermalGridState,
      relevantData: HpRelevantData,
      thermalDemands: ThermalDemandWrapper,
  ): (Boolean, Boolean, Boolean) = {

    val demandHouse = thermalDemands.houseDemand
    val demandThermalStorage = thermalDemands.heatStorageDemand
    val demandDomesticHotWaterStorage =
      thermalDemands.domesticHotWaterStorageDemand

    val noThermalStorageOrThermalStorageIsEmpty =
      currentThermalGridState.isThermalStorageEmpty

    val turnHpOn =
      (demandHouse.hasRequiredDemand && noThermalStorageOrThermalStorageIsEmpty) ||
        (demandHouse.hasAdditionalDemand && lastState.isRunning) ||
        demandThermalStorage.hasRequiredDemand ||
        (demandThermalStorage.hasAdditionalDemand && lastState.isRunning) ||
        demandDomesticHotWaterStorage.hasRequiredDemand ||
        (demandDomesticHotWaterStorage.hasAdditionalDemand && lastState.isRunning)

    val canOperate =
      demandHouse.hasRequiredDemand || demandHouse.hasAdditionalDemand ||
        demandThermalStorage.hasRequiredDemand || demandThermalStorage.hasAdditionalDemand ||
        demandDomesticHotWaterStorage.hasRequiredDemand || demandDomesticHotWaterStorage.hasAdditionalDemand
    val canBeOutOfOperation =
      !(demandHouse.hasRequiredDemand && noThermalStorageOrThermalStorageIsEmpty) && !demandDomesticHotWaterStorage.hasRequiredDemand

    (
      turnHpOn,
      canOperate,
      canBeOutOfOperation,
    )
  }

  /** Calculate state depending on whether heat pump is needed or not. Also
    * calculate inner temperature change of thermal house and update its inner
    * temperature.
    *
    * @param lastState
    *   state of the heat pump until this tick
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param isRunning
    *   determines whether the heat pump is running or not
    * @param thermalDemands
    *   holds the thermal demands of the thermal units (house, heatStorage,
    *   hotDomesticWaterStorage)
    * @param currentThermalGridState
    *   Current state of the thermalGrid
    * @return
    *   next [[HpState]]
    */
  private def calcState(
      lastState: HpState,
      relevantData: HpRelevantData,
      isRunning: Boolean,
      thermalDemands: ThermalDemandWrapper,
      currentThermalGridState: ThermalGridState,
  ): HpState = {
    val lastHouseQDot = lastState.thermalGridState.houseState
      .map(_.qDot)
      .getOrElse(zeroKW)
    val currentStorageEnergy = currentThermalGridState.storageState
      .map(_.storedEnergy)
      .getOrElse(zeroKWh)
    val currentStoragePThermal =
      thermalGrid.heatStorage.map(_.getpThermalMax).getOrElse(zeroKW)

    val (newHpActivePower, newHpThermalPower, qDotIntoGrid) = {
      if (isRunning)
        (pRated, pThermal, pThermal)
      else if (
        currentStorageEnergy > zeroKWh && demandWrapper.houseDemand.hasRequiredDemand
      ) {
        // If the house has req. demand and storage isn't empty, we can heat the house from storage.
        (zeroKW, zeroKW, currentStoragePThermal)
      } else if (
        currentStorageEnergy > zeroKWh && demandWrapper.houseDemand.hasAdditionalDemand && lastHouseQDot > zeroKW
      )
        // Edge case when em controlled: If the house was heated last state by Hp and setPower is below turnOn condition now,
        // but house didn't reach target or boundary temperature yet. House can be heated from storage, if this one is not empty.
        (zeroKW, zeroKW, currentStoragePThermal)
      else (zeroKW, zeroKW, zeroKW)
    }

    /* Push thermal energy to the thermal grid and get its updated state in return */
    val (thermalGridState, maybeThreshold) =
      thermalGrid.updateState(
        relevantData,
        lastState.thermalGridState,
        lastState.ambientTemperature.getOrElse(relevantData.ambientTemperature),
        isRunning,
        qDotIntoGrid,
        thermalDemands,
      )

    HpState(
      isRunning,
      relevantData.currentTick,
      Some(relevantData.ambientTemperature),
      newHpActivePower,
      newHpThermalPower,
      thermalGridState,
      maybeThreshold,
    )
  }

  override def determineFlexOptions(
      data: HpRelevantData,
      lastState: HpState,
  ): ProvideFlexOptions = {
    /* Determine the operating state in the given tick */
    val (canOperate, canBeOutOfOperation, updatedHpState)
        : (Boolean, Boolean, HpState) = determineState(lastState, data)

    val lowerBoundary =
      if (canBeOutOfOperation)
        zeroKW
      else
        updatedHpState.activePower
    val upperBoundary =
      if (canOperate)
        sRated.toActivePower(cosPhiRated)
      else
        zeroKW

    ProvideMinMaxFlexOptions(
      uuid,
      updatedHpState.activePower,
      lowerBoundary,
      upperBoundary,
    )
  }

  /** Handle a controlled power change. If the requested active power is greater
    * than 50 % of the rated electrical power switch on the heat pump, otherwise
    * switch it off. If it is already in the requested state, return old state
    * and next trigger information, otherwise, update the state with new
    * operating state and give back the next tick in which something will
    * change.
    *
    * @param relevantData
    *   Relevant data for model calculation
    * @param lastState
    *   The last known model state
    * @param setPower
    *   power that has been set by EmAgent
    * @return
    *   updated relevant data and an indication at which circumstances flex
    *   options will change next
    */
  override def handleControlledPowerChange(
      relevantData: HpRelevantData,
      lastState: HpState,
      setPower: Power,
  ): (HpState, FlexChangeIndicator) = {
    /* If the set point value is above 50 % of the electrical power, turn on the heat pump otherwise turn it off */
    val turnOn = setPower > (sRated.toActivePower(cosPhiRated) * 0.5)

    val (
      thermalDemands,
      updatedThermalGridState,
    ) =
      thermalGrid.energyDemandAndUpdatedState(
        relevantData,
        lastState,
      )

    val updatedHpState = calcState(
      lastState,
      relevantData,
      turnOn,
      thermalDemands,
      updatedThermalGridState,
    )

    (
      updatedHpState,
      FlexChangeIndicator(
        changesAtNextActivation = true,
        updatedHpState.maybeThermalThreshold.map(_.tick),
      ),
    )
  }
}

/** Create valid [[HpModel]] by calling the apply function.
  */
object HpModel {

  def apply(
      inputModel: HpInput,
      scaling: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      thermalGrid: ThermalGrid,
  ): HpModel = {
    val scaledInput = inputModel.copy().scale(scaling).build()

    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        scaledInput.getOperationTime,
      )

    val qControl = QControl(scaledInput.getqCharacteristics())

    val model = new HpModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      qControl,
      Kilovoltamperes(
        scaledInput.getType.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      scaledInput.getType.getCosPhiRated,
      Kilowatts(
        scaledInput.getType.getpThermal
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      thermalGrid,
    )

    model.enable()
    model
  }

  /** As the HpModel class is a dynamic model, it requires a state for its
    * calculations. The state contains all variables needed including the inner
    * temperature.
    *
    * @param isRunning
    *   indicates if HP is turned on
    * @param tick
    *   the time tick of the HpState
    * @param ambientTemperature
    *   Optional ambient temperature, if available
    * @param activePower
    *   result active power of heat pump
    * @param qDot
    *   result heat power of heat pump
    * @param thermalGridState
    *   applicable state of the thermal grid
    * @param maybeThermalThreshold
    *   An optional threshold of the thermal grid, indicating the next state
    *   change
    */
  final case class HpState(
      isRunning: Boolean,
      tick: Long,
      ambientTemperature: Option[Temperature],
      activePower: Power,
      qDot: Power,
      thermalGridState: ThermalGridState,
      maybeThermalThreshold: Option[ThermalThreshold],
  ) extends ModelState

  /** Main data required for simulation/calculation, containing a [[HpState]]
    * and the current time tick. One time tick represents one second (3600 time
    * ticks = 1 hour).
    *
    * @param currentTick
    *   contains current time tick
    * @param ambientTemperature
    *   Ambient temperature at current tick
    * @param simulationStart
    *   simulationStartDate as ZonedDateTime
    * @param houseInhabitants
    *   number of people living in the building
    */
  final case class HpRelevantData(
      currentTick: Long,
      ambientTemperature: Temperature,
      simulationStart: ZonedDateTime,
      houseInhabitants: Double,
  ) extends CalcRelevantData

  /** Internal method to construct a new [[HpModel]] based on a provided
    * [[HpInput]]
    *
    * @param hpInput
    *   instance of [[HpInput]] this chp model should be built from
    * @param simulationStartDate
    *   Simulation time at which the simulation starts
    * @param simulationEndDate
    *   Simulation time at which the simulation ends
    * @param qControl
    *   Strategy to control the reactive power output
    * @param scalingFactor
    *   Scale the output of this asset by the given factor
    * @param thermalGrid
    *   thermal grid, defining the behaviour of the connected sinks and storages
    * @return
    *   a ready-to-use [[HpModel]] with referenced electric parameters
    */
  def apply(
      hpInput: HpInput,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      qControl: QControl,
      scalingFactor: Double,
      thermalGrid: ThermalGrid,
  ): HpModel = {
    val scaledInput = hpInput.copy().scale(scalingFactor).build()

    val operationInterval = SystemComponent.determineOperationInterval(
      simulationStartDate,
      simulationEndDate,
      scaledInput.getOperationTime,
    )

    val model = new HpModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      qControl,
      Kilovoltamperes(
        scaledInput.getType.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      scaledInput.getType.getCosPhiRated,
      Kilowatts(
        scaledInput.getType.getpThermal
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      thermalGrid,
    )

    model.enable()
    model
  }
}
