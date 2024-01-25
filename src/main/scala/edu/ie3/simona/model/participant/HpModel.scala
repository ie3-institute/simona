/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.{ThermalGrid, ThermalThreshold}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities
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
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param scalingFactor
  *   Scaling the output of the system
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
    scalingFactor: Double,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    pThermal: Power,
    thermalGrid: ThermalGrid
) extends SystemParticipant[
      HpRelevantData,
      ApparentPowerAndHeat,
      HpState
    ](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    )
    with ApparentPowerAndHeatParticipant[HpRelevantData, HpState] {

  private val pRated: Power =
    sRated * cosPhiRated * scalingFactor

  /** As this is a state-full model (with respect to the current operation
    * condition and inner temperature), the power calculation operates on the
    * current state of the model, which has to be calculated beforehand by
    * [[HpModel.calculateNextState]]. This state then is fed into the power
    * calculation logic by <i>hpData</i>.
    *
    * @param modelState
    *   Current state of the heat pump
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @return
    *   active power
    */
  override protected def calculateActivePower(
      modelState: HpState,
      relevantData: HpRelevantData
  ): Power = modelState.activePower

  /** "Calculate" the heat output of the heat pump. The hp's state is already
    * updated, because the calculation of apparent power in
    * [[ApparentPowerAndHeatParticipant]] did trigger it. So we only need to
    * extract the information
    * @param tick
    *   Current simulation time for the calculation
    * @param modelState
    *   Current state of the heat pump
    * @param data
    *   Relevant (external) data for calculation
    * @return
    *   The heat, that is produced in that instant
    */
  override def calculateHeat(
      tick: Long,
      modelState: HpState,
      data: HpRelevantData
  ): Power =
    modelState.qDot

  /** Given a [[HpRelevantData]] object and the current [[HpState]], this
    * function calculates the heat pump's next state To get the actual active
    * power of this state use [[calculateActivePower]] with the generated state
    *
    * @param state
    *   Current state of the heat pump
    * @param relevantData
    *   data of heat pump including
    * @return
    *   next [[HpState]]
    */
  def calculateNextState(
      state: HpState,
      relevantData: HpRelevantData
  ): HpState = {
    val turnOn = operatesInNextState(state, relevantData)
    calcState(state, relevantData, turnOn)
  }

  /** Depending on the input, this function decides whether the heat pump will
    * run in the next state or not. The heat pump is foreseen to operate in the
    * next interval, if the thermal grid either has a demand that needs to be
    * met or the heat pump currently is in operation and the grid is able to
    * handle additional energy
    *
    * @param state
    *   Current state of the heat pump
    * @param relevantData
    *   Relevant (external) data
    * @return
    *   boolean defining if heat pump runs in next time step
    */
  def operatesInNextState(
      state: HpState,
      relevantData: HpRelevantData
  ): Boolean =
    relevantData match {
      case HpRelevantData(currentTimeTick, ambientTemperature) =>
        val demand = thermalGrid.energyDemand(
          currentTimeTick,
          ambientTemperature,
          state.thermalGridState
        )
        demand.hasRequiredDemand || (state.isRunning && demand.hasAdditionalDemand)
    }

  /** Calculate state depending on whether heat pump is needed or not. Also
    * calculate inner temperature change of thermal house and update its inner
    * temperature.
    *
    * @param state
    *   Current state of the heat pump
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @param isRunning
    *   determines whether the heat pump is running or not
    * @return
    *   next [[HpState]]
    */
  private def calcState(
      state: HpState,
      relevantData: HpRelevantData,
      isRunning: Boolean
  ): HpState = {
    val (newActivePower, newThermalPower) =
      if (isRunning)
        (pRated, pThermal * scalingFactor)
      else (DefaultQuantities.zeroKW, DefaultQuantities.zeroKW)

    /* Push thermal energy to the thermal grid and get it's updated state in return */
    val (thermalGridState, maybeThreshold) = relevantData match {
      case HpRelevantData(currentTimeTick, _) =>
        thermalGrid.updateState(
          currentTimeTick,
          state.thermalGridState,
          state.ambientTemperature,
          newThermalPower
        )
    }

    HpState(
      isRunning,
      relevantData.currentTimeTick,
      relevantData.ambientTemperature,
      newActivePower,
      newThermalPower,
      thermalGridState,
      maybeThreshold
    )
  }

  override def determineFlexOptions(
      data: HpRelevantData,
      lastState: HpState
  ): ProvideFlexOptions = {
    /* Determine the operating state in the given tick */
    val updatedState = calculateNextState(lastState, data)

    /* Determine the options we have */
    val thermalEnergyDemand = thermalGrid.energyDemand(
      data.currentTimeTick,
      data.ambientTemperature,
      lastState.thermalGridState
    )
    val canOperate =
      thermalEnergyDemand.hasRequiredDemand || thermalEnergyDemand.hasAdditionalDemand
    val canBeOutOfOperation = !thermalEnergyDemand.hasRequiredDemand

    val lowerBoundary =
      if (canBeOutOfOperation)
        Kilowatts(0d)
      else
        updatedState.activePower
    val upperBoundary =
      if (canOperate)
        sRated * cosPhiRated
      else
        Kilowatts(0d)

    ProvideMinMaxFlexOptions(
      uuid,
      updatedState.activePower,
      lowerBoundary,
      upperBoundary
    )
  }

  /** Handle a controlled power change. If the requested active power is greater
    * than 50 % of the rated electrical power switch on the heat pump, otherwise
    * switch it off. If it is already in the requested state, return old state
    * and next trigger information, otherwise, update the state with new
    * operating state and give back the next tick in which something will
    * change.
    *
    * @param data
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
      data: HpRelevantData,
      lastState: HpState,
      setPower: squants.Power
  ): (HpState, FlexChangeIndicator) = {
    /* If the setpoint value is above 50 % of the electrical power, turn on the heat pump otherwise turn it off */
    val turnOn = setPower > (sRated * cosPhiRated * 0.5)
    val updatedState = calcState(lastState, data, turnOn)

    (
      updatedState,
      FlexChangeIndicator(
        changesAtNextActivation = true,
        updatedState.maybeThermalThreshold.map(_.tick)
      )
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
      thermalGrid: ThermalGrid
  ): HpModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    val qControl = QControl(inputModel.getqCharacteristics())

    val model = new HpModel(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      scaling,
      qControl,
      Kilowatts(
        inputModel.getType.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      inputModel.getType.getCosPhiRated,
      Kilowatts(
        inputModel.getType.getpThermal
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      thermalGrid
    )

    model.enable()
    model
  }

  /** As the HpModel class is a dynamic model, it requires a state for its
    * calculations. The state contains all variables needed including the inner
    * temperature.
    *
    * @param isRunning
    *   indicates if CHP is turned on
    * @param lastTimeTick
    *   contains last time tick
    * @param ambientTemperature
    *   Ambient temperature
    * @param activePower
    *   result active power
    * @param qDot
    *   result heat power
    * @param thermalGridState
    *   Currently applicable state of the thermal grid
    * @param maybeThermalThreshold
    *   An optional threshold of the thermal grid, indicating the next state
    *   change
    */
  final case class HpState(
      isRunning: Boolean,
      lastTimeTick: Long,
      ambientTemperature: Temperature,
      activePower: Power,
      qDot: Power,
      thermalGridState: ThermalGridState,
      maybeThermalThreshold: Option[ThermalThreshold]
  ) extends ModelState

  /** Main data required for simulation/calculation, containing a [[HpState]]
    * and the current time tick. <p> [[HpRelevantData.currentTimeTick]] and
    * [[HpState.lastTimeTick]] form a time interval for the current state
    * calculation. One time tick represents one second (3600 time ticks = 1
    * hour).
    *
    * @param currentTimeTick
    *   contains current time tick
    * @param ambientTemperature
    *   Ambient temperature
    */
  final case class HpRelevantData(
      currentTimeTick: Long,
      ambientTemperature: Temperature
  ) extends CalcRelevantData

  /** Internal method to construct a new [[HpModel]] based on a provided
    * [[HpInput]]
    *
    * @param hpInput
    *   instance of [[HpInput]] this chp model should be built from
    * @param operationInterval
    *   operation interval of the simulation
    * @param qControl
    *   (no usage)
    * @param thermalGrid
    *   thermal grid, defining the behaviour of the connected sinks and storages
    * @return
    *   a ready-to-use [[HpModel]] with referenced electric parameters
    */
  def apply(
      hpInput: HpInput,
      operationInterval: OperationInterval,
      qControl: QControl,
      thermalGrid: ThermalGrid
  ): HpModel = {
    val model = new HpModel(
      hpInput.getUuid,
      hpInput.getId,
      operationInterval,
      scalingFactor = 1.0,
      qControl,
      Kilowatts(
        hpInput.getType.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      hpInput.getType.getCosPhiRated,
      Kilowatts(
        hpInput.getType.getpThermal
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      thermalGrid
    )

    model.enable()
    model
  }
}
