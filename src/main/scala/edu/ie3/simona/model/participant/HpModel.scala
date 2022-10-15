/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.HpModel._
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.{ThermalGrid, ThermalHouse}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Power, Temperature}

/** Model of a heat pump (HP) with a [[ThermalHouse]] medium and its current
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
    sRated: ComparableQuantity[Power],
    cosPhiRated: Double,
    pThermal: ComparableQuantity[Power],
    thermalGrid: ThermalGrid
) extends SystemParticipant[
      HpRelevantData,
      ApparentPowerAndHeat,
      ConstantState.type
    ](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    )
    with ApparentPowerAndHeatParticipant[HpRelevantData, ConstantState.type] {

  private val pRated: ComparableQuantity[Power] =
    sRated
      .multiply(cosPhiRated)
      .multiply(scalingFactor)
      .to(StandardUnits.ACTIVE_POWER_IN)

  /** As this is a state-full model (with respect to the current operation
    * condition and inner temperature), the power calculation operates on the
    * current state of the model, which has to be calculated beforehand by
    * [[HpModel.calculateNextState]]. This state then is fed into the power
    * calculation logic by <i>hpData</i>.
    *
    * @param hpData
    *   data of heat pump including state of the heat pump
    * @return
    *   active power
    */
  override protected def calculateActivePower(
      hpData: HpRelevantData
  ): ComparableQuantity[Power] = {
    hpData.hpState = calculateNextState(hpData)
    hpData.hpState.activePower
  }

  /** "Calculate" the heat output of the heat pump. The hp's state is already
    * updated, because the calculation of apparent power in
    * [[ApparentPowerAndHeatParticipant]] did trigger it. So we only need to
    * extract the information
    * @param tick
    *   Current simulation time for the calculation
    * @param data
    *   Needed calculation relevant data
    * @return
    *   The heat, that is produced in that instant
    */
  override def calculateHeat(
      tick: Long,
      data: HpRelevantData
  ): ComparableQuantity[Power] =
    data.hpState.qDot.to(StandardUnits.ACTIVE_POWER_RESULT)

  /** Given a [[HpRelevantData]] object, containing the [[HpState]], other
    * values and the current time tick, this function calculates the heat pump's
    * next state To get the actual active power of this state use
    * [[calculateActivePower]] with the generated state
    *
    * @param hpData
    *   data of heat pump including state of the heat pump
    * @return
    *   next [[HpState]]
    */
  def calculateNextState(hpData: HpRelevantData): HpState = {
    val turnOn = operatesInNextState(hpData)
    calcState(hpData, turnOn)
  }

  /** Depending on the input, this function decides whether the heat pump will
    * run in the next state or not. The heat pump is foreseen to operate in the
    * next interval, if the thermal grid either has a demand that needs to be
    * met or the heat pump currently is in operation and the grid is able to
    * handle additional energy
    *
    * @return
    *   boolean defining if heat pump runs in next time step
    */
  def operatesInNextState(hpData: HpRelevantData): Boolean =
    hpData match {
      case HpRelevantData(hpState, currentTimeTick, ambientTemperature) =>
        val demand = thermalGrid.energyDemand(
          currentTimeTick,
          ambientTemperature,
          hpState.thermalGridState
        )
        demand.hasRequiredDemand || (hpState.isRunning && demand.hasAdditionalDemand)
    }

  /** Calculate state depending on whether heat pump is needed or not. Also
    * calculate inner temperature change of thermal house and update its inner
    * temperature.
    *
    * @param hpData
    *   data of heat pump including state of the heat pump
    * @param isRunning
    *   determines whether the heat pump is running or not
    * @return
    *   next [[HpState]]
    */
  private def calcState(hpData: HpRelevantData, isRunning: Boolean): HpState = {
    val (newActivePower, newThermalPower) =
      if (isRunning)
        (pRated, pThermal.multiply(scalingFactor))
      else (DefaultQuantities.zeroKW, DefaultQuantities.zeroKW)

    /* Push thermal energy to the thermal grid and get it's updated state in return */
    val thermalGridState = hpData match {
      case HpRelevantData(hpState, currentTimeTick, ambientTemperature) =>
        thermalGrid.updateState(
          currentTimeTick,
          hpState.thermalGridState,
          ambientTemperature,
          newThermalPower
        )
    }

    HpState(
      isRunning,
      hpData.currentTimeTick,
      newActivePower,
      newThermalPower,
      thermalGridState._1
    )
  }

  override def determineFlexOptions(
      data: HpRelevantData,
      lastState: ConstantState.type
  ): ProvideFlexOptions = ??? // TODO actual implementation

  override def handleControlledPowerChange(
      data: HpRelevantData,
      lastState: ConstantState.type,
      setPower: ComparableQuantity[Power]
  ): (ConstantState.type, FlexChangeIndicator) =
    ??? // TODO actual implementation

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
      inputModel.getType.getsRated(),
      inputModel.getType.getCosPhiRated,
      inputModel.getType.getpThermal(),
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
    * @param activePower
    *   result active power
    * @param qDot
    *   result heat power
    * @param thermalGridState
    *   Currently applicable state of the thermal grid
    */
  final case class HpState(
      isRunning: Boolean,
      lastTimeTick: Long,
      activePower: ComparableQuantity[Power],
      qDot: ComparableQuantity[Power],
      thermalGridState: ThermalGridState
  )

  /** Main data required for simulation/calculation, containing a [[HpState]]
    * and the current time tick. <p> [[HpRelevantData.currentTimeTick]] and
    * [[HpState.lastTimeTick]] form a time interval for the current state
    * calculation. One time tick represents one second (3600 time ticks = 1
    * hour).
    *
    * @param hpState
    *   a [[HpState]]
    * @param currentTimeTick
    *   contains current time tick
    */
  final case class HpRelevantData(
      var hpState: HpState,
      currentTimeTick: Long,
      ambientTemperature: ComparableQuantity[Temperature]
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
      hpInput.getType.getsRated,
      hpInput.getType.getCosPhiRated,
      hpInput.getType.getpThermal,
      thermalGrid
    )

    model.enable()
    model
  }
}
