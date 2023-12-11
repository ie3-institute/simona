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
import edu.ie3.simona.model.thermal.ThermalGrid
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
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
    override protected val scalingFactor: Double,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    pThermal: Power,
    thermalGrid: ThermalGrid
) extends SystemParticipant[
      HpRelevantData,
      ApparentPowerAndHeat
    ](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    )
    with ApparentPowerAndHeatParticipant[HpRelevantData] {

  private val pRated: Power =
    sRated * cosPhiRated

  /** As this is a state-full model (with respect to the current operation
    * condition and inner temperature), the power calculation operates on the
    * current state of the model, which has to be calculated beforehand by
    * [[HpModel.calculateNextState]]. This state then is fed into the power
    * calculation logic by <i>hpData</i>.
    *
    * @param relevantData
    *   data of heat pump including state of the heat pump
    * @return
    *   active power
    */
  override protected def calculateActivePower(
      relevantData: HpRelevantData
  ): Power = {
    relevantData.hpState = calculateNextState(relevantData)
    relevantData.hpState.activePower
  }

  /** "Calculate" the heat output of the heat pump. The hp's state is already
    * updated, because the calculation of apparent power in
    * [[ApparentPowerAndHeatParticipant]] did trigger it. So we only need to
    * extract the information
    * @param tick
    *   Current simulation time for the calculation
    * @param data
    *   Relevant (external) data for calculation
    * @return
    *   The heat, that is produced in that instant
    */
  override def calculateHeat(
      tick: Long,
      data: HpRelevantData
  ): Power =
    data.hpState.qDot

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
  private def operatesInNextState(hpData: HpRelevantData): Boolean =
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
        (pRated, pThermal)
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
}

/** Create valid [[HpModel]] by calling the apply function.
  */
case object HpModel {

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
        inputModel.getType
          .getsRated()
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      inputModel.getType.getCosPhiRated,
      Kilowatts(
        inputModel.getType
          .getpThermal()
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
      activePower: Power,
      qDot: Power,
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
      ambientTemperature: Temperature
  ) extends CalcRelevantData

  /** Internal method to construct a new [[HpModel]] based on a provided
    * [[HpInput]]
    *
    * @param hpInput
    *   instance of [[HpInput]] this chp model should be built from
    * @param simulationStartDate
    *   wall-clock time, the simulation starts
    * @param simulationEndDate
    *   wall-clock time, the simulation ends
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
      thermalGrid: ThermalGrid
  ): HpModel = {
    val operationInterval = SystemComponent.determineOperationInterval(
      simulationStartDate,
      simulationEndDate,
      hpInput.getOperationTime
    )

    val model = new HpModel(
      hpInput.getUuid,
      hpInput.getId,
      operationInterval,
      scalingFactor,
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
