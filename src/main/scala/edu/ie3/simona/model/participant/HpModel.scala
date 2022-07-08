/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import java.util.UUID
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.HpModel._
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.ThermalHouse
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities

import javax.measure.quantity.{Power, Temperature, Time}
import tech.units.indriya.ComparableQuantity
import edu.ie3.simona.util.TickUtil.TickLong

import java.time.ZonedDateTime

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
  * @param thermalHouse
  *   Thermal house with a variable inner temperature, temperature boundaries,
  *   transmission coefficient and heat energy storage capacity
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
    thermalHouse: ThermalHouse
) extends SystemParticipant[HpData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    ) {

  private val pRated: ComparableQuantity[Power] =
    sRated
      .multiply(cosPhiRated)
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
      hpData: HpData
  ): ComparableQuantity[Power] = {
    calculateNextState(hpData)
    hpData.hpState.activePower
  }

  /** Given a [[HpData]] object, containing the [[HpState]], other values and
    * the current time tick, this function calculates the heat pump's next state
    * To get the actual active power of this state use [[calculateActivePower]]
    * with the generated state
    *
    * @param hpData
    *   data of heat pump including state of the heat pump
    * @return
    *   next [[HpState]]
    */
  def calculateNextState(hpData: HpData): HpState = {
    val turnOn = operatesInNextState(hpData)
    calcState(hpData, turnOn)
  }

  /** Depending on the input, this function decides whether the heat pump will
    * run in the next state or not. As hysteresis is considered, four possible
    * cases can be distinguished: <ul> <li>Case 1: Inner temperature is too high
    * -> Heat pump should not run in next state <li>Case 2: Inner temperature is
    * too low -> Heat pump should run in next state <li>Case 3: Heat pump is
    * running and inner temperature is between boundaries -> Heat pump should
    * run in next state (until over upper boundary) <li>Case 4: Heat pump is not
    * running and inner temperature is between boundaries -> Heat pump should
    * not run in next state (until below lower boundary) </ul>
    *
    * @return
    *   boolean defining if heat pump runs in next time step
    */
  def operatesInNextState(hpData: HpData): Boolean = {
    val isRunning = hpData.hpState.isRunning
    val tooHigh =
      thermalHouse.isInnerTemperatureTooHigh(hpData.hpState.innerTemperature)
    val tooLow =
      thermalHouse.isInnerTemperatureTooLow(hpData.hpState.innerTemperature)

    tooLow || (isRunning && !tooLow && !tooHigh)
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
  private def calcState(hpData: HpData, isRunning: Boolean): HpState = {
    val (newActivePower, newThermalPower) =
      if (isRunning)
        (pRated, pThermal)
      else (DefaultQuantities.zeroKW, DefaultQuantities.zeroKW)

    val duration: ComparableQuantity[Time] =
      hpData.hpState.lastTimeTick.durationUntil(hpData.currentTimeTick)

    val newInnerTemperature = thermalHouse.newInnerTemperature(
      newThermalPower,
      duration,
      hpData.hpState.innerTemperature,
      hpData.ambientTemperature
    )

    HpState(
      isRunning,
      hpData.currentTimeTick,
      newActivePower,
      newInnerTemperature
    )
  }

}

/** Create valid [[HpModel]] by calling the apply function.
  */
case object HpModel {

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
    * @param innerTemperature
    *   inner temperature of the thermal house
    */
  final case class HpState(
      isRunning: Boolean,
      lastTimeTick: Long,
      activePower: ComparableQuantity[Power],
      innerTemperature: ComparableQuantity[Temperature]
  )

  /** Main data required for simulation/calculation, containing a [[HpState]]
    * and the current time tick. <p> [[HpData.currentTimeTick]] and
    * [[HpState.lastTimeTick]] form a time interval for the current state
    * calculation. One time tick represents one second (3600 time ticks = 1
    * hour).
    *
    * @param hpState
    *   a [[HpState]]
    * @param currentTimeTick
    *   contains current time tick
    */
  final case class HpData(
      hpState: HpState,
      currentTimeTick: Long,
      ambientTemperature: ComparableQuantity[Temperature]
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
    * @param thermalHouse
    *   thermal house defining transmission coefficient and heat energy storage
    *   capacity
    * @return
    *   a ready-to-use [[HpModel]] with referenced electric parameters
    */
  def apply(
      hpInput: HpInput,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      qControl: QControl,
      scalingFactor: Double,
      thermalHouse: ThermalHouse
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
      hpInput.getType.getsRated,
      hpInput.getType.getCosPhiRated,
      hpInput.getType.getpThermal,
      thermalHouse
    )

    model.enable()
    model
  }
}
