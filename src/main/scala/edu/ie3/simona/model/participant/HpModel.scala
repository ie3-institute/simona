/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.simona.model.participant.HpModel._
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.ThermalHouse
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import squants.energy.{Kilowatts, Megawatts}
import squants.time.Seconds

import java.util.UUID

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
    sRated: squants.Power,
    cosPhiRated: Double,
    pThermal: squants.Power,
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

  private val pRated: squants.Power =
    sRated * cosPhiRated * scalingFactor

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
  ): squants.Power = {
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
        (pRated, pThermal * scalingFactor)
      else (DefaultQuantities.zeroKW, DefaultQuantities.zeroKW)

    val duration: squants.Time =
      Seconds(
        hpData.hpState.lastTimeTick
          .durationUntil(hpData.currentTimeTick)
          .getValue
          .longValue()
      )

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
      activePower: squants.Power,
      innerTemperature: squants.Temperature
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
      ambientTemperature: squants.Temperature
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
    * @param thermalHouse
    *   thermal house defining transmission coefficient and heat energy storage
    *   capacity
    * @return
    *   a ready-to-use [[HpModel]] with referenced electric parameters
    */
  def apply(
      hpInput: HpInput,
      operationInterval: OperationInterval,
      qControl: QControl,
      thermalHouse: ThermalHouse
  ): HpModel = {
    new HpModel(
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
      thermalHouse
    )
  }

}
