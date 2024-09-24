/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.ChpInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ChpModel._
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.thermal.{MutableStorage, ThermalStorage}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.ProvideFlexOptions
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities
import edu.ie3.util.scala.quantities.DefaultQuantities._
import squants.energy.Kilowatts
import squants.{Energy, Power, Seconds, Time}

import java.time.ZonedDateTime
import java.util.UUID

/** Model of a combined heat and power plant (CHP) with a [[ThermalStorage]]
  * medium and its current [[ChpState]].
  *
  * @param uuid
  * @param id
  *   the element's human readable id
  * @param operationInterval
  *   Interval, in which the system is in operation
  * @param qControl
  *   Type of reactive power control
  * @param sRated
  *   Rated apparent power
  * @param cosPhiRated
  *   Rated power factor
  * @param pThermal
  *   Rated thermal power
  * @param storage
  *   Storage medium
  */
final case class ChpModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    pThermal: Power,
    storage: ThermalStorage with MutableStorage,
) extends SystemParticipant[ChpRelevantData, ApparentPower, ConstantState.type](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    )
    with ApparentPowerParticipant[ChpRelevantData, ConstantState.type] {

  val pRated: Power = sRated * cosPhiRated

  /** As this is a state-full model (with respect to the current operation
    * condition and its thermal storage), the power calculation operates on the
    * current state of the model, which has to be calculated beforehand by
    * [[ChpModel.calculateNextState]]. This state then is fed into the power
    * calculation logic by <i>chpData</i>.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   active power
    */
  override protected def calculateActivePower(
      modelState: ConstantState.type,
      chpData: ChpRelevantData,
  ): Power =
    chpData.chpState.activePower

  /** Given a [[ChpRelevantData]] object, containing the [[ChpState]], the heat
    * demand and the current time tick, this function calculates the CHPs next
    * state while trying to cover the demand. To get the actual active power of
    * this state please use [[calculateActivePower]] with the generated state
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   next [[ChpState]]
    */
  def calculateNextState(
      chpData: ChpRelevantData
  ): ChpState = generateStateCalculation(chpData)(chpData)

  /** Depending on the input, this function returns a fitting 'calculateState'
    * function. The five cases, leading to four different 'calculateState'
    * variants are listed below. <ul> <li>Case 1: CHP not running and no demand
    * (isCovered is ignored as there is no demand). <li>Case 2: Existing demand
    * cannot be covered (CHP running is ignored as CHP has to be turned on
    * anyway). <li>Case 3: CHP not running and existing demand can be covered.
    * <li>Case 4: CHP running and no demand (isCovered is ignored as there is no
    * demand). In this case, like in case 5,
    * [[calculateStateRunningDemandCovered]] is called, because having no demand
    * is equivalent to having the demand covered. <li>Case 5: CHP running and
    * existing demand can be covered. </ul>
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   partially applied function taking a [[ChpRelevantData]] object
    */
  private def generateStateCalculation(
      chpData: ChpRelevantData
  ): ChpRelevantData => ChpState = {
    val isRunning = chpData.chpState.isRunning
    val hasDemand = chpData.heatDemand > DefaultQuantities.zeroKWH
    val isCovered = isDemandCovered(chpData)

    (isRunning, hasDemand, isCovered) match {
      case (false, false, _)   => calculateStateNotRunningNoDemand
      case (_, true, false)    => calculateStateDemandNotCovered
      case (false, true, true) => calculateStateNotRunningDemandCovered
      case (true, false, _)    => calculateStateRunningDemandCovered
      case (true, true, true)  => calculateStateRunningDemandCovered
    }
  }

  /** Because the CHP is not running and there is no demand, only the time tick
    * is updated.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   next [[ChpState]]
    */
  private def calculateStateNotRunningNoDemand(
      chpData: ChpRelevantData
  ): ChpState =
    ChpState(
      isRunning = false,
      chpData.currentTimeTick,
      DefaultQuantities.zeroKW,
      DefaultQuantities.zeroKWH,
    )

  /** The demand cannot be covered, therefore this function sets storage level
    * to minimum.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   next [[ChpState]]
    */
  private def calculateStateDemandNotCovered(
      chpData: ChpRelevantData
  ): ChpState = {
    val energy = chpEnergy(chpData)
    // ChpModel ignores possible lack of energy from prior time steps.
    storage.tryToTakeAndReturnLack(chpData.heatDemand)
    ChpState(isRunning = true, chpData.currentTimeTick, pRated, energy)
  }

  /** Because the stored energy is enough to cover the demand, CHP stays turned
    * off and demand is extracted from storage medium.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   next [[ChpState]]
    */
  private def calculateStateNotRunningDemandCovered(
      chpData: ChpRelevantData
  ): ChpState = {
    // Returned lack is always zero, because demand is covered.
    storage.tryToTakeAndReturnLack(chpData.heatDemand)
    ChpState(
      isRunning = false,
      chpData.currentTimeTick,
      DefaultQuantities.zeroKW,
      DefaultQuantities.zeroKWH,
    )
  }

  /** Demand can be covered by storage and is extracted from it. Because the CHP
    * is turned on, this function checks if storage 'overflows' and adjusts the
    * output energy.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   next [[ChpState]]
    */
  private def calculateStateRunningDemandCovered(
      chpData: ChpRelevantData
  ): ChpState = {
    val differenceEnergy = chpEnergy(chpData) - chpData.heatDemand
    if (differenceEnergy < zeroKWH) {
      // Returned lack is always zero, because demand is covered.
      storage.tryToTakeAndReturnLack(differenceEnergy * -1)
      calculateStateRunningSurplus(chpData)
    } else {
      val surplus = storage.tryToStoreAndReturnRemainder(differenceEnergy)
      calculateStateRunningSurplus(chpData, surplus)
    }
  }

  /** In reality the CHP turns off once the storage is full, instead of
    * producing an overflow. As this is a simulation, the function has to adjust
    * the thermal output energy, by removing the redundant surplus.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @param surplus
    *   optional surplus energy
    * @return
    *   total energy minus surplus energy
    */
  private def calculateStateRunningSurplus(
      chpData: ChpRelevantData,
      surplus: Option[Energy] = None,
  ): ChpState = {
    surplus match {
      case Some(surplusEnergy) =>
        ChpState(
          isRunning = false,
          chpData.currentTimeTick,
          pRated,
          chpEnergy(chpData) - surplusEnergy,
        )
      case None =>
        ChpState(
          isRunning = true,
          chpData.currentTimeTick,
          pRated,
          chpEnergy(chpData),
        )
    }
  }

  /** Multiply the power with time running to get the total energy.
    *
    * @param chpData
    *   data containing current time tick
    * @return
    *   energy
    */
  private def powerToEnergy(
      chpData: ChpRelevantData,
      power: Power,
  ): Energy =
    power * timeRunning(chpData)

  /** Check if the stored energy suffices to cover the heat demand. If not,
    * check if CHP thermal output energy plus stored energy is enough to cover
    * the demand.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   is demand covered
    */
  private def isDemandCovered(chpData: ChpRelevantData) =
    storage.isDemandCoveredByStorage(chpData.heatDemand) ||
      totalUsableEnergy(chpData) >= chpData.heatDemand

  private def chpEnergy(chpData: ChpRelevantData): Energy =
    powerToEnergy(chpData, pThermal)

  /** Returns the storage mediums total usable plus the CHP thermal output
    * energy. <p> See [[MutableStorage.usableThermalEnergy]] for the definition
    * of 'usable energy' of a storage.
    *
    * @param chpData
    *   state of the chp and heat demand
    * @return
    *   total usable energy
    */
  private def totalUsableEnergy(
      chpData: ChpRelevantData
  ): Energy =
    storage.usableThermalEnergy + chpEnergy(chpData)

  private def timeRunning(chpData: ChpRelevantData): Time =
    Seconds(chpData.currentTimeTick - chpData.chpState.lastTimeTick)

  override def determineFlexOptions(
      data: ChpRelevantData,
      lastState: ConstantState.type,
  ): ProvideFlexOptions =
    ProvideMinMaxFlexOptions.noFlexOption(
      uuid,
      calculateActivePower(lastState, data),
    )

  override def handleControlledPowerChange(
      data: ChpRelevantData,
      lastState: ConstantState.type,
      setPower: squants.Power,
  ): (ConstantState.type, FlexChangeIndicator) =
    (lastState, FlexChangeIndicator())

}

/** Create valid ChpModel by calling the apply function.
  */
object ChpModel {

  /** As the ChpModel class is a dynamic model, it requires a state for its
    * calculations. The state contains all variables needed, except the storage
    * level.
    *
    * @param isRunning
    *   indicates if CHP is turned on
    * @param lastTimeTick
    *   contains last time tick
    * @param activePower
    *   result active power
    * @param thermalEnergy
    *   result thermal energy
    */
  final case class ChpState(
      isRunning: Boolean,
      lastTimeTick: Long,
      activePower: Power,
      thermalEnergy: Energy,
  )

  /** Main data required for simulation/calculation, containing a [[ChpState]],
    * the heat demand and the current time tick. <p>
    * [[ChpRelevantData.currentTimeTick]] and [[ChpState.lastTimeTick]] form a
    * time interval for the current state calculation. One time tick represents
    * one second (3600 time ticks = 1 hour).
    *
    * @param chpState
    *   a [[ChpState]]
    * @param heatDemand
    *   current heat demand
    * @param currentTimeTick
    *   contains current time tick
    */
  final case class ChpRelevantData(
      chpState: ChpState,
      heatDemand: Energy,
      currentTimeTick: Long,
  ) extends CalcRelevantData

  /** Function to construct a new [[ChpModel]] based on a provided [[ChpInput]]
    *
    * @param chpInput
    *   instance of [[ChpInput]] this chp model should be built from
    * @param simulationStartDate
    *   Simulation time at which the simulation starts
    * @param simulationEndDate
    *   Simulation time at which the simulation ends
    * @param qControl
    *   Strategy to control the reactive power output
    * @param scalingFactor
    *   Scale the output of this asset by the given factor
    * @param thermalStorage
    *   instance of [[ThermalStorage]] used as thermal storage
    * @return
    *   a ready-to-use [[ChpModel]] with referenced electric parameters
    */
  def apply(
      chpInput: ChpInput,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      qControl: QControl,
      scalingFactor: Double,
      thermalStorage: ThermalStorage with MutableStorage,
  ): ChpModel = {
    val scaledInput = chpInput.copy().scale(scalingFactor).build()

    val operationInterval = SystemComponent.determineOperationInterval(
      simulationStartDate,
      simulationEndDate,
      scaledInput.getOperationTime,
    )

    val model = new ChpModel(
      scaledInput.getUuid,
      scaledInput.getId,
      operationInterval,
      qControl,
      Kilowatts(
        scaledInput.getType.getsRated
          .to(PowerSystemUnits.KILOWATT)
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
      thermalStorage,
    )

    model.enable()
    model
  }
}
