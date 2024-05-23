/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.load.markov

import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.statedata.{BaseStateData, ParticipantStateData}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.load.markov.{MarkovModel, MarkovRelevantData}
import edu.ie3.simona.model.participant.{FlexChangeIndicator, ModelState}
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.{ActorRef, FSM, Props}
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID

object MarkovAgent {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        LoadInput,
        LoadRuntimeConfig,
        ApparentPower,
      ],
      listener: Iterable[ActorRef],
  ): Props =
    Props(
      new MarkovAgent(
        scheduler,
        initStateData: ParticipantInitializeStateData[
          LoadInput,
          LoadRuntimeConfig,
          ApparentPower,
        ],
        listener,
      )
    )
}

/** Creating a load agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  * @param listener
  *   List of listeners interested in results
  */
class MarkovAgent(
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      LoadInput,
      LoadRuntimeConfig,
      ApparentPower,
    ],
    override val listener: Iterable[ActorRef],
) extends ParticipantAgent[
      ApparentPower,
      MarkovRelevantData,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      LoadInput,
      LoadRuntimeConfig,
      MarkovModel,
    ](scheduler, initStateData)
    with MarkovAgentFundamentals {
  /*
   * "Hey, SIMONA! What is handled in ParticipantAgent?"
   * "Hey, dude! The following things are handled in ParticipantAgent:
   *   1) Initialization of Agent
   *   2) Event reactions in Idle state
   *   3) Handling of incoming information
   *   4) Performing model calculations
   * "
   */
  /** Partial function, that is able to transfer
   * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
   * into a pair of active and reactive power
   */
  override protected val calculateModelPowerFunc: (Long, BaseStateData.ParticipantModelBaseStateData[ApparentPower, MarkovRelevantData, ModelState.ConstantState.type, MarkovModel], ModelState.ConstantState.type, Dimensionless) => ApparentPower = ???

  /** Abstractly calculate the power output of the participant utilising
   * secondary data. However, it might appear, that not the complete set of
   * secondary data is available for the given tick. This might especially be
   * true, if the actor has been additionally activated. This method thereby
   * has to try and fill up missing data with the last known data, as this is
   * still supposed to be valid. The secondary data therefore is put to the
   * calculation relevant data store. <p>The next state is [[Idle]], sending a
   * [[edu.ie3.simona.ontology.messages.SchedulerMessage.Completion]] to
   * scheduler and using update result values.</p> </p>Actual implementation
   * can be found in each participant's fundamentals.</p>
   *
   * @param baseStateData
   * The base state data with collected secondary data
   * @param lastModelState
   * The current model state, before applying changes by externally received
   * data
   * @param currentTick
   * Tick, the trigger belongs to
   * @param scheduler
   * [[ActorRef]] to the scheduler in the simulation
   * @return
   * [[Idle]] with updated result values
   */
  override def calculatePowerWithSecondaryDataAndGoToIdle(baseStateData: BaseStateData.ParticipantModelBaseStateData[ApparentPower, MarkovRelevantData, ModelState.ConstantState.type, MarkovModel], lastModelState: ModelState.ConstantState.type, currentTick: Long, scheduler: ActorRef): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = ???

  override protected def createInitialState(baseStateData: BaseStateData.ParticipantModelBaseStateData[ApparentPower, MarkovRelevantData, ModelState.ConstantState.type, MarkovModel]): ModelState.ConstantState.type = ???

  override protected def createCalcRelevantData(baseStateData: BaseStateData.ParticipantModelBaseStateData[ApparentPower, MarkovRelevantData, ModelState.ConstantState.type, MarkovModel], tick: Long): MarkovRelevantData = ???

  /** Handle an active power change by flex control.
   *
   * @param tick
   * Tick, in which control is issued
   * @param baseStateData
   * Base state data of the agent
   * @param data
   * Calculation relevant data
   * @param lastState
   * Last known model state
   * @param setPower
   * Setpoint active power
   * @return
   * Updated model state, a result model and a [[FlexChangeIndicator]]
   */
  override def handleControlledPowerChange(tick: Long, baseStateData: BaseStateData.ParticipantModelBaseStateData[ApparentPower, MarkovRelevantData, ModelState.ConstantState.type, MarkovModel], data: MarkovRelevantData, lastState: ModelState.ConstantState.type, setPower: Power): (ModelState.ConstantState.type, ApparentPower, FlexChangeIndicator) = ???

  /** Abstract method to build the calculation model from input
   *
   * @param inputModel
   * Input model description
   * @param modelConfig
   * Configuration for the model
   * @param simulationStartDate
   * Wall clock time of first instant in simulation
   * @param simulationEndDate
   * Wall clock time of last instant in simulation
   * @return
   */
  override def buildModel(inputModel: ParticipantStateData.InputModelContainer[LoadInput], modelConfig: LoadRuntimeConfig, simulationStartDate: ZonedDateTime, simulationEndDate: ZonedDateTime): MarkovModel = ???

  /** Update the last known model state with the given external, relevant data
   *
   * @param tick
   * Tick to update state for
   * @param modelState
   * Last known model state
   * @param calcRelevantData
   * Data, relevant for calculation
   * @param nodalVoltage
   * Current nodal voltage of the agent
   * @param model
   * Model for calculation
   * @return
   * The updated state at given tick under consideration of calculation
   * relevant data
   */
  override protected def updateState(tick: Long, modelState: ModelState.ConstantState.type, calcRelevantData: MarkovRelevantData, nodalVoltage: Dimensionless, model: MarkovModel): ModelState.ConstantState.type = ???

  /** Determine the average result within the given tick window
   *
   * @param tickToResults
   * Mapping from data tick to actual data
   * @param windowStart
   * First, included tick of the time window
   * @param windowEnd
   * Last, included tick of the time window
   * @param activeToReactivePowerFuncOpt
   * An Option on a function, that transfers the active into reactive power
   * @return
   * The averaged result
   */
  override def averageResults(tickToResults: Map[Long, ApparentPower], windowStart: Long, windowEnd: Long, activeToReactivePowerFuncOpt: Option[Power => ReactivePower]): ApparentPower = ???

  /** Determines the correct result.
   *
   * @param uuid
   * Unique identifier of the physical model
   * @param dateTime
   * Real world date of the result
   * @param result
   * The primary data to build a result model for
   * @return
   * The equivalent event
   */
  override protected def buildResult(uuid: UUID, dateTime: ZonedDateTime, result: ApparentPower): SystemParticipantResult = ???
}
