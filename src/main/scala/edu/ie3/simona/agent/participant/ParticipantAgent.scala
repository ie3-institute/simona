/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.{SimonaAgent, ValueStore}
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, SecondaryData}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FromOutsideBaseStateData,
  ModelBaseStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  CollectRegistrationConfirmMessages,
  ParticipantInitializeStateData,
  ParticipantInitializingStateData,
  ParticipantUninitializedStateData
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.{Idle, Uninitialized}
import edu.ie3.simona.agent.state.ParticipantAgentState.{
  Calculate,
  HandleInformation
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  ModelState,
  SystemParticipant
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueFlexControl,
  RequestFlexOptions
}
import edu.ie3.simona.ontology.messages.PowerMessage.RequestAssetPowerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.TriggerWithIdMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  ProvisionMessage,
  RegistrationResponseMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ParticipantTrigger.StartCalculationTrigger
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  FinishGridSimulationTrigger,
  InitializeParticipantAgentTrigger
}
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.ClassTag

/** Common properties to participant agents
  *
  * @tparam PD
  *   Type of primary data, the calculation model will produce
  * @tparam CD
  *   Type of compilation of [[SecondaryData]], that are needed for calculation
  * @tparam MS
  *   Type of model state data
  * @tparam D
  *   Type of participant state data to use
  * @tparam I
  *   Type of input model to accept
  * @tparam MC
  *   Type of model config to accept
  * @tparam M
  *   Type of the calculation model
  *
  * @author
  *   hiry
  * @version 0.1
  * @since 2019-07-04
  */
abstract class ParticipantAgent[
    PD <: PrimaryDataWithApparentPower[PD],
    CD <: CalcRelevantData,
    MS <: ModelState,
    D <: ParticipantStateData[PD],
    I <: SystemParticipantInput,
    MC <: SimonaConfig.BaseRuntimeConfig,
    M <: SystemParticipant[CD, MS]
](scheduler: ActorRef)(implicit val tag: ClassTag[MS])
    extends SimonaAgent[ParticipantStateData[PD]] {

  val alternativeResult: PD

  protected val handleUnitialized: StateFunction = {
    /* Initialize the agent */
    case Event(
          TriggerWithIdMessage(
            initTrigger @ InitializeParticipantAgentTrigger(
              ParticipantInitializeStateData(
                inputModel,
                modelConfig,
                primaryServiceProxy,
                services,
                simulationStartDate,
                simulationEndDate,
                resolution,
                requestVoltageDeviationThreshold,
                outputConfig,
                maybeEmAgent
              )
            ),
            triggerId,
            _
          ),
          _: ParticipantUninitializedStateData[PD]
        ) =>
      /* Ask the primary service proxy for data. If some is available, it will delegate the request to a worker and
       * that will confirm, otherwise, a failed registration is announced. */
      holdTickAndTriggerId(initTrigger.tick, triggerId)
      primaryServiceProxy ! PrimaryServiceRegistrationMessage(
        inputModel.getUuid
      )
      goto(HandleInformation) using ParticipantInitializingStateData(
        inputModel,
        modelConfig,
        services,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent
      )
  }

  protected val handleIdle: StateFunction = {
    case Event(
          TriggerWithIdMessage(ActivityStartTrigger(currentTick), triggerId, _),
          modelBaseStateData: ParticipantModelBaseStateData[PD, CD, MS, M]
        ) if modelBaseStateData.services.isEmpty =>
      /* An activity start trigger is sent and no data is awaited (neither secondary nor primary). Therefore go straight
       * ahead to calculations */

      log.debug(
        s"Received activity start trigger {} for tick {}",
        triggerId,
        currentTick
      )

      /* Hold tick and trigger, as state transition is needed */
      holdTickAndTriggerId(currentTick, triggerId)

      /* Remove this tick from the array of foreseen activation ticks */
      val additionalActivationTicks =
        modelBaseStateData.additionalActivationTicks.filter(_ > currentTick)

      val updatedBaseStateData = BaseStateData.updateBaseStateData(
        modelBaseStateData,
        modelBaseStateData.resultValueStore,
        modelBaseStateData.requestValueStore,
        modelBaseStateData.voltageValueStore,
        additionalActivationTicks,
        modelBaseStateData.foreseenDataTicks
      )

      self ! StartCalculationTrigger(currentTick)
      goto(Calculate) using updatedBaseStateData

    case Event(
          TriggerWithIdMessage(ActivityStartTrigger(currentTick), triggerId, _),
          modelBaseStateData: ParticipantModelBaseStateData[PD, CD, MS, M]
        ) =>
      /* An activity start trigger is sent, but I'm not sure yet, if secondary data will arrive. Figure out, if someone
       * is about to deliver new data and either go to HandleInformation, check and possibly wait for data provision
       * messages or directly go to Calculate and utilize what is already there */
      handleActivityStartTriggerAndGoToHandleInformation(
        currentTick,
        triggerId,
        modelBaseStateData
      )

    case Event(
          TriggerWithIdMessage(ActivityStartTrigger(currentTick), triggerId, _),
          fromOutsideBaseStateData: FromOutsideBaseStateData[M, PD]
        ) =>
      /* An activity start trigger is sent, but I'm still expecting primary data. Go to HandleInformation and wait for
       * a data provision message */
      handleActivityStartTriggerAndGoToHandleInformation(
        currentTick,
        triggerId,
        fromOutsideBaseStateData
      )

    case Event(
          msg: ProvisionMessage[Data],
          baseStateData: BaseStateData[PD]
        ) =>
      /* Somebody has sent new primary or secondary data. Collect, what is expected for this tick. Go over to data
       * handling */
      handleDataProvisionAndGoToHandleInformation(msg, baseStateData)

    case Event(
          RequestAssetPowerMessage(requestTick, eInPu, fInPu),
          baseStateData: BaseStateData[PD]
        ) =>
      /* Determine the reply and stay in this state (or stash the message if the request cannot yet be answered) */
      answerPowerRequestAndStayWithUpdatedStateData(
        baseStateData,
        requestTick,
        eInPu,
        fInPu,
        alternativeResult
      )

    case Event(
          FinishGridSimulationTrigger(tick),
          baseStateData: BaseStateData[PD]
        ) =>
      // clean up agent result value store
      finalizeTickAfterPF(baseStateData, tick)

    case Event(
          RequestFlexOptions(tick),
          baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M]
        ) =>
      val updatedStateData = handleFlexRequest(baseStateData, tick)

      stay() using updatedStateData

    case Event(
          RequestFlexOptions(tick),
          DataCollectionStateData(baseStateData, data, _)
        ) =>
      val participantStateData =
        baseStateData match {
          case participantStateData: ParticipantModelBaseStateData[
                PD,
                CD,
                MS,
                M
              ] =>
            participantStateData
          case otherStateData =>
            throw new IllegalStateException(
              s"Unexpected state data $otherStateData"
            )
        }

      val updatedReceivedSecondaryData = ValueStore.updateValueStore(
        participantStateData.receivedSecondaryDataStore,
        currentTick,
        data.map { case (actorRef, Some(data: SecondaryData)) =>
          actorRef -> data
        }
      )

      val updatedStateData = handleFlexRequest(
        participantStateData.copy(
          receivedSecondaryDataStore = updatedReceivedSecondaryData
        ),
        tick
      )

      stay() using updatedStateData

    case Event(
          flexCtrl: IssueFlexControl,
          baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M]
        ) =>
      handleFlexCtrl(baseStateData, flexCtrl, scheduler)
  }

  protected val handleHandleInformation: StateFunction = {
    /* Receive registration confirm from primary data service -> Set up actor for replay of data */
    case Event(
          RegistrationSuccessfulMessage(maybeNextDataTick),
          ParticipantInitializingStateData(
            inputModel: I,
            modelConfig: MC,
            secondaryDataServices,
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            _
          )
        ) =>
      log.debug("Will replay primary data")
      initializeParticipantForPrimaryDataReplay(
        inputModel,
        modelConfig,
        secondaryDataServices,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        sender() -> maybeNextDataTick,
        scheduler
      )

    /* Receive registration refuse from primary data service -> Set up actor for model calculation */
    case Event(
          RegistrationResponseMessage.RegistrationFailedMessage,
          ParticipantInitializingStateData(
            inputModel: I,
            modelConfig: MC,
            secondaryDataServices,
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            maybeEmAgent
          )
        ) =>
      log.debug("Will perform model calculations")
      initializeParticipantForModelCalculation(
        inputModel,
        modelConfig,
        secondaryDataServices,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        scheduler,
        maybeEmAgent
      )

    /* Receiving the registration replies from services and collect their next data ticks */
    case Event(
          msg: RegistrationResponseMessage,
          stateData: CollectRegistrationConfirmMessages[PD]
        ) =>
      handleRegistrationResponse(scheduler, msg, stateData)

    case Event(
          TriggerWithIdMessage(
            ActivityStartTrigger(activationTick),
            triggerId,
            _
          ),
          stateData: DataCollectionStateData[PD]
        ) =>
      /* The actor received an ActivityStartTrigger. Check, if there is everything at its place. If so, change state
       * accordingly, otherwise stay here and wait for the messages */
      holdTickAndTriggerId(activationTick, triggerId)
      checkForExpectedDataAndChangeState(
        stateData,
        isYetTriggered = true,
        activationTick,
        scheduler
      )(stateData.baseStateData.outputConfig)

    case Event(
          msg: ProvisionMessage[_],
          stateData @ DataCollectionStateData(
            baseStateData: BaseStateData[PD],
            data,
            isYetTriggered
          )
        ) =>
      /* We yet have received at least one data provision message. Handle all messages, that follow up for this tick, by
       * adding the received data to the collection state data and checking, if everything is at its place */
      if (
        data.contains(sender()) ||
        baseStateData.foreseenDataTicks.exists {
          case (ref, None) => sender() == ref
          case _           => false
        }
      ) {
        /* Update the yet received information */
        val updatedData = data + (sender() -> Some(msg.data))

        /* Depending on if a next data tick can be foreseen, either update the entry in the base state data or remove
         * it */
        val foreSeenDataTicks =
          baseStateData.foreseenDataTicks + (sender() -> msg.nextDataTick)
        val updatedBaseStateData = BaseStateData.updateBaseStateData(
          baseStateData,
          baseStateData.resultValueStore,
          baseStateData.requestValueStore,
          baseStateData.voltageValueStore,
          baseStateData.additionalActivationTicks,
          foreSeenDataTicks
        )
        val updatedStateData: DataCollectionStateData[PD] = stateData
          .copy(
            baseStateData = updatedBaseStateData,
            data = updatedData
          )
        checkForExpectedDataAndChangeState(
          updatedStateData,
          isYetTriggered,
          currentTick,
          scheduler
        )(updatedBaseStateData.outputConfig)
      } else
        throw new IllegalStateException(
          s"Did not expect message from ${sender()} at tick $currentTick"
        )

    case Event(
          RequestAssetPowerMessage(currentTick, _, _),
          DataCollectionStateData(_, data, yetTriggered)
        ) =>
      if (log.isDebugEnabled) {
        val awaitedSenders = data.filter(_._2.isEmpty).keys
        val yetReceivedSenders = data.filter(_._2.isDefined).map {
          case (actorRef, data) => s"$actorRef -> $data"
        }
        log.debug(
          s"Got asset power request for tick {}'. Will answer it later. " +
            s"I'm waiting for senders: '{}', already got from '{}'. " +
            s"The actor has {} been triggered yet.",
          s"$currentTick from ${sender()}",
          awaitedSenders,
          yetReceivedSenders,
          if (yetTriggered) "" else "NOT"
        )
      }
      stash()
      stay()

    case Event(
          _: RequestFlexOptions,
          _: DataCollectionStateData[PD]
        ) =>
      stash()
      stay()
  }

  protected val handleCalculate: StateFunction = {
    case Event(
          StartCalculationTrigger(currentTick),
          modelBaseStateData: ParticipantModelBaseStateData[PD, CD, MS, M]
        ) =>
      /* Model calculation without any secondary data needed */
      val voltage = getAndCheckNodalVoltage(modelBaseStateData, currentTick)

      calculatePowerWithoutSecondaryDataAndGoToIdle(
        modelBaseStateData,
        currentTick,
        scheduler,
        voltage,
        calculateModelPowerFunc
      )

    case Event(
          StartCalculationTrigger(currentTick),
          DataCollectionStateData(
            participantStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
            data,
            _
          )
        ) =>
      val updatedReceivedSecondaryData = ValueStore.updateValueStore(
        participantStateData.receivedSecondaryDataStore,
        currentTick,
        data.map { case (actorRef, Some(data: SecondaryData)) =>
          actorRef -> data
        }
      )

      /* At least parts of the needed data has been received or it is an additional activation, that has been triggered.
       * Anyways, the calculation routine has also to take care of filling up missing data. */
      calculatePowerWithSecondaryDataAndGoToIdle(
        participantStateData.copy(
          receivedSecondaryDataStore = updatedReceivedSecondaryData
        ),
        currentTick,
        scheduler
      )

    case Event(RequestAssetPowerMessage(currentTick, _, _), _) =>
      log.debug(
        s"Got asset power request for tick {} from '{}'. Will answer it later.",
        currentTick,
        sender()
      )
      stash()
      stay()

    case Event(_: ProvisionMessage[_], _) |
        Event(TriggerWithIdMessage(ActivityStartTrigger(_), _, _), _) =>
      /* I got faced to new data, also I'm not ready to handle it, yet OR I got asked to do something else, while I'm
       * still busy, I will put it aside and answer it later */
      stash()
      stay()
  }

  // general agent states
  // first fsm state of the agent
  startWith(Uninitialized, ParticipantUninitializedStateData[PD]())

  when(Uninitialized)(handleUnitialized)

  when(Idle)(handleIdle)

  when(HandleInformation)(handleHandleInformation)

  when(Calculate)(handleCalculate)

  // everything else
  whenUnhandled(myUnhandled())

  /** Initializing the actor in order to be able to receive and replay primary
    * data from outside. It comprises the confirmation of initialization trigger
    * as well as the establishment of needed [[BaseStateData]]
    *
    * @param inputModel
    *   Input model
    * @param modelConfig
    *   Configuration for the model
    * @param services
    *   Optional list of services, that are needed
    * @param simulationStartDate
    *   Real world time date time, when the simulation starts
    * @param simulationEndDate
    *   Real world time date time, when the simulation ends
    * @param resolution
    *   Agents regular time bin it wants to be triggered e.g one hour
    * @param requestVoltageDeviationThreshold
    *   Threshold, after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @param senderToMaybeTick
    *   Option to possible next tick, when primary data will arrive, and
    *   foreseen sender
    * @param scheduler
    *   Reference to the scheduler
    * @return
    *   Idle state with child of [[BaseStateData]]
    */
  def initializeParticipantForPrimaryDataReplay(
      inputModel: I,
      modelConfig: MC,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
      senderToMaybeTick: (ActorRef, Option[Long]),
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  /** Abstract definition of initialization method, implementation in
    * [[ParticipantAgentFundamentals]]
    *
    * @param inputModel
    *   Input model
    * @param modelConfig
    *   Configuration for the model
    * @param services
    *   Optional list of services, that are needed
    * @param simulationStartDate
    *   Real world time date time, when the simulation starts
    * @param simulationEndDate
    *   Real world time date time, when the simulation ends
    * @param resolution
    *   Agents regular time bin it wants to be triggered e.g one hour
    * @param requestVoltageDeviationThreshold
    *   Threshold, after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @param scheduler
    *   Reference to simulation scheduler
    * @return
    *   Idle state with child of [[BaseStateData]]
    */
  def initializeParticipantForModelCalculation(
      inputModel: I,
      modelConfig: MC,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
      scheduler: ActorRef,
      maybeEmAgent: Option[ActorRef]
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  /** Handles the responses from service providers, this actor has registered
    * itself with. Stay in [[HandleInformation]] as long as responses are
    * pending, go to [[Idle]], if everything is apparent.
    *
    * @param registrationResponse
    *   Incoming response
    * @param stateData
    *   Apparent state data
    * @return
    *   What happens next
    */
  def handleRegistrationResponse(
      scheduler: ActorRef,
      registrationResponse: RegistrationResponseMessage,
      stateData: CollectRegistrationConfirmMessages[PD]
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  /** Handle an [[ActivityStartTrigger]] received in [[Idle]]. Prepare the
    * foreseen senders in this tick and go over to [[HandleInformation]].
    *
    * @param tick
    *   Current tick of the simulation
    * @param triggerId
    *   Id of the [[ActivityStartTrigger]]
    * @param baseStateData
    *   The basic state data
    * @return
    *   Transition to [[HandleInformation]] utilising appropriate new
    *   [[DataCollectionStateData]]
    */
  def handleActivityStartTriggerAndGoToHandleInformation(
      tick: Long,
      triggerId: Long,
      baseStateData: BaseStateData[PD]
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    /* Hold tick and trigger, as we are about to changes states for a while */
    holdTickAndTriggerId(tick, triggerId)

    /* Prepare the state data for the HandleInformation state */
    val expectedSenders = baseStateData.foreseenDataTicks
      .collect {
        case (actorRef, Some(expectedTick)) if expectedTick == tick =>
          actorRef -> None
      }

    val unforeseenPossible =
      baseStateData.foreseenDataTicks.exists(_._2.isEmpty)

    val nextStateData = DataCollectionStateData(
      baseStateData,
      expectedSenders,
      yetTriggered = true
    )

    if (expectedSenders.nonEmpty || unforeseenPossible) {
      /* Do await provision messages in HandleInformation */
      goto(HandleInformation) using nextStateData
    } else {
      // I don't expect new data.
      baseStateData match {
        case modelStateData: ModelBaseStateData[_, _, _, _]
            if modelStateData.isEmManaged =>
          // We're em-managed. Go to Idle and wait
          goto(Idle) using nextStateData
        case _ =>
          // Make a shortcut to Calculate
          self ! StartCalculationTrigger(currentTick)
          goto(Calculate) using nextStateData
      }

    }
  }

  /** Handle an incoming data provision message in Idle, try to figure out who's
    * about to send information in this tick as well. Map together all senders
    * with their yet apparent information.
    *
    * @param msg
    *   Incoming message to be handled
    * @param baseStateData
    *   Base state data
    * @return
    *   state change to [[HandleInformation]] with updated base state data
    */
  def handleDataProvisionAndGoToHandleInformation(
      msg: ProvisionMessage[Data],
      baseStateData: BaseStateData[PD]
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  /** Checks, if all data is available and change state accordingly. Three cases
    * are possible: 1) There is still something missing: Stay here and wait 2)
    * Everything is at place and the [[ActivityStartTrigger]] has yet been sent
    * 2.1) If the agent is meant to replay external primary data: Announce
    * result, add content to result value store, go to [[Idle]] and answer the
    * scheduler, that the activity start trigger is fulfilled. 2.2) All
    * secondary data is there, go to [[Calculate]] and ask the scheduler to
    * trigger ourself for starting the model based calculation 3) Everything is
    * at place and the [[ActivityStartTrigger]] has NOT yet been sent: Stay here
    * and wait
    *
    * @param stateData
    *   Apparent state data
    * @param isYetTriggered
    *   Boolean if a trigger yet has been received
    * @param tick
    *   Current tick, the simulation is in
    * @param scheduler
    *   Scheduler for scheduling things
    * @param outputConfig
    *   Output configuration for results
    * @return
    *   What's happening next
    */
  def checkForExpectedDataAndChangeState(
      stateData: DataCollectionStateData[PD],
      isYetTriggered: Boolean,
      tick: Long,
      scheduler: ActorRef
  )(implicit
      outputConfig: ParticipantNotifierConfig
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[PD, CD, MS, M],
      ComparableQuantity[Dimensionless]
  ) => PD

  /** Abstractly calculate the power output of the participant without needing
    * any secondary data. The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage]] to
    * scheduler and using update result values. Actual implementation can be
    * found in [[ParticipantAgentFundamentals]]
    *
    * @param baseStateData
    *   Base state data to update
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @param nodalVoltage
    *   Current nodal voltage
    * @param calculateModelPowerFunc
    *   Function, that transfer the current tick, the state data and the nodal
    *   voltage to participants power exchange with the grid
    * @return
    *   [[Idle]] with updated result values
    */
  def calculatePowerWithoutSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      currentTick: Long,
      scheduler: ActorRef,
      nodalVoltage: ComparableQuantity[Dimensionless],
      calculateModelPowerFunc: (
          Long,
          ParticipantModelBaseStateData[PD, CD, MS, M],
          ComparableQuantity[Dimensionless]
      ) => PD
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  /** Abstractly calculate the power output of the participant utilising
    * secondary data. However, it might appear, that not the complete set of
    * secondary data is available for the given tick. This might especially be
    * true, if the actor has been additionally activated. This method thereby
    * has to try and fill up missing data with the last known data, as this is
    * still supposed to be valid. The secondary data therefore is put to the
    * calculation relevant data store. <p>The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage]] to
    * scheduler and using update result values.</p> </p>Actual implementation
    * can be found in each participant's fundamentals.</p>
    *
    * @param baseStateData
    *   The base state data with collected secondary data
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  protected def createInitialState(): MS

  protected def handleFlexRequest(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      tick: Long
  ): ParticipantModelBaseStateData[PD, CD, MS, M]

  protected def calculateFlexOptions(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      tick: Long
  ): ParticipantModelBaseStateData[PD, CD, MS, M]

  protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      tick: Long
  ): CD

  protected def handleFlexCtrl(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      flexCtrl: IssueFlexControl,
      scheduler: ActorRef
  ): State

  protected def calculateResult(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      tick: Long,
      activePower: ComparableQuantity[Power]
  ): PD

  /** Determining the reply to an
    * [[edu.ie3.simona.ontology.messages.PowerMessage.RequestAssetPowerMessage]],
    * send this answer and stay in the current state. If no reply can be
    * determined (because an activation or incoming data is expected), the
    * message is stashed.
    *
    * This methods goal is to find a reply as fast as possible, therefore the
    * following options are checked in subsequent order: 1) This request (in
    * terms of tick and nodal voltage) has already been answered: Send it once
    * again 2) This request (in terms of tick) has already been answered, but
    * with different nodal voltage: Recalculate reactive power and send again 3)
    * No answer on this request, yet: Determine active and reactive power from
    * previous simulation results
    *
    * @param baseStateData
    *   Base state data to update
    * @param requestTick
    *   Tick of the incoming request
    * @param eInPu
    *   Real part of the complex, dimensionless nodal voltage
    * @param fInPu
    *   Imaginary part of the complex, dimensionless nodal voltage
    * @param alternativeResult
    *   Alternative result to use, if no reasonable result can be obtained
    * @return
    *   The very same state with updated request value store
    */
  def answerPowerRequestAndStayWithUpdatedStateData(
      baseStateData: BaseStateData[PD],
      requestTick: Long,
      eInPu: ComparableQuantity[Dimensionless],
      fInPu: ComparableQuantity[Dimensionless],
      alternativeResult: PD
  ): FSM.State[AgentState, ParticipantStateData[PD]]

  /** Abstract definition to notify result listeners from every participant
    * agent after power flow convergence
    *
    * @param baseStateData
    *   Basic state data
    * @param currentTick
    *   Tick, the trigger belongs to
    */
  def announceAssetPowerRequestReply(
      baseStateData: BaseStateData[_],
      currentTick: Long,
      activePower: ComparableQuantity[Power],
      reactivePower: ComparableQuantity[Power]
  )(implicit outputConfig: ParticipantNotifierConfig): Unit

  /** Abstract definition to clean up agent value stores after power flow
    * convergence. This is necessary for agents whose results are time dependent
    * e.g. storage agents
    *
    * @param baseStateData
    *   Basic state data
    * @param currentTick
    *   Tick, the trigger belongs to
    * @return
    *   [[Idle]] with updated result values
    */
  def finalizeTickAfterPF(
      baseStateData: BaseStateData[PD],
      currentTick: Long
  ): FSM.State[AgentState, ParticipantStateData[PD]]
}

object ParticipantAgent {

  /** Verifies that a nodal voltage value has been provided in the model
    * calculation request and returns it. Actual implementation can be found in
    * [[ParticipantAgentFundamentals]]
    *
    * @param baseStateData
    *   provided basic state data
    * @param currentTick
    *   current tick
    * @return
    *   nodal voltage
    */
  def getAndCheckNodalVoltage(
      baseStateData: BaseStateData[_ <: PrimaryData],
      currentTick: Long
  ): ComparableQuantity[Dimensionless] = {
    baseStateData.voltageValueStore.last(currentTick) match {
      case Some((_, voltage)) => voltage
      case None =>
        throw new InconsistentStateException(
          "Not knowing any nodal voltage is not supposed to happen."
        )
    }
  }
}
