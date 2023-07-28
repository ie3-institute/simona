/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, FSM, PoisonPill}
import akka.event.LoggingAdapter
import akka.util
import akka.util.Timeout
import breeze.numerics.{ceil, floor, pow, sqrt}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals.RelevantResultValues
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  EnrichableData,
  PrimaryDataWithApparentPower
}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData.DateTime
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, SecondaryData}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FromOutsideBaseStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.CollectRegistrationConfirmMessages
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
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  ActorNotRegisteredException,
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.{CalcRelevantData, SystemParticipant}
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  IllegalTriggerMessage,
  ScheduleTriggerMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ProvisionMessage,
  RegistrationResponseMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.ActivityStartTrigger
import edu.ie3.simona.ontology.trigger.Trigger.ParticipantTrigger.StartCalculationTrigger
import edu.ie3.simona.service.ServiceStateData.ServiceActivationBaseStateData
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOVARHOUR,
  KILOWATTHOUR,
  MEGAVAR,
  MEGAWATT,
  PU
}
import edu.ie3.util.quantities.{QuantityUtil => PsuQuantityUtil}
import edu.ie3.util.scala.quantities.QuantityUtil
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.TimeUnit
import javax.measure.quantity.{Dimensionless, Energy, Power}
import scala.collection.SortedSet
import scala.reflect.ClassTag
import scala.util.{Failure, Success, Try}

/** Useful functions to use in [[ParticipantAgent]] s
  */
protected trait ParticipantAgentFundamentals[
    PD <: PrimaryDataWithApparentPower[PD],
    CD <: CalcRelevantData,
    D <: ParticipantStateData[PD],
    I <: SystemParticipantInput,
    MC <: SimonaConfig.BaseRuntimeConfig,
    M <: SystemParticipant[CD]
] extends ServiceRegistration[PD, CD, D, I, MC, M] {
  this: ParticipantAgent[PD, CD, D, I, MC, M] =>
  protected val pdClassTag: ClassTag[PD]
  protected implicit val timeout: util.Timeout = Timeout(10, TimeUnit.SECONDS)

  /** Tries to extract the DateTime value from the base state data and verifies,
    * that it is there
    *
    * @param baseStateData
    *   base state data to derive information from
    * @return
    *   valid DateTime value
    */
  def getAndCheckDateTime(
      baseStateData: DataCollectionStateData[_]
  ): ZonedDateTime = {
    baseStateData.extract[DateTime]() match {
      case Some(dateTime) => dateTime.dateTime
      case None =>
        throw new RuntimeException(
          "Did not receive expected information about the date time!"
        )
    }
  }

  override def initializeParticipantForPrimaryDataReplay(
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
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    val stateData = determineFromOutsideBaseStateData(
      inputModel,
      modelConfig,
      simulationStartDate,
      simulationEndDate,
      resolution,
      requestVoltageDeviationThreshold,
      outputConfig,
      senderToMaybeTick
    )

    /* Confirm final initialization */
    val (_, triggerId) = releaseTickAndTriggerId()
    val newTriggerMessages =
      ServiceActivationBaseStateData.tickToScheduleTriggerMessages(
        senderToMaybeTick._2,
        self
      )
    scheduler ! CompletionMessage(triggerId, newTriggerMessages)
    goto(Idle) using stateData
  }

  /** Determine the needed base state data to use, when data from outside is
    * expected
    *
    * @param inputModel
    *   Input model definition
    * @param modelConfig
    *   Configuration of the model
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
    *   Option onto a pair of sender and it's foreseen next data provision
    * @return
    *   [[FromOutsideBaseStateData]] with required information
    */
  def determineFromOutsideBaseStateData(
      inputModel: I,
      modelConfig: MC,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
      senderToMaybeTick: (ActorRef, Option[Long])
  ): FromOutsideBaseStateData[M, PD] = {
    val model = buildModel(
      inputModel,
      modelConfig,
      simulationStartDate,
      simulationEndDate
    )
    FromOutsideBaseStateData(
      model,
      simulationStartDate,
      simulationEndDate,
      outputConfig,
      SortedSet.empty,
      Map(senderToMaybeTick),
      modelConfig.calculateMissingReactivePowerWithModel,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution,
        inputModel.getNode
          .getvTarget()
          .to(PU)
      ),
      ValueStore.forResult[PD](resolution, 2),
      ValueStore(resolution)
    )
  }

  /** Abstract method to build the calculation model from input
    *
    * @param inputModel
    *   Input model description
    * @param modelConfig
    *   Configuration for the model
    * @param simulationStartDate
    *   Wall clock time of first instant in simulation
    * @param simulationEndDate
    *   Wall clock time of last instant in simulation
    * @return
    */
  def buildModel(
      inputModel: I,
      modelConfig: MC,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): M

  /** Initializing the agent based on the uninitialized state and additional
    * information that has been sent with
    * [[edu.ie3.simona.ontology.trigger.Trigger.InitializeParticipantAgentTrigger]].
    * The base state data is derived based on the foreseen simulation mode, all
    * (currently foreseen) activation triggers are generated and sent to the
    * scheduler. The next state is [[Idle]] using the derived base state data.
    *
    * @param inputModel
    *   Input model definition
    * @param modelConfig
    *   Configuration of the model
    * @param services
    *   Optional collection of services to register with
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
  override def initializeParticipantForModelCalculation(
      inputModel: I,
      modelConfig: MC,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[PD]] =
    try {
      /* Register for services */
      val awaitRegistrationResponsesFrom =
        registerForServices(inputModel, services)

      val baseStateData = determineModelBaseStateData(
        inputModel,
        modelConfig,
        services,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig
      )

      /* If we do have registered with secondary data providers, wait for their responses. If not, the agent does not
       * need additional data and therefore schedules itself for next activation. */
      if (awaitRegistrationResponsesFrom.nonEmpty) {
        goto(HandleInformation) using CollectRegistrationConfirmMessages(
          baseStateData,
          awaitRegistrationResponsesFrom
        )
      } else {
        /* Determine the next activation tick, create a ScheduleTriggerMessage and remove the recently triggered tick */
        val (newTriggers, nextBaseStateData) = popNextActivationTrigger(
          baseStateData
        )
        val (_, triggerId) = releaseTickAndTriggerId()

        log.debug(s"Going to {}, using {}", Idle, baseStateData)
        scheduler ! CompletionMessage(triggerId, newTriggers)
        goto(Idle) using nextBaseStateData
      }
    } catch {
      case e @ (_: AgentInitializationException |
          _: InconsistentStateException) =>
        scheduler ! IllegalTriggerMessage(
          s"Caught exception while initializing: \n ${e.getMessage}",
          self
        )
        goto(Uninitialized)
    }

  /** Abstract definition, individual implementations found in individual agent
    * fundamental classes
    */
  def determineModelBaseStateData(
      inputModel: I,
      modelConfig: MC,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
  ): ParticipantModelBaseStateData[PD, CD, M]

  /** Determine all ticks between the operation start and end of the
    * participant, that are at a full hour or integer multiples of the data's
    * resolution
    *
    * @param simulationStartDate
    *   Beginning of the simulation
    * @param resolution
    *   Resolution of the targeted information in seconds
    * @param operationStart
    *   First tick, in which the load is in operation
    * @param operationEnd
    *   Last tick, in which the load is in operation
    * @return
    *   An [[Array]] of ticks, where the actor wants to be activated
    */
  def activationTicksInOperationTime(
      simulationStartDate: ZonedDateTime,
      resolution: Long,
      operationStart: Long,
      operationEnd: Long
  ): SortedSet[Long] = {
    /* The profile load model holds values in the specified resolution (e.g. for each full quarter hour (00:00,
     * 00:15, ...)). As the simulation might not start at an integer multiple of the resolution, we have to
     * determine, what the first tick is, in which profile information do exist */
    val firstProfileTick =
      firstFullResolutionInSimulation(simulationStartDate, resolution)

    /* Determine activation ticks between participants operation start and end in the resolution of */
    val firstTick = firstProfileTick + resolution * ceil(
      (operationStart - firstProfileTick).toDouble / resolution
    ).toLong
    val lastTick = firstProfileTick + resolution * floor(
      (operationEnd - firstProfileTick).toDouble / resolution
    ).toLong

    /* Set up all ticks between the first and last tick */
    (firstTick to lastTick by resolution).to(SortedSet)
  }

  /** Assume we have information, that are available in a fixed resolution after
    * each full hour (including the full hour), then we have to determine, at
    * what first tick those information are available.
    *
    * @param simulationStartDate
    *   Beginning of the simulation
    * @param resolution
    *   Resolution of the targeted information in seconds
    * @return
    *   The first tick, that is at a full hour or an integer multiple of the
    *   resolution
    * @throws AgentInitializationException
    *   If an hour is not an integer multiple of the resolution
    */
  def firstFullResolutionInSimulation(
      simulationStartDate: ZonedDateTime,
      resolution: Long
  ): Long = {
    if (3600L % resolution != 0)
      throw new AgentInitializationException(
        "The data resolution has to be adjusted, so that an integer multiple of it fits a full hour."
      )
    val secondsSinceFullHour =
      simulationStartDate.getMinute * 60 + simulationStartDate.getSecond
    val secondsSinceQuarterHour = secondsSinceFullHour % resolution
    (resolution - secondsSinceQuarterHour) % resolution
  }

  /** Handles the registration responses from service providers, this actor has
    * registered itself with. Stay in [[HandleInformation]] as long as responses
    * are pending, go to [[Idle]], if everything is apparent.
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
  ): FSM.State[AgentState, ParticipantStateData[PD]] =
    registrationResponse match {
      case RegistrationResponseMessage.RegistrationSuccessfulMessage(
            maybeNextTick
          ) =>
        val remainingResponses =
          stateData.pendingResponses.filter(_ != sender())

        /* If the sender announces a new next tick, add it to the list of expected ticks, else remove the current entry */
        val foreseenDataTicks =
          stateData.baseStateData.foreseenDataTicks + (sender() -> maybeNextTick)

        if (remainingResponses.isEmpty) {
          /* All agent have responded. Determine the next to be used state data and reply completion to scheduler. */
          val complementedBaseStateData = BaseStateData.updateBaseStateData(
            stateData.baseStateData,
            stateData.baseStateData.resultValueStore,
            stateData.baseStateData.requestValueStore,
            stateData.baseStateData.voltageValueStore,
            stateData.baseStateData.additionalActivationTicks,
            foreseenDataTicks
          )

          goToIdleReplyCompletionAndScheduleTriggerForNextAction(
            complementedBaseStateData,
            scheduler
          )
        } else {
          val foreseenDataTicksFlattened = foreseenDataTicks.collect {
            case (ref, Some(value)) => ref -> value
          }

          /* We not have yet received all responses. Wait here a bit for next messages. */
          stay() using stateData.copy(
            pendingResponses = remainingResponses,
            foreseenNextDataTicks = foreseenDataTicksFlattened
          )
        }
      case RegistrationResponseMessage.RegistrationFailedMessage =>
        self ! PoisonPill
        throw new ActorNotRegisteredException(
          s"Registration of actor $actorName for ${sender()} failed."
        )
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
  override def handleDataProvisionAndGoToHandleInformation(
      msg: ProvisionMessage[Data],
      baseStateData: BaseStateData[PD]
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    /* Figure out, who is going to send data in this tick */
    val expectedSenders = baseStateData.foreseenDataTicks
      .flatMap { case (actorRef, optTick) =>
        optTick match {
          case Some(tick) if msg.tick == tick =>
            // expected data
            if (actorRef == sender())
              Some(actorRef -> Some(msg.data))
            else
              Some(actorRef -> None)
          case None if actorRef == sender() =>
            // unexpected data
            Some(actorRef -> Some(msg.data))
          case _ =>
            None
        }
      }

    /* If the sender announces a new next tick, add it to the list of expected ticks, else remove the current entry */
    val foreseenDataTicks =
      baseStateData.foreseenDataTicks + (sender() -> msg.nextDataTick)

    /* Go over to handling these information */
    val nextStateData = DataCollectionStateData(
      BaseStateData.updateBaseStateData(
        baseStateData,
        baseStateData.resultValueStore,
        baseStateData.requestValueStore,
        baseStateData.voltageValueStore,
        baseStateData.additionalActivationTicks,
        foreseenDataTicks
      ),
      expectedSenders,
      yetTriggered = false
    )
    goto(HandleInformation) using nextStateData
  }

  /** Checks, if all data is available and change state accordingly. Three cases
    * are possible: 1) There is still something missing: Stay in the calling
    * state and wait 2) Everything is at place and the [[ActivityStartTrigger]]
    * has yet been sent 2.1) If the agent is meant to replay external primary
    * data: Announce result, add content to result value store, go to [[Idle]]
    * and answer the scheduler, that the activity start trigger is fulfilled.
    * 2.2) All secondary data is there, go to [[Calculate]] and ask the
    * scheduler to trigger ourself for starting the model based calculation 3)
    * Everything is at place and the [[ActivityStartTrigger]] has NOT yet been
    * sent: Stay here and wait
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
  override def checkForExpectedDataAndChangeState(
      stateData: DataCollectionStateData[PD],
      isYetTriggered: Boolean,
      tick: Long,
      scheduler: ActorRef
  )(implicit
      outputConfig: ParticipantNotifierConfig
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    if (!stateData.data.exists(_._2.isEmpty) && isYetTriggered) {
      /* We got everything we expect and we are yet triggered */
      stateData.baseStateData match {
        case fromOutsideBaseStateData: BaseStateData.FromOutsideBaseStateData[
              M,
              PD
            ] =>
          /* Determine the way, the reactive power may be filled up */
          val reactivePowerFunc =
            getReactivePowerFunction(tick, fromOutsideBaseStateData)

          prepareData(stateData.data, reactivePowerFunc) match {
            case Success(mostRecentData) =>
              /* Add received information to base state data and reply, that everything is done */

              /* Announce the result */
              announceSimulationResult(
                stateData.baseStateData,
                tick,
                mostRecentData
              )

              val resultValueStore = fromOutsideBaseStateData.resultValueStore
              val updatedResultValueStore = ValueStore.updateValueStore(
                resultValueStore,
                currentTick,
                mostRecentData
              )
              val baseStateDataWithUpdatedResults =
                BaseStateData.updateBaseStateData(
                  stateData.baseStateData,
                  updatedResultValueStore,
                  stateData.baseStateData.requestValueStore,
                  stateData.baseStateData.voltageValueStore,
                  stateData.baseStateData.additionalActivationTicks,
                  stateData.baseStateData.foreseenDataTicks
                )

              goToIdleReplyCompletionAndScheduleTriggerForNextAction(
                baseStateDataWithUpdatedResults,
                scheduler
              )
            case Failure(exception) =>
              log.error(
                "Was not able to extract received primary data correctly. Tear down the simulation. Failed with",
                exception
              )
              throw exception
          }

        case _: BaseStateData.ModelBaseStateData[_, _, _] =>
          /* Go to calculation state and send a trigger for this to myself as well */
          self ! StartCalculationTrigger(currentTick)
          goto(Calculate) using stateData
        case x =>
          throw new IllegalStateException(
            s"Unsupported base state data '$x' when handling all expected incoming data"
          )
      }
    } else {
      /* We sill have to wait - either for data or activation */
      stay() using stateData
    }
  }

  /** Determining the active to reactive power function to apply
    *
    * @param tick
    *   Tick to be applied for
    * @param baseStateData
    *   Base state data
    * @return
    *   A function, that transfers active into reactive power
    */
  def getReactivePowerFunction(
      tick: Long,
      baseStateData: FromOutsideBaseStateData[M, PD]
  ): ComparableQuantity[Power] => ComparableQuantity[Power] =
    if (baseStateData.fillUpReactivePowerWithModelFunc) {
      /* Use the model's active to reactive power function */
      val currentNodalVoltage =
        baseStateData.voltageValueStore
          .last(tick)
          .map(_._2)
          .getOrElse(
            Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE)
          )
      p: ComparableQuantity[Power] =>
        baseStateData.model
          .activeToReactivePowerFunc(
            currentNodalVoltage
          )(p)
          .to(StandardUnits.REACTIVE_POWER_IN)
    } else { _: ComparableQuantity[Power] =>
      /* Use trivial reactive power */
      Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_IN)
    }

  /** Try to get and process the received data
    *
    * @param data
    *   Mapping from sending actor ref to data
    * @param reactivePowerFunction
    *   Function to use, in order to determine the missing reactive power
    * @return
    *   A trial to get and process the needed data
    */
  def prepareData(
      data: Map[ActorRef, Option[_ <: Data]],
      reactivePowerFunction: ComparableQuantity[Power] => ComparableQuantity[
        Power
      ]
  ): Try[PD] =
    data.headOption
      .flatMap { case (_, maybeData) =>
        maybeData
      }
      .fold[Try[PrimaryData]] {
        Failure(
          new IllegalStateException(
            "Not able to determine the most recent result, although it should have been sent."
          )
        )
      } {
        case result: PrimaryData
            if pdClassTag.runtimeClass.equals(result.getClass) =>
          Success(result)
        case primaryData: PrimaryData =>
          primaryData match {
            case pd: EnrichableData[_] =>
              val q =
                reactivePowerFunction(pd.p).to(StandardUnits.REACTIVE_POWER_IN)
              val enriched = pd.add(q)
              if (pdClassTag.runtimeClass.equals(enriched.getClass))
                Success(enriched)
              else
                Failure(
                  new IllegalStateException(
                    "Received primary data cannot be enriched to expected data. Expected: " + pdClassTag.runtimeClass.getName + ", got: " + primaryData.getClass.getName + ", enriched to: " + enriched.getClass.getName
                  )
                )
            case _ =>
              Failure(
                new IllegalStateException(
                  "Got the wrong primary data. Expected: " + pdClassTag.runtimeClass.getName + ", got: " + primaryData.getClass.getName
                )
              )
          }
        case secondaryData: SecondaryData =>
          Failure(
            new IllegalStateException(
              s"Did expect primary data, but got '$secondaryData'."
            )
          )
      }
      .map(_.asInstanceOf[PD])

  /** Change over to [[Idle]] state and reply completion to the scheduler. By
    * doing so, also schedule an [[ActivityStartTrigger]] for the next upcoming
    * action.
    *
    * @param baseStateData
    *   Base state data to pop next activation from
    * @param scheduler
    *   Scheduler to schedule trigger
    * @return
    *   The next state
    */
  def goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      baseStateData: BaseStateData[PD],
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    /* Determine the very next tick, where activation is required */
    val (maybeActivationTriggers, updatedBaseStateData) =
      popNextActivationTrigger(baseStateData)

    val (_, triggerId) = releaseTickAndTriggerId()
    scheduler ! CompletionMessage(triggerId, maybeActivationTriggers)
    unstashAll()
    goto(Idle) using updatedBaseStateData
  }

  /** Pop the next tick, in which the agent wishes to get triggered, build a
    * regarding message and update the base state data. This might be either a
    * tick, where new data is foreseen to be sent by a
    * [[edu.ie3.simona.service.SimonaService]] or where additional activation is
    * needed.
    *
    * @param baseStateData
    *   Base state data to be updated
    * @return
    *   An [[Option]] to a [[Seq]] of new [[ScheduleTriggerMessage]] s as well
    *   as the updated base state data. If the next activation tick is an
    *   additional activation, this tick is removed from the list of desired
    *   additional activations.
    */
  def popNextActivationTrigger(
      baseStateData: BaseStateData[PD]
  ): (Option[Seq[ScheduleTriggerMessage]], BaseStateData[PD]) = {
    /* Determine what comes next: An additional activation or new data - or both at once */
    val nextAdditionalActivation =
      baseStateData.additionalActivationTicks.headOption
    val nextDataTick =
      baseStateData.foreseenDataTicks.values.toSeq.sorted.headOption.flatten

    /* return a [[Option]] to a [[Seq]] of [[ScheduleTriggerMessage]]s */
    def toMessageSeq: (Long, ActorRef) => Option[Seq[ScheduleTriggerMessage]] =
      (tick: Long, actorToBeTriggered: ActorRef) =>
        Some(
          Seq(
            ScheduleTriggerMessage(
              ActivityStartTrigger(tick),
              actorToBeTriggered
            )
          )
        )

    (nextAdditionalActivation, nextDataTick) match {
      case (None, Some(dataTick)) =>
        /* There is only a data tick available */
        (
          toMessageSeq(dataTick, self),
          baseStateData
        )
      case (Some(additionalTick), Some(dataTick))
          if dataTick < additionalTick =>
        /* The next foreseen activation will be based on foreseen data arrival. Do nothing else, then creating a
         * trigger. */
        (
          toMessageSeq(dataTick, self),
          baseStateData
        )
      case (Some(additionalTick), _) =>
        /* The next activation is additional (either there is no foreseen data tick or it is after the additional tick.
         * Remove the tick from the list of additional activation ticks. */
        val upcomingActivationTicks =
          baseStateData.additionalActivationTicks.rangeFrom(additionalTick + 1)
        val updatedBaseStateData = BaseStateData.updateBaseStateData(
          baseStateData,
          baseStateData.resultValueStore,
          baseStateData.requestValueStore,
          baseStateData.voltageValueStore,
          upcomingActivationTicks,
          baseStateData.foreseenDataTicks
        )

        (
          toMessageSeq(additionalTick, self),
          updatedBaseStateData
        )
      case (None, None) =>
        /* We don't know nothing about either additional activation nor new incoming data */
        (None, baseStateData)
    }
  }

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
  override def answerPowerRequestAndStayWithUpdatedStateData(
      baseStateData: BaseStateData[PD],
      requestTick: Long,
      eInPu: ComparableQuantity[Dimensionless],
      fInPu: ComparableQuantity[Dimensionless],
      alternativeResult: PD
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    log.debug(s"Received asset power request for tick {}", requestTick)

    /* Check, if there is any calculation foreseen for this tick. If so, wait with the response. */
    val activationExpected =
      baseStateData.additionalActivationTicks.headOption.exists(_ < requestTick)
    val dataExpected =
      baseStateData.foreseenDataTicks.values.flatten.exists(_ < requestTick)
    if (activationExpected || dataExpected) {
      log.debug(
        s"I got a request for power from '{}' for tick '{}', but I'm still waiting for new" +
          s" results before this tick. Waiting with the response.",
        sender(),
        requestTick
      )
      stash()
      stay() using baseStateData
    } else {

      /* Update the voltage value store */
      val nodalVoltage = Quantities.getQuantity(
        sqrt(
          pow(eInPu.to(PU).getValue.doubleValue(), 2) + pow(
            fInPu.to(PU).getValue.doubleValue(),
            2
          )
        ),
        PU
      )
      val lastNodalVoltage =
        baseStateData.voltageValueStore.last(requestTick)
      val updatedVoltageStore =
        ValueStore.updateValueStore(
          baseStateData.voltageValueStore,
          requestTick,
          nodalVoltage
        )
      /* Determine the most recent request */
      val mostRecentRequest =
        baseStateData.requestValueStore.last(requestTick)

      /* === Check if this request has already been answered with same tick and nodal voltage === */
      determineFastReply(
        baseStateData,
        mostRecentRequest,
        requestTick,
        updatedVoltageStore,
        nodalVoltage,
        lastNodalVoltage
      ).getOrElse {
        /* If a fast reply is not possible, determine it the old fashioned way */
        determineReply(
          requestTick,
          baseStateData,
          mostRecentRequest,
          nodalVoltage,
          updatedVoltageStore,
          alternativeResult
        )
      }
    }
  }

  /** Checks, if a fast reply is possible, when the very same request (in terms
    * of tick and nodal voltage) already has been answered. Then a Option on
    * stay in the same state with sending an [[AssetPowerUnchangedMessage]] is
    * given back. If a fast reply is not possible, [[None]] is given back.
    * Additionally, the listener are informed about the result.
    *
    * @param baseStateData
    *   Base state data to update
    * @param mostRecentRequest
    *   [[Option]] on a tuple of last request tick and corresponding answer
    * @param requestTick
    *   Tick of the incoming request
    * @param voltageValueStore
    *   [[ValueStore]] with nodal voltages to use in updated state data
    * @param nodalVoltage
    *   Magnitude of the complex, dimensionless nodal voltage
    * @param lastNodalVoltage
    *   Lastly known magnitude of the complex, dimensionless nodal voltage
    * @return
    *   Option on a possible fast state change
    */
  final def determineFastReply(
      baseStateData: BaseStateData[PD],
      mostRecentRequest: Option[(Long, PD)],
      requestTick: Long,
      voltageValueStore: ValueStore[ComparableQuantity[Dimensionless]],
      nodalVoltage: ComparableQuantity[Dimensionless],
      lastNodalVoltage: Option[(Long, ComparableQuantity[Dimensionless])]
  ): Option[FSM.State[AgentState, ParticipantStateData[PD]]] = {
    implicit val outputConfig: ParticipantNotifierConfig =
      baseStateData.outputConfig
    mostRecentRequest match {
      case Some((mostRecentRequestTick, latestProvidedValues))
          if mostRecentRequestTick == requestTick =>
        /* A request for this tick has already been answered. Check, if it has been the same request.
         * if it has been the same request we wanna answer with the same values afterwards, this data MUST always
         * be available when we already provided data for this tick */
        baseStateData match {
          case externalBaseStateData: FromOutsideBaseStateData[M, PD] =>
            /* When data is provided from outside it is NOT altered depending on the node voltage. Send an
             * AssetPowerUnchangedMessage */
            Some(
              stay() using externalBaseStateData.copy(
                voltageValueStore = voltageValueStore
              ) replying AssetPowerUnchangedMessage(
                latestProvidedValues.p,
                latestProvidedValues.q
              )
            )
          case modelBaseStateData: ParticipantModelBaseStateData[PD, CD, M] =>
            /* Check, if the last request has been made with the same nodal voltage. If not, recalculate the reactive
             * power. */
            lastNodalVoltage match {
              case Some((voltageTick, lastVoltage))
                  if voltageTick == requestTick =>
                if (
                  PsuQuantityUtil.isEquivalentAbs(
                    lastVoltage,
                    nodalVoltage,
                    modelBaseStateData.requestVoltageDeviationThreshold
                  )
                ) {
                  /* This is the very same request (same tick and same nodal voltage). Reply with
                   * AssetPowerUnchangedMessage */
                  Some(
                    stay() using modelBaseStateData replying AssetPowerUnchangedMessage(
                      latestProvidedValues.p,
                      latestProvidedValues.q
                    )
                  )
                } else {
                  /* If the voltage is not exactly equal, continue to determine the correct reply. */
                  None
                }
              case _ =>
                throw new InconsistentStateException(
                  s"I found an already answered request for tick $requestTick, but no matching nodal voltage."
                )
            }
        }
      case _ =>
        /* No reply for the request tick or no reply at all. Continue to determine the correct reply. */
        None
    }
  }

  /** Determine a reply on a
    * [[edu.ie3.simona.ontology.messages.PowerMessage.RequestAssetPowerMessage]]
    * by looking up the detailed simulation results, averaging them and
    * returning the equivalent state transition.
    *
    * @param requestTick
    *   The tick, the request belongs to
    * @param baseStateData
    *   Base state data
    * @param mostRecentRequest
    *   The request reply, that most recently has been sent
    * @param nodalVoltage
    *   Current nodal voltage
    * @param updatedVoltageValueStore
    *   Value store with updated nodal voltages
    * @param alternativeResult
    *   Alternative result to use, if no reasonable result can be obtained
    * @return
    *   Matching state transition
    */
  def determineReply(
      requestTick: Long,
      baseStateData: BaseStateData[PD],
      mostRecentRequest: Option[(Long, PD)],
      nodalVoltage: ComparableQuantity[Dimensionless],
      updatedVoltageValueStore: ValueStore[ComparableQuantity[Dimensionless]],
      alternativeResult: PD
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    /* No fast reply possible --> Some calculations have to be made */
    mostRecentRequest match {
      case Some((lastRequestTick, _)) if lastRequestTick > requestTick =>
        throw new InvalidRequestException(
          "Got a request for a tick, whereas a later tick already has been answered. This behaviour is not yet specified!"
        )
      case Some((lastRequestTick, lastResult))
          if lastRequestTick == requestTick =>
        /* Repetitive request for the same tick, but with different voltage */
        baseStateData match {
          case modelBaseStateData: ParticipantModelBaseStateData[PD, CD, M] =>
            /* Active power is yet calculated, but reactive power needs update */
            val nextReactivePower = modelBaseStateData.model
              .calculateReactivePower(lastResult.p, nodalVoltage)

            /* Determine the reply, based new circumstances */
            val updatedRequestValueStore =
              ValueStore.updateValueStore(
                baseStateData.requestValueStore,
                requestTick,
                lastResult.withReactivePower(nextReactivePower)
              )
            val nextStateData =
              modelBaseStateData.copy(
                requestValueStore = updatedRequestValueStore,
                voltageValueStore = updatedVoltageValueStore
              )

            stay() using nextStateData replying AssetPowerChangedMessage(
              lastResult.p,
              nextReactivePower
            )
          case unexpectedStateData =>
            throw new IllegalStateException(
              s"The request reply should not be re-calculated for state data '$unexpectedStateData'"
            )
        }

      case _ =>
        /* There hasn't been a request for this tick, yet. Check, if there are simulation results. If at least one
         * is apparent, average them and answer the request. If no simulation results is apparent at all, reply with
         * zero power, although this case should have been handled earlier */
        getRelevantResultData(
          requestTick,
          baseStateData.resultValueStore,
          baseStateData.requestValueStore
        ) match {
          case Some(relevantData) =>
            /* There is at least one relevant simulation result apparent, which might also be the most recent one
             * before the last request. But this is still the most recent one considered being valid. */
            averagePowerAndStay(
              baseStateData,
              relevantData,
              requestTick,
              nodalVoltage,
              updatedVoltageValueStore,
              alternativeResult
            )
          case None =>
            /* There is no simulation result at all. Reply with zero power */
            stayWithUpdatedRequestValueStore(
              baseStateData,
              alternativeResult,
              requestTick,
              updatedVoltageValueStore
            )
        }
    }
  }

  /** Determine the relevant simulation results to calculate the average of
    * those, to answer a given request. The relevant results are those between
    * the most recent result tick BEFORE or at the last request and the most
    * recent result tick before or at this request tick. If no new information
    * are apparent, the last known simulation result (before the last answered
    * request) is taken, as this is the only still valid information.
    *
    * @param requestTick
    *   Tick, the current request belongs to
    * @param resultValueStore
    *   Storage of simulation results
    * @param requestValueStore
    *   Storage of already answered requests
    * @return
    *   An optional mapping from tick to simulation results, if some are
    *   apparent within the tick window to be considered.
    */
  def getRelevantResultData(
      requestTick: Long,
      resultValueStore: ValueStore[PD],
      requestValueStore: ValueStore[PD]
  ): Option[RelevantResultValues[PD]] = {
    /* The actual tick window for averaging is the last request tick and this request tick (both including) */
    val (averagingWindowStart, averagingWindowEnd) =
      determineTickWindow(requestTick, requestValueStore)

    /* All participants simulation results between the most recent simulation tick BEFORE or at the beginning of the
     * averaging window and it's end (both including) are relevant for averaging the simulated primary data */
    val firstRelevantTick = determineFirstRelevantTick(
      averagingWindowStart,
      resultValueStore
    )

    /* Let's see, if we got some simulation results between the first relevant simulation tick and this request's tick */
    val simulationResults =
      resultValueStore.get(firstRelevantTick, averagingWindowEnd)
    if (simulationResults.nonEmpty) {
      /* There is at least one simulation result. This might as well be the last simulation result before the last
       * request reply, which still is considered to be the last valid simulation result. */
      Some(
        RelevantResultValues(
          averagingWindowStart,
          averagingWindowEnd,
          simulationResults
        )
      )
    } else {
      None
    }
  }

  /** Determine the averaging tick window. It starts with the last request tick
    * and ends with this request tick.
    *
    * @param requestTick
    *   Tick, for which the averaged primary data has to be determined
    * @param requestValueStore
    *   Storage of already answered requests
    * @return
    *   Window between the last request or simulation start and this request
    */
  private def determineTickWindow(
      requestTick: Long,
      requestValueStore: ValueStore[_]
  ): (Long, Long) =
    requestValueStore.lastKnownTick(requestTick - 1) match {
      case Some(lastRequestTick) => (lastRequestTick, requestTick)
      case None                  => (0, requestTick)
    }

  /** Determines the first tick, that is relevant for building the average
    * primary data to answer a request at the given tick. This is either the
    * last known data tick before or at the last request tick or the beginning
    * of the simulation.
    *
    * @param lastRequestTick
    *   Last tick, at which a request has been answered
    * @param resultValueStore
    *   Storage for the simulation results
    * @return
    *   The first relevant tick for averaging primary data
    */
  private def determineFirstRelevantTick(
      lastRequestTick: Long,
      resultValueStore: ValueStore[_]
  ): Long =
    resultValueStore.lastKnownTick(lastRequestTick) match {
      case Some(firstRelevantDataTick) => firstRelevantDataTick
      case None                        => 0
    }

  /** Average the given power values, answer with the equivalent reply and stay
    * in this state.
    *
    * @param baseData
    *   Current agent's base state data
    * @param relevantResults
    *   Collection of all relevant simulation results, including the needed tick
    *   window
    * @param requestTick
    *   Tick in which power has been requested
    * @param nodalVoltage
    *   Nodal voltage magnitude in the moment of request
    * @param voltageValueStore
    *   Voltage value store to be used in the updated base state data
    * @param alternativeResult
    *   If no relevant data are apparent, then use this result instead
    * @return
    *   The very same state as the agent currently is in, but with updated base
    *   state data
    */
  final def averagePowerAndStay(
      baseData: BaseStateData[PD],
      relevantResults: RelevantResultValues[PD],
      requestTick: Long,
      nodalVoltage: ComparableQuantity[Dimensionless],
      voltageValueStore: ValueStore[ComparableQuantity[Dimensionless]],
      alternativeResult: PD
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    if (relevantResults.relevantData.nonEmpty) {
      averagePowerAndStay(
        baseData,
        relevantResults.relevantData,
        requestTick,
        relevantResults.windowStart,
        relevantResults.windowEnd,
        nodalVoltage,
        voltageValueStore
      )
    } else {
      log.debug(
        s"No relevant data apparent, stay and reply with alternative result {}.",
        alternativeResult
      )
      stayWithUpdatedRequestValueStore(
        baseData,
        alternativeResult,
        requestTick,
        voltageValueStore
      )
    }
  }

  /** Determines the reply result by averaging the given power values over the
    * queried tick frame. The next state is the very same and the agent replies
    * to the request with the freshly determined averaged power. The reply is
    * also added to the requestValueStore.
    *
    * @param baseData
    *   Current agent's base state data
    * @param tickToResult
    *   Mapping from data tick to actual data
    * @param requestTick
    *   Tick in which power has been requested
    * @param windowStartTick
    *   Beginning of the averaging window
    * @param windowEndTick
    *   End of the averaging window
    * @param nodalVoltage
    *   Nodal voltage magnitude in the moment of request
    * @param voltageValueStore
    *   Voltage value store to be used in the updated base state data
    * @return
    *   The very same state as the agent currently is in, but with updated base
    *   state data
    */
  def averagePowerAndStay(
      baseData: BaseStateData[PD],
      tickToResult: Map[Long, PD],
      requestTick: Long,
      windowStartTick: Long,
      windowEndTick: Long,
      nodalVoltage: ComparableQuantity[Dimensionless],
      voltageValueStore: ValueStore[ComparableQuantity[Dimensionless]]
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    val averageResult = determineAverageResult(
      baseData,
      tickToResult,
      windowStartTick,
      windowEndTick,
      nodalVoltage
    )
    stayWithUpdatedRequestValueStore(
      baseData,
      averageResult,
      requestTick,
      voltageValueStore
    )
  }

  /** Determines the average result, from which to reply to a request. The
    * values are determined by on averaging the results in the baseStateData
    * result value store within the given tick window
    *
    * @param baseStateData
    *   The [[BaseStateData]] to hold information about current results
    * @param tickToResult
    *   Mapping from result tick to actual result
    * @param windowStartTick
    *   Beginning of the averaging tick window
    * @param windowEndTick
    *   End of the averaging tick window
    * @param nodalVoltage
    *   Current nodal voltage magnitude in p.u.
    * @return
    *   Averaged result
    */
  def determineAverageResult(
      baseStateData: BaseStateData[PD],
      tickToResult: Map[Long, PD],
      windowStartTick: Long,
      windowEndTick: Long,
      nodalVoltage: ComparableQuantity[Dimensionless]
  ): PD = {
    /* Determine, how the single model would transfer the active into reactive power */
    val activeToReactivePowerFunction = baseStateData match {
      case _: FromOutsideBaseStateData[M, PD] => None
      case modelBaseStateData: ParticipantModelBaseStateData[PD, CD, M] =>
        Some(
          modelBaseStateData.model.activeToReactivePowerFunc(nodalVoltage)
        )
    }

    averageResults(
      tickToResult,
      windowStartTick,
      windowEndTick,
      activeToReactivePowerFunction
    )
  }

  /** Determine the average result within the given tick window
    *
    * @param tickToResults
    *   Mapping from data tick to actual data
    * @param windowStart
    *   First, included tick of the time window
    * @param windowEnd
    *   Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    *   An Option on a function, that transfers the active into reactive power
    * @return
    *   The averaged result
    */
  def averageResults(
      tickToResults: Map[Long, PD],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ] = None
  ): PD

  /** Updates the given base state data by inserting updated request value store
    * and the provided voltage value store. The next state is the very same one,
    * but with updated base state data. Additionally, an
    * [[AssetPowerChangedMessage]] is sent to the requesting entity
    *
    * @param baseStateData
    *   Current base state data
    * @param averageResult
    *   Averaged result to put to request value store
    * @param requestTick
    *   Tick of the most recent request
    * @param voltageValueStore
    *   Voltage value store to be used in the updated base state data
    * @return
    *   The very same state as the agent currently is in, but with updated base
    *   state data
    */
  def stayWithUpdatedRequestValueStore(
      baseStateData: BaseStateData[PD],
      averageResult: PD,
      requestTick: Long,
      voltageValueStore: ValueStore[ComparableQuantity[Dimensionless]]
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    val updatedRequestValueStore =
      ValueStore.updateValueStore(
        baseStateData.requestValueStore,
        requestTick,
        averageResult
      )
    val nextStateData = BaseStateData.updateBaseStateData(
      baseStateData,
      baseStateData.resultValueStore,
      updatedRequestValueStore,
      voltageValueStore,
      baseStateData.additionalActivationTicks,
      baseStateData.foreseenDataTicks
    )

    averageResult.toApparentPower match {
      case ApparentPower(p, q) =>
        stay() using nextStateData replying AssetPowerChangedMessage(p, q)
    }
  }

  /** Calculate the power output of the participant without needing any
    * secondary data. The next state is [[Idle]], sending a
    * [[CompletionMessage]] to scheduler and using update result values.
    *
    * @param baseStateData
    *   Base state data to update
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithoutSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[PD, CD, M],
      currentTick: Long,
      scheduler: ActorRef,
      nodalVoltage: ComparableQuantity[Dimensionless],
      calculateModelPowerFunc: (
          Long,
          ParticipantModelBaseStateData[PD, CD, M],
          ComparableQuantity[Dimensionless]
      ) => PD
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    val result =
      calculateModelPowerFunc(currentTick, baseStateData, nodalVoltage)

    val updatedResultValueStore =
      ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        currentTick,
        result
      )

    /* Inform the listeners about new result */
    announceSimulationResult(
      baseStateData,
      currentTick,
      result
    )(baseStateData.outputConfig)

    /* In this case, without secondary data, the agent has been triggered by an ActivityStartTrigger by itself,
     * therefore pop the next one */
    val baseStateDataWithUpdatedResultStore = BaseStateData.updateBaseStateData(
      baseStateData,
      updatedResultValueStore,
      baseStateData.requestValueStore,
      baseStateData.voltageValueStore,
      baseStateData.additionalActivationTicks,
      baseStateData.foreseenDataTicks
    )
    goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      baseStateDataWithUpdatedResultStore,
      scheduler
    )
  }

  /** Notify listeners about a new simulation result of the participant agent,
    * if the config says so.
    *
    * @param baseStateData
    *   Agent's base state data
    * @param tick
    *   Tick, the result belongs to
    * @param result
    *   The result to build a event for
    * @param outputConfig
    *   Configuration of the output behaviour
    */
  def announceSimulationResult(
      baseStateData: BaseStateData[PD],
      tick: Long,
      result: PD
  )(implicit outputConfig: ParticipantNotifierConfig): Unit =
    if (outputConfig.simulationResultInfo)
      notifyListener(
        buildResultEvent(baseStateData, tick, result)
      )

  /** Update the result and calc relevant data value stores, inform all
    * registered listeners and go to Idle using the updated base state data
    *
    * @param scheduler
    *   Actor reference of the scheduler
    * @param baseStateData
    *   The base state data of the collection state
    * @param result
    *   Result of simulation
    * @param relevantData
    *   Data, that have been relevant to this calculation
    * @return
    *   Desired state change
    */
  final def updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(
      scheduler: ActorRef,
      baseStateData: BaseStateData[PD],
      result: PD,
      relevantData: CD
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    /* Update the value stores */
    val updatedValueStore =
      ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        currentTick,
        result
      )
    val updatedRelevantDataStore =
      baseStateData match {
        case data: BaseStateData.ModelBaseStateData[_, _, _] =>
          ValueStore.updateValueStore(
            data.calcRelevantDateStore,
            currentTick,
            relevantData
          )
        case _ =>
          throw new InconsistentStateException(
            "Cannot find calculation relevant data to update."
          )
      }

    /* Inform the listeners about new result */
    announceSimulationResult(
      baseStateData,
      currentTick,
      result
    )(baseStateData.outputConfig)

    /* Update the base state data */
    val baseStateDateWithUpdatedResults =
      baseStateData match {
        case data: ParticipantModelBaseStateData[PD, CD, M] =>
          data.copy(
            resultValueStore = updatedValueStore,
            calcRelevantDateStore = updatedRelevantDataStore
          )
        case _ =>
          throw new InconsistentStateException(
            "Wrong base state data"
          )
      }

    goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      baseStateDateWithUpdatedResults,
      scheduler
    )
  }

  /** Notify listeners about a new reply on a power request, if the config says
    * so.
    *
    * @param baseStateData
    *   Agent's base state data
    * @param tick
    *   Tick, the result belongs to
    * @param activePower
    *   Active power
    * @param reactivePower
    *   Reactive power
    * @param outputConfig
    *   Configuration of the output behaviour
    */
  override def announceAssetPowerRequestReply(
      baseStateData: BaseStateData[_],
      tick: Long,
      activePower: ComparableQuantity[Power],
      reactivePower: ComparableQuantity[Power]
  )(implicit outputConfig: ParticipantNotifierConfig): Unit =
    if (outputConfig.powerRequestReply) {
      log.warning(
        "Writing out power request replies is currently not supported!"
      )
    }

  /** To clean up agent value stores after power flow convergence. This is
    * necessary for agents whose results are time dependent e.g. storage agents
    *
    * @param baseStateData
    *   Basic state data
    * @param currentTick
    *   Tick, the trigger belongs to
    * @return
    *   [[Idle]] with updated result values
    */
  override def finalizeTickAfterPF(
      baseStateData: BaseStateData[PD],
      currentTick: Long
  ): FSM.State[AgentState, ParticipantStateData[PD]] = {
    baseStateData.requestValueStore.last(currentTick).foreach {
      case (_, data) =>
        // forward information to result listeners after power flow convergence
        announceAssetPowerRequestReply(
          baseStateData,
          currentTick,
          data.p,
          data.q
        )(baseStateData.outputConfig)
    }
    goto(Idle) using baseStateData
  }

  /** Determines the correct result event.
    *
    * @param baseStateData
    *   Agent's base state data
    * @param tick
    *   Tick, the result belongs to
    * @param result
    *   The result to build an event for
    * @return
    *   The equivalent event
    */
  def buildResultEvent(
      baseStateData: BaseStateData[PD],
      tick: Long,
      result: PD
  ): ParticipantResultEvent = {
    val uuid = baseStateData.modelUuid
    val dateTime = tick.toDateTime(baseStateData.startDate)
    ParticipantResultEvent(
      buildResult(uuid, dateTime, result)
    )
  }

  /** Determines the correct result.
    *
    * @param uuid
    *   Unique identifier of the physical model
    * @param dateTime
    *   Real world date of the result
    * @param result
    *   The primary data to build a result model for
    * @return
    *   The equivalent event
    */
  protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: PD
  ): SystemParticipantResult

  /** Returns secondary service of type T or throws exception
    * @param services
    *   the services Option used in
    *   [[edu.ie3.simona.agent.participant.statedata.BaseStateData.ModelBaseStateData]]
    * @param tag
    *   ClassTag of T
    * @tparam T
    *   the type of secondary service to return
    * @return
    *   secondary service of given type
    */
  protected def getService[T <: SecondaryDataService[_]](
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]]
  )(implicit tag: ClassTag[T]): ActorRef =
    services
      .getOrElse(
        throw new InconsistentStateException(
          "No services provided by ParticipantModelBaseStateData."
        )
      )
      .find {
        case _: T => true
        case _    => false
      }
      .getOrElse(
        throw new InconsistentStateException(
          s"No $tag provided by ParticipantModelBaseStateData."
        )
      )
      .actorRef
}

case object ParticipantAgentFundamentals {

  /** Hold all necessary information for later averaging of participant
    * simulations' results.
    *
    * @param windowStart
    *   Beginning of the actual tick window, that should be covered by the
    *   averaging process
    * @param windowEnd
    *   End of the actual tick window, that should be covered by the averaging
    *   process
    * @param relevantData
    *   Collection of all necessary data for averaging (spans a wider range,
    *   than the actual window)
    * @tparam PD
    *   Type of primary data, that is relevant for the next calculation
    */
  final case class RelevantResultValues[+PD <: PrimaryData](
      windowStart: Long,
      windowEnd: Long,
      relevantData: Map[Long, PD]
  )

  /** Determine the average apparent power within the given tick window
    *
    * @param tickToResults
    *   Mapping from data tick to actual data
    * @param windowStart
    *   First, included tick of the time window
    * @param windowEnd
    *   Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    *   An Option on a function, that transfers the active into reactive power
    * @return
    *   The averaged apparent power
    */
  def averageApparentPower(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ] = None,
      log: LoggingAdapter
  ): ApparentPower = {
    val p = QuantityUtil.average(
      tickToResults.map { case (tick, pd) => tick -> pd.p },
      windowStart,
      windowEnd,
      classOf[Energy],
      KILOWATTHOUR,
      classOf[Power],
      MEGAWATT
    ) match {
      case Success(pSuccess) => pSuccess
      case Failure(exception) =>
        log.warning(
          "Unable to determine average active power. Apply 0 instead. Cause:\n\t{}",
          exception
        )
        Quantities.getQuantity(0d, MEGAWATT)
    }
    val q = QuantityUtil.average(
      tickToResults.map { case (tick, pd) =>
        activeToReactivePowerFuncOpt match {
          case Some(qFunc) => tick -> qFunc(pd.toApparentPower.p)
          case None        => tick -> pd.toApparentPower.q
        }
      },
      windowStart,
      windowEnd,
      classOf[Energy],
      KILOVARHOUR,
      classOf[Power],
      MEGAVAR
    ) match {
      case Success(pSuccess) => pSuccess
      case Failure(exception) =>
        log.warning(
          "Unable to determine average reactive power. Apply 0 instead. Cause:\n\t{}",
          exception
        )
        Quantities.getQuantity(0d, MEGAVAR)
    }

    ApparentPower(p, q)
  }

}
