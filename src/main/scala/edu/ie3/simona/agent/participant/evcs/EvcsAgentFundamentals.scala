/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.{
  EvcsResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.grid.GridAgentMessages.AssetPowerChangedMessage
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorExtEvDataService
import edu.ie3.simona.agent.participant.evcs.EvcsAgent.neededServices
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexControlledData,
  ParticipantModelBaseStateData,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.InputModelContainer
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData,
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException,
}
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.FlexChangeIndicator
import edu.ie3.simona.model.participant.evcs.EvcsModel
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexRequest,
  FlexResponse,
}
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.util.quantities.PowerSystemUnits.{MEGAVAR, MEGAWATT, PU}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.Megavars
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.typed.{ActorRef => TypedActorRef}
import org.apache.pekko.actor.{ActorRef, FSM}
import squants.energy.Megawatts
import squants.{Dimensionless, Each, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.SortedSet
import scala.reflect.{ClassTag, classTag}

protected trait EvcsAgentFundamentals
    extends ParticipantAgentFundamentals[
      ComplexPower,
      EvcsRelevantData,
      EvcsState,
      ParticipantStateData[ComplexPower],
      EvcsInput,
      EvcsRuntimeConfig,
      EvcsModel,
    ] {
  this: EvcsAgent =>
  override protected val pdClassTag: ClassTag[ComplexPower] =
    classTag[ComplexPower]

  /** Determines the needed base state data in dependence of the foreseen
    * simulation mode of the agent.
    *
    * @param inputModel
    *   Input model definition
    * @param modelConfig
    *   Configuration of the model
    * @param services
    *   Collection of services to register with
    * @param simulationStartDate
    *   Real world time date time, when the simulation starts
    * @param simulationEndDate
    *   Real world time date time, when the simulation ends
    * @param resolution
    *   Agents regular time bin it wants to be triggered e.g. one hour
    * @param requestVoltageDeviationThreshold
    *   Threshold, after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param outputConfig
    *   Config of the output behaviour for simulation results
    * @return
    *   A [[ParticipantModelBaseStateData]] that reflects the behaviour based on
    *   the data source definition
    */
  override def determineModelBaseStateData(
      inputModel: InputModelContainer[EvcsInput],
      modelConfig: EvcsRuntimeConfig,
      services: Iterable[SecondaryDataService[_ <: SecondaryData]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[TypedActorRef[FlexResponse]],
  ): ParticipantModelBaseStateData[
    ComplexPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel,
  ] = {
    /* Check for needed services */
    if (!services.toSeq.map(_.getClass).containsSlice(neededServices))
      throw new AgentInitializationException(
        s"EvcsAgent cannot be initialized without an ev data service!"
      )

    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate,
      )

    ParticipantModelBaseStateData[
      ComplexPower,
      EvcsRelevantData,
      EvcsState,
      EvcsModel,
    ](
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      SortedSet.empty, // Additional activation of the evcs agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution,
        Each(
          inputModel.electricalInputModel.getNode
            .getvTarget()
            .to(PU)
            .getValue
            .doubleValue
        ),
      ),
      ValueStore(resolution),
      ValueStore(resolution),
      ValueStore(resolution),
      ValueStore(resolution),
      maybeEmAgent.map(FlexControlledData(_, self.toTyped[FlexRequest])),
    )
  }

  override def buildModel(
      inputModel: InputModelContainer[EvcsInput],
      modelConfig: EvcsRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): EvcsModel = EvcsModel(
    inputModel.electricalInputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate,
    modelConfig.chargingStrategy,
    modelConfig.lowestEvSoc,
  )

  override protected def createInitialState(
      baseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ]
  ): EvcsState =
    EvcsState(
      Seq.empty,
      Map.empty,
      SimonaConstants.FIRST_TICK_IN_SIMULATION,
    )

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
      tick: Long,
  ): EvcsRelevantData = {
    // always only take arrivals for the current tick
    // or empty sequence if none arrived
    val arrivingEvs = baseStateData.receivedSecondaryDataStore
      .getOrElse(tick, Map.empty)
      .collectFirst {
        // filter secondary data for arriving EVs data
        case (_, arrivingEvsData: ArrivingEvs) =>
          arrivingEvsData.arrivals
      }
      .getOrElse(Seq.empty)

    EvcsRelevantData(tick, arrivingEvs)
  }

  /** Handle an active power change by flex control.
    *
    * @param tick
    *   Tick, in which control is issued
    * @param baseStateData
    *   Base state data of the agent
    * @param data
    *   Calculation relevant data
    * @param lastState
    *   Last known model state
    * @param setPower
    *   Setpoint active power
    * @return
    *   Updated model state, a result model and a [[FlexChangeIndicator]]
    */
  def handleControlledPowerChange(
      tick: Long,
      baseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
      data: EvcsRelevantData,
      lastState: EvcsState,
      setPower: Power,
  ): (
      EvcsState,
      AccompaniedSimulationResult[ComplexPower],
      FlexChangeIndicator,
  ) = {
    /* Calculate the power */
    val voltage = getAndCheckNodalVoltage(baseStateData, tick)

    val reactivePower = baseStateData.model.calculateReactivePower(
      setPower,
      voltage,
    )
    val result = AccompaniedSimulationResult(
      ComplexPower(setPower, reactivePower),
      Seq.empty[ResultEntity],
    )

    /* Handle the request within the model */
    val (updatedState, flexChangeIndicator) =
      baseStateData.model.handleControlledPowerChange(data, lastState, setPower)
    (updatedState, result, flexChangeIndicator)
  }

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
      EvcsState,
      Dimensionless,
  ) => ComplexPower =
    (_, _, _, _) =>
      throw new InvalidRequestException(
        "Evcs model cannot be run without secondary data."
      )

  /** Calculate the power output of the participant utilising secondary data.
    * However, it might appear, that not the complete set of secondary data is
    * available for the given tick. This might especially be true, if the actor
    * has been additionally activated. This method thereby has to try and fill
    * up missing data with the last known data, as this is still supposed to be
    * valid. The secondary data therefore is put to the calculation relevant
    * data store. <p>The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.Completion]] to
    * scheduler and using update result values.</p>
    *
    * @param baseStateData
    *   The base state data with collected secondary data
    * @param lastModelState
    *   Last model state
    * @param tick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
      lastModelState: EvcsState,
      tick: Long,
      scheduler: ActorRef,
  ): FSM.State[AgentState, ParticipantStateData[ComplexPower]] = {
    /* extract EV data from secondary data, which should have been requested and received before */
    baseStateData.receivedSecondaryDataStore
      .getOrElse(tick, Map.empty)
      .values
      .collectFirst {
        // filter secondary data for arriving EVs data
        case _: ArrivingEvs =>
          handleArrivingEvsAndGoIdle(
            tick,
            scheduler,
            baseStateData,
          )
      }
      .getOrElse(
        throw new InconsistentStateException(
          s"The model ${baseStateData.model} was not provided with needed EV data."
        )
      )

  }

  /** Returns the number of free parking lots based on the last available state
    * data.
    *
    * @param tick
    *   The tick that free lots have been requested for
    * @param modelBaseStateData
    *   The state data
    */
  protected def handleFreeLotsRequest(
      tick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
  ): Unit = {
    val evServiceRef = getService[ActorExtEvDataService](
      modelBaseStateData.services
    )

    val lastState =
      getLastOrInitialStateData(modelBaseStateData, tick - 1)

    evServiceRef ! FreeLotsResponse(
      modelBaseStateData.model.uuid,
      modelBaseStateData.model.chargingPoints - lastState.evs.size,
    )
  }

  /** Handle a request for returning those EVs that are departing at the current
    * tick. SOC and results are calculated and corresponding listeners are
    * informed.
    *
    * @param tick
    *   The current simulation tick
    * @param baseStateData
    *   The current Base state data
    * @param requestedDepartingEvs
    *   The UUIDs of EVs that are requested to be returned
    */
  protected def handleDepartingEvsRequest(
      tick: Long,
      requestedDepartingEvs: Seq[UUID],
      baseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
  ): ParticipantModelBaseStateData[
    ComplexPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel,
  ] = {
    val evServiceRef = getService[ActorExtEvDataService](
      baseStateData.services
    )

    // we don't take the state at the current tick, since
    // that one cannot contain the departing EVs anymore
    val lastState = baseStateData.stateDataStore
      .last(tick - 1)
      .map { case (_, lastState) =>
        lastState
      }
      .getOrElse(
        throw new InvalidRequestException(
          s"Cannot return departing EVs from EVCS ${baseStateData.modelUuid} since we have no parked EVs"
        )
      )

    baseStateData.model.validateDepartures(lastState.evs, requestedDepartingEvs)

    val updatedEvs = baseStateData.model.applySchedule(
      lastState,
      tick,
    )

    val (departingEvs, stayingEvs) = updatedEvs.partition { ev =>
      requestedDepartingEvs.contains(ev.uuid)
    }

    // send back departing EVs
    if (requestedDepartingEvs.nonEmpty) {
      evServiceRef ! DepartingEvsResponse(
        baseStateData.modelUuid,
        departingEvs,
      )
    }

    /* Calculate evcs power for interval since last update, save for updating value store, and inform listeners */
    val updatedResultValueStore =
      determineResultsAnnounceUpdateValueStore(
        lastState,
        tick,
        baseStateData,
      )

    val stayingSchedules =
      lastState.schedule
        .filterNot(requestedDepartingEvs.contains)
        .view
        .mapValues {
          // Remove schedules that ended before or at current tick.
          // Schedule entries ending at current tick do not have any
          // impact on the schedule from the current tick on
          _.filter(_.tickStop > tick)
        }
        .toMap

    val newState = EvcsState(stayingEvs, stayingSchedules, tick)

    baseStateData.copy(
      stateDataStore = ValueStore.updateValueStore(
        baseStateData.stateDataStore,
        tick,
        newState,
      ),
      resultValueStore = updatedResultValueStore,
    )
  }

  /** Handles data message that contains information on arriving EVs. Updates
    * already parked EVs (if applicable) and adds new arrivals. Also calculates
    * new schedules for all staying and arriving EVs. Sends completion message
    * to scheduler without scheduling new activations.
    *
    * @param tick
    *   The current tick that data has arrived for
    * @param scheduler
    *   The scheduler ref
    * @param modelBaseStateData
    *   The state data
    * @return
    *   [[Idle]] with updated relevant data store
    */
  private def handleArrivingEvsAndGoIdle(
      tick: Long,
      scheduler: ActorRef,
      modelBaseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
  ): FSM.State[AgentState, ParticipantStateData[ComplexPower]] = {

    val relevantData =
      createCalcRelevantData(modelBaseStateData, tick)

    val lastState = getLastOrInitialStateData(modelBaseStateData, tick)

    val updatedBaseStateData = {
      if (relevantData.arrivals.nonEmpty) {

        val currentEvs = modelBaseStateData.model.determineCurrentEvs(
          relevantData,
          lastState,
        )

        // if new EVs arrived, a new scheduling must be calculated.
        val newSchedule = modelBaseStateData.model.calculateNewScheduling(
          relevantData,
          currentEvs,
        )

        // create new current state
        val newState = EvcsState(currentEvs, newSchedule, tick)

        val updatedStateDataStore = ValueStore.updateValueStore(
          modelBaseStateData.stateDataStore,
          tick,
          newState,
        )

        /* Update the base state data with the updated state data store */
        modelBaseStateData.copy(
          stateDataStore = updatedStateDataStore
        )
      } else
        // Empty arrivals means that there is no data for this EVCS at the current tick,
        // thus we just return and wait for the next activation
        modelBaseStateData
    }

    // if the lastState's tick is the same as the actual tick the results have already been determined and announced when we handled the departedEvs
    if (lastState.tick != tick) {
      determineResultsAnnounceUpdateValueStore(
        lastState,
        currentTick,
        modelBaseStateData,
      )
    }

    // We're only here if we're not flex-controlled, thus sending a Completion is always right
    goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      updatedBaseStateData,
      scheduler,
    )
  }

  /** Determine a reply on a
    * [[edu.ie3.simona.agent.participant.ParticipantAgent.RequestAssetPowerMessage]]
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
  override def determineReply(
      requestTick: Long,
      baseStateData: BaseStateData[ComplexPower],
      mostRecentRequest: Option[(Long, ComplexPower)],
      nodalVoltage: squants.Dimensionless,
      updatedVoltageValueStore: ValueStore[squants.Dimensionless],
      alternativeResult: ComplexPower,
  ): FSM.State[AgentState, ParticipantStateData[ComplexPower]] = {
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
          case modelBaseStateData: ParticipantModelBaseStateData[
                ComplexPower,
                EvcsRelevantData,
                EvcsState,
                EvcsModel,
              ] =>
            /* Active power is yet calculated, but reactive power needs update */
            val nextReactivePower = modelBaseStateData.model
              .calculateReactivePower(lastResult.p, nodalVoltage)

            /* Determine the reply, based new circumstances */
            val updatedRequestValueStore =
              ValueStore.updateValueStore(
                baseStateData.requestValueStore,
                requestTick,
                lastResult.withReactivePower(nextReactivePower),
              )

            val nextStateData =
              modelBaseStateData.copy(
                requestValueStore = updatedRequestValueStore,
                voltageValueStore = updatedVoltageValueStore,
              )

            stay() using nextStateData replying AssetPowerChangedMessage(
              lastResult.p,
              nextReactivePower,
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

        /* ADDED: Update base state data before answering the power request to include the scheduling up to this tick.
         * Also, save the new voltage information for the calc relevant data store.
         */
        val updatedBaseStateData =
          baseStateData match {
            case modelBaseStateData: ParticipantModelBaseStateData[
                  ComplexPower,
                  EvcsRelevantData,
                  EvcsState,
                  EvcsModel,
                ] =>
              val lastState =
                getLastOrInitialStateData(modelBaseStateData, requestTick)

              val updatedResultValueStore =
                determineResultsAnnounceUpdateValueStore(
                  lastState,
                  requestTick,
                  modelBaseStateData,
                )

              modelBaseStateData.copy(
                resultValueStore = updatedResultValueStore
              )

            case unexpectedStateData =>
              throw new IllegalStateException(
                s"Unexpected state data '$unexpectedStateData'"
              )
          }

        getRelevantResultData(
          requestTick,
          updatedBaseStateData.resultValueStore,
          updatedBaseStateData.requestValueStore,
        ) match {
          case Some(relevantData) =>
            /* There is at least one relevant simulation result apparent, which might also be the most recent one
             * before the last request. But this is still the most recent one considered being valid. */
            averagePowerAndStay(
              updatedBaseStateData,
              relevantData,
              requestTick,
              nodalVoltage,
              updatedVoltageValueStore,
              alternativeResult,
            )
          case None =>
            /* There is no simulation result at all. Reply with zero power */
            stayWithUpdatedRequestValueStore(
              updatedBaseStateData,
              alternativeResult,
              requestTick,
              updatedVoltageValueStore,
            )
        }
    }
  }

  /** Speciality with EVCS: result are always calculated up until the current
    * tick. Thus, the result received as a parameter is discarded.
    *
    * @param baseStateData
    *   The base state data
    * @param result
    *   Is ignored here, result up until this tick are calculated
    * @param currentTick
    *   the current tick
    * @return
    *   updated base state data
    */
  override def handleCalculatedResult(
      baseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
      result: AccompaniedSimulationResult[ComplexPower],
      currentTick: Long,
  ): ParticipantModelBaseStateData[
    ComplexPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel,
  ] = {

    // calculate results from last schedule
    baseStateData.stateDataStore
      .last(currentTick - 1)
      .map { case (lastTick, lastState) =>
        baseStateData.resultValueStore.get(lastTick) match {
          case Some(_) =>
            // We already have a result for this tick, likely
            // because EVs already departed at this tick.
            // Thus, skip recalculating and sending out results.
            baseStateData
          case None =>
            val updatedResultValueStore =
              determineResultsAnnounceUpdateValueStore(
                lastState,
                currentTick,
                baseStateData,
              )

            baseStateData.copy(
              resultValueStore = updatedResultValueStore
            ): ParticipantModelBaseStateData[
              ComplexPower,
              EvcsRelevantData,
              EvcsState,
              EvcsModel,
            ]
        }
      }
      .getOrElse(baseStateData)

  }

  /** Determine evcs results, announce them to listeners and update the result
    * value store.
    *
    * @param lastState
    *   The state (including schedule) to calculate results for
    * @param currentTick
    *   The tick up to which results should be calculated for
    * @param modelBaseStateData
    *   Model base state data
    * @return
    *   The updated result value store
    */
  private def determineResultsAnnounceUpdateValueStore(
      lastState: EvcsState,
      currentTick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        ComplexPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel,
      ],
  ): ValueStore[ComplexPower] = {

    val voltage = modelBaseStateData.voltageValueStore
      .last(currentTick)
      .map { case (_, voltage) =>
        voltage
      }
      .getOrElse(Each(1d))

    val (evResults, evcsResults) = modelBaseStateData.model.createResults(
      lastState,
      currentTick,
      voltage,
    )

    // send out EV results
    evResults.foreach { result =>
      listener.foreach(_ ! ParticipantResultEvent(result, currentTick))
    }

    evcsResults.foldLeft(modelBaseStateData.resultValueStore) {
      case (resultValueStore, result) =>
        /* Inform the listeners about new result */
        if (modelBaseStateData.outputConfig.simulationResultInfo)
          notifyListener(
            ParticipantResultEvent(result, currentTick)
          )

        /* Update resultValueStore with result */
        ValueStore.updateValueStore(
          resultValueStore,
          result.getTime.toTick(modelBaseStateData.startDate),
          ComplexPower(
            Megawatts(result.getP.to(MEGAWATT).getValue.doubleValue),
            Megavars(result.getQ.to(MEGAVAR).getValue.doubleValue),
          ),
        )
    }
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
  override protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ComplexPower,
  ): SystemParticipantResult =
    new EvcsResult(
      dateTime,
      uuid,
      result.p.toMegawatts.asMegaWatt,
      result.q.toMegavars.asMegaVar,
    )

  /** Update the last known model state with the given external, relevant data
    *
    * @param tick
    *   Tick to update state for
    * @param modelState
    *   Last known model state
    * @param calcRelevantData
    *   Data, relevant for calculation
    * @param nodalVoltage
    *   Current nodal voltage of the agent
    * @param model
    *   Model for calculation
    * @return
    *   The updated state at given tick under consideration of calculation
    *   relevant data
    */
  override protected def updateState(
      tick: Long,
      modelState: EvcsState,
      calcRelevantData: EvcsRelevantData,
      nodalVoltage: squants.Dimensionless,
      model: EvcsModel,
  ): EvcsState = modelState
}
