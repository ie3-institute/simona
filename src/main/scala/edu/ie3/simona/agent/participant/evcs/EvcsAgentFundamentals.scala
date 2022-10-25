/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import akka.actor.{ActorRef, FSM}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.result.system.{
  EvcsResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.StatelessParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.ActorEvMovementsService
import edu.ie3.simona.agent.participant.evcs.EvcsAgent.neededServices
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.InputModelContainer
import edu.ie3.simona.agent.participant.statedata.BaseStateData
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.FlexChangeIndicator
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  ArrivingEvsData,
  DepartingEvsResponse,
  FreeLotsResponse
}
import edu.ie3.simona.model.participant.evcs.EvcsModel
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState
}
import edu.ie3.simona.ontology.messages.PowerMessage.AssetPowerChangedMessage
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

protected trait EvcsAgentFundamentals
    extends StatelessParticipantAgentFundamentals[
      ApparentPower,
      EvcsRelevantData,
      EvcsState,
      ParticipantStateData[ApparentPower],
      EvcsInput,
      EvcsRuntimeConfig,
      EvcsModel
    ] {
  this: EvcsAgent =>
  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]

  /** Determines the needed base state data in dependence of the foreseen
    * simulation mode of the agent.
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
    *   Config of the output behaviour for simulation results
    * @return
    *   A [[ParticipantModelBaseStateData]] that reflects the behaviour based on
    *   the data source definition
    */
  override def determineModelBaseStateData(
      inputModel: InputModelContainer[EvcsInput],
      modelConfig: EvcsRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel
  ] = {
    /* Check for needed services */
    if (
      !services.exists(serviceDefinitions =>
        serviceDefinitions.map(_.getClass).containsSlice(neededServices)
      )
    )
      throw new AgentInitializationException(
        s"EvcsAgent cannot be initialized without an ev data service!"
      )

    baseStateDataForModelCalculation(
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

  /** Determine needed base state data for model calculation simulation mode.
    *
    * @param inputModel
    *   Input model
    * @param modelConfig
    *   Configuration for the model
    * @param servicesOpt
    *   [[Option]] on a vector of [[SecondaryDataService]] s
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
    *   Config of the output behaviour for simulation results
    * @return
    *   Needed base state data for model calculation
    */
  def baseStateDataForModelCalculation(
      inputModel: InputModelContainer[EvcsInput],
      modelConfig: EvcsRuntimeConfig,
      servicesOpt: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel
  ] = {

    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
      )

    ParticipantModelBaseStateData[
      ApparentPower,
      EvcsRelevantData,
      EvcsState,
      EvcsModel
    ](
      simulationStartDate,
      simulationEndDate,
      model,
      servicesOpt,
      outputConfig,
      Array.emptyLongArray, // Additional activation of the evcs agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution * 10, // FIXME probably need to increase this for grid oriented scheduling
        inputModel.electricalInputModel.getNode
          .getvTarget()
          .to(PU)
      ),
      ValueStore.forResult(resolution, 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10),
      maybeEmAgent.map(FlexStateData(_, ValueStore(resolution * 10)))
    )
  }

  override def buildModel(
      inputModel: InputModelContainer[EvcsInput],
      modelConfig: EvcsRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EvcsModel = EvcsModel(
    inputModel.electricalInputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate,
    modelConfig.chargingStrategy
  )

  override protected def createInitialState(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): EvcsState =
    EvcsState(
      Set.empty,
      Map.empty,
      SimonaConstants.FIRST_TICK_IN_SIMULATION
    )

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      tick: Long
  ): EvcsRelevantData = {
    // always only take arrivals for the current tick
    // or empty sequence if none arrived
    val movements = baseStateData.receivedSecondaryDataStore
      .getOrElse(tick, Map.empty)
      .collectFirst {
        // filter secondary data for arriving EVs data
        case (_, evcsData: ArrivingEvsData) =>
          evcsData.arrivals
      }
      .getOrElse(Seq.empty)

    val voltages = baseStateData.voltageValueStore.asMap
      .map { case (tick, voltage) =>
        tick.toDateTime(baseStateData.startDate) -> voltage
      }
    EvcsRelevantData(tick, movements, voltages)
  }

  /** Handle an active power change by flex control.
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
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      data: EvcsRelevantData,
      lastState: EvcsState,
      setPower: ComparableQuantity[Power]
  ): (EvcsState, ApparentPower, FlexChangeIndicator) = {
    /* Calculate the power */
    val voltage = getAndCheckNodalVoltage(baseStateData, tick)

    val reactivePower = baseStateData.model match {
      case model: EvcsModel =>
        model.calculateReactivePower(
          setPower,
          voltage
        )
    }
    val result = ApparentPower(setPower, reactivePower)

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
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPower =
    (_, _, _) =>
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
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage]] to
    * scheduler and using update result values.</p>
    *
    * @param baseStateData
    *   The base state data with collected secondary data
    * @param maybeLastModelState
    *   Optional last model state
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      maybeLastModelState: Option[EvcsState],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
    implicit val startDateTime: ZonedDateTime = baseStateData.startDate

    /* extract EV data from secondary data, which should have been requested and received before */
    baseStateData.receivedSecondaryDataStore
      .getOrElse(currentTick, Map.empty)
      .values
      .collectFirst {
        // filter secondary data for arriving EVs data
        case _: ArrivingEvsData =>
          handleArrivingEvsAndGoIdle(
            currentTick,
            scheduler,
            baseStateData
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
    * @param tick
    *   The tick that free lots have been requested for
    * @param modelBaseStateData
    *   The state data
    */
  protected def handleFreeLotsRequest(
      tick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): Unit = {
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val lastState =
      getLastOrInitialStateData(modelBaseStateData, tick - 1)

    evServiceRef ! FreeLotsResponse(
      modelBaseStateData.model.uuid,
      modelBaseStateData.model.chargingPoints - lastState.evs.size
    )
  }

  /** Handle a charging price request. Based on the given type of charging
    * station and the current "state" of the charging station, a price is
    * calculated and sent back to the requesting entity.
    *
    * @param tick
    *   Current simulation time
    * @param modelBaseStateData
    *   Base state orientation
    */
  protected def handleCurrentPriceRequest(
      tick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): Unit = {
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val lastState =
      getLastOrInitialStateData(modelBaseStateData, tick - 1)

    modelBaseStateData.model.calculateCurrentPriceSignalToCommunicateToEvs(
      tick,
      lastState
    ) match {
      case Some(price) =>
        evServiceRef ! CurrentPriceResponse(
          modelBaseStateData.modelUuid,
          price
        )
      case None =>
        /* there is no useful price signal for this evcs, either because it is a private evcs
         * or because there is no data available to determine a useful price signal.
         * Because the evServiceRef needs a value and not an option[value], we return 0
         * --> this should be adapted
         */
        evServiceRef ! CurrentPriceResponse(
          modelBaseStateData.modelUuid,
          0d
        )
    }
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
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel
  ] = {
    val evServiceRef = getService[ActorEvMovementsService](
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
      tick
    )

    val (departingEvs, stayingEvs) = updatedEvs.partition { ev =>
      requestedDepartingEvs.contains(ev.getUuid)
    }

    // send back departing EVs
    if (requestedDepartingEvs.nonEmpty) {
      evServiceRef ! DepartingEvsResponse(
        baseStateData.modelUuid,
        departingEvs
      )
    }

    val voltage = baseStateData.voltageValueStore
      .last(tick)
      .map { case (_, voltage) =>
        voltage
      }
      .getOrElse(Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE))

    /* Calculate evcs power for interval since last update, save for updating value store, and inform listeners */
    val updatedResultValueStore =
      determineResultsAnnounceUpdateValueStore(
        lastState,
        tick,
        voltage,
        baseStateData
      )

    val stayingSchedules = lastState.schedule.flatMap {
      case (ev, maybeSchedule) if !requestedDepartingEvs.contains(ev.getUuid) =>
        Some(
          ev -> maybeSchedule.map(scheduleContainer =>
            scheduleContainer.copy(schedule =
              scheduleContainer.schedule.filter(_.tickStop >= tick)
            // filter(_.tickStop > currentTick)
            // TODO is it possible to remove also the schedules that ended at currentTick? -> probably yes, test required
            )
          )
        )
      case _ => None
    }

    val newState = EvcsState(stayingEvs, stayingSchedules, tick)

    baseStateData.copy(
      stateDataStore = ValueStore.updateValueStore(
        baseStateData.stateDataStore,
        tick,
        newState
      ),
      resultValueStore = updatedResultValueStore
    )
  }

  /** Handles a evcs movements message that contains information on arriving and
    * departing vehicles. After applying the movements to the last known set of
    * parked evs, returns departing evs and calculates new scheduling. Sends
    * completion message to scheduler without scheduling new activations.
    *
    * @param currentTick
    *   The current tick that has been triggered
    * @param scheduler
    *   The scheduler ref
    * @param modelBaseStateData
    *   The state data
    * @return
    *   [[Idle]] with updated relevant data store
    */
  private def handleArrivingEvsAndGoIdle(
      currentTick: Long,
      scheduler: ActorRef,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {

    val relevantData =
      createCalcRelevantData(modelBaseStateData, currentTick)

    val lastState = getLastOrInitialStateData(modelBaseStateData, currentTick)

    val currentEvs = modelBaseStateData.model.determineCurrentState(
      relevantData,
      lastState
    )

    // if new EVs arrived, a new scheduling must be calculated.
    val newSchedule = modelBaseStateData.model
      .calculateNewScheduling(
        relevantData,
        currentEvs
      )

    // create new current state
    val newState = EvcsState(currentEvs, newSchedule, currentTick)

    val updatedStateDataStore =
      ValueStore.updateValueStore(
        modelBaseStateData.stateDataStore,
        currentTick,
        newState
      )

    /* Update the base state data with the updated result value store and relevant data store */
    val updatedBaseStateData =
      modelBaseStateData.copy(
        stateDataStore = updatedStateDataStore
      )

    goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      updatedBaseStateData,
      scheduler
    )
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
  override def determineReply(
      requestTick: Long,
      baseStateData: BaseStateData[ApparentPower],
      mostRecentRequest: Option[(Long, ApparentPower)],
      nodalVoltage: ComparableQuantity[Dimensionless],
      updatedVoltageValueStore: ValueStore[ComparableQuantity[Dimensionless]],
      alternativeResult: ApparentPower
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
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
                ApparentPower,
                EvcsRelevantData,
                EvcsState,
                EvcsModel
              ] =>
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

        /* ADDED: Update base state data before answering the power request to include the scheduling up to this tick.
         * Also, save the new voltage information for the calc relevant data store.
         */
        val updatedBaseStateData =
          baseStateData match {
            case modelBaseStateData: ParticipantModelBaseStateData[
                  ApparentPower,
                  EvcsRelevantData,
                  EvcsState,
                  EvcsModel
                ] =>
              // FIXME this is still incomplete ?

              val lastState =
                getLastOrInitialStateData(modelBaseStateData, requestTick)

              val voltage = modelBaseStateData.voltageValueStore
                .last(requestTick)
                .map { case (_, voltage) =>
                  voltage
                }
                .getOrElse(
                  Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE)
                )

              val updatedResultValueStore =
                determineResultsAnnounceUpdateValueStore(
                  lastState,
                  requestTick,
                  voltage,
                  modelBaseStateData
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
          updatedBaseStateData.requestValueStore
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
              alternativeResult
            )
          case None =>
            /* There is no simulation result at all. Reply with zero power */
            stayWithUpdatedRequestValueStore(
              updatedBaseStateData,
              alternativeResult,
              requestTick,
              updatedVoltageValueStore
            )
        }
    }
  }

  override def handleCalculatedResult(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      tick: Long
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel
  ] = {

    // calculate results from last schedule
    baseStateData.stateDataStore
      .last(tick - 1)
      .map { case (lastTick, lastState) =>
        val voltage = baseStateData.voltageValueStore
          .last(tick - 1)
          .map { case (_, voltage) =>
            voltage
          }
          .getOrElse(
            Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE)
          )

        val updatedResultValueStore =
          determineResultsAnnounceUpdateValueStore(
            lastState,
            tick,
            voltage,
            baseStateData
          )

        baseStateData.copy(
          resultValueStore = updatedResultValueStore
        ): ParticipantModelBaseStateData[
          ApparentPower,
          EvcsRelevantData,
          EvcsState,
          EvcsModel
        ]
      }
      .getOrElse(baseStateData)

  }

  /** Determine evcs results, announce them to listeners and update the result
    * value store.
    *
    * @param lastState
    *   The state (including schedule) to calculate results for
    * @param tick
    *   The tick up to which results should be calculated for
    * @param voltage
    *   The voltage magnitude used for reactive power calculation
    * @param modelBaseStateData
    *   Model base state data
    * @return
    *   The updated result value store
    */
  private def determineResultsAnnounceUpdateValueStore(
      lastState: EvcsState,
      tick: Long,
      voltage: ComparableQuantity[Dimensionless],
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): ValueStore[ApparentPower] = {

    val (evResults, evcsResults) = modelBaseStateData.model.createResults(
      lastState,
      tick,
      voltage
    )

    // send out EV results
    evResults.foreach { result =>
      listener.foreach(_ ! ParticipantResultEvent(result))
    }

    evcsResults.foldLeft(modelBaseStateData.resultValueStore) {
      case (resultValueStore, result) =>
        /* Inform the listeners about new result */
        if (modelBaseStateData.outputConfig.simulationResultInfo)
          notifyListener(
            ParticipantResultEvent(result)
          )

        /* Update resultValueStore with result */
        ValueStore.updateValueStore(
          resultValueStore,
          result.getTime.toTick(modelBaseStateData.startDate),
          ApparentPower(result.getP, result.getQ)
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
      result: ApparentPower
  ): SystemParticipantResult =
    new EvcsResult(
      dateTime,
      uuid,
      result.p,
      result.q
    )
}
