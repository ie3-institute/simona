/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import akka.actor.{ActorRef, FSM}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.result.system.{
  EvcsResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
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
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.EvcsModel
import edu.ie3.simona.model.participant.EvcsModel.{EvcsRelevantData, EvcsState}
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  ArrivingEvsData,
  DepartingEvsResponse,
  FreeLotsResponse
}
import edu.ie3.simona.service.ev.ExtEvDataService.FALLBACK_EV_MOVEMENTS_STEM_DISTANCE
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

protected trait EvcsAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      EvcsRelevantData,
      EvcsState,
      ParticipantStateData[ApparentPower],
      EvcsInput,
      EvcsRuntimeConfig,
      EvcsModel
    ]
    with LazyLogging {
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
    * @param timeBin
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
      inputModel: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      timeBin: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
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
      timeBin,
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
    * @param timeBin
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
      inputModel: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
      servicesOpt: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      timeBin: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
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

    ParticipantModelBaseStateData(
      simulationStartDate,
      simulationEndDate,
      model,
      servicesOpt,
      outputConfig,
      Array.emptyLongArray, // Additional activation of the evcs agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        timeBin * 10,
        inputModel.getNode
          .getvTarget()
          .to(PU)
      ),
      ValueStore.forResult(timeBin, 10),
      ValueStore(timeBin * 10),
      ValueStore(timeBin * 10),
      ValueStore(timeBin * 10),
      maybeEmAgent.map(FlexStateData(_, ValueStore(timeBin * 10)))
    )
  }

  override def buildModel(
      inputModel: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EvcsModel = EvcsModel(
    inputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate
  )

  override protected def createInitialState(): EvcsState = EvcsState(Set.empty)

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      tick: Long
  ): EvcsRelevantData = {
    // TODO implement

    throw new NotImplementedError()
  }

  override protected def calculateResult(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      currentTick: Long,
      activePower: ComparableQuantity[Power]
  ): ApparentPower = {
    val voltage = getAndCheckNodalVoltage(baseStateData, currentTick)

    val reactivePower = baseStateData.model match {
      case model: EvcsModel =>
        model.calculateReactivePower(
          activePower,
          voltage
        )
    }

    ApparentPower(activePower, reactivePower)
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
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
    implicit val startDateTime: ZonedDateTime = baseStateData.startDate

    /* extract EV data from secondary data, which should have been requested and received before */
    baseStateData.receivedSecondaryDataStore
      .getOrElse(currentTick, Map.empty)
      .collectFirst {
        // filter secondary data for EV movements data
        case (_, arrivingEvs: ArrivingEvsData) =>
          handleArrivingEvsAndGoIdle(
            currentTick,
            scheduler,
            baseStateData,
            arrivingEvs.arrivals
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
        _ <: ApparentPower,
        _,
        _,
        _
      ]
  ): Unit = {
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val (_, lastEvs) =
      getTickIntervalAndLastState(tick, modelBaseStateData)

    val evcsModel = getEvcsModel(modelBaseStateData)

    evServiceRef ! FreeLotsResponse(
      evcsModel.uuid,
      evcsModel.chargingPoints - lastEvs.evs.size
    )
  }

  /** Handle a request for returning those EVs that are departing at the current
    * tick. SOC and results are calculated and corresponding listeners are
    * informed.
    *
    * @param tick
    *   The current simulation tick
    * @param modelBaseStateData
    *   The current Base state data
    * @param requestedDepartingEvs
    *   The UUIDs of EVs that are requested to be returned
    */
  protected def handleDepartingEvsRequest(
      tick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      requestedDepartingEvs: Seq[UUID]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel
  ] = {

    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    // retrieve the last updated set of parked EVs
    val (tickInterval, lastState) =
      getTickIntervalAndLastState(tick, modelBaseStateData)

    validateDepartures(lastState.evs, requestedDepartingEvs)

    val evcsModel = getEvcsModel(modelBaseStateData)

    val voltage =
      getAndCheckNodalVoltage(modelBaseStateData, tick)

    // calculate power with evs that have been parked up until now
    val relevantData = EvcsRelevantData(tickInterval)

    val (result, updateState) =
      evcsModel.calculatePowerAndEvSoc(
        tick,
        voltage,
        relevantData,
        lastState
      )

    val (departingEvs, stayingEvs) =
      updateState.evs.partition { ev =>
        // EV has been parked up until now and is now departing
        requestedDepartingEvs.contains(ev.getUuid)
      }

    val stateWithoutDeparting = updateState.copy(
      evs = stayingEvs
    )

    if (departingEvs.nonEmpty) {
      evServiceRef ! DepartingEvsResponse(
        evcsModel.uuid,
        departingEvs
      )
    }

    /* Update the base state data */
    updateValueStoresInformListeners(
      modelBaseStateData,
      tick,
      result,
      stateWithoutDeparting
    )
  }

  /** Handles EV arrivals as part of ExtEvDataService secondary data. After
    * adding the arriving EVs to the set of staying evs, resulting charging
    * power is calculated. A completion message is sent to scheduler without
    * scheduling new activations.
    *
    * @param tick
    *   The current tick that has been triggered
    * @param scheduler
    *   The scheduler ref
    * @param modelBaseStateData
    *   The state data
    * @param arrivingEvs
    *   The movement data on arrivingEvs that has been received
    * @return
    *   [[Idle]] with updated result values
    */
  private def handleArrivingEvsAndGoIdle(
      tick: Long,
      scheduler: ActorRef,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      arrivingEvs: Seq[EvModel]
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {

    val evcsModel = getEvcsModel(modelBaseStateData)

    // retrieve the last updated set of parked EVs, which could stem from
    // the current tick if there were departures for this tick as well
    val (tickInterval, lastState) =
      getTickIntervalAndLastState(tick, modelBaseStateData)

    validateArrivals(lastState.evs, arrivingEvs, evcsModel.chargingPoints)

    val relevantData = EvcsRelevantData(tickInterval)

    val updatedStateData =
      if (tickInterval > 0) {
        // if we haven't had any departing EVs for this tick,
        // this also means that we have not caught up with
        // calculating the current SOC

        val voltage =
          getAndCheckNodalVoltage(modelBaseStateData, tick)

        // calculate power with evs that have been parked up until now
        val (result, updatedState) =
          evcsModel.calculatePowerAndEvSoc(
            tick,
            voltage,
            relevantData,
            lastState
          )

        val stateWithArriving = updatedState.copy(
          evs = updatedState.evs ++ arrivingEvs
        )

        updateValueStoresInformListeners(
          modelBaseStateData,
          tick,
          result,
          stateWithArriving
        )
      } else {
        // if some EVs were departing at the current tick,
        // we're already up-to-date in that regard

        val updatedState = lastState.copy(
          evs = lastState.evs ++ arrivingEvs
        )

        val updatedStateStore = ValueStore.updateValueStore(
          modelBaseStateData.stateDataStore,
          tick,
          updatedState
        )

        modelBaseStateData.copy(
          stateDataStore = updatedStateStore
        )
      }

    goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      updatedStateData,
      scheduler
    )
  }

  /** Update the result and calc relevant data value stores and inform all
    * registered listeners
    *
    * @param baseStateData
    *   The base state data of the collection state
    * @param tick
    *   The current tick
    * @param result
    *   Result of simulation
    * @param newState
    *   The new state
    * @return
    *   Desired state change
    */
  private final def updateValueStoresInformListeners(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      tick: Long,
      result: ApparentPower,
      newState: EvcsState
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsState,
    EvcsModel
  ] = {
    /* Update the value stores */
    val updatedValueStore =
      ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        tick,
        result
      )
    val updatedStateStore = ValueStore.updateValueStore(
      baseStateData.stateDataStore,
      tick,
      newState
    )

    /* Inform the listeners about new result */
    announceSimulationResult(
      baseStateData,
      tick,
      result
    )(baseStateData.outputConfig)

    /* Update the base state data */
    baseStateData.copy(
      resultValueStore = updatedValueStore,
      stateDataStore = updatedStateStore
    )
  }

  private def getTickIntervalAndLastState(
      currentTick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _,
        _
      ]
  ): (Long, EvcsState) = {
    modelBaseStateData.stateDataStore
      .last(currentTick) match {
      case Some((tick, state: EvcsState)) =>
        (currentTick - tick, state)
      case _ =>
        /* At the first tick, we are not able to determine the tick interval from last tick
         * (since there is none). Then we use a fall back ev stem distance.
         * As no evs are charging then, the tick interval should be ignored anyway. */
        (FALLBACK_EV_MOVEMENTS_STEM_DISTANCE, createInitialState())
    }
  }

  private def getEvcsModel(
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _,
        _
      ]
  ): EvcsModel =
    modelBaseStateData.model match {
      case model: EvcsModel =>
        model
      case unsupportedModel =>
        throw new InconsistentStateException(
          s"Wrong model: $unsupportedModel!"
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

  /** Checks whether requested departing EVs are consistent with currently
    * connected EVs. Only logs warnings, does not throw exceptions.
    *
    * @param lastEvs
    *   EVs of the last tick
    * @param departures
    *   Departing EVs at the current tick
    */
  protected def validateDepartures(
      lastEvs: Set[EvModel],
      departures: Seq[UUID]
  ): Unit = {
    departures.foreach { ev =>
      if (!lastEvs.exists(_.getUuid == ev))
        logger.warn(
          s"EV $ev should depart from this station (according to external simulation), but has not been parked here."
        )
    }
  }

  /** Checks whether provided arriving EVs are consistent with charging station
    * specifications and currently connected EVs. Only logs warnings, does not
    * throw exceptions.
    *
    * @param lastEvs
    *   EVs of the last tick
    * @param arrivals
    *   Arriving EVs at the current tick
    * @param chargingPoints
    *   max number of charging points available at this CS
    */
  protected def validateArrivals(
      lastEvs: Set[EvModel],
      arrivals: Seq[EvModel],
      chargingPoints: Int
  ): Unit = {

    arrivals.foreach { ev =>
      if (lastEvs.exists(_.getUuid == ev.getUuid))
        logger.warn(
          s"EV ${ev.getId} should arrive at this station (according to external simulation), but is already parked here."
        )
    }

    val newCount = lastEvs.size +
      arrivals.count { ev =>
        !lastEvs.exists(_.getUuid == ev.getUuid)
      }

    if (newCount > chargingPoints)
      logger.warn(
        "More EVs are parking at this station than physically possible."
      )
  }
}
