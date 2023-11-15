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
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  DataCollectionStateData,
  ParticipantStateData
}
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
import edu.ie3.simona.model.participant.EvcsModel.EvcsRelevantData
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  ArrivingEvsData,
  DepartingEvsResponse,
  FreeLotsResponse
}
import edu.ie3.simona.service.ev.ExtEvDataService.FALLBACK_EV_MOVEMENTS_STEM_DISTANCE
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.Kilovars
import squants.{Each, Dimensionless}
import squants.energy.Kilowatts

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedSet
import scala.reflect.{ClassTag, classTag}

protected trait EvcsAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      EvcsRelevantData,
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
      outputConfig: ParticipantNotifierConfig
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
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
      outputConfig
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
      outputConfig: ParticipantNotifierConfig
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
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
      SortedSet.empty, // Additional activation of the evcs agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        timeBin * 10,
        Each(
          inputModel.getNode
            .getvTarget()
            .to(PU)
            .getValue
            .doubleValue
        )
      ),
      ValueStore.forResult(timeBin, 10),
      ValueStore(timeBin * 10),
      ValueStore(timeBin * 10)
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

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[ApparentPower, EvcsRelevantData, EvcsModel],
      Dimensionless
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
    * [[edu.ie3.simona.ontology.messages.SchedulerMessageTyped.Completion]] to
    * scheduler and using update result values.</p>
    *
    * @param collectionStateData
    *   State data with collected, comprehensive secondary data.
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithSecondaryDataAndGoToIdle(
      collectionStateData: DataCollectionStateData[ApparentPower],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {

    collectionStateData.baseStateData match {
      case modelBaseStateData: ParticipantModelBaseStateData[
            ApparentPower,
            EvcsRelevantData,
            EvcsModel
          ] =>
        /* extract EV data from secondary data, which should have been requested and received before */
        collectionStateData.data.values
          .collectFirst {
            // filter secondary data for arriving EVs data
            case Some(arrivingEvs: ArrivingEvsData) =>
              handleArrivingEvsAndGoIdle(
                currentTick,
                scheduler,
                modelBaseStateData,
                arrivingEvs.arrivals
              )
          }
          .getOrElse(
            throw new InconsistentStateException(
              s"The model ${modelBaseStateData.model} was not provided with needed EV data."
            )
          )

      case _ =>
        throw new InconsistentStateException(
          "Cannot find a model for model calculation."
        )
    }
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
        _
      ]
  ): Unit = {
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val (_, lastEvs) =
      getTickIntervalAndLastEvs(tick, modelBaseStateData)

    val evcsModel = getEvcsModel(modelBaseStateData)

    evServiceRef ! FreeLotsResponse(
      evcsModel.uuid,
      evcsModel.chargingPoints - lastEvs.size
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
        EvcsModel
      ],
      requestedDepartingEvs: Seq[UUID]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsModel
  ] = {

    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    // retrieve the last updated set of parked EVs
    val (tickInterval, lastEvs) =
      getTickIntervalAndLastEvs(tick, modelBaseStateData)

    validateDepartures(lastEvs, requestedDepartingEvs)

    val evcsModel = getEvcsModel(modelBaseStateData)

    val voltage =
      getAndCheckNodalVoltage(modelBaseStateData, tick)

    // calculate power with evs that have been parked up until now
    val relevantData =
      EvcsRelevantData(
        tickInterval,
        lastEvs
      )

    val (result, evModelsCalculated) =
      evcsModel.calculatePowerAndEvSoc(
        tick,
        voltage,
        relevantData
      )

    val (departingEvs, stayingEvs) =
      evModelsCalculated.partition { ev =>
        // EV has been parked up until now and is now departing
        requestedDepartingEvs.contains(ev.getUuid)
      }

    val updatedRelevantData = relevantData.copy(
      currentEvs = stayingEvs
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
      ApparentPower(
        Kilowatts(result.p.value.doubleValue),
        Kilovars(result.q.value.doubleValue)
      ),
      updatedRelevantData
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
        EvcsModel
      ],
      arrivingEvs: Seq[EvModel]
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {

    val evcsModel = getEvcsModel(modelBaseStateData)

    // retrieve the last updated set of parked EVs, which could stem from
    // the current tick if there were departures for this tick as well
    val (tickInterval, lastEvs) =
      getTickIntervalAndLastEvs(tick, modelBaseStateData)

    validateArrivals(lastEvs, arrivingEvs, evcsModel.chargingPoints)

    val relevantData =
      EvcsRelevantData(
        tickInterval,
        lastEvs
      )

    val updatedStateData =
      if (tickInterval > 0) {
        // if we haven't had any departing EVs for this tick,
        // this also means that we have not caught up with
        // calculating the current SOC

        val voltage =
          getAndCheckNodalVoltage(modelBaseStateData, tick)

        // calculate power with evs that have been parked up until now
        val (result, evModelsCalculated) =
          evcsModel.calculatePowerAndEvSoc(
            tick,
            voltage,
            relevantData
          )

        val updatedRelevantData = relevantData.copy(
          currentEvs = evModelsCalculated ++ arrivingEvs
        )

        updateValueStoresInformListeners(
          modelBaseStateData,
          tick,
          ApparentPower(
            Kilowatts(result.p.value.doubleValue),
            Kilovars(result.q.value.doubleValue)
          ),
          updatedRelevantData
        )
      } else {
        // if some EVs were departing at the current tick,
        // we're already up-to-date in that regard

        val updatedRelevantData = relevantData.copy(
          currentEvs = lastEvs ++ arrivingEvs
        )

        val updatedRelevantDataStore =
          ValueStore.updateValueStore(
            modelBaseStateData.calcRelevantDateStore,
            tick,
            updatedRelevantData
          )

        modelBaseStateData.copy(
          calcRelevantDateStore = updatedRelevantDataStore
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
    * @param relevantData
    *   Data, that have been relevant to this calculation
    * @return
    *   Desired state change
    */
  private final def updateValueStoresInformListeners(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsModel
      ],
      tick: Long,
      result: ApparentPower,
      relevantData: EvcsRelevantData
  ): ParticipantModelBaseStateData[
    ApparentPower,
    EvcsRelevantData,
    EvcsModel
  ] = {
    /* Update the value stores */
    val updatedValueStore =
      ValueStore.updateValueStore(
        baseStateData.resultValueStore,
        tick,
        result
      )
    val updatedRelevantDataStore =
      ValueStore.updateValueStore(
        baseStateData.calcRelevantDateStore,
        tick,
        relevantData
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
      calcRelevantDateStore = updatedRelevantDataStore
    )
  }

  private def getTickIntervalAndLastEvs(
      currentTick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _
      ]
  ): (Long, Set[EvModel]) = {
    modelBaseStateData.calcRelevantDateStore
      .last(currentTick) match {
      case Some((tick, EvcsRelevantData(_, evs))) =>
        (currentTick - tick, evs)
      case _ =>
        /* At the first tick, we are not able to determine the tick interval from last tick
         * (since there is none). Then we use a fall back ev stem distance.
         * As no evs are charging then, the tick interval should be ignored anyway. */
        (FALLBACK_EV_MOVEMENTS_STEM_DISTANCE, Set.empty[EvModel])
    }
  }

  private def getEvcsModel(
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
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
      result.p.toMegawatts.asMegaWatt,
      result.q.toMegavars.asMegaVar
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
