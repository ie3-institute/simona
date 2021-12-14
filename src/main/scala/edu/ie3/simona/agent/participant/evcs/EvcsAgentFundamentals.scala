/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.result.system.{
  EvResult,
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
  BaseStateData,
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.EvMovementsMessage.EvcsMovements
import edu.ie3.simona.config.SimonaConfig.EvcsRuntimeConfig
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.evcs.EvcsModel.EvcsRelevantData
import edu.ie3.simona.model.participant.evcs.{
  EvcsChargingScheduleEntry,
  EvcsModel
}
import edu.ie3.simona.ontology.messages.PowerMessage.AssetPowerChangedMessage
import edu.ie3.simona.ontology.messages.services.EvMessage.{
  CurrentPriceRequest,
  CurrentPriceResponse,
  DepartedEvsResponse,
  EvFreeLotsRequest,
  EvMovementData,
  FreeLotsResponse
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, PU}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.PERCENT

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Dimensionless
import scala.jdk.CollectionConverters.ListHasAsScala
import scala.reflect.{ClassTag, classTag}

protected trait EvcsAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      EvcsRelevantData,
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
      inputModel: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
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
      resolution,
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
      inputModel: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
      servicesOpt: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
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
      Array.emptyLongArray, // Additional activation of the evcs agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution * 10,
        inputModel.getNode
          .getvTarget()
          .to(PU)
      ),
      ValueStore.forResult(resolution, 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10)
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
    implicit val startDateTime: ZonedDateTime =
      collectionStateData.baseStateData.startDate

    collectionStateData.baseStateData match {
      case modelBaseStateData: ParticipantModelBaseStateData[
            ApparentPower,
            _,
            _
          ] =>
        /* extract data from secondary data, which should have been requested and received before */
        collectionStateData.data
          .collectFirst {
            // filter secondary data for EV movements data
            case (_, Some(evcsData: EvMovementData)) =>
              handleEvMovementsAndGoIdle(
                currentTick,
                scheduler,
                modelBaseStateData,
                evcsData
              )
            case (_, Some(EvFreeLotsRequest)) =>
              handleFreeLotsRequestAndGoIdle(
                currentTick,
                scheduler,
                modelBaseStateData
              )
            case (_, Some(CurrentPriceRequest)) =>
              handleCurrentPriceRequestAndGoIdle(
                currentTick,
                scheduler,
                modelBaseStateData
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
    * data. Sends completion message to scheduler without scheduling new
    * activations.
    * @param currentTick
    *   The current tick that has been triggered
    * @param scheduler
    *   The scheduler ref
    * @param modelBaseStateData
    *   The state data
    * @return
    *   [[Idle]] state
    */
  private def handleFreeLotsRequestAndGoIdle(
      currentTick: Long,
      scheduler: ActorRef,
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _
      ]
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val (_, lastEvs, _, _) =
      getLastCalcRelevantData(currentTick, modelBaseStateData)

    val evcsModel = getEvcsModel(modelBaseStateData)

    evServiceRef ! FreeLotsResponse(
      evcsModel.uuid,
      evcsModel.chargingPoints - lastEvs.size
    )

    goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      modelBaseStateData,
      scheduler
    )
  }

  private def handleCurrentPriceRequestAndGoIdle(
      currentTick: Long,
      scheduler: ActorRef,
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _
      ]
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val (_, lastEvs, lastSchedule, voltages) =
      getLastCalcRelevantData(currentTick, modelBaseStateData)

    val evcsModel = getEvcsModel(modelBaseStateData)

    val relevantDataForCalculationOfCurrentPrice =
      EvcsRelevantData(
        lastEvs,
        lastSchedule,
        voltages
      )

    val currentPriceSignal: Option[Double] =
      evcsModel.calculateCurrentPriceSignalToCommunicateToEvs(
        currentTick,
        modelBaseStateData.startDate,
        relevantDataForCalculationOfCurrentPrice
      )

    currentPriceSignal match {
      case Some(price) =>
        evServiceRef ! CurrentPriceResponse(
          evcsModel.uuid,
          price
        )
      case None =>
        /* there is no useful price signal for this evcs, either because it is a private evcs
         * or because there is no data available to determine a useful price signal.
         * Because the evServiceRef needs a value and not an option[value], we return 0
         * --> this should be adapted
         */
        evServiceRef ! CurrentPriceResponse(
          evcsModel.uuid,
          0d
        )
    }

    goToIdleReplyCompletionAndScheduleTriggerForNextAction(
      modelBaseStateData,
      scheduler
    )
  }

  /** Handles a evcs movements message that contains information on arriving and
    * departing vehicles. After applying the movements to the last known set of
    * parked evs, returns departing evs and calculates new scheduling. Sends
    * completion message to scheduler without scheduling new activations.
    * @param currentTick
    *   The current tick that has been triggered
    * @param scheduler
    *   The scheduler ref
    * @param modelBaseStateData
    *   The state data
    * @param evcsMovementsData
    *   The movement data that has been received
    * @return
    *   [[Idle]] with updated relevant data store
    */
  private def handleEvMovementsAndGoIdle(
      currentTick: Long,
      scheduler: ActorRef,
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _
      ],
      evcsMovementsData: EvMovementData
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {

    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    /* Get calcRelevantData from last EvMovement update, including the EVs and scheduling */
    val (lastSchedulingTick, lastEvs, lastSchedule, voltages) =
      getLastCalcRelevantData(currentTick, modelBaseStateData)

    /* Get EvcsModel */
    val evcsModel = getEvcsModel(modelBaseStateData)

    /* Validate EV movements */
    validateEvMovements(
      lastEvs,
      evcsMovementsData.movements,
      evcsModel.chargingPoints
    )

    /* Relevant data for EvcsModel to calculate new SoC for all EVs */
    val relevantDataForEvUpdates =
      EvcsRelevantData(
        lastEvs,
        lastSchedule,
        Map.empty
      )

    /*
    /* Update EV models according to the scheduling determined at last EvMovements Event */
    val updatedEvs: Set[EvModel] = evcsModel
      .updateEvsAccordingToScheduling(
        currentTick,
        lastSchedulingTick,
        relevantDataForEvUpdates
      )
     */

    /* Update EV models according to the scheduling determined at last EvMovements Event and get EvResults
     * including charging costs */
    val updatedEvsAndEvResults: Set[(EvModel, Vector[EvResult])] = evcsModel
      .updateEvsAccordingToSchedulingAndCalculateChargingPricesAsEvResults(
        currentTick,
        modelBaseStateData.startDate,
        lastSchedulingTick,
        relevantDataForEvUpdates
      )

    val updatedEvs: Set[EvModel] = updatedEvsAndEvResults.map(x => x._1)
    val evResults: Set[Vector[EvResult]] = updatedEvsAndEvResults.map(x => x._2)

    /* Write ev results after charging to csv */
    announceEvResultsToListeners(evResults)

    /* Return EVs which parking time has ended */
    val departedEvs = calculateDepartedEvs(
      updatedEvs,
      evcsMovementsData.movements
    )
    if (departedEvs.nonEmpty) {
      evServiceRef ! DepartedEvsResponse(
        evcsModel.uuid,
        departedEvs
      )
    }

    /* Calculate evcs power for interval since last update, save for updating value store, and inform listeners */
    val updatedResultValueStore = modelBaseStateData match {
      case modelBaseStateData: ParticipantModelBaseStateData[
            ApparentPower,
            EvcsRelevantData,
            EvcsModel
          ] =>
        calculatePowerSinceLastEventUpdateResultValueStoreAndInformListeners(
          currentTick,
          modelBaseStateData
        )
      case _ =>
        throw new InconsistentStateException(
          "Wrong base state data"
        )
    }

    /* Calculate set of staying and arriving EVs */
    val (stayingEvs, arrivingEvs) = calculateStayingAndArrivingEvs(
      lastEvs,
      evcsMovementsData.movements
    )

    /* Write arriving evs to csv */
    announceNewArrivingEvsToListeners(
      currentTick,
      arrivingEvs,
      modelBaseStateData
    )

    val updatedRelevantData = {
      /* if new EVs arrived, a new scheduling must be calculated. */
      if (arrivingEvs.nonEmpty) {
        /* Update relevant data required for new scheduling */
        val relevantDataForScheduling =
          EvcsRelevantData(
            stayingEvs ++ arrivingEvs,
            Set.empty,
            voltages
          )
        /* Determine new schedule for charging the EVs */
        val newSchedule: Set[EvcsChargingScheduleEntry] = evcsModel
          .calculateNewScheduling(
            currentTick,
            modelBaseStateData.startDate,
            relevantDataForScheduling
          )
        /* Update relevant data with schedule */
        relevantDataForScheduling.copy(
          schedule = newSchedule
        )
      }

      /* if no new EVs arrived, the previous scheduling is kept but filtered, only the current EVs are updated. */
      else {
        relevantDataForEvUpdates.copy(
          currentEvs = stayingEvs,
          schedule = lastSchedule.filter(_.tickStop >= currentTick),
          // filter(_.tickStop > currentTick)
          // is it possible to remove also the schedules that ended at currentTick? -> probably yes, test required
          voltages = voltages
        )
      }
    }

    goToIdleWithUpdatedBaseStateData(
      scheduler,
      modelBaseStateData,
      updatedResultValueStore,
      updatedRelevantData
    )
  }

  /** Update the calc relevant data value store and go to Idle using the updated
    * base state data
    *
    * @param scheduler
    *   Actor reference of the scheduler
    * @param baseStateData
    *   The base state data of the collection state
    * @param updatedResultValueStore
    *   The updated result value store
    * @param relevantData
    *   Data, that have been relevant to this calculation
    * @return
    *   Desired state change
    */
  final def goToIdleWithUpdatedBaseStateData(
      scheduler: ActorRef,
      baseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _
      ],
      updatedResultValueStore: ValueStore[ApparentPower],
      relevantData: EvcsRelevantData
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {

    /* Update relevant data store */
    val updatedRelevantDataStore: ValueStore[EvcsRelevantData] =
      baseStateData match {
        case data: ParticipantModelBaseStateData[
              ApparentPower,
              EvcsRelevantData,
              EvcsModel
            ] =>
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

    /* Update the base state data with the updated result value store and relevant data store */
    val updatedBaseStateData =
      baseStateData match {
        case data: ParticipantModelBaseStateData[
              ApparentPower,
              EvcsRelevantData,
              EvcsModel
            ] =>
          data.copy(
            resultValueStore = updatedResultValueStore,
            calcRelevantDateStore = updatedRelevantDataStore
          )
        case _ =>
          throw new InconsistentStateException(
            "Wrong base state data"
          )
      }

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

            /* ADDED: Update relevant data store with voltage information for this tick */
            val updatedRelevantDataStore =
              updateRelevantDataStoreWithVoltageInformation(
                requestTick,
                nodalVoltage,
                modelBaseStateData
              )

            val nextStateData =
              modelBaseStateData.copy(
                requestValueStore = updatedRequestValueStore,
                voltageValueStore = updatedVoltageValueStore,
                calcRelevantDateStore = updatedRelevantDataStore
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
                  EvcsModel
                ] =>
              /* Update result value store with power changes since last update */
              val updatedResultValueStore: ValueStore[ApparentPower] =
                calculatePowerSinceLastEventUpdateResultValueStoreAndInformListeners(
                  requestTick,
                  modelBaseStateData
                )

              /* Update relevant data store with new voltage information */
              val updatedRelevantDataStore =
                updateRelevantDataStoreWithVoltageInformation(
                  requestTick,
                  nodalVoltage,
                  modelBaseStateData
                )

              modelBaseStateData.copy(
                resultValueStore = updatedResultValueStore,
                calcRelevantDateStore = updatedRelevantDataStore
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

  /** Update the calc relevant data store with new voltage information for the
    * voltage memory. The voltage memory either gets a new entry or an existing
    * entry might be updated. To limit the overall memory size, voltage
    * information older than 3 weeks is removed.
    * @param requestTick
    *   the tick the voltage information belongs to
    * @param nodalVoltage
    *   the voltage at this tick
    * @param modelBaseStateData
    *   model base state data
    * @return
    *   updated calc relevant data store
    */
  private def updateRelevantDataStoreWithVoltageInformation(
      requestTick: Long,
      nodalVoltage: ComparableQuantity[Dimensionless],
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsModel
      ]
  ): ValueStore[EvcsRelevantData] = {

    val (lastSchedulingTick, evs, schedule, voltages) =
      getLastCalcRelevantData(
        requestTick + 1,
        modelBaseStateData
      )

    /* Add or update the voltage memory with the voltage for the current tick.
     * Also remove voltage information that is older than 3 weeks to limit memory size. */
    val updatedVoltages = voltages.filter(
      _._1.isAfter(
        requestTick.toDateTime(modelBaseStateData.startDate).minusWeeks(3)
      )
    ) + (requestTick.toDateTime(modelBaseStateData.startDate) -> nodalVoltage)

    val updatedRelevantData =
      EvcsRelevantData(
        evs,
        schedule,
        updatedVoltages
      )

    ValueStore.updateValueStore(
      modelBaseStateData.calcRelevantDateStore,
      lastSchedulingTick, // update the value store at the tick where the schedule was calculated instead of a new one
      updatedRelevantData
    )

  }

  /** Calculate the power changes since the last update, which either happened
    * at a power request or at EV movements update.
    * @param requestTick
    *   The tick, the request belongs to
    * @param modelBaseStateData
    *   The model base state data
    * @return
    *   updated result value store with added power changes since the last
    *   request
    */
  private def calculatePowerSinceLastEventUpdateResultValueStoreAndInformListeners(
      requestTick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsModel
      ]
  ): ValueStore[ApparentPower] = {

    /* The resultValueStore is updated, whenever a power request is processed or a EV movements happen, which lead
     * to an updated schedule. Therefore, we always only need to consider the last schedule to determine the power
     * updates.
     */
    val (lastSchedulingTick, _, lastSchedule, _) =
      getLastCalcRelevantData(requestTick, modelBaseStateData)

    /* Power updates are already in the resultValueStore until the tick of the last event. Determine this tick to
     * know from where on new updates need to be written into the value store. This tick is either from the last update
     * of the calcRelevantDataStore (= EvMovements Event) or requestValueStore (= Power Request).
     */
    val lastUpdateTick: Long = math.max(
      lastSchedulingTick,
      modelBaseStateData.requestValueStore
        .lastKnownTick(requestTick - 1)
        .getOrElse(0L)
    )

    /* Get EvcsModel */
    val evcsModel = getEvcsModel(modelBaseStateData)

    /* Get last known node voltage */
    val voltage: ComparableQuantity[Dimensionless] =
      getAndCheckNodalVoltage(modelBaseStateData, requestTick)

    /* Calculate the power results according to the schedule from the lastUpdateTick until the current tick. */
    val results: Set[(Long, ApparentPower)] = if (lastSchedule.nonEmpty) {
      /* There is a schedule, which might include charging power changes. Calculate powers. */
      evcsModel.calculatePowerForLastInterval(
        lastSchedule,
        voltage,
        requestTick,
        lastUpdateTick
      )
    } else {
      Set.empty
    }

    /* Save updates for result value store and announce to listeners for each power change */
    val updatedResultValueStore: ValueStore[ApparentPower] = results.foldLeft(
      modelBaseStateData.resultValueStore
    )((valueStore, result) => {
      /* Update resultValueStore with result */
      val update = ValueStore.updateValueStore(
        valueStore,
        result._1,
        result._2
      )
      /* Inform the listeners about new result */
      announceSimulationResult(
        modelBaseStateData,
        result._1,
        result._2
      )(modelBaseStateData.outputConfig)
      update
    })

    updatedResultValueStore
  }

  /** Announce already constructed EvResults to listeners. Currently, the p and
    * q values are used to output the charged energy and charging costs.
    * @param evResults
    *   the set of ev results to announce
    */
  private def announceEvResultsToListeners(
      evResults: Set[Vector[EvResult]]
  ): Unit = {
    evResults
      .foreach(resultVector => {
        resultVector.foreach(result => {
          listener.foreach(_ ! ParticipantResultEvent(result))
        })
      })
  }

  /** Announce the new arriving evs to listeners. Currently, the p and q values
    * are used to output the charged energy and charging costs.
    * @param tick
    *   tick of the ev arrival
    * @param evs
    *   the evs for which the updates are announced
    * @param modelBaseStateData
    *   the model base state data
    */
  private def announceNewArrivingEvsToListeners(
      tick: Long,
      evs: Set[EvModel],
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _
      ]
  ): Unit = {

    evs.foreach(ev => {
      listener.foreach(
        _ ! ParticipantResultEvent(
          new EvResult(
            modelBaseStateData.modelUuid,
            tick.toDateTime(modelBaseStateData.startDate),
            ev.getUuid,
            Quantities
              .getQuantity(0, KILOWATT), // charging powers not used currently
            Quantities
              .getQuantity(0, KILOWATT), // charging powers not used currently
            ev.getStoredEnergy
              .divide(ev.getEStorage)
              .asType(classOf[Dimensionless])
              .to(PERCENT)
          )
        )
      )
    })

  }

  /** Get the last entry in the calc relevant data store BEFORE OR AT the
    * current tick.
    * @param currentTick
    *   current tick
    * @param modelBaseStateData
    *   the model base state data
    * @return
    *   the entries in the calc relevant data store before or the requested tick
    */
  private def getLastCalcRelevantData(
      currentTick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        _ <: ApparentPower,
        _,
        _
      ]
  ): (
      Long,
      Set[EvModel],
      Set[EvcsChargingScheduleEntry],
      Map[ZonedDateTime, ComparableQuantity[Dimensionless]]
  ) = {
    modelBaseStateData.calcRelevantDateStore
      .last(currentTick - 1) match {
      case Some((lastTick, EvcsRelevantData(evs, schedule, voltages))) =>
        (lastTick, evs, schedule, voltages)
      case _ =>
        /* At the first tick, we are not able to determine the data for the previous tick
         * (since there is none). */
        (0, Set.empty[EvModel], Set.empty[EvcsChargingScheduleEntry], Map.empty)
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

  private def calculateDepartedEvs(
      calculatedEvs: Set[EvModel],
      evcsMovements: EvcsMovements
  ): Set[EvModel] =
    calculatedEvs.filter { ev =>
      // EV has been parked up until now and has now departed
      evcsMovements.getDepartures.contains(ev.getUuid)
    }

  private def calculateStayingAndArrivingEvs(
      lastEvs: Set[EvModel],
      evcsMovements: EvcsMovements
  ): (Set[EvModel], Set[EvModel]) = {
    // If we say that a car departs at t, it means that it stays parked up to and including t.
    // Evs that have been parked here and have not departed
    val stayingEvs = lastEvs.filter { ev =>
      !evcsMovements.getDepartures.contains(ev.getUuid)
    }
    // New arriving evs
    val arrivingEvs =
      evcsMovements.getArrivals.asScala.filter { ev =>
        !lastEvs.exists(_.getUuid == ev.getUuid)
      }.toSet
    (stayingEvs, arrivingEvs)
  }

  /** Checks whether received EV movement data is consistent with charging
    * station specifications and currently connected EVs. Only logs warnings,
    * does not throw exceptions.
    *
    * @param lastEvs
    *   EVs of the last tick
    * @param evMovementsData
    *   received EV movement data
    * @param chargingPoints
    *   max number of charging points available at this CS
    */
  private def validateEvMovements(
      lastEvs: Set[EvModel],
      evMovementsData: EvcsMovements,
      chargingPoints: Int
  ): Unit = {
    evMovementsData.getDepartures.asScala.foreach { ev =>
      if (!lastEvs.exists(_.getUuid == ev))
        log.warning(
          s"EV $ev should depart from this station (according to external simulation), but has not been parked here."
        )
    }

    evMovementsData.getArrivals.asScala.foreach { ev =>
      if (lastEvs.exists(_.getUuid == ev.getUuid))
        log.warning(
          s"EV ${ev.getId} should arrive at this station (according to external simulation), but is already parked here."
        )
    }

    val newCount =
      lastEvs.count { ev =>
        !evMovementsData.getDepartures.contains(ev.getUuid)
      } +
        evMovementsData.getArrivals.asScala.count { ev =>
          !lastEvs.exists(_.getUuid == ev.getUuid)
        }

    if (newCount > chargingPoints)
      log.warning(
        "More EVs are parking at this station than physically possible."
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
}
