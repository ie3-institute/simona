/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.StandardUnits
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
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
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
import edu.ie3.simona.model.participant.evcs.EvcsModel
import edu.ie3.simona.model.participant.evcs.EvcsModel.{
  EvcsRelevantData,
  EvcsState,
  PowerEntry
}
import edu.ie3.simona.ontology.messages.PowerMessage.AssetPowerChangedMessage
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.util.SimonaConstants
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.jdk.CollectionConverters.ListHasAsScala
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
      services,
      outputConfig,
      Array.emptyLongArray, // Additional activation of the evcs agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution * 10, // FIXME probably need to increase this
        inputModel.getNode
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
      inputModel: EvcsInput,
      modelConfig: EvcsRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EvcsModel = EvcsModel(
    inputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate,
    modelConfig.chargingStrategy
  )

  override protected def createInitialState(): EvcsState =
    EvcsState(Set.empty, Map.empty)

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      tick: Long
  ): EvcsRelevantData = {
    val voltages = baseStateData.voltageValueStore.asMap
      .map { case (tick, voltage) =>
        tick.toDateTime(baseStateData.startDate) -> voltage
      }
    EvcsRelevantData(voltages)
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
        case (_, evcsData: EvMovementData) =>
          handleEvMovementsAndGoIdle(
            currentTick,
            scheduler,
            baseStateData,
            evcsData
          )
        case (_, CurrentPriceRequest) =>
          handleCurrentPriceRequestAndGoIdle(
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

    val evcsModel = getEvcsModel(modelBaseStateData)

    evServiceRef ! FreeLotsResponse(
      evcsModel.uuid,
      evcsModel.chargingPoints - lastState.evs.size
    )
  }

  /** Handle a charging price request. Based on the given type of charging
    * station and the current "state" of the charging station, a price is
    * calculated and send back to the requesting entity.
    *
    * @param currentTick
    *   Current simulation time
    * @param scheduler
    *   Reference to the scheduler
    * @param modelBaseStateData
    *   Base state orientation
    * @return
    *   The next state (idle)
    */
  private def handleCurrentPriceRequestAndGoIdle(
      currentTick: Long,
      scheduler: ActorRef,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val lastState =
      getLastOrInitialStateData(modelBaseStateData, currentTick - 1)

    val evcsModel = getEvcsModel(modelBaseStateData)

    evcsModel.calculateCurrentPriceSignalToCommunicateToEvs(
      currentTick,
      modelBaseStateData.startDate,
      lastState
    ) match {
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
    *
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
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      evcsMovementsData: EvMovementData
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
    /* Prepare needed data */
    val evServiceRef = getService[ActorEvMovementsService](
      modelBaseStateData.services
    )

    val (lastSchedulingTick, lastState) = modelBaseStateData.stateDataStore
      .last(currentTick - 1)
      .getOrElse(
        (SimonaConstants.FIRST_TICK_IN_SIMULATION, createInitialState())
      )

    val evcsModel = getEvcsModel(modelBaseStateData)

    /* Validate EV movements */
    validateEvMovements(
      lastState.evs,
      evcsMovementsData.movements,
      evcsModel.chargingPoints
    )

    /* Relevant data for EvcsModel to calculate new SoC for all EVs */
    val voltage = modelBaseStateData.voltageValueStore
      .last(currentTick)
      .map { case (_, voltage) =>
        voltage
      }
      .getOrElse(Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE))

    /* Update EV models according to the scheduling determined at last EvMovements Event and get EvResults
     * including charging costs */
    val (updatedEvs, evResults, powerEntries) = evcsModel
      .applySchedule(
        currentTick,
        modelBaseStateData.startDate,
        lastSchedulingTick,
        voltage,
        lastState
      )
      .unzip3

    /* Write ev results after charging to csv */
    announceEvResultsToListeners(evResults)

    val (arrivingEvs, stayingEvs, departingEvs) =
      evsByMovementType(updatedEvs, evcsMovementsData.movements)
    /* Return EVs which parking time has ended */
    if (departingEvs.nonEmpty) {
      evServiceRef ! DepartedEvsResponse(
        evcsModel.uuid,
        departingEvs
      )
    }

    /* Calculate evcs power for interval since last update, save for updating value store, and inform listeners */
    val updatedResultValueStore =
      determineResultsAnnounceUpdateValueStore(
        powerEntries.flatten,
        currentTick,
        lastSchedulingTick,
        modelBaseStateData
      )

    val updatedState = {
      /* if new EVs arrived, a new scheduling must be calculated. */
      if (arrivingEvs.nonEmpty) {
        /* Determine new schedule for charging the EVs */
        val currentEvs = stayingEvs ++ arrivingEvs

        val relevantData =
          createCalcRelevantData(modelBaseStateData, currentTick)

        val newSchedule = evcsModel
          .calculateNewScheduling(
            currentTick,
            modelBaseStateData.startDate,
            relevantData,
            currentEvs
          )
        /* Update relevant data with schedule */
        lastState.copy(
          evs = currentEvs,
          schedule = newSchedule
        )
      }
      /* if no new EVs arrived, the previous scheduling is kept but filtered, only the current EVs are updated. */
      else {
        /* Filter out the single schedule entries, which do lay in the past and are already applied */
        val updatedSchedules = lastState.schedule.map {
          case (ev, maybeSchedule) =>
            ev -> maybeSchedule.map(scheduleContainer =>
              scheduleContainer.copy(schedule =
                scheduleContainer.schedule.filter(_.tickStop >= currentTick)
              )
            )
        }

        lastState.copy(
          evs = stayingEvs,
          schedule = updatedSchedules
          // filter(_.tickStop > currentTick)
          // is it possible to remove also the schedules that ended at currentTick? -> probably yes, test required
        )
      }
    }

    goToIdleWithUpdatedBaseStateData(
      scheduler,
      modelBaseStateData,
      updatedResultValueStore,
      updatedState
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
    * @param state
    *   The current state data
    * @return
    *   Desired state change
    */
  final def goToIdleWithUpdatedBaseStateData(
      scheduler: ActorRef,
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ],
      updatedResultValueStore: ValueStore[ApparentPower],
      state: EvcsState
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {

    /* Update relevant data store */
    val updatedStateDataStore =
      ValueStore.updateValueStore(
        baseStateData.stateDataStore,
        currentTick,
        state
      )

    /* Update the base state data with the updated result value store and relevant data store */
    val updatedBaseStateData =
      baseStateData.copy(
        resultValueStore = updatedResultValueStore,
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
              /* Relevant data for EvcsModel to calculate new SoC for all EVs */
              val voltage = modelBaseStateData.voltageValueStore
                .last(requestTick)
                .map { case (_, voltage) =>
                  voltage
                }
                .getOrElse(
                  Quantities.getQuantity(1d, StandardUnits.VOLTAGE_MAGNITUDE)
                )

              val (lastSchedulingTick, lastState) =
                modelBaseStateData.stateDataStore
                  .last(requestTick - 1)
                  .getOrElse(
                    (
                      SimonaConstants.FIRST_TICK_IN_SIMULATION,
                      createInitialState()
                    )
                  )

              val (_, _, powerEntries) =
                modelBaseStateData.model
                  .applySchedule(
                    requestTick,
                    modelBaseStateData.startDate,
                    lastSchedulingTick,
                    voltage,
                    lastState
                  )
                  .unzip3

              val updatedResultValueStore =
                determineResultsAnnounceUpdateValueStore(
                  powerEntries.flatten,
                  requestTick,
                  lastSchedulingTick,
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

  /** Determine evcs results according to DEVS logic (in each tick, where
    * something relevant happens), announce them to listeners and update the
    * result value store.
    *
    * @param powerEntries
    *   Set of [[PowerEntry]]s determined, while applying the schedule to each
    *   ev
    * @param requestTick
    *   Tick, where the result is requested
    * @param lastSchedulingTick
    *   Last tick, when a schedule has been applied
    * @param modelBaseStateData
    *   Model base state data
    * @return
    *   The updated result value store
    */
  private def determineResultsAnnounceUpdateValueStore(
      powerEntries: Set[PowerEntry],
      requestTick: Long,
      lastSchedulingTick: Long,
      modelBaseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsState,
        EvcsModel
      ]
  ): ValueStore[ApparentPower] = {
    /* Power updates are already in the resultValueStore until the tick of the last event. Determine this tick to
     * know from where on new updates need to be written into the value store. This tick is either from the last update
     * of the calcRelevantDataStore (= EvMovements Event) or requestValueStore (= Power Request).
     */
    val lastUpdateTick = math.max(
      lastSchedulingTick,
      modelBaseStateData.requestValueStore
        .lastKnownTick(requestTick - 1)
        .getOrElse(0L)
    )

    /* Get applicable power values */
    val filteredPowerEntries = powerEntries.filter {
      case PowerEntry(start, end, _) =>
        end >= lastUpdateTick && start <= requestTick
    }

    val tickToApparentPower =
      filteredPowerEntries.map(_.start).toSeq.distinct.sorted.map { tick =>
        /* An power entry might start before the actual time frame of interest, thereby correct this */
        val powerTick = math.max(tick, lastUpdateTick)
        val power = filteredPowerEntries
          .filter { case PowerEntry(start, end, _) =>
            tick >= start && tick < end
          }
          .map { case PowerEntry(_, _, power) =>
            (power.p, power.q)
          }
          .foldLeft(
            Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
            Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT)
          ) { case ((p, q), (currentP, currentQ)) =>
            (p.add(currentP), q.add(currentQ))
          } match {
          case (p, q) => ApparentPower(p, q)
        }
        powerTick -> power
      }

    tickToApparentPower.foldLeft(modelBaseStateData.resultValueStore) {
      case (resultValueStore, (tick, apparentPower)) =>
        /* Inform the listeners about new result */
        announceSimulationResult(
          modelBaseStateData,
          tick,
          apparentPower
        )(modelBaseStateData.outputConfig)
        /* Update resultValueStore with result */
        ValueStore.updateValueStore(
          resultValueStore,
          tick,
          apparentPower
        )
    }
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

  /** Determine, which cars arrive, stay or depart
    *
    * @param evs
    *   Collection of all apparent evs
    * @param movements
    *   Collection of all movements to be applied
    * @return
    *   Sets of arriving, staying and departing cars
    */
  private def evsByMovementType(
      evs: Set[EvModel],
      movements: EvcsMovements
  ): (Set[EvModel], Set[EvModel], Set[EvModel]) = {
    val (departing, staying) = evs.partition { ev =>
      movements.getDepartures.contains(ev.getUuid)
    }
    val arriving =
      movements.getArrivals.asScala.filter { ev =>
        !evs.exists(_.getUuid == ev.getUuid)
      }.toSet
    (arriving, staying, departing)
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
