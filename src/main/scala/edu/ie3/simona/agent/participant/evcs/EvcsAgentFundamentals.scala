/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.evcs

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.result.system.{
  EvcsResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data
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
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology.EvMovementsMessage.EvcsMovements
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
  DepartedEvsResponse,
  EvMovementData,
  FreeLotsResponse
}
import edu.ie3.simona.service.ev.ExtEvDataService.FALLBACK_EV_MOVEMENTS_STEM_DISTANCE
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
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

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
        EvcsModel
      ],
      tick: Long,
      secondaryData: Map[ActorRef, Option[_ <: Data]]
  ): EvcsRelevantData = {
    // TODO implement

    throw new NotImplementedError()
  }

  override protected def calculateResult(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        EvcsRelevantData,
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
        /* extract EV data from secondary data, which should have been requested and received before */
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

  /** Handles a evcs movements message that contains information on arriving and
    * departing vehicles. After applying the movements to the last known set of
    * parked evs, calculates resulting charging power. Sends completion message
    * to scheduler without scheduling new activations.
    * @param currentTick
    *   The current tick that has been triggered
    * @param scheduler
    *   The scheduler ref
    * @param modelBaseStateData
    *   The state data
    * @param evcsMovementsData
    *   The movement data that has been received
    * @return
    *   [[Idle]] with updated result values
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

    val (tickInterval, lastEvs) =
      getTickIntervalAndLastEvs(currentTick, modelBaseStateData)

    val evcsModel = getEvcsModel(modelBaseStateData)

    val voltage =
      getAndCheckNodalVoltage(modelBaseStateData, currentTick)

    validateEvMovements(
      lastEvs,
      evcsMovementsData.movements,
      evcsModel.chargingPoints
    )

    // calculate power with evs that have been parked up until now
    val relevantData =
      EvcsRelevantData(
        tickInterval,
        lastEvs
      )

    val (power, evModelsCalculated) =
      evcsModel.calculatePowerAndEvSoc(
        currentTick,
        voltage,
        relevantData
      )

    {
      val departedEvs = calculateDepartedEvs(
        evModelsCalculated,
        evcsMovementsData.movements
      )
      if (departedEvs.nonEmpty) {
        evServiceRef ! DepartedEvsResponse(
          evcsModel.uuid,
          departedEvs
        )
      }
    }

    val stayingEvs = calculateStayingEvs(
      lastEvs,
      evcsMovementsData.movements
    )
    val updatedRelevantData = relevantData.copy(
      currentEvs = stayingEvs
    )

    updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(
      scheduler,
      modelBaseStateData,
      power,
      updatedRelevantData
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
      .last(currentTick - 1) match {
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

  private def calculateDepartedEvs(
      calculatedEvs: Set[EvModel],
      evcsMovements: EvcsMovements
  ): Set[EvModel] =
    calculatedEvs.filter { ev =>
      // EV has been parked up until now and has now departed
      evcsMovements.getDepartures.contains(ev.getUuid)
    }

  private def calculateStayingEvs(
      calculatedEvs: Set[EvModel],
      evcsMovements: EvcsMovements
  ): Set[EvModel] =
    // If we say that a car departs at t, it means that it stays parked up to and including t.
    calculatedEvs.filter { ev =>
      // Evs that have been parked here and have not departed
      !evcsMovements.getDepartures.contains(ev.getUuid)
    } ++ // new evs
      evcsMovements.getArrivals.asScala.filter { ev =>
        !calculatedEvs.exists(_.getUuid == ev.getUuid)
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
