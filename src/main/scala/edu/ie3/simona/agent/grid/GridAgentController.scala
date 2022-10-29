/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorContext, ActorRef}
import akka.event.LoggingAdapter
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.container.{
  SubGridContainer,
  SystemParticipants
}
import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.actor.SimonaActorNaming._
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.{
  ActorEvMovementsService,
  ActorWeatherService
}
import edu.ie3.simona.agent.participant.em.EmAgent
import edu.ie3.simona.agent.participant.em.EmAgent.EmAgentInitializeStateData
import edu.ie3.simona.agent.participant.evcs.EvcsAgent
import edu.ie3.simona.agent.participant.fixedfeedin.FixedFeedInAgent
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.load.LoadAgent
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.InitializeStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.wec.WecAgent
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig._
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleTriggerMessage
import edu.ie3.simona.ontology.trigger.Trigger.InitializeParticipantAgentTrigger
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.ConfigUtil._
import edu.ie3.simona.agent.participant.storage.StorageAgent

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._

/** Holds all methods that should be available to a [[GridAgent]]
  *
  * @param gridAgentContext
  *   ActorContext of the grid agent
  * @param environmentRefs
  *   The environment refs
  * @param simulationStartDate
  *   Simulation start date
  * @param simulationEndDate
  *   Simulation end date
  * @param participantsConfig
  *   Configuration information for participant models
  * @param outputConfig
  *   Configuration information for output behaviour
  * @param resolution
  *   The agents time resolution it wants to be triggered
  * @param listener
  *   System participant listeners
  * @param log
  *   The logging adapter to use here
  *
  * @since 2019-07-18
  */
class GridAgentController(
    gridAgentContext: ActorContext,
    environmentRefs: EnvironmentRefs,
    simulationStartDate: ZonedDateTime,
    simulationEndDate: ZonedDateTime,
    participantsConfig: SimonaConfig.Simona.Runtime.Participant,
    outputConfig: SimonaConfig.Simona.Output.Participant,
    resolution: Long,
    listener: Iterable[ActorRef],
    log: LoggingAdapter
) extends LazyLogging {
  def buildSystemParticipants(
      subGridContainer: SubGridContainer,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid]
  ): Map[UUID, Set[ActorRef]] = {

    val systemParticipants =
      filterSysParts(subGridContainer, environmentRefs)

    /* Browse through all system participants, build actors and map their node's UUID to the actor references */
    buildParticipantToActorRef(
      participantsConfig,
      outputConfig,
      subGridContainer.getSystemParticipants,
      systemParticipants,
      thermalIslandGridsByBusId,
      environmentRefs
    )
  }

  /** Takes the provided [[SubGridContainer]] and removes all
    * [[SystemParticipantInput]] of which no agent implementations are available
    * at the moment or which are connected to some EM system. This method needs
    * to be adapted whenever a new agent implementation is ready.
    *
    * To disable a filter for a specific system participant, adapt the code
    * below.
    *
    * @param subGridContainer
    *   the original subGrid container
    * @return
    *   a filtered subGrid container w/o assets for which no agent
    *   implementations exist atm
    */
  private def filterSysParts(
      subGridContainer: SubGridContainer,
      environmentRefs: EnvironmentRefs
  ) = {

    val emParticipants =
      subGridContainer.getSystemParticipants.getEmSystems.asScala
        .flatMap(_.getConnectedAssets)

    val (notProcessedElements, availableSysParts) =
      subGridContainer.getSystemParticipants
        .allEntitiesAsList()
        .asScala
        .foldLeft((Set.empty[String], Vector.empty[SystemParticipantInput])) {
          case (
                (notProcessedElements, availableSystemParticipants),
                curSysPart
              ) =>
            curSysPart match {
              case entity @ (_: BmInput | _: ChpInput | _: EvInput) =>
                (
                  notProcessedElements + entity.getClass.getSimpleName,
                  availableSystemParticipants
                )
              // only include evcs if ev data service is present
              case evcsInput: EvcsInput
                  if environmentRefs.evDataService.isEmpty =>
                log.warning(
                  s"Evcs ${evcsInput.getId} has been removed because no ev movements service is present."
                )
                (notProcessedElements, availableSystemParticipants)
              case entity if emParticipants.contains(entity.getUuid) =>
                log.debug(
                  s"System participant {} is part of an energy-managed system and thus not directly connected to the grid.",
                  entity
                )
                (notProcessedElements, availableSystemParticipants)
              case entity =>
                (notProcessedElements, availableSystemParticipants :+ entity)
            }
        }

    if (notProcessedElements.nonEmpty)
      log.warning(
        s"The following elements have been removed, " +
          s"as the agents are not implemented yet: $notProcessedElements"
      )

    availableSysParts

  }

  /** Go through all provided input models, build agents for those and group the
    * resulting actor references for each connection nodes. All participant
    * agents are also introduced to the agent environment and the scheduler is
    * requested to send a initialisation trigger.
    *
    * @param participantsConfig
    *   Configuration information for participant models
    * @param outputConfig
    *   Configuration information for output behaviour
    * @param filteredParticipants
    *   Set of system participants to create agents for
    * @param thermalIslandGridsByBusId
    *   Collection of thermal island grids, mapped by their thermal bus uuid
    * @param environmentRefs
    *   References to singleton entities representing the agent environment
    * @return
    *   A map from coupling point to set of actor references
    */
  private def buildParticipantToActorRef(
      participantsConfig: SimonaConfig.Simona.Runtime.Participant,
      outputConfig: SimonaConfig.Simona.Output.Participant,
      allParticipants: SystemParticipants,
      filteredParticipants: Vector[SystemParticipantInput],
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      environmentRefs: EnvironmentRefs
  ): Map[UUID, Set[ActorRef]] = {
    /* Prepare the config util for the participant models, which (possibly) utilizes as map to speed up the initialization
     * phase */
    val participantConfigUtil =
      ConfigUtil.ParticipantConfigUtil(participantsConfig)
    val outputConfigUtil = ConfigUtil.OutputConfigUtil(outputConfig)

    val emParticipantsUuids =
      allParticipants.getEmSystems.asScala
        .flatMap(_.getConnectedAssets)

    val emParticipantMap = allParticipants
      .allEntitiesAsList()
      .asScala
      .filter(sp => emParticipantsUuids.contains(sp.getUuid))
      .map(sp => sp.getUuid -> sp)
      .toMap

    filteredParticipants
      .map(participant => {
        val node = participant.getNode
        val (actorRef, initStateData) =
          // build
          buildParticipantActor(
            participantsConfig.requestVoltageDeviationThreshold,
            participantConfigUtil,
            outputConfigUtil,
            participant,
            thermalIslandGridsByBusId,
            environmentRefs,
            emParticipantMap,
            None // FIXME
          ) // introduce to environment
        introduceAgentToEnvironment(
          actorRef,
          InitializeParticipantAgentTrigger[PrimaryData, InitializeStateData[
            PrimaryData
          ]](initStateData)
        )
        // return uuid to actorRef
        node.getUuid -> actorRef
      })
      .toSet[(UUID, ActorRef)]
      .groupMap(entry => entry._1)(entry => entry._2)
  }

  private def buildParticipantActor(
      requestVoltageDeviationThreshold: Double,
      participantConfigUtil: ConfigUtil.ParticipantConfigUtil,
      outputConfigUtil: OutputConfigUtil,
      participantInputModel: SystemParticipantInput,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      environmentRefs: EnvironmentRefs,
      emParticipantMap: Map[UUID, SystemParticipantInput],
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      InitializeStateData[_ <: PrimaryData]
  ) = participantInputModel match {
    case input: FixedFeedInInput =>
      buildFixedFeedIn(
        input,
        participantConfigUtil.getOrDefault[FixedFeedInRuntimeConfig](
          input.getUuid
        ),
        environmentRefs.primaryServiceProxy,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfigUtil.getOrDefault(NotifierIdentifier.FixedFeedIn),
        maybeEmAgent
      )
    case input: LoadInput =>
      buildLoad(
        input,
        participantConfigUtil.getOrDefault[LoadRuntimeConfig](
          input.getUuid
        ),
        environmentRefs.primaryServiceProxy,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfigUtil.getOrDefault(NotifierIdentifier.Load),
        maybeEmAgent
      )
    case input: PvInput =>
      buildPv(
        input,
        participantConfigUtil.getOrDefault[PvRuntimeConfig](
          input.getUuid
        ),
        environmentRefs.primaryServiceProxy,
        environmentRefs.weather,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfigUtil.getOrDefault(NotifierIdentifier.PvPlant),
        maybeEmAgent
      )
    case input: WecInput =>
      buildWec(
        input,
        participantConfigUtil.getOrDefault[WecRuntimeConfig](
          input.getUuid
        ),
        environmentRefs.primaryServiceProxy,
        environmentRefs.weather,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfigUtil.getOrDefault(NotifierIdentifier.Wec),
        maybeEmAgent
      )
    case input: EvcsInput =>
      buildEvcs(
        input,
        participantConfigUtil.getOrDefault[EvcsRuntimeConfig](
          input.getUuid
        ),
        environmentRefs.primaryServiceProxy,
        environmentRefs.evDataService.getOrElse(
          throw new GridAgentInitializationException(
            "EvMovementsService required for setting up evcs."
          )
        ),
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfigUtil.getOrDefault(NotifierIdentifier.Evcs),
        maybeEmAgent
      )

    case hpInput: HpInput =>
      thermalIslandGridsByBusId.get(hpInput.getThermalBus.getUuid) match {
        case Some(thermalGrid) =>
          buildHp(
            hpInput,
            thermalGrid,
            participantConfigUtil.getOrDefault[HpRuntimeConfig](
              hpInput.getUuid
            ),
            environmentRefs.primaryServiceProxy,
            environmentRefs.weather,
            requestVoltageDeviationThreshold,
            outputConfigUtil.getOrDefault(NotifierIdentifier.Hp),
            maybeEmAgent
          )
        case None =>
          throw new GridAgentInitializationException(
            s"Unable to find thermal island grid for heat pump '${hpInput.getUuid}' with thermal bus '${hpInput.getThermalBus.getUuid}'."
          )
      }

    case emInput: EmInput =>
      buildEm(
        emInput,
        participantConfigUtil.getOrDefault[EmRuntimeConfig](emInput.getUuid),
        environmentRefs.primaryServiceProxy,
        environmentRefs.weather,
        requestVoltageDeviationThreshold,
        outputConfigUtil.getOrDefault(NotifierIdentifier.Em),
        emParticipantMap,
        participantConfigUtil,
        outputConfigUtil,
        thermalIslandGridsByBusId
      )

    case input: StorageInput =>
      buildStorage(
        input,
        participantConfigUtil.getOrDefault[StorageRuntimeConfig](
          input.getUuid
        ),
        environmentRefs.primaryServiceProxy,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfigUtil.getOrDefault(NotifierIdentifier.Storage),
        maybeEmAgent
      )

    case input: SystemParticipantInput =>
      throw new NotImplementedError(
        s"Building ${input.getClass.getSimpleName} is not implemented, yet."
      )
    case unknown =>
      throw new GridAgentInitializationException(
        "Received unknown input model type " + unknown.toString + "."
      )
  }

  /** Creates a fixed feed in agent and determines the needed additional
    * information for later initialization of the agent.
    *
    * @param fixedFeedInInput
    *   Fixed Feed In input model to derive information from
    * @param modelConfiguration
    *   User-provided configuration for this specific fixed feed in model
    * @param primaryServiceProxy
    *   Reference to the primary data service proxy
    * @param simulationStartDate
    *   First wall clock time in simulation
    * @param simulationEndDate
    *   Last wall clock time in simulation
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @return
    *   A pair of [[FixedFeedInAgent]] 's [[ActorRef]] as well as the equivalent
    *   [[InitializeParticipantAgentTrigger]] to sent for initialization
    */
  private def buildFixedFeedIn(
      fixedFeedInInput: FixedFeedInInput,
      modelConfiguration: FixedFeedInRuntimeConfig,
      primaryServiceProxy: ActorRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      ParticipantInitializeStateData[
        FixedFeedInInput,
        SimonaConfig.FixedFeedInRuntimeConfig,
        ApparentPower
      ]
  ) = (
    gridAgentContext.simonaActorOf(
      FixedFeedInAgent.props(
        maybeEmAgent.getOrElse(environmentRefs.scheduler),
        listener // TODO this needs to be a param
      ),
      fixedFeedInInput.getId
    ),
    ParticipantInitializeStateData(
      fixedFeedInInput,
      modelConfiguration,
      primaryServiceProxy,
      None,
      simulationStartDate,
      simulationEndDate,
      resolution,
      requestVoltageDeviationThreshold,
      outputConfig,
      maybeEmAgent
    )
  )

  /** Creates a load agent and determines the needed additional information for
    * later initialization of the agent.
    *
    * @param loadInput
    *   Load input model to derive information from
    * @param modelConfiguration
    *   User-provided configuration for this specific load model
    * @param primaryServiceProxy
    *   Reference to the primary data service proxy
    * @param simulationStartDate
    *   First wall clock time in simulation
    * @param simulationEndDate
    *   Last wall clock time in simulation
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @return
    *   A pair of [[FixedFeedInAgent]] 's [[ActorRef]] as well as the equivalent
    * @return
    *   A pair of [[LoadAgent]] 's [[ActorRef]] as well as the equivalent
    *   [[InitializeParticipantAgentTrigger]] to sent for initialization
    */
  private def buildLoad(
      loadInput: LoadInput,
      modelConfiguration: LoadRuntimeConfig,
      primaryServiceProxy: ActorRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      ParticipantInitializeStateData[
        LoadInput,
        SimonaConfig.LoadRuntimeConfig,
        ApparentPower
      ]
  ) = (
    gridAgentContext.simonaActorOf(
      LoadAgent.props(
        maybeEmAgent.getOrElse(environmentRefs.scheduler),
        listener,
        modelConfiguration
      ),
      loadInput.getId
    ),
    ParticipantInitializeStateData(
      loadInput,
      modelConfiguration,
      primaryServiceProxy,
      None,
      simulationStartDate,
      simulationEndDate,
      resolution,
      requestVoltageDeviationThreshold,
      outputConfig,
      maybeEmAgent
    )
  )

  /** Creates a pv agent and determines the needed additional information for
    * later initialization of the agent.
    *
    * @param pvInput
    *   Pv input model to derive information from
    * @param modelConfiguration
    *   User-provided configuration for this specific load model
    * @param primaryServiceProxy
    *   Reference to the primary data service proxy
    * @param weatherService
    *   Reference to the weather service actor
    * @param simulationStartDate
    *   First wall clock time in simulation
    * @param simulationEndDate
    *   Last wall clock time in simulation
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @return
    *   A pair of [[PvAgent]] 's [[ActorRef]] as well as the equivalent
    *   [[InitializeParticipantAgentTrigger]] to sent for initialization
    */
  private def buildPv(
      pvInput: PvInput,
      modelConfiguration: PvRuntimeConfig,
      primaryServiceProxy: ActorRef,
      weatherService: ActorRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      ParticipantInitializeStateData[
        PvInput,
        SimonaConfig.PvRuntimeConfig,
        ApparentPower
      ]
  ) =
    (
      gridAgentContext.simonaActorOf(
        PvAgent.props(
          maybeEmAgent.getOrElse(environmentRefs.scheduler),
          listener
        ),
        pvInput.getId
      ),
      ParticipantInitializeStateData(
        pvInput,
        modelConfiguration,
        primaryServiceProxy,
        Some(Vector(ActorWeatherService(weatherService))),
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent
      )
    )

  /** Creates an Evcs agent and determines the needed additional information for
    * later initialization of the agent.
    *
    * @param evcsInput
    *   Evcs input model to derive information from
    * @param modelConfiguration
    *   User-provided configuration for this specific load model
    * @param primaryServiceProxy
    *   Reference to the primary data service proxy
    * @param evMovementsService
    *   Reference to the ev movements service actor
    * @param simulationStartDate
    *   First wall clock time in simulation
    * @param simulationEndDate
    *   Last wall clock time in simulation
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @return
    *   A pair of [[EvcsAgent]] 's [[ActorRef]] as well as the equivalent
    *   [[InitializeParticipantAgentTrigger]] to sent for initialization
    */
  private def buildEvcs(
      evcsInput: EvcsInput,
      modelConfiguration: EvcsRuntimeConfig,
      primaryServiceProxy: ActorRef,
      evMovementsService: ActorRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      ParticipantInitializeStateData[
        EvcsInput,
        EvcsRuntimeConfig,
        ApparentPower
      ]
  ) = {
    val sources = Some(
      Vector(
        ActorEvMovementsService(
          evMovementsService
        )
      )
    )

    (
      gridAgentContext.simonaActorOf(
        EvcsAgent.props(
          maybeEmAgent.getOrElse(environmentRefs.scheduler),
          listener
        ),
        s"EvcsAgent_${evcsInput.getUuid}"
      ),
      ParticipantInitializeStateData(
        evcsInput,
        modelConfiguration,
        primaryServiceProxy,
        sources,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent
      )
    )
  }

  /** Builds an [[HpAgent]] from given input
    * @param hpInput
    *   Input model
    * @param thermalGrid
    *   The thermal grid, that this heat pump is ought to handle
    * @param modelConfiguration
    *   Runtime configuration for the agent
    * @param primaryServiceProxy
    *   Proxy actor reference for primary data
    * @param weatherService
    *   Actor reference for weather service
    * @param requestVoltageDeviationThreshold
    *   Permissible voltage magnitude deviation to consider being equal
    * @param outputConfig
    *   Configuration for output notification
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @return
    *   A tuple of actor reference and [[ParticipantInitializeStateData]]
    */
  private def buildHp(
      hpInput: HpInput,
      thermalGrid: ThermalGrid,
      modelConfiguration: HpRuntimeConfig,
      primaryServiceProxy: ActorRef,
      weatherService: ActorRef,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      ParticipantInitializeStateData[
        HpInput,
        HpRuntimeConfig,
        ApparentPowerAndHeat
      ]
  ) = (
    gridAgentContext.simonaActorOf(
      HpAgent.props(
        environmentRefs.scheduler,
        listener
      ),
      hpInput.getId
    ),
    ParticipantInitializeStateData(
      hpInput,
      thermalGrid,
      modelConfiguration,
      primaryServiceProxy,
      Some(Vector(ActorWeatherService(weatherService))),
      simulationStartDate,
      simulationEndDate,
      resolution,
      requestVoltageDeviationThreshold,
      outputConfig,
      maybeEmAgent
    )
  )

  /** Creates a wec agent and determines the needed additional information for
    * later initialization of the agent.
    *
    * @param wecInput
    *   WEC input model to derive information from
    * @param modelConfiguration
    *   User-provided configuration for this specific wec model
    * @param primaryServiceProxy
    *   Reference to the primary data service proxy
    * @param weatherService
    *   Reference to the weather service actor
    * @param simulationStartDate
    *   First wall clock time in simulation
    * @param simulationEndDate
    *   Last wall clock time in simulation
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @return
    *   A pair of [[WecAgent]] 's [[ActorRef]] as well as the equivalent
    *   [[InitializeParticipantAgentTrigger]] to sent for initialization
    */
  private def buildWec(
      wecInput: WecInput,
      modelConfiguration: WecRuntimeConfig,
      primaryServiceProxy: ActorRef,
      weatherService: ActorRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      ParticipantInitializeStateData[
        WecInput,
        SimonaConfig.WecRuntimeConfig,
        ApparentPower
      ]
  ) =
    (
      gridAgentContext.simonaActorOf(
        WecAgent.props(
          maybeEmAgent.getOrElse(environmentRefs.scheduler),
          listener
        ),
        wecInput.getId
      ),
      ParticipantInitializeStateData(
        wecInput,
        modelConfiguration,
        primaryServiceProxy,
        Some(Vector(ActorWeatherService(weatherService))),
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent
      )
    )

  /** Creates a storage agent and determines the needed additional information
    * for later initialization of the agent.
    *
    * @param storageInput
    *   Storage input model to derive information from
    * @param modelConfiguration
    *   User-provided configuration for this specific storage model
    * @param primaryServiceProxy
    *   Reference to the primary data service proxy
    * @param simulationStartDate
    *   First wall clock time in simulation
    * @param simulationEndDate
    *   Last wall clock time in simulation
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @return
    *   A pair of [[StorageAgent]] 's [[ActorRef]] as well as the equivalent
    *   [[InitializeParticipantAgentTrigger]] to sent for initialization
    */
  private def buildStorage(
      storageInput: StorageInput,
      modelConfiguration: SimonaConfig.StorageRuntimeConfig,
      primaryServiceProxy: ActorRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): (
      ActorRef,
      ParticipantInitializeStateData[
        StorageInput,
        SimonaConfig.StorageRuntimeConfig,
        ApparentPower
      ]
  ) =
    (
      gridAgentContext.simonaActorOf(
        StorageAgent.props(
          maybeEmAgent.getOrElse(environmentRefs.scheduler),
          listener
        ),
        storageInput.getId
      ),
      ParticipantInitializeStateData(
        storageInput,
        modelConfiguration,
        primaryServiceProxy,
        None,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent
      )
    )

  /** Builds an [[EmAgent]] from given input
    *
    * @param emInput
    *   Input model
    * @param modelConfiguration
    *   Runtime configuration for the agent
    * @param primaryServiceProxy
    *   Proxy actor reference for primary data
    * @param weatherService
    *   Actor reference for weather service
    * @param requestVoltageDeviationThreshold
    *   Permissible voltage magnitude deviation to consider being equal
    * @param outputConfig
    *   Configuration for output notification
    * @param emParticipantMap
    *   Map from UUID to all participants that are connected to some EM system
    * @return
    *   A tuple of actor reference and [[ParticipantInitializeStateData]]
    */
  private def buildEm(
      emInput: EmInput,
      modelConfiguration: EmRuntimeConfig,
      primaryServiceProxy: ActorRef,
      weatherService: ActorRef,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      emParticipantMap: Map[UUID, SystemParticipantInput],
      participantConfigUtil: ConfigUtil.ParticipantConfigUtil,
      outputConfigUtil: OutputConfigUtil,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid]
  ): (
      ActorRef,
      EmAgentInitializeStateData
  ) = {
    val emAgentRef = gridAgentContext.simonaActorOf(
      EmAgent.props(
        environmentRefs.scheduler,
        listener
      ),
      emInput.getId
    )

    val connectedAgents = emInput.getConnectedAssets
      .map { uuid =>
        emParticipantMap.getOrElse(
          uuid,
          throw new GridAgentInitializationException(
            s"Agent with UUID $uuid connected to $emInput could not be found."
          )
        )
      }
      .map { sp =>
        val (actorRef, initData) = buildParticipantActor(
          requestVoltageDeviationThreshold,
          participantConfigUtil,
          outputConfigUtil,
          sp,
          thermalIslandGridsByBusId,
          environmentRefs,
          emParticipantMap,
          Some(emAgentRef)
        )

        val initTrigger =
          InitializeParticipantAgentTrigger[PrimaryData, InitializeStateData[
            PrimaryData
          ]](
            initData
          )

        (actorRef, initTrigger, sp)
      }
      .toSeq

    (
      emAgentRef,
      EmAgentInitializeStateData(
        emInput,
        modelConfiguration,
        primaryServiceProxy,
        Some(Vector(ActorWeatherService(weatherService))),
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        connectedAgents
      )
    )
  }

  /** Introduces the given agent to the agent environment and additionally sends
    * an [[InitializeParticipantAgentTrigger]] to this agent to start its
    * initialization
    *
    * @param actorRef
    *   Reference to the actor to add to the environment
    * @param initTrigger
    *   Trigger to start initialization
    */
  private def introduceAgentToEnvironment(
      actorRef: ActorRef,
      initTrigger: InitializeParticipantAgentTrigger[
        _ <: PrimaryData,
        _ <: InitializeStateData[_]
      ]
  ): Unit = {
    gridAgentContext.watch(actorRef)
    environmentRefs.scheduler ! ScheduleTriggerMessage(
      initTrigger,
      actorRef
    )
  }

}
