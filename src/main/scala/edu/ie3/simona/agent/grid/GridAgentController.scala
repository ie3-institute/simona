/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.input.EmInput
import edu.ie3.datamodel.models.input.container.{SubGridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.actor.SimonaActorNaming._
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.{
  ActorExtEvDataService,
  ActorWeatherService,
}
import edu.ie3.simona.agent.participant.evcs.EvcsAgent
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.storage.StorageAgent
import edu.ie3.simona.agent.participant.wec.WecAgent
import edu.ie3.simona.agent.participant2.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.agent.participant2.{
  ParticipantAgent,
  ParticipantAgentInit,
}
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.OutputConfig.ParticipantOutputConfig
import edu.ie3.simona.config.SimonaConfig.AssetConfigs
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexResponse
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.ConfigUtil._
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.slf4j.Logger
import squants.Each

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters.RichOptional

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
  * @since 2019-07-18
  */
class GridAgentController(
    gridAgentContext: ActorContext[GridAgent.Request],
    environmentRefs: EnvironmentRefs,
    simulationStartDate: ZonedDateTime,
    simulationEndDate: ZonedDateTime,
    participantsConfig: Participant,
    outputConfig: AssetConfigs[ParticipantOutputConfig],
    resolution: Long,
    listener: Iterable[ActorRef[ResultEvent]],
    log: Logger,
) {
  def buildSystemParticipants(
      subGridContainer: SubGridContainer,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
  ): Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] = {

    val systemParticipants =
      filterSysParts(subGridContainer, environmentRefs)

    /* Browse through all system participants, build actors and map their node's UUID to the actor references */
    buildParticipantToActorRef(
      participantsConfig,
      outputConfig,
      systemParticipants,
      thermalIslandGridsByBusId,
      environmentRefs,
    )
  }

  /** Takes the provided [[SubGridContainer]] and removes all
    * [[SystemParticipantInput]] of which no agent implementations are available
    * at the moment. This method needs to be adapted whenever a new agent
    * implementation is ready.
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
      environmentRefs: EnvironmentRefs,
  ) = {

    val (notProcessedElements, availableSysParts) =
      subGridContainer.getSystemParticipants
        .allEntitiesAsList()
        .asScala
        .foldLeft((Set.empty[String], Vector.empty[SystemParticipantInput])) {
          case (
                (notProcessedElements, availableSystemParticipants),
                curSysPart,
              ) =>
            curSysPart match {
              case entity @ (_: BmInput | _: ChpInput | _: EvInput) =>
                (
                  notProcessedElements + entity.getClass.getSimpleName,
                  availableSystemParticipants,
                )
              // only include evcs if ev data service is present
              case evcsInput: EvcsInput
                  if environmentRefs.evDataService.isEmpty =>
                log.warn(
                  s"Evcs ${evcsInput.getId} has been removed because no ev movements service is present."
                )
                (notProcessedElements, availableSystemParticipants)
              case entity =>
                (notProcessedElements, availableSystemParticipants :+ entity)
            }
        }

    if (notProcessedElements.nonEmpty)
      log.warn(
        s"The following elements have been removed, " +
          s"as the agents are not implemented yet: $notProcessedElements"
      )

    availableSysParts

  }

  /** Go through all provided input models, build agents for those and group the
    * resulting actor references for each connection nodes. All participant
    * agents are also introduced to the agent environment and the scheduler is
    * requested to send an initialisation trigger.
    *
    * @param participantsConfig
    *   Configuration information for participant models
    * @param outputConfig
    *   Configuration information for output behaviour
    * @param participants
    *   Set of system participants to create agents for
    * @param thermalIslandGridsByBusId
    *   Collection of thermal island grids, mapped by their thermal bus uuid
    * @param environmentRefs
    *   References to singleton entities representing the agent environment
    * @return
    *   A map from coupling point to set of actor references
    */
  private def buildParticipantToActorRef(
      participantsConfig: Participant,
      outputConfig: AssetConfigs[ParticipantOutputConfig],
      participants: Vector[SystemParticipantInput],
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      environmentRefs: EnvironmentRefs,
  ): Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] = {
    /* Prepare the config util for the participant models, which (possibly) utilizes as map to speed up the initialization
     * phase */
    val participantConfigUtil =
      ConfigUtil.ParticipantConfigUtil(participantsConfig)
    val outputConfigUtil =
      ConfigUtil.OutputConfigUtil.participants(outputConfig)

    // ems that control at least one participant directly
    val firstLevelEms = participants.flatMap {
      _.getControllingEm.toScala.map(em => em.getUuid -> em)
    }.toMap

    val allEms = buildEmsRecursively(
      participantConfigUtil,
      outputConfigUtil,
      firstLevelEms,
    )

    participants
      .map { participant =>
        val node = participant.getNode

        val controllingEm =
          participant.getControllingEm.toScala
            .map(_.getUuid)
            .map(uuid =>
              allEms.getOrElse(
                uuid,
                throw new CriticalFailureException(
                  s"EM actor with UUID $uuid not found."
                ),
              )
            )

        val actorRef = buildParticipantActor(
          participantsConfig.requestVoltageDeviationThreshold,
          participantConfigUtil,
          outputConfigUtil,
          participant,
          thermalIslandGridsByBusId,
          environmentRefs,
          controllingEm,
        )
        // return uuid to actorRef
        node.getUuid -> actorRef
      }
      .toSet[(UUID, ActorRef[ParticipantAgent.Request])]
      .groupMap(entry => entry._1)(entry => entry._2)
  }

  /** Recursively builds the [[EmAgent]] structure. Recursion starts with
    * first-level EMs (controlling at least one system participant), and works
    * its way up to EMs at root level, which are not EM-controlled themselves.
    * The first level can also be root level.
    *
    * @param participantConfigUtil
    *   Configuration util for participant models
    * @param outputConfigUtil
    *   Configuration util for output behaviour
    * @param emInputs
    *   EMs of the current level, which can be controlled by further EMs at
    *   higher levels
    * @param previousLevelEms
    *   EMs that have been built by the previous recursion level
    * @return
    *   Map from model UUID to EmAgent ActorRef
    */
  private def buildEmsRecursively(
      participantConfigUtil: ConfigUtil.ParticipantConfigUtil,
      outputConfigUtil: OutputConfigUtil,
      emInputs: Map[UUID, EmInput],
      previousLevelEms: Map[UUID, ActorRef[FlexResponse]] = Map.empty,
  ): Map[UUID, ActorRef[FlexResponse]] = {
    // For the current level, split controlled and uncontrolled EMs.
    // Uncontrolled EMs can be built right away.
    val (controlledEmInputs, uncontrolledEms) = emInputs
      .partitionMap { case (uuid, emInput) =>
        if (emInput.getControllingEm.isPresent)
          Left(uuid -> emInput)
        else {
          val actor = buildEm(
            emInput,
            participantConfigUtil.getOrDefault[EmRuntimeConfig](uuid),
            outputConfigUtil.getOrDefault(NotifierIdentifier.Em),
            maybeControllingEm = None,
          )
          Right(uuid -> actor)
        }
      }

    val previousLevelAndUncontrolledEms =
      previousLevelEms ++ uncontrolledEms.toMap

    if (controlledEmInputs.nonEmpty) {
      // For controlled EMs at the current level, more EMs
      // might need to be built at the next recursion level.
      val controllingEms = controlledEmInputs.toMap.flatMap {
        case (uuid, emInput) =>
          emInput.getControllingEm.toScala.map(uuid -> _)
      }

      // Return value includes previous level and uncontrolled EMs of this level
      val recursiveEms = buildEmsRecursively(
        participantConfigUtil,
        outputConfigUtil,
        controllingEms,
        previousLevelAndUncontrolledEms,
      )

      val controlledEms = controlledEmInputs.map { case (uuid, emInput) =>
        val controllingEm = emInput.getControllingEm.toScala
          .map(_.getUuid)
          .map(uuid =>
            recursiveEms.getOrElse(
              uuid,
              throw new CriticalFailureException(
                s"Actor for EM $uuid not found."
              ),
            )
          )

        uuid -> buildEm(
          emInput,
          participantConfigUtil.getOrDefault[EmRuntimeConfig](uuid),
          outputConfigUtil.getOrDefault(NotifierIdentifier.Em),
          maybeControllingEm = controllingEm,
        )
      }.toMap

      recursiveEms ++ controlledEms
    } else {
      previousLevelAndUncontrolledEms
    }
  }

  private def buildParticipantActor(
      requestVoltageDeviationThreshold: Double,
      participantConfigUtil: ConfigUtil.ParticipantConfigUtil,
      outputConfigUtil: OutputConfigUtil,
      participantInputModel: SystemParticipantInput,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      environmentRefs: EnvironmentRefs,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[ParticipantAgent.Request] = {

    val serviceMap: Map[ServiceType, ClassicRef] =
      Seq(
        Some(ServiceType.WeatherService -> environmentRefs.weather),
        environmentRefs.evDataService.map(ref =>
          ServiceType.EvMovementService -> ref
        ),
      ).flatten.toMap

    val participantRefs = ParticipantRefs(
      gridAgentContext.self,
      environmentRefs.primaryServiceProxy,
      serviceMap,
      listener,
    )

    val simParams = SimulationParameters(
      resolution,
      Each(requestVoltageDeviationThreshold),
      simulationStartDate,
      simulationEndDate,
    )

    participantInputModel match {
      case input: FixedFeedInInput =>
        buildParticipant(
          input,
          participantConfigUtil.getOrDefault[FixedFeedInRuntimeConfig](
            input.getUuid
          ),
          outputConfigUtil.getOrDefault(NotifierIdentifier.FixedFeedIn),
          participantRefs,
          simParams,
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: LoadInput =>
        buildParticipant(
          input,
          participantConfigUtil.getOrDefault[LoadRuntimeConfig](
            input.getUuid
          ),
          outputConfigUtil.getOrDefault(NotifierIdentifier.Load),
          participantRefs,
          simParams,
          environmentRefs.scheduler,
          maybeControllingEm,
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
          maybeControllingEm,
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
          maybeControllingEm,
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
          maybeControllingEm,
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
              maybeControllingEm,
            )
          case None =>
            throw new GridAgentInitializationException(
              s"Unable to find thermal island grid for heat pump '${hpInput.getUuid}' with thermal bus '${hpInput.getThermalBus.getUuid}'."
            )
        }
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
          maybeControllingEm,
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
  }

  private def buildParticipant(
      participantInput: SystemParticipantInput,
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      participantRefs: ParticipantRefs,
      simParams: SimulationParameters,
      scheduler: ActorRef[SchedulerMessage],
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[ParticipantAgent.Request] = {
    val participant = gridAgentContext.spawn(
      ParticipantAgentInit(
        participantInput,
        runtimeConfig,
        notifierConfig,
        participantRefs,
        simParams,
        maybeControllingEm.toRight(scheduler),
      ),
      name = actorName(
        participantInput.getClass.getSimpleName.replace("Input", ""),
        participantInput.getId,
      ),
    )
    gridAgentContext.watch(participant)

    participant
  }

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
    *   The simulation time at which the simulation starts
    * @param simulationEndDate
    *   The simulation time at which the simulation ends
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[PvAgent]] 's [[ActorRef]]
    */
  private def buildPv(
      pvInput: PvInput,
      modelConfiguration: PvRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      weatherService: ClassicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[ParticipantAgent.Request] = {
    val participant = gridAgentContext.toClassic
      .simonaActorOf(
        PvAgent.props(
          environmentRefs.scheduler.toClassic,
          ParticipantInitializeStateData(
            pvInput,
            modelConfiguration,
            primaryServiceProxy,
            Iterable(ActorWeatherService(weatherService)),
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            maybeControllingEm,
          ),
          listener.map(_.toClassic),
        ),
        pvInput.getId,
      )
      .toTyped
    introduceAgentToEnvironment(participant)

    participant
  }

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
    *   The simulation time at which the simulation starts
    * @param simulationEndDate
    *   The simulation time at which the simulation ends
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[EvcsAgent]] 's [[ActorRef]]
    */
  private def buildEvcs(
      evcsInput: EvcsInput,
      modelConfiguration: EvcsRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      evMovementsService: ClassicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[ParticipantAgent.Request] = {
    val participant = gridAgentContext.toClassic
      .simonaActorOf(
        EvcsAgent.props(
          environmentRefs.scheduler.toClassic,
          ParticipantInitializeStateData(
            evcsInput,
            modelConfiguration,
            primaryServiceProxy,
            Iterable(
              ActorExtEvDataService(
                evMovementsService
              )
            ),
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            maybeControllingEm,
          ),
          listener.map(_.toClassic),
        ),
        evcsInput.getId,
      )
      .toTyped
    introduceAgentToEnvironment(participant)

    participant
  }

  /** Builds an [[HpAgent]] from given input
    *
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
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   A tuple of actor reference and [[ParticipantInitializeStateData]]
    */
  private def buildHp(
      hpInput: HpInput,
      thermalGrid: ThermalGrid,
      modelConfiguration: HpRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      weatherService: ClassicRef,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[ParticipantAgent.Request] = {
    val participant = gridAgentContext.toClassic
      .simonaActorOf(
        HpAgent.props(
          environmentRefs.scheduler.toClassic,
          ParticipantInitializeStateData(
            hpInput,
            thermalGrid,
            modelConfiguration,
            primaryServiceProxy,
            Iterable(ActorWeatherService(weatherService)),
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            maybeControllingEm,
          ),
          listener.map(_.toClassic),
        ),
        hpInput.getId,
      )
      .toTyped
    introduceAgentToEnvironment(participant)

    participant
  }

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
    *   The simulation time at which the simulation starts
    * @param simulationEndDate
    *   The simulation time at which the simulation ends
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[WecAgent]] 's [[ActorRef]]
    */
  private def buildWec(
      wecInput: WecInput,
      modelConfiguration: WecRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      weatherService: ClassicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[ParticipantAgent.Request] = {
    val participant = gridAgentContext.toClassic
      .simonaActorOf(
        WecAgent.props(
          environmentRefs.scheduler.toClassic,
          ParticipantInitializeStateData(
            wecInput,
            modelConfiguration,
            primaryServiceProxy,
            Iterable(ActorWeatherService(weatherService)),
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            maybeControllingEm,
          ),
          listener.map(_.toClassic),
        ),
        wecInput.getId,
      )
      .toTyped
    introduceAgentToEnvironment(participant)

    participant
  }

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
    *   The simulation time at which the simulation starts
    * @param simulationEndDate
    *   The simulation time at which the simulation ends
    * @param resolution
    *   Frequency of power flow calculations
    * @param requestVoltageDeviationThreshold
    *   Maximum deviation in p.u. of request voltages to be considered equal
    * @param outputConfig
    *   Configuration of the output behavior
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[StorageAgent]] 's [[ActorRef]]
    */
  private def buildStorage(
      storageInput: StorageInput,
      modelConfiguration: StorageRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantAgent.Request] = {
    val participant = gridAgentContext.toClassic
      .simonaActorOf(
        StorageAgent.props(
          environmentRefs.scheduler.toClassic,
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
            maybeControllingEm,
          ),
          listener.map(_.toClassic),
        ),
        storageInput.getId,
      )
      .toTyped
    introduceAgentToEnvironment(participant)

    participant
  }

  /** Builds an [[EmAgent]] from given input
    *
    * @param emInput
    *   The input model
    * @param modelConfiguration
    *   Runtime configuration for the agent
    * @param outputConfig
    *   Configuration for output notification
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[EmAgent]] 's [[ActorRef]]
    */
  private def buildEm(
      emInput: EmInput,
      modelConfiguration: EmRuntimeConfig,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[FlexResponse] =
    gridAgentContext.spawn(
      EmAgent(
        emInput,
        modelConfiguration,
        outputConfig,
        emInput.getControlStrategy,
        simulationStartDate,
        maybeControllingEm.toRight(
          environmentRefs.scheduler
        ),
        listener,
      ),
      actorName(classOf[EmAgent.type], emInput.getId),
    )

  /** Introduces the given agent to scheduler
    *
    * @param actorRef
    *   Reference to the actor to add to the environment
    */
  private def introduceAgentToEnvironment(
      actorRef: ActorRef[ParticipantAgent.Request]
  ): Unit = {
    gridAgentContext.watch(actorRef)
    environmentRefs.scheduler ! ScheduleActivation(
      actorRef.toClassic.toTyped,
      INIT_SIM_TICK,
    )
  }

}
