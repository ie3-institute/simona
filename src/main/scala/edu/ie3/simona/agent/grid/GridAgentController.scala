/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.EmInput
import edu.ie3.datamodel.models.input.container.{SubGridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.actor.SimonaActorNaming._
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.em.EmAgent
import edu.ie3.simona.agent.em.EmAgent.EmMessage
import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantMessage
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService.{
  ActorEvMovementsService,
  ActorWeatherService,
}
import edu.ie3.simona.agent.participant.evcs.EvcsAgent
import edu.ie3.simona.agent.participant.fixedfeedin.FixedFeedInAgent
import edu.ie3.simona.agent.participant.hp.HpAgent
import edu.ie3.simona.agent.participant.load.LoadAgent
import edu.ie3.simona.agent.participant.pv.PvAgent
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.ParticipantInitializeStateData
import edu.ie3.simona.agent.participant.wec.WecAgent
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig._
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexResponse
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.ConfigUtil._
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import org.slf4j.Logger

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
  *
  * @since 2019-07-18
  */
class GridAgentController(
    gridAgentContext: ActorContext[_],
    environmentRefs: EnvironmentRefs,
    simulationStartDate: ZonedDateTime,
    simulationEndDate: ZonedDateTime,
    participantsConfig: SimonaConfig.Simona.Runtime.Participant,
    rootEmConfig: Option[SimonaConfig.Simona.Runtime.RootEm],
    outputConfig: SimonaConfig.Simona.Output.Participant,
    resolution: Long,
    listener: Iterable[ActorRef[ResultEvent]],
    log: Logger,
) extends LazyLogging {
  def buildSystemParticipants(
      subGridContainer: SubGridContainer,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
  ): Map[UUID, Set[ActorRef[ParticipantMessage]]] = {

    val systemParticipants =
      filterSysParts(subGridContainer, environmentRefs)

    /* Browse through all system participants, build actors and map their node's UUID to the actor references */
    buildParticipantToActorRef(
      participantsConfig,
      outputConfig,
      systemParticipants,
      thermalIslandGridsByBusId,
      environmentRefs,
      rootEmConfig,
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
              case entity @ (_: BmInput | _: ChpInput | _: EvInput |
                  _: StorageInput) =>
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
              case entity if entity.getControllingEm.isPresent =>
                log.debug(
                  s"System participant {} is part of an energy-managed system and thus not directly connected to the grid.",
                  entity,
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
    * requested to send a initialisation trigger.
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
      participantsConfig: SimonaConfig.Simona.Runtime.Participant,
      outputConfig: SimonaConfig.Simona.Output.Participant,
      participants: Vector[SystemParticipantInput],
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      environmentRefs: EnvironmentRefs,
      rootEmConfig: Option[SimonaConfig.Simona.Runtime.RootEm],
  ): Map[UUID, Set[ActorRef[ParticipantMessage]]] = {
    /* Prepare the config util for the participant models, which (possibly) utilizes as map to speed up the initialization
     * phase */
    val participantConfigUtil =
      ConfigUtil.ParticipantConfigUtil(participantsConfig)
    val outputConfigUtil = ConfigUtil.OutputConfigUtil(outputConfig)

    def buildEmsRecursively(
        emInputs: Map[UUID, EmInput],
        existingEms: Map[UUID, ActorRef[EmMessage]],
    ): Map[UUID, ActorRef[EmMessage]] = {
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
              rootEmConfig = rootEmConfig,
            )
            Right(uuid -> actor)
          }
        }

      val existingAndUncontrolledEms = existingEms ++ uncontrolledEms.toMap

      if (controlledEmInputs.nonEmpty) {
        // EMs that are controlling EMs at this level
        val controllingEms = controlledEmInputs.toMap.flatMap {
          case (uuid, emInput) =>
            emInput.getControllingEm.toScala.map(uuid -> _)
        }

        val recursiveEms = buildEmsRecursively(
          controllingEms,
          existingAndUncontrolledEms,
        )

        val controlledEms = controlledEmInputs.map { case (uuid, emInput) =>
          val controllingEm = emInput.getControllingEm.toScala
            .map(_.getUuid)
            .flatMap(uuid => recursiveEms.get(uuid))

          uuid -> buildEm(
            emInput,
            participantConfigUtil.getOrDefault[EmRuntimeConfig](uuid),
            outputConfigUtil.getOrDefault(NotifierIdentifier.Em),
            maybeControllingEm = controllingEm,
            rootEmConfig = None,
          )
        }.toMap

        recursiveEms ++ controlledEms
      } else {
        existingAndUncontrolledEms
      }
    }

    // all ems that control at least one participant directly
    val firstLevelEms = participants.flatMap {
      _.getControllingEm.toScala.map(em => em.getUuid -> em)
    }.toMap

    val allEms = buildEmsRecursively(firstLevelEms, Map.empty)

    participants
      .map { participant =>
        val node = participant.getNode

        val actorRef = buildParticipantActor(
          participantsConfig.requestVoltageDeviationThreshold,
          participantConfigUtil,
          outputConfigUtil,
          participant,
          thermalIslandGridsByBusId,
          environmentRefs,
          allEms.get(participant.getUuid),
        )
        introduceAgentToEnvironment(actorRef)
        // return uuid to actorRef
        node.getUuid -> actorRef
      }
      .toSet[(UUID, ActorRef[ParticipantMessage])]
      .groupMap(entry => entry._1)(entry => entry._2)
  }

  private def buildParticipantActor(
      requestVoltageDeviationThreshold: Double,
      participantConfigUtil: ConfigUtil.ParticipantConfigUtil,
      outputConfigUtil: OutputConfigUtil,
      participantInputModel: SystemParticipantInput,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      environmentRefs: EnvironmentRefs,
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantMessage] = participantInputModel match {
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
        maybeControllingEm,
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
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[FixedFeedInAgent]] 's [[ActorRef]]
    */
  private def buildFixedFeedIn(
      fixedFeedInInput: FixedFeedInInput,
      modelConfiguration: FixedFeedInRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext.toClassic
      .simonaActorOf(
        FixedFeedInAgent.props(
          environmentRefs.scheduler.toClassic,
          ParticipantInitializeStateData(
            fixedFeedInInput,
            modelConfiguration,
            primaryServiceProxy,
            Iterable.empty,
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            maybeControllingEm,
          ),
          listener.map(_.toClassic),
        ),
        fixedFeedInInput.getId,
      )
      .toTyped

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
    * @param maybeParmaybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[LoadAgent]] 's [[ActorRef]]
    */
  private def buildLoad(
      loadInput: LoadInput,
      modelConfiguration: LoadRuntimeConfig,
      primaryServiceProxy: ClassicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeParmaybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext.toClassic
      .simonaActorOf(
        LoadAgent.props(
          environmentRefs.scheduler.toClassic,
          ParticipantInitializeStateData(
            loadInput,
            modelConfiguration,
            primaryServiceProxy,
            Iterable.empty,
            simulationStartDate,
            simulationEndDate,
            resolution,
            requestVoltageDeviationThreshold,
            outputConfig,
            maybeParmaybeControllingEm,
          ),
          listener.map(_.toClassic),
        ),
        loadInput.getId,
      )
      .toTyped

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
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext.toClassic
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
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext.toClassic
      .simonaActorOf(
        EvcsAgent.props(
          environmentRefs.scheduler.toClassic,
          ParticipantInitializeStateData(
            evcsInput,
            modelConfiguration,
            primaryServiceProxy,
            Iterable(
              ActorEvMovementsService(
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
        )
      )
      .toTyped

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
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext.toClassic
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
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext.toClassic
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

  /** Builds an [[EmAgent]] from given input
    *
    * @param emInput
    *   Input model
    * @param modelConfiguration
    *   Runtime configuration for the agent
    * @param outputConfig
    *   Configuration for output notification
    * @return
    *   A tuple of actor reference and [[ParticipantInitializeStateData]]
    */
  private def buildEm(
      emInput: EmInput,
      modelConfiguration: EmRuntimeConfig,
      outputConfig: NotifierConfig,
      maybeControllingEm: Option[ActorRef[FlexResponse]] = None,
      rootEmConfig: Option[SimonaConfig.Simona.Runtime.RootEm] = None,
  ): ActorRef[EmMessage] =
    gridAgentContext.spawn(
      EmAgent(
        emInput,
        modelConfiguration,
        outputConfig,
        rootEmConfig
          .map(_ => "PROPORTIONAL")
          .getOrElse("PRIORITIZED"),
        simulationStartDate,
        maybeControllingEm.toRight(
          environmentRefs.scheduler
        ),
        rootEmConfig,
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
      actorRef: ActorRef[ParticipantMessage]
  ): Unit = {
    gridAgentContext.watch(actorRef)
    environmentRefs.scheduler ! ScheduleActivation(
      actorRef.toClassic.toTyped,
      INIT_SIM_TICK,
    )
  }

}
