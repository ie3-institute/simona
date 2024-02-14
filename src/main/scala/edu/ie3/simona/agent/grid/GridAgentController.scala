/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.container.{SubGridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.actor.SimonaActorNaming._
import edu.ie3.simona.agent.EnvironmentRefs
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
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.ConfigUtil._
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  ClassicActorRefOps,
  TypedActorRefOps,
}
import org.apache.pekko.actor.{ActorContext, ActorRef => classicRef}
import org.slf4j.Logger

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
    gridAgentContext: ActorContext, // TODO: Change to typed after the participants are typed
    environmentRefs: EnvironmentRefs,
    simulationStartDate: ZonedDateTime,
    simulationEndDate: ZonedDateTime,
    participantsConfig: SimonaConfig.Simona.Runtime.Participant,
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
    )
  }

  /** Takes the provided [[SubGridContainer]] and removes all
    * [[SystemParticipantInput]] of which no agent implementations are available
    * at the moment. This method needs to be adapted whenever a new agent
    * implementation is ready. Hopefully, it can be removed soon.
    *
    * To disabled a filter fo a specific asset adapt the code below.
    *
    * @param subGridContainer
    *   the original subGrid container
    * @return
    *   a filtered subGrid container w/o assets no agent implementations exist
    *   atm
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
  ): Map[UUID, Set[ActorRef[ParticipantMessage]]] = {
    /* Prepare the config util for the participant models, which (possibly) utilizes as map to speed up the initialization
     * phase */
    val participantConfigUtil =
      ConfigUtil.ParticipantConfigUtil(participantsConfig)
    val outputConfigUtil = ConfigUtil.OutputConfigUtil(outputConfig)

    participants
      .map(participant => {
        val node = participant.getNode
        // build
        val actorRef =
          buildParticipantActor(
            participantsConfig.requestVoltageDeviationThreshold,
            participantConfigUtil,
            outputConfigUtil,
            participant,
            thermalIslandGridsByBusId,
            environmentRefs,
          )
        introduceAgentToEnvironment(actorRef.toClassic)
        // return uuid to actorRef
        node.getUuid -> actorRef
      })
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
      )
    case hpInput: HpInput =>
      thermalIslandGridsByBusId.get(hpInput.getThermalBus.getUuid) match {
        case Some(thermalGrid) =>
          buildHp(
            hpInput,
            thermalGrid,
            participantConfigUtil.getOrDefault(hpInput.getUuid),
            environmentRefs.primaryServiceProxy,
            environmentRefs.weather,
            requestVoltageDeviationThreshold,
            outputConfigUtil.getOrDefault(NotifierIdentifier.Hp),
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
    * @return
    *   The [[FixedFeedInAgent]] 's [[classicRef]]
    */
  private def buildFixedFeedIn(
      fixedFeedInInput: FixedFeedInInput,
      modelConfiguration: FixedFeedInRuntimeConfig,
      primaryServiceProxy: classicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext
      .simonaActorOf(
        FixedFeedInAgent.props(
          environmentRefs.scheduler,
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
    * @return
    *   The [[LoadAgent]] 's [[classicRef]]
    */
  private def buildLoad(
      loadInput: LoadInput,
      modelConfiguration: LoadRuntimeConfig,
      primaryServiceProxy: classicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext
      .simonaActorOf(
        LoadAgent.props(
          environmentRefs.scheduler,
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
    * @return
    *   The [[PvAgent]] 's [[classicRef]]
    */
  private def buildPv(
      pvInput: PvInput,
      modelConfiguration: PvRuntimeConfig,
      primaryServiceProxy: classicRef,
      weatherService: classicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext
      .simonaActorOf(
        PvAgent.props(
          environmentRefs.scheduler,
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
    * @return
    *   The [[EvcsAgent]] 's [[classicRef]]
    */
  private def buildEvcs(
      evcsInput: EvcsInput,
      modelConfiguration: EvcsRuntimeConfig,
      primaryServiceProxy: classicRef,
      evMovementsService: classicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext
      .simonaActorOf(
        EvcsAgent.props(
          environmentRefs.scheduler,
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
          ),
          listener.map(_.toClassic),
        )
      )
      .toTyped

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
    * @return
    *   A tuple of actor reference and [[ParticipantInitializeStateData]]
    */
  private def buildHp(
      hpInput: HpInput,
      thermalGrid: ThermalGrid,
      modelConfiguration: HpRuntimeConfig,
      primaryServiceProxy: classicRef,
      weatherService: classicRef,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext
      .simonaActorOf(
        HpAgent.props(
          environmentRefs.scheduler,
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
          ),
          listener.map(_.toClassic),
        ),
        hpInput.getId,
      )
      .toTyped

  /** Creates a pv agent and determines the needed additional information for
    * later initialization of the agent.
    *
    * @param wecInput
    *   WEC input model to derive information from
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
    * @return
    *   The [[WecAgent]] 's [[classicRef]]
    */
  private def buildWec(
      wecInput: WecInput,
      modelConfiguration: WecRuntimeConfig,
      primaryServiceProxy: classicRef,
      weatherService: classicRef,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
  ): ActorRef[ParticipantMessage] =
    gridAgentContext
      .simonaActorOf(
        WecAgent.props(
          environmentRefs.scheduler,
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
          ),
          listener.map(_.toClassic),
        ),
        wecInput.getId,
      )
      .toTyped

  /** Introduces the given agent to scheduler
    *
    * @param actorRef
    *   Reference to the actor to add to the environment
    */
  private def introduceAgentToEnvironment(
      actorRef: classicRef
  ): Unit = {
    gridAgentContext.watch(actorRef)
    environmentRefs.scheduler ! ScheduleActivation(
      actorRef.toTyped,
      INIT_SIM_TICK,
    )
  }

}
