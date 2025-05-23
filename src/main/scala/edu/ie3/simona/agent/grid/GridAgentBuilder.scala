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
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.agent.participant.{ParticipantAgent, ParticipantAgentInit}
import edu.ie3.simona.config.OutputConfig.ParticipantOutputConfig
import edu.ie3.simona.config.RuntimeConfig._
import edu.ie3.simona.config.SimonaConfig.AssetConfigs
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.InputModelContainer
import edu.ie3.simona.model.InputModelContainer.{
  SimpleInputContainer,
  WithHeatInputContainer,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexResponse
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.ServiceType
import edu.ie3.simona.util.ConfigUtil
import edu.ie3.simona.util.ConfigUtil._
import edu.ie3.simona.util.SimonaConstants.PRE_INIT_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
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
class GridAgentBuilder(
    gridAgentContext: ActorContext[GridAgent.Request],
    environmentRefs: EnvironmentRefs,
    simulationStartDate: ZonedDateTime,
    simulationEndDate: ZonedDateTime,
    emConfigs: AssetConfigs[EmRuntimeConfig],
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

    val outputConfigUtil =
      ConfigUtil.OutputConfigUtil.participants(outputConfig)

    // ems that control at least one participant directly
    val firstLevelEms = systemParticipants.flatMap {
      _.getControllingEm.toScala.map(em => em.getUuid -> em)
    }.toMap

    val allEms = buildEmsRecursively(
      EmConfigUtil(emConfigs),
      outputConfigUtil,
      firstLevelEms,
    )

    /* Browse through all system participants, build actors and map their node's UUID to the actor references */
    buildParticipantToActorRef(
      participantsConfig,
      allEms,
      outputConfigUtil,
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
              case entity @ (_: ChpInput | _: EvInput) =>
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
    * @param emAgents
    *   mapping: em uuid to agent
    * @param outputConfigUtil
    *   Containing configuration information for output behaviour
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
      emAgents: Map[UUID, ActorRef[FlexResponse]],
      outputConfigUtil: OutputConfigUtil,
      participants: Vector[SystemParticipantInput],
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      environmentRefs: EnvironmentRefs,
  ): Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] = {
    /* Prepare the config util for the participant models, which (possibly) utilizes as map to speed up the initialization
     * phase */
    val participantConfigUtil =
      ConfigUtil.ParticipantConfigUtil(participantsConfig)

    participants
      .map { participant =>
        val node = participant.getNode

        val controllingEm =
          participant.getControllingEm.toScala
            .map(_.getUuid)
            .map(uuid =>
              emAgents.getOrElse(
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
    * @param emConfigUtil
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
      emConfigUtil: ConfigUtil.EmConfigUtil,
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
            emConfigUtil.getOrDefault(uuid),
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
        case (_, emInput) =>
          emInput.getControllingEm.toScala.map(em => em.getUuid -> em)
      }

      // Return value includes previous level and uncontrolled EMs of this level
      val recursiveEms = buildEmsRecursively(
        emConfigUtil,
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
          emConfigUtil.getOrDefault(uuid),
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

    val serviceMap: Map[ServiceType, ActorRef[_ >: ServiceMessage]] =
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
          SimpleInputContainer(input),
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
          SimpleInputContainer(input),
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
        buildParticipant(
          SimpleInputContainer(input),
          participantConfigUtil.getOrDefault[PvRuntimeConfig](
            input.getUuid
          ),
          outputConfigUtil.getOrDefault(NotifierIdentifier.PvPlant),
          participantRefs,
          simParams,
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: BmInput =>
        buildParticipant(
          SimpleInputContainer(input),
          participantConfigUtil.getOrDefault[BmRuntimeConfig](
            input.getUuid
          ),
          outputConfigUtil.getOrDefault(NotifierIdentifier.BioMassPlant),
          participantRefs,
          simParams,
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: WecInput =>
        buildParticipant(
          SimpleInputContainer(input),
          participantConfigUtil.getOrDefault[WecRuntimeConfig](
            input.getUuid
          ),
          outputConfigUtil.getOrDefault(NotifierIdentifier.Wec),
          participantRefs,
          simParams,
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: EvcsInput =>
        buildParticipant(
          SimpleInputContainer(input),
          participantConfigUtil.getOrDefault[EvcsRuntimeConfig](
            input.getUuid
          ),
          outputConfigUtil.getOrDefault(NotifierIdentifier.Evcs),
          participantRefs,
          simParams,
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: HpInput =>
        thermalIslandGridsByBusId.get(input.getThermalBus.getUuid) match {
          case Some(thermalGrid) =>
            buildParticipant(
              WithHeatInputContainer(input, thermalGrid),
              participantConfigUtil.getOrDefault[HpRuntimeConfig](
                input.getUuid
              ),
              outputConfigUtil.getOrDefault(NotifierIdentifier.Hp),
              participantRefs,
              simParams,
              environmentRefs.scheduler,
              maybeControllingEm,
            )
          case None =>
            throw new GridAgentInitializationException(
              s"Unable to find thermal island grid for heat pump '${input.getUuid}' with thermal bus '${input.getThermalBus.getUuid}'."
            )
        }
      case input: StorageInput =>
        buildParticipant(
          SimpleInputContainer(input),
          participantConfigUtil.getOrDefault[StorageRuntimeConfig](
            input.getUuid
          ),
          outputConfigUtil.getOrDefault(NotifierIdentifier.Storage),
          participantRefs,
          simParams,
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: SystemParticipantInput =>
        throw new NotImplementedError(
          s"Building ${input.getClass.getSimpleName} is not implemented, yet."
        )
    }
  }

  private def buildParticipant(
      inputContainer: InputModelContainer[_ <: SystemParticipantInput],
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      participantRefs: ParticipantRefs,
      simParams: SimulationParameters,
      scheduler: ActorRef[SchedulerMessage],
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  ): ActorRef[ParticipantAgent.Request] = {

    val key = ScheduleLock.singleKey(gridAgentContext, scheduler, PRE_INIT_TICK)

    val participant = gridAgentContext.spawn(
      ParticipantAgentInit(
        inputContainer,
        runtimeConfig,
        notifierConfig,
        participantRefs,
        simParams,
        maybeControllingEm.toRight(scheduler),
        key,
      ),
      name = actorName(
        inputContainer.electricalInputModel.getClass.getSimpleName
          .replace("Input", ""),
        inputContainer.electricalInputModel.getId,
      ),
    )
    gridAgentContext.watch(participant)

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

}
