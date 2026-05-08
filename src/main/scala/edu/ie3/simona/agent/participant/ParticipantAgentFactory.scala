/*
 * © 2020-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import edu.ie3.datamodel.models.input.EmInput
import edu.ie3.datamodel.models.input.container.{
  SystemParticipants,
  ThermalGrid,
}
import edu.ie3.datamodel.models.input.system.*
import edu.ie3.simona.actor.SimonaActorNaming.*
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.em.{EmAgent, EmAgentInit}
import edu.ie3.simona.agent.participant.ParticipantAgentInit.{
  ParticipantRefs,
  SimulationParameters,
}
import edu.ie3.simona.agent.participant.{ParticipantAgent, ParticipantAgentInit}
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.InputModelContainer
import edu.ie3.simona.model.InputModelContainer.{
  SimpleInputContainer,
  WithHeatInputContainer,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexResponse
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.em.ExtEmDataService
import edu.ie3.simona.util.ConfigUtil.*
import edu.ie3.simona.util.SimonaConstants.PRE_INIT_TICK
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.Each

import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional

/** Provides functionality for building system participants.
  *
  * @since 2019-07-18
  */
object ParticipantAgentFactory {

  final case class BuildData(
      simonaConfig: SimonaConfig,
      resolution: Long,
      simStartTime: ZonedDateTime,
      simEndTime: ZonedDateTime,
  ) {
    val participantConfigUtil: ParticipantConfigUtil = ParticipantConfigUtil(
      simonaConfig.runtime.participant
    )

    val outputConfigUtil: OutputConfigUtil =
      OutputConfigUtil.participants(simonaConfig.output.participant)

    val emConfigUtil: EmConfigUtil = EmConfigUtil(simonaConfig.runtime.em)
  }

  /** Builds the relevant system participant.
    *
    * @param systemParticipants
    *   The system participants.
    * @param thermalIslandGridsByBusId
    *   Thermal islands by bus UUID.
    * @param simonaConfig
    *   The config of SIMONA.
    * @param environmentRefs
    *   The environmental references.
    * @param ctx
    *   ActorContext.
    * @return
    *   A map from coupling point to set of actor references.
    */
  def buildSystemParticipants(
      systemParticipants: SystemParticipants,
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      simonaConfig: SimonaConfig,
  )(using
      environmentRefs: EnvironmentRefs,
      ctx: ActorContext[?],
  ): Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] = {
    given BuildData = BuildData(
      simonaConfig,
      simonaConfig.powerflow
        .map(_.resolution.toSeconds)
        .getOrElse(Long.MaxValue),
      simonaConfig.time.simStartTime,
      simonaConfig.time.simEndTime,
    )

    val validSystemParticipants =
      filterValidSysParts(systemParticipants, environmentRefs)

    // ems that control at least one participant directly
    val firstLevelEms = validSystemParticipants.flatMap {
      _.getControllingEm.toScala.map(em => em.getUuid -> em)
    }.toMap

    val allEms = buildEmsRecursively(
      firstLevelEms,
      emDataService = environmentRefs.emDataService,
    )

    /* Browse through all system participants, build actors and map their node's UUID to the actor references */
    buildParticipantToActorRef(
      allEms,
      validSystemParticipants,
      thermalIslandGridsByBusId,
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
    * @param systemParticipants
    *   The system participants.
    * @return
    *   The filtered participants w/o assets for which no agent implementations
    *   exist atm.
    */
  private def filterValidSysParts(
      systemParticipants: SystemParticipants,
      environmentRefs: EnvironmentRefs,
  )(using ctx: ActorContext[?]): Seq[SystemParticipantInput] = {

    val (notProcessedElements, availableSysParts) =
      systemParticipants
        .allEntitiesAsList()
        .asScala
        .foldLeft((Set.empty[String], Seq.empty[SystemParticipantInput])) {
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
                ctx.log.warn(
                  s"Evcs ${evcsInput.getId} has been removed because no ev movements service is present."
                )
                (notProcessedElements, availableSystemParticipants)
              case entity =>
                (notProcessedElements, availableSystemParticipants :+ entity)
            }
        }

    if notProcessedElements.nonEmpty then
      ctx.log.warn(
        s"The following elements have been removed, " +
          s"as the agents are not implemented yet: $notProcessedElements"
      )

    availableSysParts

  }

  /** Go through all provided input models, build agents for those and group the
    * resulting actor references for each connection nodes. All participant
    * agents are also introduced to the agent environment and the scheduler is
    * requested to send an initialization trigger.
    *
    * @param emAgents
    *   Mapping: em uuid to agent.
    * @param participants
    *   Set of system participants to create agents for.
    * @param thermalIslandGridsByBusId
    *   Collection of thermal island grids, mapped by their thermal bus uuid.
    * @return
    *   A map from coupling point to set of actor references.
    */
  private def buildParticipantToActorRef(
      emAgents: Map[UUID, ActorRef[FlexResponse]],
      participants: Seq[SystemParticipantInput],
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
  )(using
      environmentRefs: EnvironmentRefs,
      buildData: BuildData,
      ctx: ActorContext[?],
  ): Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] = {
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
          thermalIslandGridsByBusId,
          participant,
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
    * @param emInputs
    *   EMs of the current level, which can be controlled by further EMs at
    *   higher levels.
    * @param previousLevelEms
    *   EMs that have been built by the previous recursion level.
    * @param emDataService
    *   An energy management service.
    * @return
    *   Map from model UUID to EmAgent ActorRef.
    */
  private def buildEmsRecursively(
      emInputs: Map[UUID, EmInput],
      previousLevelEms: Map[UUID, ActorRef[FlexResponse]] = Map.empty,
      emDataService: Option[ActorRef[ExtEmDataService.Message]],
  )(using
      environmentRefs: EnvironmentRefs,
      buildData: BuildData,
      ctx: ActorContext[?],
  ): Map[UUID, ActorRef[FlexResponse]] = {
    // For the current level, split controlled and uncontrolled EMs.
    val (controlledEmInputs, uncontrolledEms) = emInputs.partition {
      case (_, emInput) => emInput.getControllingEm.isPresent
    }

    // Uncontrolled EMs can be built right away.
    val uncontrolledEmAgents = uncontrolledEms.flatMap {
      case (uuid, emInput) if !previousLevelEms.contains(uuid) =>
        val actor = buildEm(
          emInput,
          maybeControllingEm = None,
        )
        Some(uuid -> actor)
      case (uuid, _) =>
        ctx.log.warn(s"Agent with uuid '$uuid' was already built!")
        None
    }

    val previousLevelAndUncontrolledEms =
      previousLevelEms ++ uncontrolledEmAgents

    if controlledEmInputs.nonEmpty then {
      // For controlled EMs at the current level, more EMs
      // might need to be built at the next recursion level.
      val controllingEms = controlledEmInputs.flatMap { case (_, emInput) =>
        emInput.getControllingEm.toScala.map(em => em.getUuid -> em)
      }

      // Return value includes previous level and uncontrolled EMs of this level
      val recursiveEms = buildEmsRecursively(
        controllingEms,
        previousLevelAndUncontrolledEms,
        emDataService,
      )

      val controlledEms = controlledEmInputs.flatMap {
        case (uuid, emInput) if !recursiveEms.contains(uuid) =>
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

          Some(
            uuid -> buildEm(
              emInput,
              maybeControllingEm = controllingEm,
            )
          )
        case _ => None
      }

      recursiveEms ++ controlledEms
    } else {
      previousLevelAndUncontrolledEms
    }
  }

  private def buildParticipantActor(
      thermalIslandGridsByBusId: Map[UUID, ThermalGrid],
      participantInputModel: SystemParticipantInput,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  )(using
      environmentRefs: EnvironmentRefs,
      buildData: BuildData,
      ctx: ActorContext[?],
  ): ActorRef[ParticipantAgent.Request] = {

    given ParticipantRefs = ParticipantRefs(
      environmentRefs.primaryServiceProxy,
      environmentRefs.resultProxy,
      environmentRefs.serviceMap,
    )

    given SimulationParameters = SimulationParameters(
      buildData.resolution,
      Each(
        buildData.simonaConfig.runtime.participant.requestVoltageDeviationThreshold
      ),
      buildData.simStartTime,
      buildData.simEndTime,
    )

    participantInputModel match {
      case input: FixedFeedInInput =>
        buildParticipant(
          SimpleInputContainer(input),
          buildData.participantConfigUtil
            .getOrDefault[FixedFeedInRuntimeConfig](
              input.getUuid
            ),
          buildData.outputConfigUtil.getOrDefault(
            NotifierIdentifier.FixedFeedIn
          ),
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: LoadInput =>
        buildParticipant(
          SimpleInputContainer(input),
          buildData.participantConfigUtil.getOrDefault[LoadRuntimeConfig](
            input.getUuid
          ),
          buildData.outputConfigUtil.getOrDefault(NotifierIdentifier.Load),
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: PvInput =>
        buildParticipant(
          SimpleInputContainer(input),
          buildData.participantConfigUtil.getOrDefault[PvRuntimeConfig](
            input.getUuid
          ),
          buildData.outputConfigUtil.getOrDefault(
            NotifierIdentifier.PvPlant
          ),
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: BmInput =>
        buildParticipant(
          SimpleInputContainer(input),
          buildData.participantConfigUtil.getOrDefault[BmRuntimeConfig](
            input.getUuid
          ),
          buildData.outputConfigUtil.getOrDefault(
            NotifierIdentifier.BioMassPlant
          ),
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: WecInput =>
        buildParticipant(
          SimpleInputContainer(input),
          buildData.participantConfigUtil.getOrDefault[WecRuntimeConfig](
            input.getUuid
          ),
          buildData.outputConfigUtil.getOrDefault(NotifierIdentifier.Wec),
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: EvcsInput =>
        buildParticipant(
          SimpleInputContainer(input),
          buildData.participantConfigUtil.getOrDefault[EvcsRuntimeConfig](
            input.getUuid
          ),
          buildData.outputConfigUtil.getOrDefault(NotifierIdentifier.Evcs),
          environmentRefs.scheduler,
          maybeControllingEm,
        )
      case input: HpInput =>
        thermalIslandGridsByBusId.get(input.getThermalBus.getUuid) match {
          case Some(thermalGrid) =>
            buildParticipant(
              WithHeatInputContainer(input, thermalGrid),
              buildData.participantConfigUtil.getOrDefault[HpRuntimeConfig](
                input.getUuid
              ),
              buildData.outputConfigUtil.getOrDefault(NotifierIdentifier.Hp),
              environmentRefs.scheduler,
              maybeControllingEm,
            )
          case None =>
            throw new CriticalFailureException(
              s"Unable to find thermal island grid for heat pump '${input.getUuid}' with thermal bus '${input.getThermalBus.getUuid}'."
            )
        }
      case input: StorageInput =>
        buildParticipant(
          SimpleInputContainer(input),
          buildData.participantConfigUtil.getOrDefault[StorageRuntimeConfig](
            input.getUuid
          ),
          buildData.outputConfigUtil.getOrDefault(
            NotifierIdentifier.Storage
          ),
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
      inputContainer: InputModelContainer[? <: SystemParticipantInput],
      runtimeConfig: BaseRuntimeConfig,
      notifierConfig: NotifierConfig,
      scheduler: ActorRef[SchedulerMessage],
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  )(using
      participantRefs: ParticipantRefs,
      simParams: SimulationParameters,
      ctx: ActorContext[?],
  ): ActorRef[ParticipantAgent.Request] = {

    val key = ScheduleLock.singleKey(ctx, scheduler, PRE_INIT_TICK)

    val participant = ctx.spawn(
      ParticipantAgentInit(
        inputContainer,
        runtimeConfig,
        notifierConfig,
        maybeControllingEm.toRight(scheduler),
        key,
      ),
      name = actorName(
        inputContainer.electricalInputModel.getClass.getSimpleName
          .replace("Input", ""),
        inputContainer.electricalInputModel.getId,
      ),
    )
    ctx.watch(participant)

    participant
  }

  /** Builds an [[EmAgent]] from given input
    *
    * @param emInput
    *   The input model
    * @param maybeControllingEm
    *   The parent EmAgent, if applicable
    * @return
    *   The [[EmAgent]] 's [[ActorRef]]
    */
  private def buildEm(
      emInput: EmInput,
      maybeControllingEm: Option[ActorRef[FlexResponse]],
  )(using
      environmentRefs: EnvironmentRefs,
      buildData: BuildData,
      ctx: ActorContext[?],
  ): ActorRef[FlexResponse] =
    ctx.spawn(
      EmAgentInit(
        emInput,
        buildData.emConfigUtil.getOrDefault(emInput.getUuid),
        buildData.outputConfigUtil.getOrDefault(NotifierIdentifier.Em),
        buildData.simStartTime,
        maybeControllingEm.toRight(environmentRefs.scheduler),
        environmentRefs.serviceMap,
        environmentRefs.resultProxy,
        environmentRefs.emDataService,
      ),
      actorName(classOf[EmAgent.type], emInput.getId),
    )

}
