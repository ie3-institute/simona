/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.statedata

import edu.ie3.datamodel.models.input.container.ThermalGrid
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, SecondaryData}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.config.RuntimeConfig.{
  BaseRuntimeConfig,
  SimpleRuntimeConfig,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexResponse
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.{ActorRef => ClassicActorRef}

import java.time.ZonedDateTime

/** Trait to denote all common forms of state data related to each participant
  * agent
  */
trait ParticipantStateData[+PD <: PrimaryData]

object ParticipantStateData {

  /** Data for the state, in which the agent is not initialized, yet.
    * <p>IMPORTANT: Needs to be an empty case class due to typing</p>
    */
  final class ParticipantUninitializedStateData[+PD <: PrimaryData]
      extends UninitializedStateData[PD]

  object ParticipantUninitializedStateData {
    def apply[PD <: PrimaryData](): ParticipantUninitializedStateData[PD] =
      new ParticipantUninitializedStateData()
  }

  /** State data, that is used, while the initialization process is still
    * ongoing
    *
    * @param inputModel
    *   Model for simulation
    * @param modelConfig
    *   Configuration for this type of model
    * @param secondaryDataServices
    *   Option to a collection of secondary data service definitions
    * @param simulationStartDate
    *   Date of the very first tick in the simulation
    * @param simulationEndDate
    *   Date of the very last tick in the simulation
    * @param requestVoltageDeviationThreshold
    *   Threshold, after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @tparam I
    *   Type of input model to carry
    * @tparam C
    *   Type of model config to carry
    * @tparam PD
    *   Type of [[PrimaryData]], that the model will generate
    */
  final case class ParticipantInitializingStateData[
      I <: SystemParticipantInput,
      C <: BaseRuntimeConfig,
      PD <: PrimaryData,
  ](
      inputModel: InputModelContainer[I],
      modelConfig: C,
      secondaryDataServices: Iterable[SecondaryDataService[_ <: SecondaryData]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef[FlexResponse]],
  ) extends ParticipantStateData[PD]

  /** State data to use, when initializing the participant agent
    *
    * @param inputModel
    *   Model for simulation
    * @param modelConfig
    *   Configuration for this type of model
    * @param primaryServiceProxy
    *   Reference to the primary data service proxy
    * @param secondaryDataServices
    *   Option to a collection of secondary data service definitions
    * @param simulationStartDate
    *   Date of the very first tick in the simulation
    * @param simulationEndDate
    *   Date of the very last tick in the simulation
    * @param requestVoltageDeviationThreshold
    *   Threshold, after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param outputConfig
    *   Config for the output behaviour of simulation results
    * @param maybeEmAgent
    *   The EmAgent if this participant is em-controlled
    * @tparam I
    *   Type of input model to carry
    * @tparam C
    *   Type of model config to carry
    * @tparam PD
    *   Type of [[PrimaryData]], that the model will generate
    */
  final case class ParticipantInitializeStateData[
      I <: SystemParticipantInput,
      C <: BaseRuntimeConfig,
      PD <: PrimaryData,
  ](
      inputModel: InputModelContainer[I],
      modelConfig: C,
      primaryServiceProxy: ClassicActorRef,
      secondaryDataServices: Iterable[SecondaryDataService[_ <: SecondaryData]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef[FlexResponse]] = None,
  ) extends InitializeStateData[PD]

  object ParticipantInitializeStateData {

    def apply[
        I <: SystemParticipantInput,
        C <: BaseRuntimeConfig,
        PD <: PrimaryData,
    ](
        inputModel: I,
        modelConfig: C,
        primaryServiceProxy: ClassicActorRef,
        secondaryDataServices: Iterable[
          SecondaryDataService[_ <: SecondaryData]
        ],
        simulationStartDate: ZonedDateTime,
        simulationEndDate: ZonedDateTime,
        resolution: Long,
        requestVoltageDeviationThreshold: Double,
        outputConfig: NotifierConfig,
    ): ParticipantInitializeStateData[I, C, PD] =
      new ParticipantInitializeStateData[I, C, PD](
        SimpleInputContainer(inputModel),
        modelConfig,
        primaryServiceProxy,
        secondaryDataServices,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent = None,
      )
    def apply[
        I <: SystemParticipantInput,
        C <: BaseRuntimeConfig,
        PD <: PrimaryData,
    ](
        inputModel: I,
        modelConfig: C,
        primaryServiceProxy: ClassicActorRef,
        secondaryDataServices: Iterable[
          SecondaryDataService[_ <: SecondaryData]
        ],
        simulationStartDate: ZonedDateTime,
        simulationEndDate: ZonedDateTime,
        resolution: Long,
        requestVoltageDeviationThreshold: Double,
        outputConfig: NotifierConfig,
        maybeEmAgent: Option[ActorRef[FlexResponse]],
    ): ParticipantInitializeStateData[I, C, PD] =
      new ParticipantInitializeStateData[I, C, PD](
        SimpleInputContainer(inputModel),
        modelConfig,
        primaryServiceProxy,
        secondaryDataServices,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent,
      )

    def apply[
        I <: SystemParticipantInput,
        C <: BaseRuntimeConfig,
        PD <: PrimaryData,
    ](
        inputModel: I,
        thermalGrid: ThermalGrid,
        modelConfig: C,
        primaryServiceProxy: ClassicActorRef,
        secondaryDataServices: Iterable[
          SecondaryDataService[_ <: SecondaryData]
        ],
        simulationStartDate: ZonedDateTime,
        simulationEndDate: ZonedDateTime,
        resolution: Long,
        requestVoltageDeviationThreshold: Double,
        outputConfig: NotifierConfig,
    ): ParticipantInitializeStateData[I, C, PD] =
      new ParticipantInitializeStateData[I, C, PD](
        WithHeatInputContainer(inputModel, thermalGrid),
        modelConfig,
        primaryServiceProxy,
        secondaryDataServices,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent = None,
      )

    def apply[
        I <: SystemParticipantInput,
        C <: BaseRuntimeConfig,
        PD <: PrimaryData,
    ](
        inputModel: I,
        thermalGrid: ThermalGrid,
        modelConfig: C,
        primaryServiceProxy: ClassicActorRef,
        secondaryDataServices: Iterable[
          SecondaryDataService[_ <: SecondaryData]
        ],
        simulationStartDate: ZonedDateTime,
        simulationEndDate: ZonedDateTime,
        resolution: Long,
        requestVoltageDeviationThreshold: Double,
        outputConfig: NotifierConfig,
        maybeEmAgent: Option[ActorRef[FlexResponse]],
    ): ParticipantInitializeStateData[I, C, PD] =
      new ParticipantInitializeStateData[I, C, PD](
        WithHeatInputContainer(inputModel, thermalGrid),
        modelConfig,
        primaryServiceProxy,
        secondaryDataServices,
        simulationStartDate,
        simulationEndDate,
        resolution,
        requestVoltageDeviationThreshold,
        outputConfig,
        maybeEmAgent,
      )
  }

  /** StateData to be used, while waiting for registration replies
    *
    * @param baseStateData
    *   Base state data that shall be used, when registration phase is completed
    * @param pendingResponses
    *   List of services, that not yet have responded
    * @param foreseenNextDataTicks
    *   Mapping from service provider to foreseen next tick, it will send new
    *   data
    * @tparam PD
    *   Type of [[PrimaryDataWithApparentPower]], that is covered by given
    *   [[BaseStateData]]
    */
  final case class CollectRegistrationConfirmMessages[
      +PD <: PrimaryDataWithApparentPower[PD]
  ](
      baseStateData: BaseStateData[PD],
      pendingResponses: Iterable[ClassicActorRef],
      foreseenNextDataTicks: Map[ClassicActorRef, Long] = Map.empty,
  ) extends ParticipantStateData[PD]

  sealed trait InputModelContainer[+I <: SystemParticipantInput] {
    val electricalInputModel: I
  }

  final case class SimpleInputContainer[+I <: SystemParticipantInput](
      override val electricalInputModel: I
  ) extends InputModelContainer[I]

  final case class WithHeatInputContainer[+I <: SystemParticipantInput](
      override val electricalInputModel: I,
      thermalGrid: ThermalGrid,
  ) extends InputModelContainer[I]
}
