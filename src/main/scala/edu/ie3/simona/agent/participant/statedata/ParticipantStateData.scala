/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.statedata

import akka.actor.ActorRef
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, SecondaryData}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig

import java.time.ZonedDateTime

/** Trait to denote all common forms of state data related to each participant
  * agent
  */
trait ParticipantStateData[+PD <: PrimaryData]

object ParticipantStateData {

  /** Data for the state, in which the agent is not initialized, yet.
    * <p>IMPORTANT: Needs to be an empty case class due to typing</p>
    */
  final class ParticipantUninitializedStateData[+PD <: PrimaryData]()
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
    * @tparam I
    *   Type of input model to carry
    * @tparam C
    *   Type of model config to carry
    * @tparam PD
    *   Type of [[PrimaryData]], that the model will generate
    */
  final case class ParticipantInitializingStateData[
      I <: SystemParticipantInput,
      C <: SimonaConfig.BaseRuntimeConfig,
      PD <: PrimaryData
  ](
      inputModel: I,
      modelConfig: C,
      secondaryDataServices: Option[
        Vector[SecondaryDataService[_ <: SecondaryData]]
      ],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
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
    * @tparam I
    *   Type of input model to carry
    * @tparam C
    *   Type of model config to carry
    * @tparam PD
    *   Type of [[PrimaryData]], that the model will generate
    */
  final case class ParticipantInitializeStateData[
      I <: SystemParticipantInput,
      C <: SimonaConfig.BaseRuntimeConfig,
      PD <: PrimaryData
  ](
      inputModel: I,
      modelConfig: C,
      primaryServiceProxy: ActorRef,
      secondaryDataServices: Option[
        Vector[SecondaryDataService[_ <: SecondaryData]]
      ],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
  ) extends InitializeStateData[PD]

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
      pendingResponses: Vector[ActorRef],
      foreseenNextDataTicks: Map[ActorRef, Long] = Map.empty
  ) extends ParticipantStateData[PD]
}
