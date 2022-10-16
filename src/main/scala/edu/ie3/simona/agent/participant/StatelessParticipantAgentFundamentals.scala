/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.PrimaryDataWithApparentPower
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.InconsistentStateException
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  ModelState,
  SystemParticipant
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

protected trait StatelessParticipantAgentFundamentals[
    PD <: PrimaryDataWithApparentPower[PD],
    CD <: CalcRelevantData,
    MS <: ModelState,
    D <: ParticipantStateData[PD],
    I <: SystemParticipantInput,
    MC <: SimonaConfig.BaseRuntimeConfig,
    M <: SystemParticipant[CD, PD, MS]
] extends ParticipantAgentFundamentals[PD, CD, MS, D, I, MC, M] {
  this: ParticipantAgent[PD, CD, MS, D, I, MC, M] =>

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[PD, CD, MS, M],
      ComparableQuantity[Dimensionless]
  ) => PD

  /** Calculate the power output of the participant without needing any
    * secondary data. The next state is [[Idle]], sending a
    * [[CompletionMessage]] to scheduler and using update result values.
    *
    * @param baseStateData
    *   Base state data to update
    * @param maybeLastModelState
    *   The current model state, before updating it
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithoutSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[PD, CD, MS, M],
      maybeLastModelState: Option[MS],
      currentTick: Long,
      scheduler: ActorRef,
      nodalVoltage: ComparableQuantity[Dimensionless]
  ): FSM.State[AgentState, ParticipantStateData[PD]] =
    maybeLastModelState match {
      case Some(_) =>
        throw new InconsistentStateException(
          "Received a model state although this is a stateless agent"
        )
      case None =>
        val result =
          calculateModelPowerFunc(currentTick, baseStateData, nodalVoltage)

        val updatedResultValueStore =
          ValueStore.updateValueStore(
            baseStateData.resultValueStore,
            currentTick,
            result
          )

        /* Inform the listeners about new result */
        announceSimulationResult(
          baseStateData,
          currentTick,
          AccompaniedSimulationResult(result)
        )(baseStateData.outputConfig)

        /* In this case, without secondary data, the agent has been triggered by an ActivityStartTrigger by itself,
         * therefore pop the next one */
        val baseStateDataWithUpdatedResultStore =
          BaseStateData.updateBaseStateData(
            baseStateData,
            updatedResultValueStore,
            baseStateData.requestValueStore,
            baseStateData.voltageValueStore,
            baseStateData.additionalActivationTicks,
            baseStateData.foreseenDataTicks
          )
        goToIdleReplyCompletionAndScheduleTriggerForNextAction(
          baseStateDataWithUpdatedResultStore,
          scheduler
        )
    }
}
