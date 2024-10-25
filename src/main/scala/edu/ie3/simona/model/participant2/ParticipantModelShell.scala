/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.config.SimonaConfig.BaseRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.em.EmTools
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelChangeIndicator,
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
import edu.ie3.simona.model.participant2.ParticipantModelShell.ResultsContainer
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  ProvideFlexOptions,
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.Dimensionless
import squants.energy.Power

import java.time.ZonedDateTime

/** Takes care of:
  *   - holding id information
  *   - storing:
  *     - states (only current needed)
  *       - operating points (only current needed)
  *       - operation relevant data (only current needed)
  *       - flex options? (only current needed)
  */
final case class ParticipantModelShell[
    OP <: OperatingPoint[_],
    S <: ModelState,
    OR <: OperationRelevantData,
](
    model: ParticipantModel[OP, S, OR] with ParticipantFlexibility[OP, S, OR],
    operationInterval: OperationInterval,
    simulationStartDate: ZonedDateTime,
    state: S,
    relevantData: Option[OR],
    flexOptions: Option[ProvideFlexOptions],
    lastOperatingPoint: Option[OP],
    operatingPoint: Option[OP],
    modelChange: ModelChangeIndicator,
) {

  def updateRelevantData(
      receivedData: Seq[SecondaryData],
      nodalVoltage: Dimensionless,
      tick: Long,
  ): ParticipantModelShell[OP, S, OR] = {
    val currentSimulationTime = tick.toDateTime(simulationStartDate)
    val updatedRelevantData =
      model.createRelevantData(
        receivedData,
        nodalVoltage,
        tick,
        currentSimulationTime,
      )

    copy(relevantData = Some(updatedRelevantData))
  }

  def updateOperatingPoint(
      currentTick: Long
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(currentTick)

    if (currentState.tick != currentTick)
      throw new CriticalFailureException(
        s"New state $currentState is not set to current tick $currentTick"
      )

    val (newOperatingPoint, newNextTick) =
      model.determineOperatingPoint(
        state,
        relevantData.getOrElse(
          throw new CriticalFailureException("No relevant data available!")
        ),
      )

    val (adaptedOperatingPoint, adaptedNextTick) =
      if (!operationInterval.includes(currentTick)) {
        // Current tick is outside of operation interval.
        // Set operating point to "zero"
        (newOperatingPoint.zero, None)
      } else
        (newOperatingPoint, newNextTick)

    copy(
      state = currentState,
      lastOperatingPoint = operatingPoint,
      operatingPoint = Some(adaptedOperatingPoint),
      modelChange = ModelChangeIndicator(changesAtTick = adaptedNextTick),
    )
  }

  def activeToReactivePowerFunc: Dimensionless => Power => ReactivePower =
    model.activeToReactivePowerFunc

  def determineResults(
      currentTick: Long,
      nodalVoltage: Dimensionless,
  ): ResultsContainer = {
    val op = operatingPoint
      .getOrElse(
        throw new CriticalFailureException("No operating point available!")
      )

    val activePower = op.activePower
    val reactivePower = op.reactivePower.getOrElse(
      activeToReactivePowerFunc(nodalVoltage)(activePower)
    )
    val complexPower = ApparentPower(activePower, reactivePower)

    val participantResults = model.createResults(
      state,
      lastOperatingPoint,
      op,
      complexPower,
      currentTick.toDateTime(simulationStartDate),
    )

    ResultsContainer(
      complexPower,
      participantResults,
    )
  }

  def updateFlexOptions(currentTick: Long): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(currentTick)
    val flexOptions = model.calcFlexOptions(
      currentState,
      relevantData.getOrElse(
        throw new CriticalFailureException("No relevant data available!")
      ),
    )

    copy(state = currentState, flexOptions = Some(flexOptions))
  }

  def updateOperatingPoint(
      flexControl: IssueFlexControl
  ): ParticipantModelShell[OP, S, OR] = {
    val fo = flexOptions.getOrElse(
      throw new CriticalFailureException("No flex options available!")
    )

    val currentState = determineCurrentState(flexControl.tick)

    val setPointActivePower = EmTools.determineFlexPower(
      fo,
      flexControl,
    )

    val (newOperatingPoint, modelChange) =
      model.handlePowerControl(currentState, fo, setPointActivePower)

    copy(
      state = currentState,
      lastOperatingPoint = operatingPoint,
      operatingPoint = Some(newOperatingPoint),
      modelChange = modelChange,
    )
  }

  def handleRequest(
      ctx: ActorContext[ParticipantAgent.Request],
      request: ParticipantRequest,
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(request.tick)
    val updatedState = model.handleRequest(currentState, ctx, request)

    copy(state = updatedState)
  }

  private def determineCurrentState(currentTick: Long): S =
    operatingPoint
      .flatMap { op =>
        Option.when(state.tick < currentTick) {
          model.determineState(state, op, currentTick)
        }
      }
      .getOrElse(state)

}

object ParticipantModelShell {

  final case class ResultsContainer(
      totalPower: ApparentPower,
      modelResults: Iterable[SystemParticipantResult],
  )

  def createForPrimaryData(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): ParticipantModelShell[_, _, _] = {
    // todo T parameter, receive from primary proxy
    val modelContainer = ParticipantModelInit.createPrimaryModel(
      participantInput,
      config,
    )
    createShell(
      modelContainer,
      participantInput,
      simulationEndDate,
      simulationStartDate,
    )
  }

  def createForModel(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): ParticipantModelShell[_, _, _] = {
    val modelContainer = ParticipantModelInit.createModel(
      participantInput,
      config,
    )
    createShell(
      modelContainer,
      participantInput,
      simulationEndDate,
      simulationStartDate,
    )
  }

  private def createShell(
      modelContainer: ParticipantModelInit.ParticipantModelInitContainer[_],
      participantInput: SystemParticipantInput,
      simulationEndDate: ZonedDateTime,
      simulationStartDate: ZonedDateTime,
  ): ParticipantModelShell[_, _, _] = {
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        participantInput.getOperationTime,
      )

    new ParticipantModelShell(
      model = modelContainer.model,
      operationInterval = operationInterval,
      simulationStartDate = simulationStartDate,
      state = modelContainer.initialState,
      relevantData = None,
      flexOptions = None,
      lastOperatingPoint = None,
      operatingPoint = None,
      modelChange = ModelChangeIndicator(),
    )
  }
}
