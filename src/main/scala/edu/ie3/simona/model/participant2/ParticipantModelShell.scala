/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
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
  ResultsContainer,
}
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
    OP <: OperatingPoint,
    S <: ModelState,
    OR <: OperationRelevantData,
](
    model: ParticipantModel[OP, S, OR] with ParticipantFlexibility[OP, S, OR],
    operationInterval: OperationInterval,
    simulationStartDate: ZonedDateTime,
    state: S,
    relevantData: Option[OR],
    flexOptions: Option[ProvideFlexOptions],
    operatingPoint: Option[OP],
    modelChange: ModelChangeIndicator,
) {

  def updateRelevantData(
      receivedData: Seq[SecondaryData],
      nodalVoltage: Dimensionless,
      tick: Long,
  ): ParticipantModelShell[OP, S, OR] = {
    val updatedRelevantData =
      model.createRelevantData(receivedData, nodalVoltage, tick)

    copy(relevantData = Some(updatedRelevantData))
  }

  def updateOperatingPoint(
      currentTick: Long
  ): ParticipantModelShell[OP, S, OR] = {
    // todo consider operationInterval

    val currentState = determineCurrentState(currentTick)

    if (currentState.tick != currentTick)
      throw new CriticalFailureException(
        s"New state $currentState is not set to current tick $currentTick"
      )

    val (newOperatingPoint, maybeNextTick) =
      model.determineOperatingPoint(
        state,
        relevantData.getOrElse("No relevant data available!"),
      )

    copy(
      state = currentState,
      operatingPoint = Some(newOperatingPoint),
      modelChange = ModelChangeIndicator(changesAtTick = maybeNextTick),
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
      relevantData.getOrElse("No relevant data available!"),
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

  def createForPrimaryData(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): ParticipantModelShell[_, _, _] = {
    // todo T parameter, receive from primary proxy
    val model = ParticipantModelInit.createPrimaryModel(
      participantInput,
      config,
    )
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        participantInput.getOperationTime,
      )

    new ParticipantModelShell(
      model = model,
      operationInterval = operationInterval,
      simulationStartDate = simulationStartDate,
      state = model.getInitialState(),
      relevantData = None,
      flexOptions = None,
      operatingPoint = None,
      modelChange = ModelChangeIndicator(),
    )
  }

  def createForModel(
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): ParticipantModelShell[_, _, _] = {

    val model = ParticipantModelInit.createModel(
      participantInput,
      config,
    )
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        participantInput.getOperationTime,
      )

    new ParticipantModelShell(
      model = model,
      operationInterval = operationInterval,
      simulationStartDate = simulationStartDate,
      state = model.getInitialState(),
      relevantData = None,
      flexOptions = None,
      operatingPoint = None,
      modelChange = ModelChangeIndicator(),
    )
  }

}
