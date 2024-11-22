/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.data.Data.{
  PrimaryData,
  PrimaryDataMeta,
  SecondaryData,
}
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
import edu.ie3.simona.model.participant2.ParticipantModelInit.ParticipantModelInitContainer
import edu.ie3.simona.model.participant2.ParticipantModelShell.ResultsContainer
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  ProvideFlexOptions,
}
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.Dimensionless
import squants.energy.Power

import java.time.ZonedDateTime
import scala.reflect.ClassTag

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

  /** Update operating point when the model is '''not''' em-controlled.
    *
    * @param currentTick
    * @return
    */
  def updateOperatingPoint(
      currentTick: Long
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(currentTick)

    if (currentState.tick != currentTick)
      throw new CriticalFailureException(
        s"New state $currentState is not set to current tick $currentTick"
      )

    def modelOperatingPoint() = {
      val (modelOp, modelNextTick) = model.determineOperatingPoint(
        state,
        relevantData.getOrElse(
          throw new CriticalFailureException("No relevant data available!")
        ),
      )
      val modelIndicator = ModelChangeIndicator(changesAtTick = modelNextTick)
      (modelOp, modelIndicator)
    }

    val (newOperatingPoint, newChangeIndicator) =
      determineOperatingPointInInterval(modelOperatingPoint, currentTick)

    copy(
      state = currentState,
      lastOperatingPoint = operatingPoint,
      operatingPoint = Some(newOperatingPoint),
      modelChange = newChangeIndicator,
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
    val complexPower = ComplexPower(activePower, reactivePower)

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

    val flexOptions =
      if (operationInterval.includes(currentTick)) {
        model.calcFlexOptions(
          currentState,
          relevantData.getOrElse(
            throw new CriticalFailureException("No relevant data available!")
          ),
        )
      } else {
        // Out of operation, there's no way to operate besides 0 kW
        ProvideMinMaxFlexOptions.noFlexOption(model.uuid, zeroKW)
      }

    copy(state = currentState, flexOptions = Some(flexOptions))
  }

  /** Update operating point on receiving [[IssueFlexControl]], i.e. when the
    * model is em-controlled.
    *
    * @param flexControl
    * @return
    */
  def updateOperatingPoint(
      flexControl: IssueFlexControl
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(flexControl.tick)

    val currentTick = flexControl.tick

    def modelOperatingPoint() = {
      val fo = flexOptions.getOrElse(
        throw new CriticalFailureException("No flex options available!")
      )

      val setPointActivePower = EmTools.determineFlexPower(
        fo,
        flexControl,
      )

      model.handlePowerControl(
        currentState,
        relevantData.getOrElse(
          throw new CriticalFailureException("No relevant data available!")
        ),
        fo,
        setPointActivePower,
      )
    }

    val (newOperatingPoint, newChangeIndicator) =
      determineOperatingPointInInterval(modelOperatingPoint, currentTick)

    copy(
      state = currentState,
      lastOperatingPoint = operatingPoint,
      operatingPoint = Some(newOperatingPoint),
      modelChange = newChangeIndicator,
    )
  }

  private def determineOperatingPointInInterval(
      modelOperatingPoint: () => (OP, ModelChangeIndicator),
      currentTick: Long,
  ): (OP, ModelChangeIndicator) = {
    if (operationInterval.includes(currentTick)) {
      val (modelOp, modelIndicator) = modelOperatingPoint()

      // Check if the end of the operation interval is *before* the next tick calculated by the model
      val adaptedNextTick =
        Seq(
          modelIndicator.changesAtTick,
          Option(operationInterval.end),
        ).flatten.minOption

      (modelOp, modelIndicator.copy(changesAtTick = adaptedNextTick))
    } else {
      // Current tick is outside of operation interval.
      // Set operating point to "zero"
      val op = model.zeroPowerOperatingPoint

      // If the model is not active *yet*, schedule the operation start
      val nextTick = Option.when(operationInterval.start < currentTick)(
        operationInterval.start
      )

      (op, ModelChangeIndicator(changesAtTick = nextTick))
    }
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
      totalPower: ComplexPower,
      modelResults: Iterable[SystemParticipantResult],
  )

  def createForPrimaryData[P <: PrimaryData: ClassTag](
      participantInput: SystemParticipantInput,
      config: BaseRuntimeConfig,
      primaryDataMeta: PrimaryDataMeta[P],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): ParticipantModelShell[_, _, _] = {
    val modelContainer = ParticipantModelInit.createPrimaryModel(
      participantInput,
      config,
      primaryDataMeta,
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

  private def createShell[
      OP <: OperatingPoint,
      S <: ModelState,
      OR <: OperationRelevantData,
  ](
      modelContainer: ParticipantModelInitContainer[OP, S, OR],
      participantInput: SystemParticipantInput,
      simulationEndDate: ZonedDateTime,
      simulationStartDate: ZonedDateTime,
  ): ParticipantModelShell[OP, S, OR] = {

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
