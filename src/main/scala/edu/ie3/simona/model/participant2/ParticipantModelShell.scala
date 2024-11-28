/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, PrimaryDataMeta}
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.config.SimonaConfig.BaseRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.em.EmTools
import edu.ie3.simona.model.participant2.ParticipantModel.{
  OperationChangeIndicator,
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
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
    state: Option[S] = None,
    relevantData: Option[OR] = None,
    flexOptions: Option[ProvideFlexOptions] = None,
    lastOperatingPoint: Option[OP] = None,
    operatingPoint: Option[OP] = None,
    private val modelChange: OperationChangeIndicator =
      OperationChangeIndicator(),
) {

  def updateRelevantData(
      receivedData: Seq[Data],
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

    def modelOperatingPoint() = {
      val (modelOp, modelNextTick) = model.determineOperatingPoint(
        currentState,
        relevantData.getOrElse(
          throw new CriticalFailureException("No relevant data available!")
        ),
      )
      val modelIndicator =
        OperationChangeIndicator(changesAtTick = modelNextTick)
      (modelOp, modelIndicator)
    }

    val (newOperatingPoint, newChangeIndicator) =
      determineOperatingPoint(modelOperatingPoint, currentTick)

    copy(
      state = Some(currentState),
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
      state.getOrElse(
        throw new CriticalFailureException("No model state available!")
      ),
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

    copy(state = Some(currentState), flexOptions = Some(flexOptions))
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
      determineOperatingPoint(modelOperatingPoint, currentTick)

    copy(
      state = Some(currentState),
      lastOperatingPoint = operatingPoint,
      operatingPoint = Some(newOperatingPoint),
      modelChange = newChangeIndicator,
    )
  }

  private def determineOperatingPoint(
      modelOperatingPoint: () => (OP, OperationChangeIndicator),
      currentTick: Long,
  ): (OP, OperationChangeIndicator) = {
    if (operationInterval.includes(currentTick)) {
      modelOperatingPoint()
    } else {
      // Current tick is outside of operation interval.
      // Set operating point to "zero"
      (model.zeroPowerOperatingPoint, OperationChangeIndicator())
    }
  }

  /** Determines and returns the next activation tick considering the operating
    * interval and given next data tick.
    */
  def getChangeIndicator(
      currentTick: Long,
      nextDataTick: Option[Long],
  ): OperationChangeIndicator = {
    if (operationInterval.includes(currentTick)) {
      // The next activation tick should be the earliest of
      // the next tick request by the model, the next data tick and
      // the end of the operation interval
      val adaptedNextTick =
        Seq(
          modelChange.changesAtTick,
          nextDataTick,
          Option(operationInterval.end),
        ).flatten.minOption

      modelChange.copy(changesAtTick = adaptedNextTick)
    } else {
      // If the model is not active, all activation ticks are ignored besides
      // potentially the operation start
      val nextTick = Option.when(operationInterval.start > currentTick)(
        operationInterval.start
      )

      OperationChangeIndicator(changesAtTick = nextTick)
    }
  }

  def handleRequest(
      ctx: ActorContext[ParticipantAgent.Request],
      request: ParticipantRequest,
  ): ParticipantModelShell[OP, S, OR] = {
    val currentState = determineCurrentState(request.tick)
    val updatedState = model.handleRequest(currentState, ctx, request)

    copy(state = Some(updatedState))
  }

  private def determineCurrentState(currentTick: Long): S = {
    // new state is only calculated if there's an old state and an operating point
    val currentState = state
      .zip(operatingPoint)
      .flatMap { case (st, op) =>
        Option.when(st.tick < currentTick) {
          model.determineState(st, op, currentTick)
        }
      }
      .getOrElse(model.initialState(currentTick))

    if (currentState.tick != currentTick)
      throw new CriticalFailureException(
        s"New state $currentState is not set to current tick $currentTick"
      )

    currentState
  }

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
    val model = ParticipantModelInit.createPrimaryModel(
      participantInput,
      config,
      primaryDataMeta,
    )
    createShell(
      model,
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
    val model = ParticipantModelInit.createModel(
      participantInput,
      config,
    )
    createShell(
      model,
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
      model: ParticipantModel[OP, S, OR],
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
      model = model,
      operationInterval = operationInterval,
      simulationStartDate = simulationStartDate,
    )
  }
}
