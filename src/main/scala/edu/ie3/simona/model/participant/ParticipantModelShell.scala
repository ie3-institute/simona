/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.ParticipantModelShell.ResultsContainer
import edu.ie3.simona.model.participant.flex.ParticipantFlexModelShell
import edu.ie3.simona.ontology.messages.ServiceMessage.DirectAgentRequest
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, FlexType}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.IssueFlexControl
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.{Data, DataTimeType}
import edu.ie3.simona.util.SimonaConstants.FIRST_TICK_IN_SIMULATION
import edu.ie3.simona.util.TickUtil.toDateTime
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.Dimensionless
import squants.energy.Power

import java.time.ZonedDateTime
import java.util.UUID

/** A shell allowing interactions with the [[ParticipantModel]] that it holds.
  * Inputs and outputs are buffered and reused where applicable. The operation
  * interval is considered when determining model operating points.
  *
  * Model parameters include the model state and operating point. A new state is
  * determined given a former state and an operating point that has been valid
  * since then. A new operating point can be determined on the basis of the
  * current state.
  *
  * @param model
  *   The [[ParticipantModel]] that determines operating parameters.
  * @param flexModelShell
  *   The flex model shell (if applicable) that is handling operations regarding
  *   flex options.
  * @param operationInterval
  *   The operation interval in which the participant model is active. Outside
  *   the interval, no power is produced or consumed.
  * @param state
  *   The most recent model state.
  * @param operatingPoint
  *   The most recent operating point.
  * @param lastOperatingPoint
  *   The operating point valid before the current [[operatingPoint]], if
  *   applicable.
  * @param operationChange
  *   The operation change indicator, which indicates until when the current
  *   results are valid.
  * @param simulationStart
  *   The date and time at which simulation started.
  * @param identifier
  *   A unique identifier for the model held by this model shell, including the
  *   type, UUID and id of the model, for the purpose of log or exception
  *   messaging.
  * @tparam OP
  *   The type of operating point used by the [[ParticipantModel]].
  * @tparam S
  *   The type of state used by the [[ParticipantModel]].
  */
final case class ParticipantModelShell[
    OP <: OperatingPoint,
    S <: ModelState,
](
    private val model: ParticipantModel[OP, S],
    private val flexModelShell: Option[ParticipantFlexModelShell[OP, S]],
    private val operationInterval: OperationInterval,
    private val state: S,
    private val operatingPoint: OP,
    private val lastOperatingPoint: Option[OP] = None,
    private val operationChange: OperationChangeIndicator =
      OperationChangeIndicator.empty,
)(using simulationStart: ZonedDateTime, identifier: String) {

  /** Returns an identifier of the model held by this shell.
    *
    * @return
    *   The identifier.
    */
  def getIdentifier: String = identifier

  /** Returns the model UUID.
    *
    * @return
    *   The UUID of the model.
    */
  def uuid: UUID = model.uuid

  /** Returns the start of the operation interval of the model.
    *
    * @return
    *   The start tick of the operation interval.
    */
  def operationStart: Long = operationInterval.start

  def hasFlexOptions: Boolean = getFlexModelShell.flexOptions.isDefined

  /** Returns the current flex options, if present, or throws a
    * [[CriticalFailureException]]. Only call this if you are certain the flex
    * options have been set.
    *
    * @return
    *   The flex options.
    */
  def getFlexOptions: FlexOptions =
    getFlexModelShell.getFlexOptions

  /** Returns the reactive power function that takes a nodal voltage value and
    * an active power as input.
    *
    * @return
    *   The reactive power function.
    */
  def reactivePowerFunc: Dimensionless => Power => ReactivePower =
    model.reactivePowerFunc

  /** Updates the model state according to the received data, the current nodal
    * voltage and the current tick.
    *
    * @param receivedData
    *   The received input data.
    * @param nodalVoltage
    *   The current nodal voltage.
    * @param tick
    *   The current tick.
    * @return
    *   An updated [[ParticipantModelShell]].
    */
  def updateInputData(
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
      tick: Long,
  ): ParticipantModelShell[OP, S] = {
    val currentState = determineCurrentState(tick)
    val updatedState =
      model.handleInput(currentState, receivedData, nodalVoltage)

    copy(state = updatedState)
  }

  /** Update operating point when the model is '''not''' em-controlled.
    *
    * @param tick
    *   The current tick.
    * @return
    *   An updated [[ParticipantModelShell]].
    */
  def updateOperatingPoint(
      tick: Long
  ): ParticipantModelShell[OP, S] = {
    val currentState = determineCurrentState(tick)

    def modelOperatingPoint(): (OP, OperationChangeIndicator) = {
      val (modelOp, modelNextTick) =
        model.determineOperatingPoint(currentState)
      // Sanity check
      if modelNextTick.exists(_ <= tick) then
        throw new CriticalFailureException(
          s"$identifier: Next tick ($modelNextTick) is same as or earlier than the current tick ($tick)."
        )

      val modelIndicator =
        OperationChangeIndicator(changesAtTick = modelNextTick)
      (modelOp, modelIndicator)
    }

    val (newOperatingPoint, newChangeIndicator) =
      determineOperatingPoint(modelOperatingPoint, tick)

    copy(
      state = currentState,
      lastOperatingPoint = Some(operatingPoint),
      operatingPoint = newOperatingPoint,
      operationChange = newChangeIndicator,
    )
  }

  /** Determines flex options results for the current flex options, which have
    * to have been calculated before.
    *
    * @param tick
    *   The current tick.
    * @return
    *   The flex options results.
    */
  def determineFlexOptionsResult(
      tick: Long
  ): FlexOptionsResult =
    getFlexModelShell.determineResult(tick)

  /** Determines and returns results of the current operating point, which has
    * to have been calculated before.
    *
    * @param tick
    *   The current tick.
    * @param nodalVoltage
    *   The current nodal voltage.
    * @return
    *   The model results.
    */
  def determineResults(
      tick: Long,
      nodalVoltage: Dimensionless,
  ): ResultsContainer = {
    val activePower = operatingPoint.activePower
    val reactivePower = operatingPoint.reactivePower.getOrElse(
      reactivePowerFunc(nodalVoltage)(activePower)
    )
    val complexPower = ComplexPower(activePower, reactivePower)

    val participantResults = model.createResults(
      determineCurrentState(tick),
      lastOperatingPoint,
      operatingPoint,
      complexPower,
      tick.toDateTime(using simulationStart),
    )

    ResultsContainer(
      complexPower,
      participantResults,
    )
  }

  /** Updates the flex options on basis of the current state
    *
    * @param tick
    *   The current tick.
    * @return
    *   An updated [[ParticipantModelShell]].
    */
  def updateFlexOptions(
      tick: Long
  ): ParticipantModelShell[OP, S] = {
    val currentState = determineCurrentState(tick)

    val updatedFlexModelShell = getFlexModelShell.updateFlexOptions(
      currentState,
      operationInterval.includes(tick),
    )

    copy(state = currentState, flexModelShell = Some(updatedFlexModelShell))
  }

  /** Update operating point on receiving [[IssueFlexControl]], i.e. when the
    * model is em-controlled.
    *
    * @param flexControl
    *   The received flex control message.
    * @return
    *   An updated [[ParticipantModelShell]].
    */
  def updateOperatingPoint(
      flexControl: IssueFlexControl
  ): ParticipantModelShell[OP, S] = {
    val currentTick = flexControl.tick

    val currentState = determineCurrentState(currentTick)

    def modelOperatingPoint(): (OP, OperationChangeIndicator) = {

      val setPointActivePower =
        getFlexModelShell.determineFlexPower(flexControl)

      val op = model.determineOperatingPoint(
        currentState,
        setPointActivePower,
      )

      val changeIndicator = getFlexModelShell.determineNextActivation(
        currentState,
        op,
        setPointActivePower,
      )

      (op, changeIndicator)
    }

    val (newOperatingPoint, newChangeIndicator) =
      determineOperatingPoint(modelOperatingPoint, currentTick)

    // Sanity check
    if newChangeIndicator.changesAtTick.exists(_ <= currentTick) then
      throw new CriticalFailureException(
        s"$identifier: Next tick (${newChangeIndicator.changesAtTick}) is same as or earlier than the current tick ($currentTick)."
      )

    copy(
      state = currentState,
      lastOperatingPoint = Some(operatingPoint),
      operatingPoint = newOperatingPoint,
      operationChange = newChangeIndicator,
    )
  }

  /** Determines the operating point by taking into account the operation
    * interval of the model.
    *
    * @param modelOperatingPoint
    *   A function determining the operating point if we're inside the operation
    *   interval.
    * @param currentTick
    *   The current tick.
    * @return
    *   A new [[OperatingPoint]] and an [[OperationChangeIndicator]].
    */
  private def determineOperatingPoint(
      modelOperatingPoint: () => (OP, OperationChangeIndicator),
      currentTick: Long,
  ): (OP, OperationChangeIndicator) = {
    if operationInterval.includes(currentTick) then {
      modelOperatingPoint()
    } else {
      // Current tick is outside of operation interval.
      // Set operating point to "zero"
      (model.zeroPowerOperatingPoint, OperationChangeIndicator.empty)
    }
  }

  /** Determines and returns the next activation tick considering the operating
    * interval and given next data tick.
    *
    * @param currentTick
    *   The current tick.
    * @param nextDataTick
    *   The next tick at which data is expected, if any.
    * @return
    *   The [[OperationChangeIndicator]] indicating the next activation.
    */
  def getChangeIndicator(
      currentTick: Long,
      nextDataTick: Option[Long],
  ): OperationChangeIndicator = {
    if operationInterval.includes(currentTick) then {
      // The next activation tick should be the earliest of
      // the next tick request by the model, the next data tick and
      // the end of the operation interval
      val adaptedNextTick =
        Seq(
          operationChange.changesAtTick,
          nextDataTick,
          Option(operationInterval.end),
        ).flatten.minOption

      operationChange.copy(changesAtTick = adaptedNextTick)
    } else {
      // If the model is not active, all activation ticks are ignored besides
      // potentially the operation start
      val nextTick = Option.when(operationInterval.start > currentTick)(
        operationInterval.start
      )

      OperationChangeIndicator(changesAtTick = nextTick)
    }
  }

  /** Handles a request specific to the [[ParticipantModel]]. The model is
    * allowed to send replies using the provided [[ActorContext]] and to update
    * the model state, which is then stored within the shell.
    *
    * @param ctx
    *   The [[ActorContext]] used for sending replies.
    * @param request
    *   The received request.
    * @return
    *   An updated [[ParticipantModelShell]].
    */
  def handleRequest(
      ctx: ActorContext[ParticipantAgent.Message],
      request: DirectAgentRequest,
  ): ParticipantModelShell[OP, S] = {
    val currentState = determineCurrentState(request.tick)
    val updatedState = model.handleRequest(currentState, ctx, request)

    copy(state = updatedState)
  }

  /** Determines the current state (if it has not been determined before) using
    * the former state, the operating point and the current tick.
    *
    * @param tick
    *   The current tick.
    * @return
    *   The current state.
    */
  private def determineCurrentState(tick: Long): S = {
    // new state is only calculated if there's an old state and an operating point
    val newState =
      if state.tick < tick then {
        // If the state is old, an operating point needs
        // to be present to determine the curren state
        model.determineState(
          state,
          operatingPoint,
          tick,
          tick.toDateTime(using simulationStart),
        )
      } else {
        // The state is up-to-date, no need to update
        state
      }

    if newState.tick != tick then
      throw new CriticalFailureException(
        s"$identifier: The current state $newState is not set to current tick $tick"
      )

    newState
  }

  /** Returns the configured flex model shell, if present, or throws a
    * [[CriticalFailureException]]. Only call this if you are certain the flex
    * model shell has been initialized.
    *
    * @return
    *   The flex model shell.
    */
  private def getFlexModelShell: ParticipantFlexModelShell[OP, S] =
    flexModelShell
      .getOrElse(
        throw new CriticalFailureException(
          s"$identifier: Flex model shell has not been provided!"
        )
      )

}

object ParticipantModelShell {

  /** Creates a model shell using the model factory.
    *
    * @param modelFactory
    *   The participant model factory.
    * @param flexParams
    *   The flexibility parameters, if flexibility is enabled.
    * @param operationTime
    *   The operation time of the participant.
    * @param simulationStart
    *   The simulation start date and time.
    * @param simulationEnd
    *   The simulation end date and time.
    * @return
    *   The constructed [[ParticipantModelShell]].
    */
  def create[S <: ModelState](
      modelFactory: ParticipantModelFactory[S],
      flexParams: Option[(FlexType, DataTimeType)],
      operationTime: OperationTime,
      simulationStart: ZonedDateTime,
      simulationEnd: ZonedDateTime,
  ): ParticipantModelShell[? <: OperatingPoint, S] = {

    val model = modelFactory.create()

    val initialState = modelFactory.getInitialState(
      FIRST_TICK_IN_SIMULATION,
      simulationStart,
    )

    ParticipantModelShell(
      model,
      initialState,
      flexParams,
      operationTime,
      simulationStart,
      simulationEnd,
    )
  }

  /** Additional method that is required for compliant operating point type.
    */
  private def apply[OP <: OperatingPoint, S <: ModelState](
      model: ParticipantModel[OP, S],
      initialState: S,
      flexParams: Option[(FlexType, DataTimeType)],
      operationTime: OperationTime,
      simulationStart: ZonedDateTime,
      simulationEnd: ZonedDateTime,
  ): ParticipantModelShell[OP, S] = {

    given ZonedDateTime = simulationStart
    given String = s"${model.getClass.getSimpleName}[${model.id}/${model.uuid}]"

    val operationInterval = SystemComponent.determineOperationInterval(
      simulationStart,
      simulationEnd,
      operationTime,
    )

    val flexModelShell = flexParams.map { case (flexType, dataTimeType) =>
      val flexModel = model.flexModels.getOrElse(
        flexType,
        throw new CriticalFailureException(
          s"Model ${model.getClass.getSimpleName} does not provide flex type $flexType."
        ),
      )

      ParticipantFlexModelShell[OP, S](
        model.uuid,
        flexModel,
        flexType,
        dataTimeType,
      )
    }

    new ParticipantModelShell(
      model = model,
      flexModelShell = flexModelShell,
      operationInterval = operationInterval,
      state = initialState,
      operatingPoint = model.zeroPowerOperatingPoint,
    )

  }

  /** Container holding the resulting total complex power as well as
    * [[ResultEntity]] specific to the [[ParticipantModel]].
    *
    * @param totalPower
    *   The total complex power produced or consumed.
    * @param modelResults
    *   The model results.
    */
  final case class ResultsContainer(
      totalPower: ComplexPower,
      modelResults: Iterable[ResultEntity],
  )

}
