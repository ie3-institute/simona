/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.result.system.{
  FlexOptionsResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ComplexPower
import edu.ie3.simona.agent.participant2.ParticipantAgent
import edu.ie3.simona.agent.participant2.ParticipantAgent.ParticipantRequest
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.em.EmTools
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationChangeIndicator,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant2.ParticipantModelShell.ResultsContainer
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.IssueFlexControl
import edu.ie3.simona.ontology.messages.flex.{FlexOptions, MinMaxFlexOptions}
import edu.ie3.simona.util.SimonaConstants.FIRST_TICK_IN_SIMULATION
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.ReactivePower
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import squants.Dimensionless
import squants.energy.Power

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Try}

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
  * @param operationInterval
  *   The operation interval in which the participant model is active. Outside
  *   the interval, no power is produced or consumed.
  * @param simulationStart
  *   The date and time at which simulation started.
  * @param state
  *   The most recent model state.
  * @param operatingPoint
  *   The most recent operating point.
  * @param lastOperatingPoint
  *   The operating point valid before the current [[operatingPoint]], if
  *   applicable.
  * @param flexOptions
  *   The most recent flex options, if they have been calculated already.
  * @param operationChange
  *   The operation change indicator, which indicates until when the current
  *   results are valid.
  * @tparam OP
  *   The type of operating point used by the [[ParticipantModel]].
  * @tparam S
  *   The type of state used by the [[ParticipantModel]].
  */
final case class ParticipantModelShell[
    OP <: OperatingPoint,
    S <: ModelState,
](
    private val model: ParticipantModel[OP, S]
      with ParticipantFlexibility[OP, S],
    private val operationInterval: OperationInterval,
    private val simulationStart: ZonedDateTime,
    private val state: S,
    private val operatingPoint: OP,
    private val lastOperatingPoint: Option[OP] = None,
    private val flexOptions: Option[FlexOptions] = None,
    private val operationChange: OperationChangeIndicator =
      OperationChangeIndicator(),
) {

  /** Returns a unique identifier for the model held by this model shell,
    * including the type, UUID and id of the model, for the purpose of log or
    * exception messaging.
    *
    * @return
    *   A unique identifier for the model
    */
  def identifier: String =
    s"${model.getClass.getSimpleName}[${model.id}/$uuid]"

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

  /** Returns the current flex options, if present, or throws a
    * [[CriticalFailureException]]. Only call this if you are certain the flex
    * options have been set.
    *
    * @return
    *   The flex options.
    */
  def getFlexOptions: FlexOptions =
    flexOptions.getOrElse(
      throw new CriticalFailureException(
        "Flex options have not been calculated!"
      )
    )

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
  def handleInputData(
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
      if (modelNextTick.exists(_ <= tick))
        throw new CriticalFailureException(
          s"Next tick ($modelNextTick) is same as or earlier than the current tick ($tick)."
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
  ): FlexOptionsResult = {
    val minMaxFlexOptions =
      getFlexOptions match {
        case flex: MinMaxFlexOptions => flex
      }

    new FlexOptionsResult(
      tick.toDateTime(simulationStart),
      uuid,
      minMaxFlexOptions.ref.toMegawatts.asMegaWatt,
      minMaxFlexOptions.min.toMegawatts.asMegaWatt,
      minMaxFlexOptions.max.toMegawatts.asMegaWatt,
    )
  }

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
      tick.toDateTime(simulationStart),
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
  def updateFlexOptions(tick: Long): ParticipantModelShell[OP, S] = {
    val currentState = determineCurrentState(tick)

    val flexOptions =
      if (operationInterval.includes(tick)) {
        model.determineFlexOptions(currentState)
      } else {
        // Out of operation, there's no way to operate besides 0 kW
        MinMaxFlexOptions.noFlexOption(zeroKW)
      }

    copy(state = currentState, flexOptions = Some(flexOptions))
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
      val fo = flexOptions.getOrElse(
        throw new CriticalFailureException("No flex options available!")
      )

      val setPointActivePower =
        Try(EmTools.determineFlexPower(fo, flexControl))
          .recoverWith(exception =>
            Failure(
              new CriticalFailureException(
                s"$identifier: Determining flex power failed",
                exception,
              )
            )
          )
          .get

      model.determineOperatingPoint(
        currentState,
        setPointActivePower,
      )
    }

    val (newOperatingPoint, newChangeIndicator) =
      determineOperatingPoint(modelOperatingPoint, currentTick)

    // Sanity check
    if (newChangeIndicator.changesAtTick.exists(_ <= currentTick))
      throw new CriticalFailureException(
        s"Next tick (${newChangeIndicator.changesAtTick}) is same as or earlier than the current tick ($currentTick)."
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
    if (operationInterval.includes(currentTick)) {
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
      ctx: ActorContext[ParticipantAgent.Request],
      request: ParticipantRequest,
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
      if (state.tick < tick) {
        // If the state is old, an operating point needs
        // to be present to determine the curren state
        model.determineState(
          state,
          operatingPoint,
          tick,
          tick.toDateTime(simulationStart),
        )
      } else {
        // The state is up-to-date, no need to update
        state
      }

    if (newState.tick != tick)
      throw new CriticalFailureException(
        s"The current state $newState is not set to current tick $tick"
      )

    newState
  }

}

object ParticipantModelShell {

  /** Container holding the resulting total complex power as well as
    * [[SystemParticipantResult]] specific to the [[ParticipantModel]].
    *
    * @param totalPower
    *   The total complex power produced or consumed.
    * @param modelResults
    *   The model results.
    */
  final case class ResultsContainer(
      totalPower: ComplexPower,
      modelResults: Iterable[SystemParticipantResult],
  )

  /** Creates a model shell using the model factory.
    *
    * @param modelFactory
    *   The participant model factory.
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
      operationTime: OperationTime,
      simulationStart: ZonedDateTime,
      simulationEnd: ZonedDateTime,
  ): ParticipantModelShell[_ <: OperatingPoint, S] = {

    val model = modelFactory.create()

    val operationInterval = SystemComponent.determineOperationInterval(
      simulationStart,
      simulationEnd,
      operationTime,
    )

    val initialState = modelFactory.getInitialState(
      FIRST_TICK_IN_SIMULATION,
      simulationStart,
    )

    ParticipantModelShell(
      model = model,
      operationInterval = operationInterval,
      simulationStart = simulationStart,
      state = initialState,
    )
  }

  /** Additional method that is required for compliant operating point type.
    */
  private def apply[OP <: OperatingPoint, S <: ModelState](
      model: ParticipantModel[OP, S],
      operationInterval: OperationInterval,
      simulationStart: ZonedDateTime,
      state: S,
  ): ParticipantModelShell[OP, S] =
    new ParticipantModelShell(
      model = model,
      operationInterval = operationInterval,
      simulationStart = simulationStart,
      state = state,
      operatingPoint = model.zeroPowerOperatingPoint,
    )

}
