/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import breeze.numerics.{pow, sqrt}
import edu.ie3.simona.agent.DataInputHandler
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  ProvidedPowerResponse,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModelShell
import edu.ie3.simona.ontology.messages.AgentMessage.{ActivationRequest, tick}
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  DataMessage,
  DirectAgentRequest,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.{
  Activation,
  SchedulerMessage,
  ServiceMessage,
}
import edu.ie3.util.scala.Scope
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import squants.{Dimensionless, Each}

/** Agent that represents and acts on behalf of any system participant model,
  * which is defined as a subclass of
  * [[edu.ie3.simona.model.participant.ParticipantModel]].
  */
object ParticipantAgent {

  type Message = Request | ActivationRequest | ServiceMessage.Response

  sealed trait Request

  /** This message, sent by the [[edu.ie3.simona.agent.grid.GridAgent]],
    * requests the power values for the requested tick from this
    * [[ParticipantAgent]] and provides the latest nodal voltage.
    *
    * @param tick
    *   The current tick.
    * @param eInPu
    *   Real part of the complex, dimensionless nodal voltage.
    * @param fInPu
    *   Imaginary part of the complex, dimensionless nodal voltage.
    * @param replyTo
    *   Actor reference to send the reply to
    */
  final case class RequestAssetPowerMessage(
      tick: Long,
      eInPu: Dimensionless,
      fInPu: Dimensionless,
      replyTo: ActorRef[ProvidedPowerResponse],
  ) extends Request

  /** Message announcing that calculations by the
    * [[edu.ie3.simona.agent.grid.GridAgent]] have come to an end and regular
    * participant activities can continue.
    *
    * @param tick
    *   The current tick.
    * @param nextRequestTick
    *   The next tick at which asset power is requested via
    *   [[RequestAssetPowerMessage]].
    */
  final case class GridSimulationFinished(
      tick: Long,
      nextRequestTick: Long,
  ) extends Request

  /** Container that conveniently holds various data for the
    * [[ParticipantAgent]].
    *
    * @param modelShell
    *   The [[ParticipantModelShell]], holding the model.
    * @param inputHandler
    *   The [[DataInputHandler]], dealing with primary and secondary input data.
    * @param gridAdapter
    *   The [[ParticipantGridAdapter]], handling interactions with the
    *   [[edu.ie3.simona.agent.grid.GridAgent]].
    * @param resultHandler
    *   The [[ParticipantResultHandler]], handling model results.
    * @param activation
    *   The current activation, if the participant agent is in active state.
    */
  final case class ParticipantAgentData(
      modelShell: ParticipantModelShell[?, ?],
      inputHandler: DataInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultHandler: ParticipantResultHandler,
      activation: Option[ActivationRequest],
  )

  def apply(
      modelShell: ParticipantModelShell[?, ?],
      inputHandler: DataInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultHandler: ParticipantResultHandler,
  )(using
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]]
  ): Behavior[Message] = ParticipantAgent(
    ParticipantAgentData(
      modelShell = modelShell,
      inputHandler = inputHandler,
      gridAdapter = gridAdapter,
      resultHandler = resultHandler,
      activation = None,
    )
  )

  def apply(
      data: ParticipantAgentData
  )(using
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]]
  ): Behavior[Message] =
    Behaviors.receivePartial {
      case (ctx, request: DirectAgentRequest) =>
        // DirectRequests are always directly answered
        // without taking into account possible new input data
        val updatedShell = data.modelShell.handleRequest(ctx, request)

        ParticipantAgent(
          data.copy(modelShell = updatedShell)
        )

      case (ctx, activation: ActivationRequest) =>
        given ActorRef[Message] = ctx.self

        val updatedData = maybeCalculate(
          data.copy(activation = Some(activation))
        )

        ParticipantAgent(updatedData)

      case (ctx, msg: DataMessage) =>
        given ActorRef[Message] = ctx.self

        val updatedData = maybeCalculate(
          data.copy(inputHandler = data.inputHandler.handleDataMessage(msg))
        )

        ParticipantAgent(updatedData)

      case (
            ctx,
            RequestAssetPowerMessage(currentTick, eInPu, fInPu, replyTo),
          ) =>
        // we do not have to wait for the resulting power of the current tick,
        // since the current power is irrelevant for the average power up until now

        val reactivePowerFunc = data.modelShell.reactivePowerFunc

        val nodalVoltage = Each(
          sqrt(
            pow(eInPu.toEach, 2) +
              pow(fInPu.toEach, 2)
          )
        )

        val updatedGridAdapter = data.gridAdapter
          .handlePowerRequest(
            nodalVoltage,
            currentTick,
            Some(reactivePowerFunc),
            ctx.log,
          )

        val result = updatedGridAdapter.avgPowerResult.getOrElse(
          throw new CriticalFailureException(
            "Power result has not been calculated"
          )
        )
        replyTo !
          (if result.newResult then {
             AssetPowerChangedMessage(
               ctx.self,
               result.avgPower.p,
               result.avgPower.q,
             )
           } else {
             AssetPowerUnchangedMessage(
               ctx.self,
               result.avgPower.p,
               result.avgPower.q,
             )
           })

        ParticipantAgent(
          data.copy(gridAdapter = updatedGridAdapter)
        )

      case (ctx, GridSimulationFinished(_, nextRequestTick)) =>
        given ActorRef[Message] = ctx.self

        val gridAdapterFinished =
          data.gridAdapter.updateNextRequestTick(nextRequestTick)

        // Possibly start simulation if we've been activated
        val updatedData = maybeCalculate(
          data.copy(gridAdapter = gridAdapterFinished)
        )

        ParticipantAgent(updatedData)
    }

  /** Starts a model calculation if all requirements have been met. A model
    * calculation could be the determination of flex options and operating point
    * when EM-controlled, and only operating point when not EM-controlled.
    * Requirements include all necessary data having been received and power
    * flow calculation having finished, if applicable.
    *
    * @param data
    *   The current participant data.
    * @param parent
    *   The parent of this [[ParticipantAgent]].
    * @param self
    *   An [[ActorRef]] of this agent.
    * @return
    *   The updated [[ParticipantAgentData]].
    */
  private def maybeCalculate(
      data: ParticipantAgentData
  )(using
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      self: ActorRef[Message],
  ): ParticipantAgentData = {
    if expectedMessagesReceived(data) then {

      val activation = data.activation.getOrElse(
        throw new CriticalFailureException(
          "Activation should be present when data collection is complete"
        )
      )

      // determines, if we need to wait for a set point
      // we only wait if we received a flex activation
      val waitForSetPoint = activation match {
        case _: FlexActivation => true
        case _                 => false
      }

      // inform the result proxy that this participant agent will send new results
      data.resultHandler.informProxy(
        data.modelShell.uuid,
        activation.tick,
        waitForSetPoint,
      )

      val (updatedShell, updatedGridAdapter) = Scope(data.modelShell)
        .map(
          _.updateInputData(
            data.inputHandler.getData,
            data.gridAdapter.nodalVoltage,
            activation.tick,
          )
        )
        .map { shell =>
          activation match {
            case Activation(tick) =>
              val (shellWithOP, gridAdapterWithResult) =
                if isCalculationRequired(shell, data.inputHandler, activation)
                then {
                  val newShell = shell.updateOperatingPoint(tick)

                  val results =
                    newShell.determineResults(
                      tick,
                      data.gridAdapter.nodalVoltage,
                    )

                  results.modelResults.foreach(data.resultHandler.maybeSend)

                  val newGridAdapter =
                    data.gridAdapter.storePowerValue(results.totalPower, tick)

                  (newShell, newGridAdapter)
                } else {
                  // inform result proxy not to wait for results
                  data.resultHandler.sendNoResult(data.modelShell.uuid, tick)

                  (shell, data.gridAdapter)
                }

              val changeIndicator = shellWithOP.getChangeIndicator(
                tick,
                data.inputHandler.getNextDataTick,
              )

              parent.fold(
                _ ! Completion(
                  self,
                  changeIndicator.changesAtTick,
                ),
                _ =>
                  throw new CriticalFailureException(
                    "Received activation while controlled by EM"
                  ),
              )
              (shellWithOP, gridAdapterWithResult)

            case FlexActivation(tick) =>
              val shellWithFlex =
                if isCalculationRequired(shell, data.inputHandler, activation)
                then {
                  val newShell = shell.updateFlexOptions(tick)
                  data.resultHandler.maybeSend(
                    newShell.determineFlexOptionsResult(tick)
                  )
                  newShell
                } else {
                  // inform result proxy not to wait for results
                  data.resultHandler.sendNoResult(data.modelShell.uuid, tick)

                  shell
                }

              parent.fold(
                _ =>
                  throw new CriticalFailureException(
                    "Received flex activation while not controlled by EM"
                  ),
                _ ! ProvideFlexOptions(
                  shellWithFlex.uuid,
                  shellWithFlex.getFlexOptions,
                ),
              )

              (shellWithFlex, data.gridAdapter)

            case flexControl: IssueFlexControl =>
              val shellWithOP = shell.updateOperatingPoint(flexControl)

              // todo we determine results even if no new data arrived, and EM is also activated...
              val results = shellWithOP.determineResults(
                flexControl.tick,
                data.gridAdapter.nodalVoltage,
              )

              results.modelResults.foreach(data.resultHandler.maybeSend)

              val gridAdapterWithResult =
                data.gridAdapter.storePowerValue(
                  results.totalPower,
                  flexControl.tick,
                )

              val changeIndicator = shellWithOP.getChangeIndicator(
                flexControl.tick,
                data.inputHandler.getNextDataTick,
              )

              parent.fold(
                _ =>
                  throw new CriticalFailureException(
                    "Received issue flex control while not controlled by EM"
                  ),
                emAgent => {
                  emAgent ! FlexResult(
                    shellWithOP.uuid,
                    results.totalPower,
                  )
                  emAgent ! FlexCompletion(
                    shellWithOP.uuid,
                    changeIndicator.changesAtNextActivation,
                    changeIndicator.changesAtTick,
                  )
                },
              )

              (shellWithOP, gridAdapterWithResult)
            case unexpected =>
              throw new CriticalFailureException(
                s"Unexpected activation message $unexpected."
              )
          }
        }
        .get

      data.copy(
        modelShell = updatedShell,
        inputHandler = data.inputHandler.clear(),
        gridAdapter = updatedGridAdapter,
        // clear activation, as it has been dealt with
        activation = None,
      )
    } else data
  }

  /** Checks if all required messages needed for calculation have been received.
    * These are:
    *   - agent is activated (activation has been received and not completed
    *     yet).
    *   - all required data has been received.
    *   - the grid adapter is not waiting for power requests (the new voltage
    *     needs to be received before starting calculations for the current
    *     tick).
    *
    * @param data
    *   The current participant data.
    * @return
    *   Whether power can be calculated or not.
    */
  private def expectedMessagesReceived(
      data: ParticipantAgentData
  ): Boolean = {
    data.activation.exists(activation =>
      !data.gridAdapter.isPowerRequestAwaited(activation.tick) &&
        data.inputHandler.allMessagesReceived(activation.tick)
    )
  }

  /** Checks if conditions for recalculation (i.e. determination of operating
    * point, flex options etc.) are present. This is not the case if all
    * registered services have delivered
    * [[edu.ie3.simona.ontology.messages.ServiceMessage.NoDataProvision]]
    * messages only, but can still be the case if the model itself requested
    * recalculation.
    *
    * @param modelShell
    *   The model shell.
    * @param inputHandler
    *   The participant input handler.
    * @param activation
    *   The current activation request.
    * @return
    */
  private def isCalculationRequired(
      modelShell: ParticipantModelShell[?, ?],
      inputHandler: DataInputHandler,
      activation: ActivationRequest,
  ): Boolean =
    inputHandler.hasNewData(activation.tick) ||
      modelShell
        .getChangeIndicator(activation.tick - 1, None)
        .changesAtTick
        .contains(activation.tick)

}
