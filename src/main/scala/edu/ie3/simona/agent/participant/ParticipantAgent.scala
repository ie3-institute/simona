/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import breeze.numerics.{pow, sqrt}
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  ProvidedPowerResponse,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.AdditionalFactoryData
import edu.ie3.simona.model.participant.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.*
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.ResultResponseMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.Data
import edu.ie3.simona.service.Data.{PrimaryData, PrimaryDataExtra}
import edu.ie3.simona.service.results.ExtResultProvider
import edu.ie3.util.scala.Scope
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import squants.{Dimensionless, Each}

/** Agent that represents and acts on behalf of any system participant model,
  * which is defined as a subclass of
  * [[edu.ie3.simona.model.participant.ParticipantModel]].
  */
object ParticipantAgent {

  type Message = Request | ActivationRequest

  type ActivationRequest = Activation | FlexRequest

  /** Extension method for the `Activation` and `FlexRequest` types to retrieve
    * the tick associated with the activation.
    */
  extension (activation: ActivationRequest) {
    def tick: Long =
      activation match {
        case a: Activation  => a.tick
        case f: FlexRequest => f.tick
      }
  }

  sealed trait Request

  /** Messages that are sent by services as responses to registration requests.
    */
  sealed trait RegistrationResponseMessage extends Request {
    val serviceRef: ActorRef[ServiceMessage]
  }

  /** Message confirming a successful registration with a secondary service.
    */
  final case class RegistrationSuccessfulMessage(
      override val serviceRef: ActorRef[ServiceMessage],
      firstDataTick: Long,
      additionalData: Option[AdditionalFactoryData] = None,
  ) extends RegistrationResponseMessage

  /** Message confirming a successful registration with the primary service.
    *
    * @param firstDataTick
    *   The first tick at which data will be sent.
    * @param primaryDataExtra
    *   Extra functionality specific to the primary data class.
    */
  final case class PrimaryRegistrationSuccessfulMessage(
      override val serviceRef: ActorRef[ServiceMessage],
      firstDataTick: Long,
      primaryDataExtra: PrimaryDataExtra[?],
  ) extends RegistrationResponseMessage

  /** Message announcing a failed registration.
    */
  final case class RegistrationFailedMessage(
      override val serviceRef: ActorRef[ServiceMessage]
  ) extends RegistrationResponseMessage

  /** Data provision messages sent by data services.
    */
  sealed trait DataInputMessage extends Request {

    /** The current tick.
      */
    val tick: Long

    /** The sending service actor ref.
      */
    val serviceRef: ActorRef[ServiceMessage]

    /** Next tick at which data could arrive. If None, no data is expected for
      * the rest of the simulation.
      */
    val nextDataTick: Option[Long]
  }

  /** Providing primary or secondary data to the [[ParticipantAgent]].
    *
    * @param data
    *   The data.
    * @tparam D
    *   The type of the provided data.
    */
  final case class DataProvision[D <: Data](
      override val tick: Long,
      override val serviceRef: ActorRef[ServiceMessage],
      data: D,
      override val nextDataTick: Option[Long],
  ) extends DataInputMessage

  /** Providing the information that no data will be provided by the sending
    * service for the current tick. The participant could thus potentially skip
    * calculations for the current tick and reschedule calculation for the next
    * data tick.
    */
  final case class NoDataProvision(
      override val tick: Long,
      override val serviceRef: ActorRef[ServiceMessage],
      override val nextDataTick: Option[Long],
  ) extends DataInputMessage

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

  final case class RequestLastResult(
      replyTo: ActorRef[ExtResultProvider.Message]
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

  /** A request to the [[edu.ie3.simona.model.participant.ParticipantModel]]
    * outside of regular requests related to participant operation.
    */
  trait ParticipantRequest extends Request {

    /** The tick for which the request is valid, which is the current tick.
      */
    val tick: Long
  }

  def apply(
      modelShell: ParticipantModelShell[?, ?],
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultHandler: ParticipantResultHandler,
  )(using
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]]
  ): Behavior[Message] =
    Behaviors.receivePartial {
      case (ctx, request: ParticipantRequest) =>
        // ParticipantRequests are always directly answered
        // without taking into account possible new input data
        val updatedShell = modelShell.handleRequest(ctx, request)

        ParticipantAgent(
          updatedShell,
          inputHandler,
          gridAdapter,
          resultHandler,
        )

      case (ctx, activation: ActivationRequest) =>
        given ActorRef[Message] = ctx.self

        val coreWithActivation = inputHandler.handleActivation(activation)

        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            coreWithActivation,
            gridAdapter,
            resultHandler,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultHandler,
        )

      case (ctx, msg: DataInputMessage) =>
        given ActorRef[Message] = ctx.self

        val inputHandlerWithData = inputHandler.handleDataInputMessage(msg)

        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            inputHandlerWithData,
            gridAdapter,
            resultHandler,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultHandler,
        )

      case (
            ctx,
            RequestAssetPowerMessage(currentTick, eInPu, fInPu, replyTo),
          ) =>
        // we do not have to wait for the resulting power of the current tick,
        // since the current power is irrelevant for the average power up until now

        val reactivePowerFunc = modelShell.reactivePowerFunc

        val nodalVoltage = Each(
          sqrt(
            pow(eInPu.toEach, 2) +
              pow(fInPu.toEach, 2)
          )
        )

        val updatedGridAdapter = gridAdapter
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
          (if (result.newResult) {
             AssetPowerChangedMessage(
               result.avgPower.p,
               result.avgPower.q,
             )
           } else {
             AssetPowerUnchangedMessage(
               result.avgPower.p,
               result.avgPower.q,
             )
           })

        ParticipantAgent(
          modelShell,
          inputHandler,
          updatedGridAdapter,
          resultHandler,
        )

      case (ctx, GridSimulationFinished(_, nextRequestTick)) =>
        given ActorRef[Message] = ctx.self

        val gridAdapterFinished =
          gridAdapter.updateNextRequestTick(nextRequestTick)

        // Possibly start simulation if we've been activated
        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            inputHandler,
            gridAdapterFinished,
            resultHandler,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultHandler,
        )

      case (ctx, RequestLastResult(replyTo)) =>
        // send last calculated results to result service
        replyTo ! ResultResponseMessage(gridAdapter.lastResults)

        ParticipantAgent(
          modelShell,
          inputHandler,
          gridAdapter,
          resultHandler,
        )
    }

  /** Starts a model calculation if all requirements have been met. A model
    * calculation could be the determination of flex options and operating point
    * when EM-controlled, and only operating point when not EM-controlled.
    * Requirements include all necessary data having been received and power
    * flow calculation having finished, if applicable.
    *
    * @param modelShell
    *   The [[ParticipantModelShell]].
    * @param inputHandler
    *   The [[ParticipantInputHandler]].
    * @param gridAdapter
    *   The [[ParticipantGridAdapter]].
    * @param resultHandler
    *   The [[ParticipantResultHandler]].
    * @param parent
    *   The parent of this [[ParticipantAgent]].
    * @param self
    *   An [[ActorRef]] of this agent.
    * @return
    *   An updated [[ParticipantModelShell]], [[ParticipantInputHandler]] and
    *   [[ParticipantGridAdapter]].
    */
  private def maybeCalculate(
      modelShell: ParticipantModelShell[?, ?],
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultHandler: ParticipantResultHandler,
  )(using
      parent: Either[ActorRef[SchedulerMessage], ActorRef[FlexResponse]],
      self: ActorRef[Message],
  ): (
      ParticipantModelShell[?, ?],
      ParticipantInputHandler,
      ParticipantGridAdapter,
  ) = {
    if (expectedMessagesReceived(inputHandler, gridAdapter)) {

      val activation = inputHandler.activation.getOrElse(
        throw new CriticalFailureException(
          "Activation should be present when data collection is complete"
        )
      )

      val (updatedShell, updatedGridAdapter) = Scope(modelShell)
        .map(
          _.handleInputData(
            inputHandler.getData,
            gridAdapter.nodalVoltage,
            activation.tick,
          )
        )
        .map { shell =>
          activation match {
            case Activation(tick) =>
              val (shellWithOP, gridAdapterWithResult) =
                if (isCalculationRequired(shell, inputHandler)) {
                  val newShell = shell.updateOperatingPoint(tick)

                  val results =
                    newShell.determineResults(tick, gridAdapter.nodalVoltage)

                  results.modelResults.foreach(resultHandler.maybeSend)

                  val newGridAdapter = gridAdapter.storeResults(results, tick)

                  (newShell, newGridAdapter)
                } else
                  (shell, gridAdapter)

              val changeIndicator = shellWithOP.getChangeIndicator(
                tick,
                inputHandler.getNextDataTick,
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
                if (isCalculationRequired(shell, inputHandler)) {
                  val newShell = shell.updateFlexOptions(tick)
                  resultHandler.maybeSend(
                    newShell.determineFlexOptionsResult(tick)
                  )
                  newShell
                } else
                  shell

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

              (shellWithFlex, gridAdapter)

            case flexControl: IssueFlexControl =>
              val shellWithOP = shell.updateOperatingPoint(flexControl)

              // todo we determine results even if no new data arrived, and EM is also activated...
              val results = shellWithOP.determineResults(
                flexControl.tick,
                gridAdapter.nodalVoltage,
              )

              results.modelResults.foreach(resultHandler.maybeSend)

              val gridAdapterWithResult =
                gridAdapter.storeResults(results, flexControl.tick)

              val changeIndicator = shellWithOP.getChangeIndicator(
                flexControl.tick,
                inputHandler.getNextDataTick,
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
          }
        }
        .get

      (updatedShell, inputHandler.completeActivation(), updatedGridAdapter)
    } else
      (modelShell, inputHandler, gridAdapter)
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
    * @param inputHandler
    *   The participant input handler.
    * @param gridAdapter
    *   The participant grid adapter.
    * @return
    *   Whether power can be calculated or not.
    */
  private def expectedMessagesReceived(
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
  ): Boolean = {
    inputHandler.allMessagesReceived &&
    inputHandler.activation.exists(activation =>
      !gridAdapter.isPowerRequestAwaited(activation.tick)
    )
  }

  /** Checks if conditions for recalculation (i.e. determination of operating
    * point, flex options etc.) are present. This is not the case if all
    * registered services have delivered [[NoDataProvision]] messages only, but
    * can still be the case if the model itself requested recalculation.
    *
    * @param modelShell
    *   The model shell.
    * @param inputHandler
    *   The participant input handler.
    * @return
    */
  private def isCalculationRequired(
      modelShell: ParticipantModelShell[?, ?],
      inputHandler: ParticipantInputHandler,
  ): Boolean =
    inputHandler.hasNewData ||
      inputHandler.activation.exists(activation =>
        modelShell
          .getChangeIndicator(activation.tick - 1, None)
          .changesAtTick
          .contains(activation.tick)
      )

}
