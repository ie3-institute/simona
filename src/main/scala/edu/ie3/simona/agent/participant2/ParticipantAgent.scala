/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import breeze.numerics.{pow, sqrt}
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  ProvidedPowerResponse,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.{
  PrimaryData,
  PrimaryDataExtra,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.util.scala.Scope
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import squants.{Dimensionless, Each}

import scala.reflect.ClassTag

/** Agent that represents and acts on behalf of any system participant model,
  * which is defined as a subclass of
  * [[edu.ie3.simona.model.participant2.ParticipantModel]].
  */
object ParticipantAgent {

  sealed trait Request

  /** This is extended by all requests that activate an [[ParticipantAgent]],
    * i.e. activations, flex requests and control messages.
    */
  private[participant2] sealed trait ActivationRequest extends Request {
    val tick: Long
  }

  /** Wrapper for an [[Activation]] to be received by an adapter. Activations
    * can only be received if this agent is not EM-controlled.
    *
    * @param tick
    *   The tick to activate.
    */
  private[participant2] final case class ParticipantActivation(
      override val tick: Long
  ) extends ActivationRequest

  /** Wrapper for [[FlexRequest]] messages to be received by an adapter (if this
    * [[ParticipantAgent]] is EM-controlled).
    *
    * @param msg
    *   The wrapped flex request.
    */
  private[participant2] final case class Flex(msg: FlexRequest)
      extends ActivationRequest {
    override val tick: Long = msg.tick
  }

  /** Messages that are sent by services as responses to registration requests.
    */
  sealed trait RegistrationResponseMessage extends Request {
    val serviceRef: ClassicRef
  }

  /** Message confirming a successful registration with a secondary service.
    */
  final case class RegistrationSuccessfulMessage(
      override val serviceRef: ClassicRef,
      firstDataTick: Long,
  ) extends RegistrationResponseMessage

  /** Message confirming a successful registration with the primary service.
    *
    * @param firstDataTick
    *   The first tick at which data will be sent.
    * @param primaryDataExtra
    *   Extra functionality specific to the primary data class.
    * @tparam P
    *   The type of primary data to be received.
    */
  final case class PrimaryRegistrationSuccessfulMessage[
      P <: PrimaryData: ClassTag
  ](
      override val serviceRef: ClassicRef,
      firstDataTick: Long,
      primaryDataExtra: PrimaryDataExtra[P],
  ) extends RegistrationResponseMessage

  /** Message announcing a failed registration.
    */
  final case class RegistrationFailedMessage(
      override val serviceRef: ClassicRef
  ) extends RegistrationResponseMessage

  /** Data provision messages sent by data services.
    */
  sealed trait DataInputMessage extends Request {

    /** The current tick.
      */
    val tick: Long

    /** The sending service actor ref.
      */
    val serviceRef: ClassicRef

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
      override val serviceRef: ClassicRef,
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
      override val serviceRef: ClassicRef,
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

  /** Data object that holds the actor reference to the
    * [[edu.ie3.simona.scheduler.Scheduler]] activating this agent, indicating
    * that this [[ParticipantAgent]] is not EM-controlled.
    *
    * @param scheduler
    *   The scheduler that is activating this agent.
    * @param activationAdapter
    *   The activation adapter handling [[Activation]] messages.
    */
  final case class SchedulerData(
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
  )

  /** Data object that holds the actor reference to the corresponding
    * [[edu.ie3.simona.agent.em.EmAgent]], indicating that this
    * [[ParticipantAgent]] is not EM-controlled.
    *
    * @param emAgent
    *   The parent EmAgent that is controlling this agent.
    * @param flexAdapter
    *   The flex adapter handling [[FlexRequest]] messages.
    * @param lastFlexOptions
    *   Last flex options that have been calculated for this agent.
    */
  final case class FlexControlledData(
      emAgent: ActorRef[FlexResponse],
      flexAdapter: ActorRef[FlexRequest],
      lastFlexOptions: Option[ProvideFlexOptions] = None,
  )

  /** A request to the [[edu.ie3.simona.model.participant2.ParticipantModel]]
    * outside of regular requests related to participant operation.
    */
  trait ParticipantRequest extends Request {

    /** The tick for which the request is valid, which is the current tick.
      */
    val tick: Long
  }

  def apply(
      modelShell: ParticipantModelShell[_, _],
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultHandler: ParticipantResultHandler,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] =
    Behaviors.receivePartial {
      case (ctx, request: ParticipantRequest) =>
        val updatedShell = modelShell
          .updateModelInput(
            inputHandler.getData,
            gridAdapter.nodalVoltage,
            request.tick,
          )
          .handleRequest(ctx, request)

        ParticipantAgent(
          updatedShell,
          inputHandler,
          gridAdapter,
          resultHandler,
          parentData,
        )

      case (_, activation: ActivationRequest) =>
        val coreWithActivation = inputHandler.handleActivation(activation)

        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            coreWithActivation,
            gridAdapter,
            resultHandler,
            parentData,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultHandler,
          parentData,
        )

      case (_, msg: DataInputMessage) =>
        val inputHandlerWithData = inputHandler.handleDataInputMessage(msg)

        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            inputHandlerWithData,
            gridAdapter,
            resultHandler,
            parentData,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultHandler,
          parentData,
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
          parentData,
        )

      case (_, GridSimulationFinished(_, nextRequestTick)) =>
        val gridAdapterFinished =
          gridAdapter.updateNextRequestTick(nextRequestTick)

        // Possibly start simulation if we've been activated
        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            inputHandler,
            gridAdapterFinished,
            resultHandler,
            parentData,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultHandler,
          parentData,
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
    * @param parentData
    *   The parent of this [[ParticipantAgent]].
    * @return
    *   An updated [[ParticipantModelShell]], [[ParticipantInputHandler]] and
    *   [[ParticipantGridAdapter]].
    */
  private def maybeCalculate(
      modelShell: ParticipantModelShell[_, _],
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultHandler: ParticipantResultHandler,
      parentData: Either[SchedulerData, FlexControlledData],
  ): (
      ParticipantModelShell[_, _],
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
          _.updateModelInput(
            inputHandler.getData,
            gridAdapter.nodalVoltage,
            activation.tick,
          )
        )
        .map { shell =>
          activation match {
            case ParticipantActivation(tick) =>
              val (shellWithOP, gridAdapterWithResult) =
                if (isCalculationRequired(shell, inputHandler)) {
                  val newShell = shell.updateOperatingPoint(tick)

                  val results =
                    newShell.determineResults(tick, gridAdapter.nodalVoltage)

                  results.modelResults.foreach(resultHandler.maybeSend)

                  val newGridAdapter =
                    gridAdapter.storePowerValue(results.totalPower, tick)

                  (newShell, newGridAdapter)
                } else
                  (shell, gridAdapter)

              val changeIndicator = shellWithOP.getChangeIndicator(
                tick,
                inputHandler.getNextDataTick,
              )

              parentData.fold(
                schedulerData =>
                  schedulerData.scheduler ! Completion(
                    schedulerData.activationAdapter,
                    changeIndicator.changesAtTick,
                  ),
                _ =>
                  throw new CriticalFailureException(
                    "Received activation while controlled by EM"
                  ),
              )
              (shellWithOP, gridAdapterWithResult)

            case Flex(FlexActivation(tick)) =>
              val shellWithFlex =
                if (isCalculationRequired(shell, inputHandler)) {
                  val newShell = shell.updateFlexOptions(tick)
                  resultHandler.maybeSend(
                    newShell.determineFlexOptionsResult(tick)
                  )
                  newShell
                } else
                  shell

              parentData.fold(
                _ =>
                  throw new CriticalFailureException(
                    "Received flex activation while not controlled by EM"
                  ),
                _.emAgent ! shellWithFlex.flexOptions,
              )

              (shellWithFlex, gridAdapter)

            case Flex(flexControl: IssueFlexControl) =>
              val shellWithOP = shell.updateOperatingPoint(flexControl)

              // todo we determine results even if no new data arrived, and EM is also activated...
              val results = shellWithOP.determineResults(
                flexControl.tick,
                gridAdapter.nodalVoltage,
              )

              results.modelResults.foreach(resultHandler.maybeSend)

              val gridAdapterWithResult =
                gridAdapter.storePowerValue(
                  results.totalPower,
                  flexControl.tick,
                )

              val changeIndicator = shellWithOP.getChangeIndicator(
                flexControl.tick,
                inputHandler.getNextDataTick,
              )

              parentData.fold(
                _ =>
                  throw new CriticalFailureException(
                    "Received issue flex control while not controlled by EM"
                  ),
                _.emAgent ! FlexCompletion(
                  shellWithOP.uuid,
                  changeIndicator.changesAtNextActivation,
                  changeIndicator.changesAtTick,
                ),
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
      modelShell: ParticipantModelShell[_, _],
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
