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
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, PrimaryDataMeta}
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.ParticipantResultEvent
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

object ParticipantAgent {

  sealed trait Request

  /** Extended by all requests that activate an [[ParticipantAgent]], i.e.
    * activations, flex requests and control messages
    */
  private[participant2] sealed trait ActivationRequest extends Request {
    val tick: Long
  }

  /** Wrapper for an [[Activation]] for usage by an adapter. Activations can
    * only be received if this agent is not EM-controlled.
    *
    * @param tick
    *   The tick to activate
    */
  private[participant2] final case class ParticipantActivation(
      override val tick: Long
  ) extends ActivationRequest

  /** Wrapper for [[FlexRequest]] messages for usage by an adapter (if this
    * [[ParticipantAgent]] is EM-controlled itself)
    *
    * @param msg
    *   The wrapped flex request
    */
  private[participant2] final case class Flex(msg: FlexRequest)
      extends ActivationRequest {
    override val tick: Long = msg.tick
  }

  sealed trait RegistrationResponseMessage extends Request {
    val serviceRef: ClassicRef
  }

  /** Message, that is used to confirm a successful registration
    */
  final case class RegistrationSuccessfulMessage(
      override val serviceRef: ClassicRef,
      firstDataTick: Long,
  ) extends RegistrationResponseMessage

  /** Message, that is used to confirm a successful registration with primary
    * service
    */
  final case class PrimaryRegistrationSuccessfulMessage[
      P <: PrimaryData: ClassTag
  ](
      override val serviceRef: ClassicRef,
      firstDataTick: Long,
      primaryDataMeta: PrimaryDataMeta[P],
  ) extends RegistrationResponseMessage

  /** Message, that is used to announce a failed registration
    */
  final case class RegistrationFailedMessage(
      override val serviceRef: ClassicRef
  ) extends RegistrationResponseMessage

  /** @param tick
    * @param serviceRef
    * @param data
    * @param nextDataTick
    *   Next tick at which data could arrive. If None, no data is expected for
    *   the rest of the simulation
    *
    * @tparam D
    *
    * TODO this should suffice as secondary data provision message
    */
  final case class ProvideData[D <: Data](
      tick: Long,
      serviceRef: ClassicRef,
      data: D,
      nextDataTick: Option[Long],
  ) extends Request

  /** Request the power values for the requested tick from an AssetAgent and
    * provide the latest nodal voltage
    *
    * @param currentTick
    *   The tick that power values are requested for
    * @param eInPu
    *   Real part of the complex, dimensionless nodal voltage
    * @param fInPu
    *   Imaginary part of the complex, dimensionless nodal voltage
    */
  final case class RequestAssetPowerMessage(
      currentTick: Long,
      eInPu: Dimensionless,
      fInPu: Dimensionless,
  ) extends Request

  /** @param currentTick
    * @param nextRequestTick
    */
  final case class FinishParticipantSimulation(
      currentTick: Long,
      nextRequestTick: Long,
  ) extends Request

  /** The existence of this data object indicates that the corresponding agent
    * is not EM-controlled, but activated by a
    * [[edu.ie3.simona.scheduler.Scheduler]]
    *
    * @param scheduler
    *   The scheduler that is activating this agent
    * @param activationAdapter
    *   The activation adapter handling [[Activation]] messages
    */
  final case class SchedulerData(
      scheduler: ActorRef[SchedulerMessage],
      activationAdapter: ActorRef[Activation],
  )

  /** The existence of this data object indicates that the corresponding agent
    * is EM-controlled (by [[emAgent]]).
    *
    * @param emAgent
    *   The parent EmAgent that is controlling this agent.
    * @param flexAdapter
    *   The flex adapter handling [[FlexRequest]] messages
    * @param lastFlexOptions
    *   Last flex options that have been calculated for this agent.
    */
  final case class FlexControlledData(
      emAgent: ActorRef[FlexResponse],
      flexAdapter: ActorRef[FlexRequest],
      lastFlexOptions: Option[ProvideFlexOptions] = None,
  )

  /** A request to the participant agent that is not covered by the standard
    * ways of interacting with the agent
    */
  trait ParticipantRequest extends Request {

    /** The tick for which the request is valid, which is the current tick
      */
    val tick: Long
  }

  def apply(
      modelShell: ParticipantModelShell[_, _],
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
      resultListener: Iterable[ActorRef[ResultEvent]],
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
          resultListener,
          parentData,
        )

      case (_, activation: ActivationRequest) =>
        val coreWithActivation = inputHandler.handleActivation(activation)

        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            coreWithActivation,
            gridAdapter,
            resultListener,
            parentData,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultListener,
          parentData,
        )

      case (_, msg: ProvideData[Data]) =>
        val inputHandlerWithData = inputHandler.handleDataProvision(msg)

        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            inputHandlerWithData,
            gridAdapter,
            resultListener,
            parentData,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultListener,
          parentData,
        )

      case (ctx, RequestAssetPowerMessage(currentTick, eInPu, fInPu)) =>
        // we do not have to wait for the resulting power of the current tick,
        // since the current power is irrelevant for the average power up until now

        val activeToReactivePowerFunc = modelShell.activeToReactivePowerFunc

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
            Some(activeToReactivePowerFunc),
            ctx.log,
          )

        val result = updatedGridAdapter.avgPowerResult.getOrElse(
          throw new CriticalFailureException(
            "Power result has not been calculated"
          )
        )
        gridAdapter.gridAgent !
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
          resultListener,
          parentData,
        )

      case (_, FinishParticipantSimulation(_, nextRequestTick)) =>
        val gridAdapterFinished =
          gridAdapter.updateNextRequestTick(nextRequestTick)

        // Possibly start simulation if we've been activated
        val (updatedShell, updatedInputHandler, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            inputHandler,
            gridAdapterFinished,
            resultListener,
            parentData,
          )

        ParticipantAgent(
          updatedShell,
          updatedInputHandler,
          updatedGridAdapter,
          resultListener,
          parentData,
        )
    }

  private def maybeCalculate(
      modelShell: ParticipantModelShell[_, _],
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
      listener: Iterable[ActorRef[ResultEvent]],
      parentData: Either[SchedulerData, FlexControlledData],
  ): (
      ParticipantModelShell[_, _],
      ParticipantInputHandler,
      ParticipantGridAdapter,
  ) = {
    if (isReadyForCalculation(inputHandler, gridAdapter)) {

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
              val shellWithOP = shell.updateOperatingPoint(tick)

              val results =
                shellWithOP.determineResults(tick, gridAdapter.nodalVoltage)

              results.modelResults.foreach { res =>
                listener.foreach(_ ! ParticipantResultEvent(res))
              }

              val gridAdapterWithResult =
                gridAdapter.storePowerValue(results.totalPower, tick)

              val changeIndicator = shellWithOP.getChangeIndicator(
                tick,
                inputHandler.getNextActivationTick,
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
              val modelWithFlex = shell.updateFlexOptions(tick)

              parentData.fold(
                _ =>
                  throw new CriticalFailureException(
                    "Received flex activation while not controlled by EM"
                  ),
                _.emAgent ! modelWithFlex.flexOptions,
              )

              (modelWithFlex, gridAdapter)

            case Flex(flexControl: IssueFlexControl) =>
              val shellWithOP = shell.updateOperatingPoint(flexControl)

              val results =
                shellWithOP.determineResults(
                  flexControl.tick,
                  gridAdapter.nodalVoltage,
                )

              results.modelResults.foreach { res =>
                listener.foreach(_ ! ParticipantResultEvent(res))
              }

              val gridAdapterWithResult =
                gridAdapter.storePowerValue(
                  results.totalPower,
                  flexControl.tick,
                )

              val changeIndicator = shellWithOP.getChangeIndicator(
                flexControl.tick,
                inputHandler.getNextActivationTick,
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

      (updatedShell, inputHandler.completeActivity(), updatedGridAdapter)
    } else
      (modelShell, inputHandler, gridAdapter)
  }

  /** Checks if all requirements for calculation have been met. These are:
    *   - agent is activated (activation has been received and not completed
    *     yet)
    *   - all required data has been received
    *   - the grid adapter is not waiting for power requests
    *
    * @param inputHandler
    *   The participant input handler
    * @param gridAdapter
    *   The participant grid adapter
    * @return
    *   Whether power can be calculated or not.
    */
  private def isReadyForCalculation(
      inputHandler: ParticipantInputHandler,
      gridAdapter: ParticipantGridAdapter,
  ): Boolean = {
    inputHandler.isComplete &&
    inputHandler.activation.exists(activation =>
      !gridAdapter.isPowerRequestAwaited(activation.tick)
    )
  }

}
