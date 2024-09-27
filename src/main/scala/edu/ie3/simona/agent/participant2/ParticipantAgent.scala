/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import breeze.numerics.{pow, sqrt}
import edu.ie3.simona.agent.grid.GridAgentMessages.AssetPowerChangedMessage
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  FlexActivation,
  FlexCompletion,
  FlexRequest,
  FlexResponse,
  IssueFlexControl,
  ProvideFlexOptions,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ProvisionMessage
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.util.scala.Scope
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}
import squants.{Dimensionless, Each}

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
      nextDataTick: Long,
  ) extends RegistrationResponseMessage

  /** Message, that is used to announce a failed registration
    */
  final case class RegistrationFailedMessage(
      override val serviceRef: ClassicRef
  ) extends RegistrationResponseMessage

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
    val tick: Long
  }

  def apply(
      modelShell: ParticipantModelShell[_, _, _],
      dataCore: ParticipantDataCore,
      gridAdapter: ParticipantGridAdapter,
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] =
    Behaviors.receivePartial {
      case (ctx, request: ParticipantRequest) =>
        val updatedShell = modelShell.handleRequest(ctx, request)

        ParticipantAgent(updatedShell, dataCore, gridAdapter, parentData)

      case (ctx, activation: ActivationRequest) =>
        val coreWithActivation = dataCore.handleActivation(activation)

        val (updatedShell, updatedCore, updatedGridAdapter) =
          maybeCalculate(
            modelShell,
            coreWithActivation,
            gridAdapter,
            parentData,
          )

        ParticipantAgent(
          updatedShell,
          updatedCore,
          updatedGridAdapter,
          parentData,
        )

      case (ctx, msg: ProvisionMessage[Data]) =>
        val coreWithData = dataCore.handleDataProvision(msg)

        val (updatedShell, updatedCore, updatedGridAdapter) =
          maybeCalculate(modelShell, coreWithData, gridAdapter, parentData)

        ParticipantAgent(
          updatedShell,
          updatedCore,
          updatedGridAdapter,
          parentData,
        )

      case (ctx, RequestAssetPowerMessage(currentTick, eInPu, fInPu)) =>
        val activeToReactivePowerFunc = modelShell.activeToReactivePowerFunc

        val nodalVoltage = Each(
          sqrt(
            pow(eInPu.toEach, 2) +
              pow(fInPu.toEach, 2)
          )
        )

        val updatedGridAdapter = gridAdapter
          .updateNodalVoltage(nodalVoltage)
          .updateAveragePower(
            currentTick,
            Some(activeToReactivePowerFunc),
            ctx.log,
          )

        val avgPower = updatedGridAdapter.avgPowerCache.get
        gridAdapter.gridAgent ! AssetPowerChangedMessage(avgPower.p, avgPower.q)

        ParticipantAgent(
          modelShell,
          dataCore,
          updatedGridAdapter,
          parentData,
        )
    }

  private def maybeCalculate(
      modelShell: ParticipantModelShell[_, _, _],
      dataCore: ParticipantDataCore,
      gridAdapter: ParticipantGridAdapter,
      parentData: Either[SchedulerData, FlexControlledData],
  ): (
      ParticipantModelShell[_, _, _],
      ParticipantDataCore,
      ParticipantGridAdapter,
  ) = {
    if (dataCore.isComplete) {

      val activation = dataCore.activation.getOrElse(
        throw new CriticalFailureException(
          "Activation should be present when data collection is complete"
        )
      )

      val receivedData = dataCore.getData.map {
        case data: SecondaryData => data
        case other =>
          throw new CriticalFailureException(
            s"Received unexpected data $other, should be secondary data"
          )
      }

      val updatedShell = Scope(modelShell)
        .map(_.updateRelevantData(receivedData, activation.tick))
        .map { shell =>
          activation match {
            case ParticipantActivation(tick) =>
              val modelWithOP = shell.updateOperatingPoint(tick)

              if (!gridAdapter.isPowerRequestExpected(tick)) {
                // we don't expect a power request that could change voltage,
                // so we can go ahead and calculate results
                val results = shell.determineResults(tick, ???)

              }

              parentData.fold(
                schedulerData =>
                  schedulerData.scheduler ! Completion(
                    schedulerData.activationAdapter,
                    modelWithOP.modelChange.changesAtTick,
                  ),
                _ =>
                  throw new CriticalFailureException(
                    "Received activation while controlled by EM"
                  ),
              )
              modelWithOP

            case Flex(FlexActivation(tick)) =>
              val modelWithFlex = shell.updateFlexOptions(tick)

              parentData.fold(
                _ =>
                  throw new CriticalFailureException(
                    "Received flex activation while not controlled by EM"
                  ),
                _.emAgent ! modelWithFlex.flexOptions,
              )

              modelWithFlex

            case Flex(flexControl: IssueFlexControl) =>
              val modelWithOP = shell.updateOperatingPoint(flexControl)

              // todo results

              parentData.fold(
                _ =>
                  throw new CriticalFailureException(
                    "Received issue flex control while not controlled by EM"
                  ),
                _.emAgent ! FlexCompletion(
                  shell.model.uuid,
                  shell.modelChange.changesAtNextActivation,
                  shell.modelChange.changesAtTick,
                ),
              )

              modelWithOP
          }
        }
        .get

      (updatedShell, dataCore.completeActivity(), ???)
    } else
      (modelShell, dataCore, gridAdapter)
  }

  private def primaryData(): Behavior[Request] = Behaviors.receivePartial {
    case _ => Behaviors.same
  }

}
