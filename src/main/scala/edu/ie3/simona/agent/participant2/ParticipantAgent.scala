/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.simona.agent.participant.data.Data.SecondaryData
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
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.util.scala.Scope
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

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
      parentData: Either[SchedulerData, FlexControlledData],
  ): Behavior[Request] =
    Behaviors.receivePartial {
      case (ctx, request: ParticipantRequest) =>
        val updatedShell = modelShell.handleRequest(ctx, request)

        ParticipantAgent(updatedShell, dataCore, parentData)

      case (ctx, activation: ActivationRequest) =>
        // handle issueControl differently?
        val updatedCore = dataCore.handleActivation(activation.tick)

        val updatedShell = if (dataCore.isComplete) {

          val receivedData = dataCore.getData.map {
            case data: SecondaryData => data
            case other =>
              throw new CriticalFailureException(
                s"Received unexpected data $other, should be secondary data"
              )
          }

          Scope(modelShell)
            .map(_.updateRelevantData(receivedData, activation.tick))
            .map { shell =>
              activation match {
                case ParticipantActivation(tick) =>
                  val modelWithOP = shell.updateOperatingPoint(tick)

                  // todo results

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
        } else
          modelShell

        ParticipantAgent(updatedShell, updatedCore, parentData)
    }

  private def primaryData(): Behavior[Request] = Behaviors.receivePartial {
    case _ => Behaviors.same
  }

}
