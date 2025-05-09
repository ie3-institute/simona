/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  ScheduleServiceActivation,
  ServiceRegistrationMessage,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
  ServiceConstantStateData,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

/** Abstract description of a service agent, that is able to announce new
  * information to registered participants
  *
  * @tparam T
  *   The type of messages this service accepts
  */
abstract class SimonaService[
    T >: ServiceMessage
] {

  /** The service specific type of the [[ServiceStateData]]
    */
  type S <: ServiceBaseStateData

  def apply(
      scheduler: ActorRef[SchedulerMessage],
      bufferSize: Int = 10000,
  ): Behavior[T] = Behaviors.withStash(bufferSize) { buffer =>
    Behaviors.setup { ctx =>
      val activationAdapter: ActorRef[Activation] =
        ctx.messageAdapter[Activation](msg => WrappedActivation(msg))

      val constantData: ServiceConstantStateData =
        ServiceConstantStateData(
          scheduler,
          activationAdapter,
        )

      uninitialized(constantData, buffer)
    }
  }

  /** Receive method that is used before the service is initialized. Represents
    * the state "Uninitialized".
    *
    * @return
    *   IdleInternal methods for the uninitialized state
    */
  def uninitialized(implicit
      constantData: ServiceConstantStateData,
      buffer: StashBuffer[T],
  ): Behavior[T] = Behaviors.receive {
    case (
          _,
          Create(
            initializeStateData: InitializeServiceStateData,
            unlockKey: ScheduleKey,
          ),
        ) =>
      constantData.scheduler ! ScheduleActivation(
        constantData.activationAdapter,
        INIT_SIM_TICK,
        Some(unlockKey),
      )

      initializing(initializeStateData)

    // not ready yet to handle registrations, stash request away
    case (_, msg: ServiceRegistrationMessage) =>
      buffer.stash(msg)
      Behaviors.same

  }

  private def initializing(
      initializeStateData: InitializeServiceStateData
  )(implicit
      constantData: ServiceConstantStateData,
      buffer: StashBuffer[T],
  ): Behavior[T] = Behaviors.receive {
    case (ctx, WrappedActivation(Activation(INIT_SIM_TICK))) =>
      // init might take some time and could go wrong if invalid initialize service data is received
      // execute complete and unstash only if init is carried out successfully
      init(initializeStateData) match {
        case Success((serviceStateData, maybeNewTick)) =>
          constantData.scheduler ! Completion(
            constantData.activationAdapter,
            maybeNewTick,
          )
          buffer.unstashAll(idle(serviceStateData, constantData))
        case Failure(exception) =>
          // initialize service trigger with invalid data
          ctx.log.error(
            "Error during service initialization." +
              s"\nReceivedData: {}" +
              s"\nException: {}",
            initializeStateData,
            exception,
          )

          // if a service fails startup we don't want to go on with the simulation
          throw new CriticalFailureException(
            "Error during service initialization.",
            exception,
          )
      }

    // not ready yet to handle registrations, stash request away
    case (_, msg: ServiceRegistrationMessage) =>
      buffer.stash(msg)
      Behaviors.same

    case (_, msg: WrappedActivation) =>
      buffer.stash(msg)
      Behaviors.same

    // unhandled message
    case (ctx, x) =>
      ctx.log.error(s"Received unhandled message: $x")
      Behaviors.unhandled
  }

  /** Default receive method when the service is initialized. Requires the
    * actual state data of this service to be ready to be used.
    *
    * @param stateData
    *   The state data of this service
    * @return
    *   Default idleInternal method when the service is initialized
    */
  final protected def idle(implicit
      stateData: S,
      constantData: ServiceConstantStateData,
  ): Behavior[T] = Behaviors.receive[T] { case (ctx, msg) =>
    idleInternal
      .orElse(idleExternal)
      .applyOrElse((ctx, msg), unhandled.tupled)
  }

  private def idleInternal(implicit
      stateData: S,
      constantData: ServiceConstantStateData,
  ): PartialFunction[(ActorContext[T], T), Behavior[T]] = {
    // agent registration process
    case (ctx, registrationMsg: ServiceRegistrationMessage) =>
      /* Someone asks to register for information from the service */
      handleRegistrationRequest(registrationMsg)(stateData, ctx) match {
        case Success(stateData) => idle(stateData, constantData)
        case Failure(exception) =>
          ctx.log.error(
            "Error during registration." +
              "\nMsg: {}" +
              "\nException: {}",
            registrationMsg,
            exception,
          )

          throw new CriticalFailureException(
            "Error during registration.",
            exception,
          )
      }

    case (_, ScheduleServiceActivation(tick, unlockKey)) =>
      constantData.scheduler ! ScheduleActivation(
        constantData.activationAdapter,
        tick,
        Some(unlockKey),
      )

      idle

    // activity start trigger for this service
    case (ctx, WrappedActivation(Activation(tick))) =>
      /* The scheduler sends out an activity start trigger. Announce new data to all registered recipients. */
      val (updatedStateData, maybeNextTick) =
        announceInformation(tick)(stateData, ctx)

      constantData.scheduler ! Completion(
        constantData.activationAdapter,
        maybeNextTick,
      )

      idle(updatedStateData, constantData)
  }

  private def unhandled: (ActorContext[T], T) => Behavior[T] = {
    case (ctx, msg) =>
      ctx.log.error("Unhandled message received:{}", msg)
      Behaviors.unhandled
  }

  /** Internal api method that allows handling incoming messages from external
    * simulations.
    *
    * @param stateData
    *   The state data of this service.
    * @return
    *   Empty partial function as default. To override, extend
    *   [[ExtDataSupport]].
    */
  protected def idleExternal(implicit
      stateData: S,
      constantData: ServiceConstantStateData,
  ): PartialFunction[(ActorContext[T], T), Behavior[T]] = PartialFunction.empty

  /** Initialize the concrete service implementation using the provided
    * initialization data. This method should perform all heavyweight tasks
    * before the actor becomes ready. The return values are a) the state data of
    * the initialized service and b) optional triggers that should be sent to
    * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
    * message that is sent in response to the trigger that is sent to start the
    * initialization process
    *
    * @param initServiceData
    *   The data that should be used for initialization
    * @return
    *   The state data of this service and optional tick that should be included
    *   in the completion message
    */
  def init(
      initServiceData: InitializeServiceStateData
  ): Try[(S, Option[Long])]

  /** Handle a request to register for information from this service
    *
    * @param registrationMessage
    *   Registration message to handle
    * @param serviceStateData
    *   Current state data of the actor
    * @return
    *   The service stata data that should be used in the next state (normally
    *   with updated values)
    */
  protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(implicit
      serviceStateData: S,
      ctx: ActorContext[T],
  ): Try[S]

  /** Send out the information to all registered recipients
    *
    * @param tick
    *   Current tick data should be announced for
    * @param serviceStateData
    *   The current state data of this service
    * @return
    *   The service stata data that should be used in the next state (normally
    *   with updated values) together with the completion message that is sent
    *   in response to the trigger that was sent to start this announcement
    */
  protected def announceInformation(tick: Long)(implicit
      serviceStateData: S,
      ctx: ActorContext[T],
  ): (S, Option[Long])

}
