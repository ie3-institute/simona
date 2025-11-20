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
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  Create,
  ServiceRegistrationMessage,
  ServiceResponseMessage,
}
import edu.ie3.simona.ontology.messages.{
  Activation,
  SchedulerMessage,
  ServiceMessage,
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.Logger

import scala.util.{Failure, Success, Try}

/** Abstract description of a service agent, that is able to announce new
  * information to registered participants.
  */
abstract class SimonaService {

  /** Describes all messages this service can receive.
    */
  type Message >: ServiceMessage | Activation

  /** The service specific type of the [[ServiceStateData]].
    */
  type S <: ServiceBaseStateData

  def apply(
      scheduler: ActorRef[SchedulerMessage],
      bufferSize: Int = 10000,
  ): Behavior[Message] =
    Behaviors.withStash[Message](bufferSize) { buffer =>
      uninitialized(using scheduler, buffer)
    }

  /** Receive method that is used before the service is initialized. Represents
    * the state "Uninitialized".
    *
    * @return
    *   IdleInternal methods for the uninitialized state.
    */
  def uninitialized(using
      scheduler: ActorRef[SchedulerMessage],
      buffer: StashBuffer[Message],
  ): Behavior[Message] = Behaviors.receive {
    case (
          ctx,
          Create(
            initializeStateData: InitializeServiceStateData,
            unlockKey: ScheduleKey,
          ),
        ) =>
      scheduler ! ScheduleActivation(
        ctx.self,
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
  )(using
      scheduler: ActorRef[SchedulerMessage],
      buffer: StashBuffer[Message],
  ): Behavior[Message] = Behaviors.receive {
    case (ctx, Activation(INIT_SIM_TICK)) =>
      // init might take some time and could go wrong if invalid initialize service data is received
      // execute complete and unstash only if init is carried out successfully
      init(initializeStateData)(using ctx.log) match {
        case Success((serviceStateData, maybeNewTick)) =>
          scheduler ! Completion(
            ctx.self,
            maybeNewTick,
          )
          buffer.unstashAll(idle(using serviceStateData, scheduler))
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

    case (_, msg: Activation) =>
      buffer.stash(msg)
      Behaviors.same

    case (ctx, msg: ServiceResponseMessage) =>
      handleServiceResponse(msg)(using ctx)
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
    *   The state data of this service.
    * @return
    *   Default idleInternal method when the service is initialized.
    */
  final protected def idle(using
      stateData: S,
      scheduler: ActorRef[SchedulerMessage],
  ): Behavior[Message] = Behaviors.receive[Message] { case (ctx, msg) =>
    idleInternal
      .orElse(idleExternal)
      .applyOrElse((ctx, msg), unhandled.tupled)
  }

  private def idleInternal(using
      stateData: S,
      scheduler: ActorRef[SchedulerMessage],
  ): PartialFunction[(ActorContext[Message], Message), Behavior[Message]] = {
    // agent registration process
    case (ctx, registrationMsg: ServiceRegistrationMessage) =>
      /* Someone asks to register for information from the service */
      handleRegistrationRequest(registrationMsg)(using stateData, ctx) match {
        case Success(stateData) => idle(using stateData, scheduler)
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

    // activity start trigger for this service
    case (ctx, Activation(tick)) =>
      /* The scheduler sends out an activity start trigger. Announce new data to all registered recipients. */
      val (updatedStateData, maybeNextTick) =
        announceInformation(tick)(using stateData, ctx)

      maybeNextTick match {
        case Some(nextTick) if nextTick == tick =>
          // we need to do an additional activation of this service
          ctx.self ! Activation(tick)

          None

        case Some(nextTick) if nextTick == -1 =>
          // this indicated that no completion should be sent
          None

        case _ =>
          scheduler ! Completion(
            ctx.self,
            maybeNextTick,
          )
      }

      idle(using updatedStateData, scheduler)
  }

  private def unhandled
      : (ActorContext[Message], Message) => Behavior[Message] = {
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
  protected def idleExternal(using
      stateData: S,
      scheduler: ActorRef[SchedulerMessage],
  ): PartialFunction[(ActorContext[Message], Message), Behavior[
    Message
  ]] = PartialFunction.empty

  /** Initialize the concrete service implementation using the provided
    * initialization data. This method should perform all heavyweight tasks
    * before the actor becomes ready. The return values are a) the state data of
    * the initialized service and b) optional triggers that should be sent to
    * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
    * message that is sent in response to the trigger that is sent to start the
    * initialization process.
    *
    * @param initServiceData
    *   The data that should be used for initialization.
    * @param log
    *   The logger for logging.
    * @return
    *   The state data of this service and optional tick that should be included
    *   in the completion message.
    */
  def init(
      initServiceData: InitializeServiceStateData
  )(using log: Logger): Try[(S, Option[Long])]

  /** Handles some service response messages that are received during
    * initialization.
    * @param serviceResponse
    *   To handle.
    * @param ctx
    *   The actor context of this service.
    */
  protected def handleServiceResponse(
      serviceResponse: ServiceResponseMessage
  )(using
      ctx: ActorContext[Message]
  ): Unit = ctx.log.warn(
    s"This service (${ctx.self}) received an unhandled service response message during initialization. Msg: $serviceResponse"
  )

  /** Handle a request to register for information from this service.
    *
    * @param registrationMessage
    *   Registration message to handle.
    * @param serviceStateData
    *   Current state data of the actor.
    * @return
    *   The service stata data that should be used in the next state (normally
    *   with updated values).
    */
  protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(using
      serviceStateData: S,
      ctx: ActorContext[Message],
  ): Try[S]

  /** Send out the information to all registered recipients.
    *
    * @param tick
    *   The current tick.
    * @param serviceStateData
    *   The current state data of this service.
    * @return
    *   The service stata data that should be used in the next state (normally
    *   with updated values) together with an optional next activation tick that
    *   is used in response to the trigger that was sent to start this
    *   announcement.
    */
  protected def announceInformation(tick: Long)(using
      serviceStateData: S,
      ctx: ActorContext[Message],
  ): (S, Option[Long])

}
