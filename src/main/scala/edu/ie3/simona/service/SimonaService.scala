/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service

import edu.ie3.simona.logging.SimonaActorLogging
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ScheduleServiceActivation,
  ServiceRegistrationMessage,
}
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.SimonaService.Create
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{Actor, ActorContext, ActorRef, Stash}

import scala.util.{Failure, Success, Try}

object SimonaService {

  /** Service initialization data can sometimes only be constructed once the
    * service actor is created (e.g.
    * [[edu.ie3.simona.service.ev.ExtEvDataService]]). Thus, we need an extra
    * initialization message.
    */
  final case class Create[+I <: InitializeServiceStateData](
      initializeStateData: I,
      unlockKey: ScheduleKey,
  )
}

/** Abstract description of a service agent, that is able to announce new
  * information to registered participants
  *
  * @param scheduler
  *   actor reference of the scheduler
  * @tparam S
  *   the service specific type of the [[ServiceStateData]]
  */
abstract class SimonaService[
    S <: ServiceBaseStateData
](protected val scheduler: ActorRef)
    extends Actor
    with Stash
    with SimonaActorLogging {

  override def receive: Receive = uninitialized

  /** Receive method that is used before the service is initialized. Represents
    * the state "Uninitialized".
    *
    * @return
    *   idleInternal methods for the uninitialized state
    */
  private def uninitialized: Receive = {

    case Create(
          initializeStateData: InitializeServiceStateData,
          unlockKey: ScheduleKey,
        ) =>
      scheduler ! ScheduleActivation(
        self.toTyped,
        INIT_SIM_TICK,
        Some(unlockKey),
      )

      context become initializing(initializeStateData)

    // not ready yet to handle registrations, stash request away
    case _: ServiceRegistrationMessage =>
      stash()

  }

  private def initializing(
      initializeStateData: InitializeServiceStateData
  ): Receive = {

    case Activation(INIT_SIM_TICK) =>
      // init might take some time and could go wrong if invalid initialize service data is received
      // execute complete and unstash only if init is carried out successfully
      init(
        initializeStateData
      ) match {
        case Success((serviceStateData, maybeNewTick)) =>
          scheduler ! Completion(self.toTyped, maybeNewTick)
          unstashAll()
          context become idle(serviceStateData)
        case Failure(exception) =>
          // initialize service trigger with invalid data
          log.error(
            "Error during service initialization." +
              s"\nReceivedData: {}" +
              s"\nException: {}",
            initializeStateData,
            exception,
          )
          throw exception // if a service fails startup we don't want to go on with the simulation
      }

    // not ready yet to handle registrations, stash request away
    case _: ServiceRegistrationMessage | _: Activation =>
      stash()

    // unhandled message
    case x =>
      log.error(s"Received unhandled message: $x")
      unhandled(x)

  }

  /** Default receive method when the service is initialized. Requires the
    * actual state data of this service to be ready to be used.
    *
    * @param stateData
    *   the state data of this service
    * @return
    *   default idleInternal method when the service is initialized
    */
  final protected def idle(implicit stateData: S): Receive =
    idleExternal.applyOrElse(_, idleInternal(stateData))

  private def idleInternal(implicit stateData: S): Receive = {
    // agent registration process
    case registrationMsg: ServiceRegistrationMessage =>
      /* Someone asks to register for information from the service */
      handleRegistrationRequest(registrationMsg) match {
        case Success(stateData) => context become idle(stateData)
        case Failure(exception) =>
          log.error(
            "Error during registration." +
              "\nMsg: {}" +
              "\nException: {}",
            registrationMsg,
            exception,
          )
          unhandled(registrationMsg)
      }

    case ScheduleServiceActivation(tick, unlockKey) =>
      scheduler ! ScheduleActivation(
        self.toTyped,
        tick,
        Some(unlockKey),
      )

    // activity start trigger for this service
    case Activation(tick) =>
      /* The scheduler sends out an activity start trigger. Announce new data to all registered recipients. */
      val (updatedStateData, maybeNewTriggers) =
        announceInformation(tick)(stateData, context)
      scheduler ! Completion(self.toTyped, maybeNewTriggers)
      context become idle(updatedStateData)

    // unhandled message
    case x =>
      log.error("Unhandled message received:{}", x)
      unhandled(x)
  }

  /** Internal api method that allows handling incoming messages from external
    * simulations
    *
    * @param stateData
    *   the state data of this service
    * @return
    *   empty behavior to ensure it only is called if it is overridden
    */
  private[service] def idleExternal(implicit stateData: S): Receive =
    Actor.emptyBehavior

  /** Initialize the concrete service implementation using the provided
    * initialization data. This method should perform all heavyweight tasks
    * before the actor becomes ready. The return values are a) the state data of
    * the initialized service and b) optional triggers that should be sent to
    * the [[edu.ie3.simona.scheduler.Scheduler]] together with the completion
    * message that is sent in response to the trigger that is sent to start the
    * initialization process
    *
    * @param initServiceData
    *   the data that should be used for initialization
    * @return
    *   the state data of this service and optional tick that should be included
    *   in the completion message
    */
  def init(
      initServiceData: InitializeServiceStateData
  ): Try[(S, Option[Long])]

  /** Handle a request to register for information from this service
    *
    * @param registrationMessage
    *   registration message to handle
    * @param serviceStateData
    *   current state data of the actor
    * @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values)
    */
  protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(implicit
      serviceStateData: S
  ): Try[S]

  /** Send out the information to all registered recipients
    *
    * @param tick
    *   current tick data should be announced for
    * @param serviceStateData
    *   the current state data of this service
    * @return
    *   the service stata data that should be used in the next state (normally
    *   with updated values) together with the completion message that is sent
    *   in response to the trigger that was sent to start this announcement
    */
  protected def announceInformation(tick: Long)(implicit
      serviceStateData: S,
      ctx: ActorContext,
  ): (S, Option[Long])

}
