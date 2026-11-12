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
import edu.ie3.simona.ontology.messages.ServiceMessage.ServiceRegistrationMessage
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
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.Logger
import squants.Time

import scala.collection.immutable.SortedMap
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
      initializeStateData: InitializeServiceStateData,
      scheduleKey: ScheduleKey,
  ): Behavior[Message] = Behaviors.setup { ctx =>
    init(initializeStateData)(using ctx.log) match {
      case Success((serviceStateData, maybeNewTick)) =>
        maybeNewTick match {
          case Some(newTick) =>
            scheduler ! ScheduleActivation(
              ctx.self,
              newTick,
              Some(scheduleKey),
            )
          case None =>
            scheduleKey.unlock()
        }
        idle(using serviceStateData, scheduler)
      case Failure(exception) =>
        scheduleKey.unlock()

        // if a service fails startup we don't want to go on with the simulation
        throw new CriticalFailureException(
          "Error during service initialization.",
          exception,
        )
    }
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
    idleExternal
      .orElse(idleInternal)
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

        case Some(nextTick) if nextTick == -1 =>
        // this indicated that no completion should be sent
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
      _scheduler: ActorRef[SchedulerMessage],
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

  /** Reduces the resolution of given time series to at least given resolution
    * by removing elements from the time series.
    *
    * @param timeSeries
    *   The time series to adapt.
    * @param resolution
    *   The time resolution to aim for.
    * @tparam T
    *   The type of time series data.
    * @return
    *   The adapted time series.
    */
  def reduceTimeSeriesResolution[T](
      timeSeries: SortedMap[Long, T],
      resolution: Time,
  ): SortedMap[Long, T] = {
    val resolutionSeconds = resolution.toSeconds.toLong

    timeSeries.foldLeft(timeSeries) { case (result, (tick, _)) =>
      result.maxBefore(tick) match {
        case Some((last, _)) =>
          if last + resolutionSeconds > tick then
            // interval from last to current key is too short
            result.removed(tick)
          else result
        case None =>
          // no data before the current key, keep it
          result
      }
    }
  }

}
