/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import edu.ie3.simona.service.results.ExtResultDataProvider
import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import scala.concurrent.duration.DurationInt

/** Helper that provides functionality for delayed stopping of actors, i.e. upon
  * receiving [[FlushAndStop]], the actor is stopped after a fixed amount of
  * time after the last message has been received
  */
object DelayedStopHelper {

  /** Note: Needs to extend be message traits for actors that want to use this
    * functionality
    */
  sealed trait StoppingMsg
      extends ResultEventListener.Request
      with RuntimeEventListener.Request
      with ExtResultDataProvider.Request

  /** Message indicating that [[RuntimeEventListener]] should stop. Instead of
    * using [[org.apache.pekko.actor.typed.scaladsl.ActorContext.stop]], this
    * way of stopping allows the current mailbox to be processed, plus more
    * messages that are pending to be received.
    */
  case object FlushAndStop extends StoppingMsg

  private case object StopTimeout extends StoppingMsg

  def handleMsg[T >: StoppingMsg]
      : PartialFunction[(ActorContext[T], StoppingMsg), Behavior[T]] = {

    case (ctx, FlushAndStop) =>
      ctx.log.debug(
        s"Received FlushAndStop message, shutting down once no message has been received for 5 seconds."
      )
      ctx.setReceiveTimeout(5.seconds, StopTimeout)
      Behaviors.same

    case (ctx, StopTimeout) =>
      // there have been no messages for 5 seconds, let's end this
      ctx.log.debug(s"${getClass.getSimpleName} is now stopped.")
      Behaviors.stopped
  }

}
