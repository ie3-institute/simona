/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.typed.Behavior
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}

import scala.concurrent.duration.DurationInt

/** todo
  */
object DelayedStopHelper {

  sealed trait StoppingMsg
      extends ResultEventListener.Incoming
      with RuntimeEventListener.Incoming

  /** Message indicating that [[RuntimeEventListener]] should stop. Instead of
    * using [[org.apache.pekko.actor.typed.scaladsl.ActorContext.stop()]], this
    * way of stopping allows all messages that have been queued before to be
    * processed. todo
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
