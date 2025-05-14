/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.logging

import org.apache.pekko.actor.{Actor, ActorSystem, ExtendedActorSystem}
import org.apache.pekko.event.{
  LogSource,
  LoggingAdapter,
  LoggingBus,
  LoggingFilter,
}
import edu.ie3.simona.actor.SimonaActorNaming

private[logging] trait SimonaLogging {
  this: Actor =>

  /** May be overwritten by implementation if the logging prefix agent name
    * should not be derived from the agents unique name in the agent system.
    *
    * @return
    */
  protected def actorName: String =
    SimonaActorNaming.actorName(self)
}

object SimonaLogging {

  private[logging] def createAdapter[T: LogSource, S](
      system: ActorSystem,
      state: () => S,
      logSource: T,
      agentName: String,
  ): LoggingAdapter = {
    val (str, clazz) = LogSource(logSource, system)
    SimonaBusLogging(
      system.eventStream,
      str,
      clazz,
      logFilter(system),
      () => fsmPrefix(agentName, state),
    )
  }

  private[logging] def createAdapter[T: LogSource](
      system: ActorSystem,
      logSource: T,
      agentName: String,
  ): LoggingAdapter = {
    val (str, clazz) = LogSource(logSource, system)
    SimonaBusLogging(
      system.eventStream,
      str,
      clazz,
      logFilter(system),
      () => actorPrefix(agentName),
    )
  }

  private def fsmPrefix[S]: (String, () => S) => String =
    (actorName, actorState) => s"[$actorName:${actorState.apply()}]"

  private val actorPrefix: String => String = actorName => s"[$actorName]"

  private val logFilter: ActorSystem => LoggingFilter = {
    case system: ExtendedActorSystem =>
      system.logFilter
    case _ =>
      throw new IllegalArgumentException(
        "Extended Actor System required to use SimonaLogging!"
      )
  }

  final case class SimonaBusLogging(
      bus: LoggingBus,
      logSource: String,
      logClass: Class[?],
      loggingFilter: LoggingFilter,
      prefix: () => String,
  ) extends LoggingAdapter {

    import org.apache.pekko.event.Logging.*

    def isErrorEnabled: Boolean =
      loggingFilter.isErrorEnabled(logClass, logSource)

    def isWarningEnabled: Boolean =
      loggingFilter.isWarningEnabled(logClass, logSource)

    def isInfoEnabled: Boolean =
      loggingFilter.isInfoEnabled(logClass, logSource)

    def isDebugEnabled: Boolean =
      loggingFilter.isDebugEnabled(logClass, logSource)

    protected def notifyError(message: String): Unit =
      bus.publish(Error(logSource, logClass, withPrefix(message), mdc))

    protected def notifyError(cause: Throwable, message: String): Unit =
      bus.publish(Error(cause, logSource, logClass, withPrefix(message), mdc))

    protected def notifyWarning(message: String): Unit =
      bus.publish(Warning(logSource, logClass, withPrefix(message), mdc))

    protected def notifyInfo(message: String): Unit =
      bus.publish(Info(logSource, logClass, withPrefix(message), mdc))

    protected def notifyDebug(message: String): Unit =
      bus.publish(Debug(logSource, logClass, withPrefix(message), mdc))

    private def withPrefix(msg: String) = s"${prefix.apply()} $msg"

  }
}
