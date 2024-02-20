/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

import org.apache.pekko.actor.typed.{Behavior, PostStop}
import org.apache.pekko.actor.typed.scaladsl.Behaviors
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.PowerFlowFailed
import edu.ie3.simona.io.runtime.RuntimeEventSink.RuntimeStats
import edu.ie3.simona.io.runtime.{
  RuntimeEventKafkaSink,
  RuntimeEventLogSink,
  RuntimeEventQueueSink,
  RuntimeEventSink,
}
import edu.ie3.util.TimeUtil

import java.util.concurrent.BlockingQueue

/** Listener that handles all runtime events. Multiple sinks can be configured
  * in parallel. By default, a [[RuntimeEventLogSink]] is initialized and used
  * for every RuntimeEventListener.
  */
object RuntimeEventListener {

  trait Incoming

  /** Message indicating that [[RuntimeEventListener]] should stop. Instead of
    * using [[org.apache.pekko.actor.typed.scaladsl.ActorContext.stop()]], this
    * way of stopping allows all messages that have been queued before to be
    * processed.
    */
  case object Stop extends Incoming

  /** Creates a runtime event listener behavior with given configuration.
    *
    * @param listenerConf
    *   configuration that determines additional sinks and event filters
    * @param queue
    *   if present, received events are appended to this queue
    * @param startDateTimeString
    *   the simulation start time
    * @return
    *   the [[RuntimeEventListener]] behavior
    */
  def apply(
      listenerConf: SimonaConfig.Simona.Runtime.Listener,
      queue: Option[BlockingQueue[RuntimeEvent]],
      startDateTimeString: String,
  ): Behavior[Incoming] = Behaviors.setup { ctx =>
    val listeners = Iterable(
      Some(
        RuntimeEventLogSink(
          TimeUtil.withDefaults.toZonedDateTime(startDateTimeString),
          ctx.log,
        )
      ),
      queue.map(qu => RuntimeEventQueueSink(qu)),
      listenerConf.kafka.map(kafkaConf =>
        RuntimeEventKafkaSink(kafkaConf, ctx.log)
      ),
    ).flatten

    apply(
      listeners,
      listenerConf.eventsToProcess,
    )
  }

  private def apply(
      listeners: Iterable[RuntimeEventSink],
      eventsToProcess: Option[List[String]] = None,
      runtimeStats: RuntimeStats = RuntimeStats(),
  ): Behavior[Incoming] = Behaviors
    .receive[Incoming] {
      case (_, PowerFlowFailed) =>
        val updatedRuntimeData = runtimeStats
          .copy(failedPowerFlows = runtimeStats.failedPowerFlows + 1)
        RuntimeEventListener(listeners, eventsToProcess, updatedRuntimeData)

      case (ctx, event: RuntimeEvent) =>
        val process = eventsToProcess.forall(_.contains(event.id))

        if (process)
          processEvent(listeners, event, runtimeStats)
        else
          ctx.log.debug(
            "Skipping event {} as it is not in the list of events to process.",
            event.id,
          )
        Behaviors.same

      case (ctx, Stop) =>
        ctx.log.debug("")
        Behaviors.stopped
    }
    .receiveSignal { case (_, PostStop) =>
      listeners.foreach(_.close())
      Behaviors.same
    }

  private def processEvent(
      listeners: Iterable[RuntimeEventSink],
      event: RuntimeEvent,
      runtimeStats: RuntimeStats,
  ): Unit =
    listeners.foreach(_.handleRuntimeEvent(event, runtimeStats))

}
