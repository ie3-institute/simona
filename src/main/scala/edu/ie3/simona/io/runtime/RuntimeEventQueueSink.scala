/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.runtime

import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.io.runtime.RuntimeEventSink.RuntimeStats

import java.util.concurrent.BlockingQueue

/** Runtime event sink that appends all received events to a queue.
  * @param queue
  *   the queue that events are appended to
  */
final case class RuntimeEventQueueSink(queue: BlockingQueue[RuntimeEvent])
    extends RuntimeEventSink {

  override def handleRuntimeEvent(
      runtimeEvent: RuntimeEvent,
      runtimeStats: RuntimeStats,
  ): Unit =
    queue.put(runtimeEvent)

  override def close(): Unit = {
    // nothing to close
  }
}
