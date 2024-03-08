/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.runtime

import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.io.runtime.RuntimeEventSink.RuntimeStats

/** Runtime event sinks are handling runtime events. More than one sink can
  * exist in parallel.
  */
trait RuntimeEventSink {

  /** Handling of a [[RuntimeEvent]], e.g. performing an I/O operation to a file
    * or a database.
    *
    * @param runtimeEvent
    *   The runtime event that should be processed
    * @param runtimeStats
    *   The runtime statistical data
    */
  def handleRuntimeEvent(
      runtimeEvent: RuntimeEvent,
      runtimeStats: RuntimeStats,
  ): Unit

  /** Contains all cleanup operations before closing this sink. Should be
    * blocking to ensure that everything inside a buffer or similar is written
    * out.
    */
  def close(): Unit
}

object RuntimeEventSink {

  /** Runtime object that keeps track of statistics
    *
    * @param failedPowerFlows
    *   The number of failed power flow calculations so far
    */
  final case class RuntimeStats(
      failedPowerFlows: Int = 0
  )
}
