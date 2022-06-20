/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.runtime

import edu.ie3.simona.event.RuntimeEvent
import org.slf4j.Logger

/** Runtime event sinks are handling runtime events. More than one sink can
  * exist in parallel.
  */
trait RuntimeEventSink {

  /** Handling of a [[RuntimeEvent]], e.g. performing an I/O operation to a file
    * or a database.
    *
    * @param runtimeEvent
    *   the runtime event that should be processed
    * @param log
    *   the logger to use
    */
  def handleRuntimeEvent(runtimeEvent: RuntimeEvent, log: Logger): Unit

  /** Contains all cleanup operations before closing this sink. Should be
    * blocking to ensure that everything inside a buffer or similar is written
    * out.
    */
  def close(): Unit
}
