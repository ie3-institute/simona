/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.simona.exceptions.ProcessResultEventException

/** Trait that should be mixed into each sink in [[edu.ie3.simona.io.result]] to
  * allow an easy calling of different sinks e.g. csv-file sink or database sink
  */
trait ResultEntitySink {

  /** Handling of a [[ResultEntity]] to perform an I/O operation to e.g. a file
    * or a database Should not be blocking but using futures internally to speed
    * up persisting
    *
    * @param resultEntity
    *   the result entity that should be processed
    * @return
    *   a future holding information about the handling process
    */
  @throws(classOf[ProcessResultEventException])
  def handleResultEntity(resultEntity: ResultEntity): Unit

  /** Contains all cleanup operations before closing this sink. Should be
    * blocking to ensure that everything inside a buffer or similar is written
    * out.
    */
  def close(): Unit

}
