/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.result

import edu.ie3.datamodel.io.connectors.InfluxDbConnector
import edu.ie3.datamodel.io.sink.InfluxDbSink
import edu.ie3.datamodel.models.result.ResultEntity

import scala.concurrent.Future

/** Sink for [[ResultEntity]] s that can be used to write results into an influx
  * database
  *
  * @param influxDbSink
  *   instance of [[InfluxDbSink]] to be used
  */
final case class ResultEntityInfluxDbSink private (influxDbSink: InfluxDbSink)
    extends ResultEntitySink {

  /** Handling of a [[ResultEntity]] to perform an I/O operation to e.g. a file
    * or a database
    *
    * @param resultEntity
    *   the result entity that should be processed
    * @return
    *   a future holding information about the handling process
    */
  override def handleResultEntity(
      resultEntity: ResultEntity
  ): Unit =
    // this call is by default non-blocking as the underlying influxDbSink
    // uses a non-blocking library to access the db
    // wrapping this call inside a try might slow down things a lot
    // maybe this needs to be addressed in the future
    influxDbSink.persist(resultEntity)

  /** Contains all cleanup operations before closing this sink
    */
  override def close(): Unit = influxDbSink.shutdown()
}

object ResultEntityInfluxDbSink {

  def apply(
      databaseUrl: String,
      databaseName: String,
      scenarioName: String,
  ): Future[ResultEntityInfluxDbSink] =
    Future.successful(
      new ResultEntityInfluxDbSink(
        new InfluxDbSink(
          new InfluxDbConnector(databaseUrl, databaseName, scenarioName)
        )
      )
    )

}
