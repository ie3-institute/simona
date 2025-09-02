/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.io

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.sink.CsvFileSink
import edu.ie3.datamodel.io.source.{GraphicSource, RawGridSource, TypeSource}
import edu.ie3.datamodel.io.source.csv.CsvDataSource

import java.nio.file.Paths

/** When manually deleting lines and nodes, their graphic information won't get
  * updated. This causes issues when reading in the grid afterwards. This script
  * cleans the node graphic and line graphic elements file by reading it in and
  * writing out an updated version with graphic elements only for the existing,
  * non-deleted nodes and lines
  */
object GraphicDataCleaner {

  def main(args: Array[String]): Unit = {

    /* config params */
    val csvSep = ","
    val baseFolder = Paths.get("")
    val targetFolder = Paths.get("")
    val fileNamingStrategy = new FileNamingStrategy()

    /* setup */
    val dataSource: CsvDataSource =
      new CsvDataSource(csvSep, baseFolder, fileNamingStrategy)

    val csvTypeSource: TypeSource =
      new TypeSource(dataSource)

    val csvRawGridSource: RawGridSource = new RawGridSource(
      csvTypeSource,
      dataSource,
    )

    val csvGraphicSource: GraphicSource = new GraphicSource(
      csvTypeSource,
      csvRawGridSource,
      dataSource,
    )

    /* read - by default, the csvGraphicSource only returns valid, that means elements with all
     * dependant objects they rely on resolved, entities. Hence, just reading and writing does the job. */

    val lineGraphicElements = csvGraphicSource.getLineGraphicInput
    val nodeGraphicElements = csvGraphicSource.getNodeGraphicInput

    /* write */
    val csvFileSink = new CsvFileSink(targetFolder)

    csvFileSink.persistAll(lineGraphicElements)
    csvFileSink.persistAll(nodeGraphicElements)

    /* shutdown */
    csvFileSink.shutdown()

  }
}
