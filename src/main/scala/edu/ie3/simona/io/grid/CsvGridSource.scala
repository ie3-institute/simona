/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv._
import edu.ie3.datamodel.models.input.container._

import scala.jdk.OptionConverters._

object CsvGridSource {

  def readGrid(
      gridName: String,
      csvSep: String,
      baseFolder: String,
      fileNamingStrategy: FileNamingStrategy
  ): Option[JointGridContainer] = {

    // build the sources
    val csvTypeSource: CsvTypeSource =
      new CsvTypeSource(csvSep, baseFolder, fileNamingStrategy)
    val csvRawGridSource: CsvRawGridSource = new CsvRawGridSource(
      csvSep,
      baseFolder,
      fileNamingStrategy,
      csvTypeSource
    )
    val csvThermalSource: CsvThermalSource = new CsvThermalSource(
      csvSep,
      baseFolder,
      fileNamingStrategy,
      csvTypeSource
    )
    val csvSystemParticipantSource: CsvSystemParticipantSource =
      new CsvSystemParticipantSource(
        csvSep,
        baseFolder,
        fileNamingStrategy,
        csvTypeSource,
        csvThermalSource,
        csvRawGridSource
      )
    val csvGraphicSource: CsvGraphicSource = new CsvGraphicSource(
      csvSep,
      baseFolder,
      fileNamingStrategy,
      csvTypeSource,
      csvRawGridSource
    )

    // read and get the models
    val rawGridElements = csvRawGridSource.getGridData
    val systemParticipants =
      csvSystemParticipantSource.getSystemParticipants
    val graphicElements = csvGraphicSource.getGraphicElements

    (rawGridElements, systemParticipants, graphicElements) match {
      case (
            rawGridElements,
            systemParticipants,
            graphicElements
          ) =>
        Some(
          new JointGridContainer(
            gridName,
            rawGridElements,
            systemParticipants,
            graphicElements
          )
        )
      case (_, _, _) => None
    }
  }

}
