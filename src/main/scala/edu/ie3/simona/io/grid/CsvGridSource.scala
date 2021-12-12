/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv._
import edu.ie3.datamodel.models.input._
import edu.ie3.datamodel.models.input.connector._
import edu.ie3.datamodel.models.input.container._
import edu.ie3.datamodel.models.input.graphics._
import edu.ie3.datamodel.models.input.system._

import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._

protected case object CsvGridSource {

  def apply(
      gridName: String,
      csvSep: String,
      baseFolder: String,
      fileNamingStrategy: FileNamingStrategy
  ): Option[JointGridContainer] = {
    readGrid(gridName, csvSep, baseFolder, fileNamingStrategy)
  }

  private def readGrid(
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
    val rawGridElements = csvRawGridSource.getGridData.toScala
    val systemParticipants =
      csvSystemParticipantSource.getSystemParticipants.toScala
    val graphicElements = csvGraphicSource.getGraphicElements.toScala

    (rawGridElements, systemParticipants, graphicElements) match {
      case (
            Some(rawGridElements),
            Some(systemParticipants),
            Some(graphicElements)
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

  /** Creates an empty grid without any element. Is only for intermediate state,
    * as the PowerSystemDataModel doesn't provide a csv reader, yet.
    *
    * @return
    *   An empty [[JointGridContainer]]
    */
  @deprecated("Use a proper reader instead.")
  def getEmptyGrid: JointGridContainer = {
    val rawGridElements = new RawGridElements(
      Set.empty[NodeInput].asJava,
      Set.empty[LineInput].asJava,
      Set.empty[Transformer2WInput].asJava,
      Set.empty[Transformer3WInput].asJava,
      Set.empty[SwitchInput].asJava,
      Set.empty[MeasurementUnitInput].asJava
    )
    val systemParticipants = new SystemParticipants(
      Set.empty[BmInput].asJava,
      Set.empty[ChpInput].asJava,
      Set.empty[EvcsInput].asJava,
      Set.empty[EvInput].asJava,
      Set.empty[FixedFeedInInput].asJava,
      Set.empty[HpInput].asJava,
      Set.empty[LoadInput].asJava,
      Set.empty[PvInput].asJava,
      Set.empty[StorageInput].asJava,
      Set.empty[WecInput].asJava
    )
    val graphicElements = new GraphicElements(
      Set.empty[NodeGraphicInput].asJava,
      Set.empty[LineGraphicInput].asJava
    )
    new JointGridContainer(
      "I'm so empty",
      rawGridElements,
      systemParticipants,
      graphicElements
    )
  }
}
