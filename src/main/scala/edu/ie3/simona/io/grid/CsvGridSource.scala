/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source._
import edu.ie3.datamodel.io.source.csv._
import edu.ie3.datamodel.models.input.container._
import edu.ie3.datamodel.models.input.thermal.{ThermalBusInput, ThermalHouseInput, ThermalStorageInput}

import java.nio.file.Path
import scala.jdk.CollectionConverters._

object CsvGridSource{
  def readGrid(
      gridName: String,
      csvSep: String,
      baseFolder: Path,
      fileNamingStrategy: FileNamingStrategy
  ): Option[JointGridContainer] = {
    val csvDataSource =
      new CsvDataSource(csvSep, baseFolder, fileNamingStrategy)
    // build the sources
    val csvTypeSource: TypeSource = new TypeSource(csvDataSource)

    val csvRawGridSource: RawGridSource =
      new RawGridSource(csvTypeSource, csvDataSource)

    val csvThermalSource: ThermalSource =
      new ThermalSource(csvTypeSource, csvDataSource)

    val csvSystemParticipantSource: SystemParticipantSource =
      new SystemParticipantSource(
        csvTypeSource,
        csvThermalSource,
        csvRawGridSource,
        csvDataSource
      )

    val csvGraphicSource: GraphicSource =
      new GraphicSource(csvTypeSource, csvRawGridSource, csvDataSource)

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

  def readThermalGrids(
      csvSep: String,
      baseFolder: Path,
      fileNamingStrategy: FileNamingStrategy
  ): Map[ThermalBusInput, ThermalGrid] = {
    val csvDataSource =
      new CsvDataSource(csvSep, baseFolder, fileNamingStrategy)
    val csvTypeSource: TypeSource =
      new TypeSource(csvDataSource)
    val csvThermalSource: ThermalSource =
      new ThermalSource(csvTypeSource, csvDataSource)
    val operators = csvTypeSource.getOperators
    val busses = csvThermalSource.getThermalBuses()
    val houses = csvThermalSource
      .getThermalHouses(operators, busses)
      .asScala
      .groupBy(thermalHouse => thermalHouse.getThermalBus)
      .map { case (bus, houses) =>
        bus -> houses.toSet
      }
    val storages = csvThermalSource
      .getThermalStorages(operators, busses)
      .asScala
      .groupBy(thermalStorage => thermalStorage.getThermalBus)
      .map { case (bus, storages) =>
        bus -> storages.toSet
      }

    busses.asScala.map { bus =>
      val h: java.util.Collection[ThermalHouseInput] =
        houses.getOrElse(bus, Set.empty[ThermalHouseInput]).toSeq.asJava
      val s: java.util.Collection[ThermalStorageInput] =
        storages.getOrElse(bus, Set.empty[ThermalStorageInput]).toSeq.asJava
      bus -> new ThermalGrid(
        bus,
        h,
        s
      )
    }.toMap
  }
}
