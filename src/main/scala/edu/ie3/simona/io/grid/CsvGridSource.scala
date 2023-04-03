/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv._
import edu.ie3.datamodel.models.input.container._
import edu.ie3.datamodel.models.input.thermal.{
  ThermalBusInput,
  ThermalHouseInput,
  ThermalStorageInput
}

import scala.jdk.CollectionConverters._
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

  def readThermalGrids(
      csvSep: String,
      baseFolder: String,
      fileNamingStrategy: FileNamingStrategy
  ): Map[ThermalBusInput, ThermalGrid] = {
    val csvTypeSource: CsvTypeSource =
      new CsvTypeSource(csvSep, baseFolder, fileNamingStrategy)
    val csvThermalSource: CsvThermalSource = new CsvThermalSource(
      csvSep,
      baseFolder,
      fileNamingStrategy,
      csvTypeSource
    )
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
