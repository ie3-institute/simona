/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.{ThermalSource, TypeSource}
import edu.ie3.datamodel.io.source.csv._
import edu.ie3.datamodel.models.input.container._
import edu.ie3.datamodel.models.input.thermal.{
  ThermalBusInput,
  ThermalHouseInput,
  ThermalStorageInput,
}

import java.nio.file.Path
import scala.jdk.CollectionConverters._

object CsvGridSource {
  def readThermalGrids(
      csvSep: String,
      baseFolder: Path,
      fileNamingStrategy: FileNamingStrategy,
  ): Map[ThermalBusInput, ThermalGrid] = {
    val dataSource =
      new CsvDataSource(csvSep, baseFolder, fileNamingStrategy)
    val typeSource = new TypeSource(dataSource)
    val thermalSource = new ThermalSource(
      typeSource,
      dataSource,
    )

    val operators = typeSource.getOperators
    val busses = thermalSource.getThermalBuses()
    val houses = thermalSource
      .getThermalHouses(operators, busses)
      .asScala
      .groupBy(thermalHouse => thermalHouse.getThermalBus)
      .map { case (bus, houses) =>
        bus -> houses.toSet
      }
    val storages = thermalSource
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
        s,
      )
    }.toMap
  }
}
