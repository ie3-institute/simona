/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.{
  CsvJointGridContainerSource,
  CsvThermalGridSource,
}
import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  ThermalGrid,
}
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.datamodel.utils.validation.ValidationUtils
import edu.ie3.simona.config.SimonaConfig

import java.nio.file.Path
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

/** Takes [[edu.ie3.simona.config.SimonaConfig.Simona.Input.Grid.Datasource]] as
  * input and provides a [[JointGridContainer]] based on the configuration incl.
  * necessary sanity checks
  *
  * @version 0.1
  * @since 28.04.20
  */
object GridProvider extends LazyLogging {

  def gridFromConfig(
      simulationName: String,
      gridDataSource: SimonaConfig.Simona.Input.Grid.Datasource,
  ): JointGridContainer = {
    GridSourceType(gridDataSource.id.toLowerCase) match {
      case GridSourceType.CSV =>
        gridDataSource.csvParams match {
          case Some(params) =>
            val jointGridContainer = CsvJointGridContainerSource.read(
              simulationName,
              params.csvSep,
              Path.of(params.directoryPath),
              params.isHierarchic,
            )

            // checks the grid container and throws exception if there is an error
            ValidationUtils.check(jointGridContainer)

            jointGridContainer
          case None =>
            throw new RuntimeException(
              "CSVGridSource requires csv params to be set!"
            )
        }
      case GridSourceType.DB =>
        throw new NotImplementedError(
          "DatabaseGridSource is not implemented yet!"
        )
      case _ =>
        throw new RuntimeException(
          s"No provision of a GridDataSource is not allowed! Please choose from one of the following parameters ${GridSourceType.values
              .mkString(", ")}."
        )
    }
  }

  def getThermalGridsFromConfig(
      gridDataSource: SimonaConfig.Simona.Input.Grid.Datasource
  ): Map[ThermalBusInput, ThermalGrid] = GridSourceType(
    gridDataSource.id.toLowerCase
  ) match {
    case GridSourceType.CSV =>
      gridDataSource.csvParams match {
        case Some(params) =>
          CsvThermalGridSource
            .read(
              params.csvSep,
              Path.of(params.directoryPath),
              new FileNamingStrategy(),
            )
            .asScala
            .map(thermalGrid => thermalGrid.bus() -> thermalGrid)
            .toMap
        case None =>
          throw new RuntimeException(
            "CSVGridSource requires csv params to be set!"
          )
      }
    case GridSourceType.DB =>
      throw new NotImplementedError(
        "DatabaseGridSource is not implemented yet!"
      )
    case _ =>
      throw new RuntimeException(
        s"No provision of a GridDataSource is not allowed! Please choose from one of the following parameters ${GridSourceType.values
            .mkString(", ")}."
      )
  }
}
