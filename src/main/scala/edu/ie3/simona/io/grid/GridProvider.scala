/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.io.grid

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.models.input.container.JointGridContainer
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.InitializationException

/** Takes [[edu.ie3.simona.config.SimonaConfig.Simona.Input.Grid.Datasource]] as
  * input and provides a [[JointGridContainer]] based on the configuration incl.
  * necessary sanity checks
  *
  * @version 0.1
  * @since 28.04.20
  */
object GridProvider {

  def gridFromConfig(
      simulationName: String,
      gridDataSource: SimonaConfig.Simona.Input.Grid.Datasource
  ): JointGridContainer = {

    GridSourceType(gridDataSource.id.toLowerCase) match {
      case GridSourceType.CSV =>
        gridDataSource.csvParams match {
          case Some(params) =>
            CsvGridSource(
              simulationName,
              params.csvSep,
              params.directoryPath,
              new FileNamingStrategy()
            ).getOrElse(
              throw new InitializationException(
                "Error while initializing CsvGridSource! Cannot proceed without a valid GridSource!"
              )
            )
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
}
