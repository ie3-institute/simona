/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config.util

import edu.ie3.simona.exceptions.InvalidConfigParameterException

import java.io.File

case object CsvConfigUtil {
  def checkCsvParams(
      csvParamsName: String,
      csvSep: String,
      folderPath: String
  ): Unit = {
    if (!(csvSep.equals(";") || csvSep.equals(",")))
      throw new InvalidConfigParameterException(
        s"The csvSep parameter '$csvSep' for '$csvParamsName' configuration is invalid! Please choose between ';' or ','!"
      )
    if (
      folderPath.isEmpty || !new File(folderPath)
        .exists() || new File(folderPath).isFile
    )
      throw new InvalidConfigParameterException(
        s"The provided folderPath for .csv-files '$folderPath' for '$csvParamsName' configuration is invalid! Please correct the path!"
      )
  }

}
