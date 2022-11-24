/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.io

import edu.ie3.datamodel.models.timeseries.individual.{
  IndividualTimeSeries,
  TimeBasedValue
}
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.util.quantities.PowerSystemUnits
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import tech.units.indriya.quantity.Quantities

import java.io.{File, FileInputStream}
import java.time.ZoneId
import java.util.UUID
import javax.measure
import javax.measure.quantity.Power
import scala.jdk.CollectionConverters.{IteratorHasAsScala, SetHasAsJava}
import scala.util.{Try, Using}

object FlexSignalFromExcel {

  /** Gets flexibility signals from an Excel file with defined format.
    * @param filePath
    *   Path to the excel file
    * @param unit
    *   Physical unit to set for values
    * @param zoneId
    *   Time zone of the incorporated date times
    * @return
    *   A trial to a mapping from nodeId to [[TimeSeriesType]] to individual
    *   time series
    */
  def flexSignals(
      filePath: String,
      unit: measure.Unit[Power] = PowerSystemUnits.MEGAWATT,
      zoneId: ZoneId = ZoneId.of("UTC")
  ): Try[
    Map[String, Map[TimeSeriesType.Value, IndividualTimeSeries[PValue]]]
  ] = {
    Using {
      val file = new File(filePath)
      val fileStream = new FileInputStream(file)
      new XSSFWorkbook(fileStream)
    } { workbook =>
      workbook
        .sheetIterator()
        .asScala
        .map { sheet =>
          val nodeId = sheet.getSheetName

          val rows = sheet.rowIterator().asScala
          /* Assess the table header */
          val header =
            rows.next().cellIterator().asScala.toList.map(_.getStringCellValue)
          val dateTimeIndex = header.indexOf("time")
          val colToTimeseriesType = header.zipWithIndex
            .filterNot(_._1 == "time")
            .map { case (token, idx) =>
              idx -> TimeSeriesType(token)
            }
            .toMap

          /* Go through the file and parse the contents */
          val values = rows.toList
            .filterNot(_.getCell(0).getCellType == CellType.BLANK)
            .flatMap { row =>
              val dateTime = row
                .getCell(dateTimeIndex)
                .getLocalDateTimeCellValue
                .atZone(zoneId)

              val tsTypeToValue = colToTimeseriesType.map {
                case (col, tsType) =>
                  val raw = -row.getCell(col).getNumericCellValue
                  val value = new PValue(
                    Quantities.getQuantity(raw, unit)
                  )
                  val timeBasedValue =
                    new TimeBasedValue[PValue](dateTime, value)
                  tsType -> timeBasedValue
              }
              tsTypeToValue
            }
            .toList
            .groupBy(_._1)
            .map { case (tsType, collection) =>
              tsType -> collection.map(_._2).toSet
            }

          /* Finally process the single value lists into individual time series */
          val tsTypeToTs = values.map { case (tsType, valueSet) =>
            tsType -> new IndividualTimeSeries[PValue](
              UUID.randomUUID(),
              valueSet.asJava
            )
          }

          nodeId -> tsTypeToTs
        }
        .toMap
    }
  }

  object TimeSeriesType extends Enumeration {
    val Generation, Load, OtherGeneration, OtherLoad, Import, ImportIntern,
        ResidualLoad, SimonaGeneration, SimonaLoad = Value

    def apply(token: String): TimeSeriesType.Value = {
      token match {
        case "gen"                 => Generation
        case "loads"               => Load
        case "otherGen"            => OtherGeneration
        case "otherLoads"          => OtherLoad
        case "importExport"        => Import
        case "importExport_intern" => ImportIntern
        case "resLoad"             => ResidualLoad
        case "SIMONA_gen"          => SimonaGeneration
        case "SIMONA_load"         => SimonaLoad
        case _ =>
          throw new IllegalArgumentException(s"Don't know the token '$token'.")
      }
    }
  }
}
