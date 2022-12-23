/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.util.scala.io

import edu.ie3.datamodel.models.timeseries.individual.{IndividualTimeSeries, TimeBasedValue}
import edu.ie3.datamodel.models.value.PValue
import edu.ie3.simona.config.SimonaConfig.Simona.Runtime.RootEm
import edu.ie3.util.quantities.PowerSystemUnits
import org.apache.poi.ss.usermodel.CellType
import org.apache.poi.xssf.usermodel.XSSFWorkbook
import squants.energy.Megawatts
import tech.units.indriya.quantity.Quantities

import java.io.{File, FileInputStream}
import java.time.ZoneId
import java.util.UUID
import javax.measure
import javax.measure.quantity.Power
import scala.jdk.CollectionConverters.{IterableHasAsScala, IteratorHasAsScala, SetHasAsJava}
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try, Using}

object FlexSignalFromExcel {

  /** Gets flexibility signals from an Excel file with defined format.
    * @param filePath
    *   Path to the excel file
    * @param nodeId
    *   The node id, i.e. name of the sheet
    * @param timeSeriesType
    *   The time series type
    * @param unit
    *   Physical unit to set for values
    * @param zoneId
    *   Time zone of the incorporated date times
    * @return
    *   A trial to the resulting time series
    */
  def flexSignals(
      filePath: String,
      nodeId: String,
      timeSeriesType: TimeSeriesType.Value = TimeSeriesType.ResidualLoad,
      unit: measure.Unit[Power] = PowerSystemUnits.MEGAWATT,
      zoneId: ZoneId = ZoneId.of("UTC")
  ): Try[IndividualTimeSeries[PValue]] = {
    Using {
      val file = new File(filePath)
      val fileStream = new FileInputStream(file)
      new XSSFWorkbook(fileStream)
    } { workbook =>
      val sheet = workbook.getSheet(nodeId)

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

          val tsTypeToValue = colToTimeseriesType.map { case (col, tsType) =>
            // negate the flex signal to get residual power
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

      tsTypeToTs(timeSeriesType)
    }
  }

  def getCorrespondingMinMaxValues(
      timeSeriesType: TimeSeriesType.Value,
      timeSeries: IndividualTimeSeries[PValue],
      config: RootEm
  ): (squants.Power, squants.Power) = {

    // todo this is very use case dependant and has to be reworked
    /* instead of using the residual load we take the total res load to determine min and max
     values for threshold calculation as this also includes self oriented reference behavior of Simona*/
    val minMaxTs = if (timeSeriesType == TimeSeriesType.ResidualLoad) {
      FlexSignalFromExcel
        .flexSignals(
          config.filePath,
          config.nodeId,
          TimeSeriesType.TotalResLoad
        ) match {
        case Success(timeSeries) => timeSeries
        case Failure(exception)  => throw exception
      }
    } else timeSeries
    val allValues =
      minMaxTs.getEntries.asScala.flatMap(_.getValue.getP.toScala)
    val maybeMinValue = allValues.minByOption(
      _.to(PowerSystemUnits.MEGAWATT).getValue.doubleValue
    )
    val maybeMaxValue = allValues.maxByOption(
      _.to(PowerSystemUnits.MEGAWATT).getValue.doubleValue
    )

    val (minValue, maxValue) = maybeMinValue
      .zip(maybeMaxValue)
      .getOrElse(
        throw new RuntimeException(s"Time series for $config is empty")
      )
    (Megawatts(minValue.getValue.doubleValue()), Megawatts(maxValue.getValue.doubleValue()))
  }

  object TimeSeriesType extends Enumeration {
    val Generation, Load, OtherGeneration, OtherLoad, Import, ImportIntern,
        ResidualLoad, TotalResLoad, SimonaGeneration, SimonaLoad = Value

    def apply(token: String): TimeSeriesType.Value = {
      token match {
        case "gen"                 => Generation
        case "loads"               => Load
        case "otherGen"            => OtherGeneration
        case "otherLoads"          => OtherLoad
        case "importExport"        => Import
        case "importExport_intern" => ImportIntern
        case "resLoad"             => ResidualLoad
        case "totalResLoad"        => TotalResLoad
        case "SIMONA_gen"          => SimonaGeneration
        case "SIMONA_load"         => SimonaLoad
        case _ =>
          throw new IllegalArgumentException(s"Don't know the token '$token'.")
      }
    }
  }
}
