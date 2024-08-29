/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona

import edu.ie3.datamodel.io.sink.CsvFileSink
import edu.ie3.datamodel.io.source.TimeSeriesMappingSource.MappingEntry
import edu.ie3.datamodel.io.source.csv.CsvJointGridContainerSource

import java.nio.file.Path
import java.time.format.DateTimeFormatter
import java.time.{LocalDateTime, ZoneId, ZonedDateTime}
import java.util.UUID
import scala.jdk.CollectionConverters._

object TimeseriesBuilder {
  def main(args: Array[String]): Unit = {
    val dateTime = LocalDateTime.of(2016, 1, 1, 0, 0, 0)
    val start = ZonedDateTime.of(dateTime, ZoneId.of("UTC"))

    val loadString = load(start)
    val feedInNoCM = noCM(start)
    val feedInTwoPerDayString = twoPerDay(start)
    val feedInEachHour = eachHour(start)

    println("finished")
  }

  def load(start: ZonedDateTime): String = {
    val hours = (0 until 366 * 24 + 1).map(start.plusHours(_))

    val loadBuilder = new StringBuilder()
    hours
      .map(hour =>
        s"2.7,1.31,${hour.format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      .foreach(loadBuilder.append)
    loadBuilder.toString().replaceAll("\\[UTC]", "")
  }

  def noCM(start: ZonedDateTime): String = {
    val hours = (0 until 366 * 24 + 1).map(start.plusHours(_))
    val stringBuilder = new StringBuilder()
    hours
      .map(hour => s"0.0,${hour.format(DateTimeFormatter.ISO_DATE_TIME)}\n")
      .foreach(stringBuilder.append)
    stringBuilder.toString().replaceAll("\\[UTC]", "")
  }

  def twoPerDay(start: ZonedDateTime): String = {
    val stringBuilder = new StringBuilder()

    (0 until 366).map(start.plusDays(_)).foreach { day =>
      stringBuilder
        .append(s"0.0,${day.format(DateTimeFormatter.ISO_DATE_TIME)}\n")
      stringBuilder.append(
        s"0.0,${day.plusHours(1).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(2).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(3).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(4).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(5).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(6).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(7).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(8).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(9).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(10).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(11).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(12).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(13).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(14).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(15).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(16).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"-20.0,${day.plusHours(17).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(18).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(19).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(20).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(21).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(22).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
      stringBuilder.append(
        s"0.0,${day.plusHours(23).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      )
    }

    stringBuilder.append(
      s"0.0,${start.plusYears(1).format(DateTimeFormatter.ISO_DATE_TIME)}\n"
    )

    stringBuilder.toString().replaceAll("\\[UTC]", "")
  }

  def eachHour(start: ZonedDateTime): String = {
    val hours = (0 until 366 * 24 + 1).map(start.plusHours(_))

    val feedInBuilder = new StringBuilder()
    hours.zipWithIndex
      .map { case (hour, i) =>
        if (i % 2 == 0) {
          s"-20.0,${hour.format(DateTimeFormatter.ISO_DATE_TIME)}\n"
        } else s"5.0,${hour.format(DateTimeFormatter.ISO_DATE_TIME)}\n"
      }
      .foreach(feedInBuilder.append)

    feedInBuilder.toString().replaceAll("\\[UTC]", "")
  }

  def mapping(): Unit = {
    val feedInSeries = UUID.fromString("0ac0477b-cee6-4026-91c9-864d1c6871c9")
    val loadSeries = UUID.fromString("0c24f963-67b0-40c8-997e-0091fcfae00a")

    val jointGridContainer = CsvJointGridContainerSource.read(
      "grid",
      ",",
      Path.of(".", "input", "ma_thesis", "fullGrid"),
      false,
    )
    val participants = jointGridContainer.getSystemParticipants

    val loads = participants.getLoads.asScala.map(_.getUuid)
    val feedIns = participants.getFixedFeedIns.asScala.map(_.getUuid)

    val mappingEntries = loads.map(load =>
      new MappingEntry(load, loadSeries)
    ) ++ feedIns.map(feedIn => new MappingEntry(feedIn, feedInSeries))

    val sink = new CsvFileSink(
      Path.of(".", "input", "ma_thesis", "fullGrid", "primary-x")
    )
    sink.persistAll(mappingEntries.asJava)
  }
}
