/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.ResultEntitySource
import edu.ie3.datamodel.io.source.csv.{
  CsvDataSource,
  CsvJointGridContainerSource,
}
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.LineResult

import java.io.{File, FileOutputStream, OutputStreamWriter}
import java.nio.file.Path
import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.CollectionHasAsScala

object ResultFilter {
  def main(args: Array[String]): Unit = {
    val grid = Path.of("input", "grid")
    val jointGrid = CsvJointGridContainerSource.read("grid", ";", grid, false)
    val results = Path.of("output", "grid")

    val source = new CsvDataSource(",", results, new FileNamingStrategy())
    val resultSource = new ResultEntitySource(source)

    val nodeSubnetRes = results.resolve("subnetVoltages.csv").toFile
    write("time,subnet,v_max,v_min", nodeSubnetRes, append = false)

    val lineSubnetRes = results.resolve("subnetLineUtilisation.csv").toFile
    write("time,subnet,max", lineSubnetRes, append = false)

    val nodeToSubnet = jointGrid.getRawGrid.getNodes.asScala
      .map(n => n.getUuid -> n.getSubnet)
      .toMap

    val lineToSubnet = jointGrid.getRawGrid.getLines.asScala.map { l =>
      l.getUuid -> l.getNodeA.getSubnet
    }.toMap

    subnetVoltages(nodeToSubnet, resultSource, nodeSubnetRes)
    subnetLineUtilisation(lineToSubnet, resultSource, lineSubnetRes)
  }

  private def subnetVoltages(
      nodeToSubnet: Map[UUID, Int],
      resultSource: ResultEntitySource,
      file: File,
  ): Unit = {
    val results: Map[ZonedDateTime, Iterable[NodeResult]] =
      resultSource.getNodeResults.asScala
        .map(res => res.getTime -> res)
        .groupBy(_._1)
        .map { case (time, tuples) => time -> tuples.map(_._2) }

    results.foreach { case (time, results) =>
      println(time)

      results
        .map(res =>
          nodeToSubnet(res.getInputModel) -> res
            .getvMag()
            .getValue
            .doubleValue()
        )
        .groupBy(_._1)
        .map { case (i, tuples) =>
          val set = tuples.map(_._2).toSet
          val str = s"$time,$i,${set.max},${set.min}"

          write(str, file)
        }
    }
  }

  private def subnetLineUtilisation(
      lineToSubnet: Map[UUID, Int],
      resultSource: ResultEntitySource,
      file: File,
  ): Unit = {
    val results: Map[ZonedDateTime, Iterable[LineResult]] =
      resultSource.getLineResults.asScala
        .map(res => res.getTime -> res)
        .groupBy(_._1)
        .map { case (time, tuples) => time -> tuples.map(_._2) }

    results.foreach { case (time, results) =>
      println(time)

      results
        .map(res =>
          lineToSubnet(res.getInputModel) -> Math.max(
            res.getiAMag().getValue.doubleValue(),
            res.getiBMag().getValue.doubleValue(),
          )
        )
        .groupBy(_._1)
        .map { case (i, tuples) =>
          val set = tuples.map(_._2).toSet
          val str = s"$time,$i,${set.max}"

          write(str, file)
        }
    }
  }

  def write(str: String, subnetVoltages: File, append: Boolean = true): Unit = {
    val writer = new OutputStreamWriter(
      new FileOutputStream(subnetVoltages, append)
    )
    writer.write(str)
    writer.close()
  }
}
