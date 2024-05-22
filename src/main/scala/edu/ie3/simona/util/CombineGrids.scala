/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.sink.CsvFileSink
import edu.ie3.datamodel.io.source.csv.CsvJointGridContainerSource
import edu.ie3.datamodel.models.input.container.{
  GraphicElements,
  JointGridContainer,
  RawGridElements,
  SystemParticipants,
}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble

import java.io.File
import java.nio.file.Path
import scala.jdk.CollectionConverters._

object CombineGrids {
  def main(args: Array[String]): Unit = {
    val input = Path.of(".", "input", "grids")
    val output = Path.of(".", "output", "grid")

    val files: List[File] = input.toFile.listFiles().toList

    files.foreach(println)

    val grids = files.map(file =>
      CsvJointGridContainerSource.read(file.getName, ";", file.toPath, false)
    )

    val combinedGrid = combine(grids(0), grids.drop(1))

    val sink: CsvFileSink =
      new CsvFileSink(output, new FileNamingStrategy(), ";")
    sink.persistJointGrid(combinedGrid)
  }

  def combine(
      first: JointGridContainer,
      grids: List[JointGridContainer],
  ): JointGridContainer = {

    val lvGrids: Seq[(RawGridElements, SystemParticipants)] = grids.map {
      grid =>
        val raw = grid.getRawGrid

        val nodes = raw.getNodes.asScala.filter(node =>
          node.getVoltLvl.getNominalVoltage.isEquivalentTo(0.4.asKiloVolt)
        )
        val lines =
          raw.getLines.asScala.filter(line => nodes.contains(line.getNodeA))

        val t2w =
          raw.getTransformer2Ws.asScala.filter(t => nodes.contains(t.getNodeB))
        val t3w =
          raw.getTransformer3Ws.asScala.filter(t => nodes.contains(t.getNodeB))

        val switches =
          raw.getSwitches.asScala.filter(s => nodes.contains(s.getNodeA))
        val measurementUnits = raw.getMeasurementUnits.asScala.filter(mU =>
          nodes.contains(mU.getNode)
        )

        val participants = grid.getSystemParticipants
          .allEntitiesAsList()
          .asScala
          .filter(p => p.allNodes().asScala.forall(nodes.contains))

        (
          new RawGridElements(
            nodes.asJava,
            lines.asJava,
            t2w.asJava,
            t3w.asJava,
            switches.asJava,
            measurementUnits.asJava,
          ),
          new SystemParticipants(participants.asJava),
        )
    }

    val rawGrids = lvGrids.map(_._1)
    val participants = lvGrids.map(_._2)

    val allRawGridElements = new RawGridElements(
      List(first.getRawGrid).appendedAll(rawGrids).asJava
    )
    val allParticipants = new SystemParticipants(
      List(first.getSystemParticipants).appendedAll(participants).asJava
    )

    new JointGridContainer(
      "",
      allRawGridElements,
      allParticipants,
      new GraphicElements(List.empty[GraphicElements].asJava),
    )
  }
}
