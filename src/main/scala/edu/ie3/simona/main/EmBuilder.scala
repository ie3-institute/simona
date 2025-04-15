package edu.ie3.simona.main

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.sink.CsvFileSink
import edu.ie3.datamodel.io.source.csv.CsvDataSource
import edu.ie3.datamodel.io.source._
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.{EmInput, OperatorInput}

import java.nio.file.Path
import java.util.UUID
import scala.jdk.CollectionConverters.{MapHasAsScala, SetHasAsJava, SetHasAsScala}

object EmBuilder {

  def main(args: Array[String]): Unit = {
    val path = Path.of("simona", "input", "fullGrid")

    val csvSource = new CsvDataSource(";", path, new FileNamingStrategy())

    val typeSource = new TypeSource(csvSource)
    val gridSource = new RawGridSource(typeSource, csvSource)
    val emSource = new EnergyManagementSource(typeSource, csvSource)
    val thermalSource = new ThermalSource(typeSource, csvSource)
    val participantSource = new SystemParticipantSource(typeSource, thermalSource, gridSource, emSource, csvSource)

    val (_, otherNodes) = gridSource.getNodes.asScala.toMap.partition { case (_, node) => node.isSlack}

    val emSup = new EmInput(
      UUID.randomUUID(),
      "EM_Scada",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      "PROPORTIONAL",
      null
    )


    val ems = otherNodes.map { case (_, node) =>
      node -> new EmInput(
        UUID.randomUUID(),
        s"Em_${node.getId}",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        "PROPORTIONAL",
        emSup
      )
    }

    val allEms = ems.values.toSet ++ Set(emSup)

    val fixedFeedIns = participantSource.getFixedFeedIns.asScala.map { ffi =>
      val node = ffi.getNode
      ffi.copy().em(ems(node)).build()
    }

    val loads = participantSource.getLoads.asScala.map { load =>
      val node = load.getNode
      load.copy().em(ems(node)).build()
    }

    val sink = new CsvFileSink(path.resolve("withEm"), new FileNamingStrategy(), ";")

    sink.persistAllIgnoreNested(allEms.asJava)
    sink.persistAllIgnoreNested(fixedFeedIns.asJava)
    sink.persistAllIgnoreNested(loads.asJava)
  }
}
