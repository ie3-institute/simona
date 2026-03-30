package edu.ie3.simona.main

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.sink.CsvFileSink
import edu.ie3.datamodel.io.source.csv.CsvJointGridContainerSource
import edu.ie3.datamodel.models.input.container.{JointGridContainer, SystemParticipants}
import edu.ie3.datamodel.models.input.system.`type`.{HpTypeInput, StorageTypeInput}
import edu.ie3.datamodel.models.input.system.characteristic.ReactivePowerCharacteristic
import edu.ie3.datamodel.models.input.system.{HpInput, PvInput, StorageInput}
import edu.ie3.datamodel.models.input.thermal.{CylindricalStorageInput, ThermalBusInput}
import edu.ie3.datamodel.models.input.{EmInput, NodeInput}
import edu.ie3.util.quantities.QuantityUtils.*

import java.nio.file.Path
import java.util.UUID
import scala.jdk.CollectionConverters.{CollectionHasAsScala, SeqHasAsJava, SetHasAsScala}
import scala.util.Random

object GridBuilder {

  def main(args: Array[String]): Unit = {
    val input = Path.of("./simona/input/ReCoDE/1-LV-semiurb5--2-no_sw")
    val output = Path.of("./simona/output/ReCoDE/1-LV-semiurb5--2-no_sw")


    val grid = CsvJointGridContainerSource.read("_", ";", input, false)
    val sink = new CsvFileSink(output, new FileNamingStrategy(), ";")

    val nodes = grid.getRawGrid.getNodes.asScala.filterNot(n => n.isSlack).map { n => n.getUuid -> n }.toMap

    val nodeToEm = grid.getSystemParticipants.allEntitiesAsList().asScala.map { p =>
      p.getNode.getUuid -> p.getControllingEm.get()
    }.toMap


    val qCharacteristic = ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.0,1.0)}")

    val pvs = nodes.map { case (nodeUuid, node) =>
      val id = node.getId
      val em = nodeToEm(nodeUuid)

      val rnd = new Random()

      new PvInput(
        UUID.randomUUID(),
        s"Pv at node $id",
        node,
        qCharacteristic,
        em,
        0.20000000298023224,
        rnd.between(-90.0, 90.0).asDegreeGeom,
        95.0.asPercent,
        45.0.asDegreeGeom,
        0.8999999761581421,
        1.0,
        false,
        10.0.asKiloVoltAmpere,
        0.95
      )

    }.toSeq.asJava

    sink.persistAllIgnoreNested(pvs)

  }

  def generate(participants: SystemParticipants, nodes: Map[UUID, NodeInput], sink: CsvFileSink): Unit = {
    val supEm = new EmInput(
      UUID.randomUUID(),
      s"client0",
      "PROPORTIONAL",
      null
    )

    val nodeToEm = nodes.zipWithIndex.map { case ((nodUuid, node), idx) =>
      val em = new EmInput(
        UUID.randomUUID(),
        s"client${idx + 1}",
        "PROPORTIONAL",
        supEm
      )

      nodUuid -> em
    }.toMap

    val ffi = participants.getFixedFeedIns.asScala.map { p =>
      val node = p.getNode.getUuid
      p.copy().em(nodeToEm(node)).build()
    }.toSeq.asJava

    val loads = participants.getLoads.asScala.map { p =>
      val node = p.getNode.getUuid
      p.copy().em(nodeToEm(node)).build()
    }.toSeq.asJava

    val storageType = new StorageTypeInput(
      UUID.randomUUID(),
      "default_storage_type",
      0.0.asEuro,
      0.0.asEuroPerWattHour,
      16.0.asKiloWattHour,
      11.58.asKiloVoltAmpere,
      0.95,
      11.0.asKiloWatt,
      50.0.asPercentPerHour,
      95.0.asPercent
    )

    val hpType = new HpTypeInput(
      UUID.randomUUID(),
      "default_heat_pump_type",
      0.0.asEuro,
      0.0.asEuroPerWattHour,
      10.asKiloVoltAmpere,
      0.95,
      9.5.asKiloWatt
    )

    val qCharacteristic = ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.0,1.0)}")

    val newParticipants = nodes.flatMap { case (nodeUuid, node) =>
      val id = node.getId
      val em = nodeToEm(nodeUuid)

      val storage = new StorageInput(
        UUID.randomUUID(),
        s"Storage at node $id",
        node,
        qCharacteristic,
        em,
        storageType
      )

      val thermalBus = new ThermalBusInput(
        UUID.randomUUID(),
        s"Thermal bus at node $id"
      )

      val cylindricalStorage = new CylindricalStorageInput(
        UUID.randomUUID(),
        s"Cylindrical storage at node $id",
        thermalBus,
        100.0.asCubicMetre,
        30.0.asDegreeCelsius,
        40.0.asDegreeCelsius,
        1.15.asKiloWattHourPerKelvinTimesCubicMetre,
        50.0.asKiloWatt
      )

      val heatPump = new HpInput(
        UUID.randomUUID(),
        s"Heat pump at node $id",
        node,
        thermalBus,
        qCharacteristic,
        em,
        hpType
      )


      Seq(storage, cylindricalStorage, thermalBus, heatPump)
    }.toSeq.asJava


    sink.persistIgnoreNested(storageType)
    sink.persistIgnoreNested(hpType)

    sink.persistAllIgnoreNested(newParticipants)
    sink.persistAllIgnoreNested(loads)
    sink.persistAllIgnoreNested(ffi)
    sink.persistAllIgnoreNested(nodeToEm.values.toSeq.asJava)
  }

}
