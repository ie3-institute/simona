/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity
import edu.ie3.datamodel.models.input.connector.{
  CableDeploymentInput,
  LineInput,
}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.CigreT880LandCable33kV
import com.typesafe.config.ConfigFactory
import org.locationtech.jts.geom.{Coordinate as JtsCoordinate, GeometryFactory}
import org.locationtech.jts.linearref.LengthIndexedLine
import org.mockito.Mockito.{mock, when}
import org.scalatest.matchers.should.Matchers
import squants.Meters
import squants.thermal.Celsius
import squants.space.Length
import edu.ie3.util.scala.quantities.{
  KelvinMetersPerWatt,
  JoulesPerCubicMeterKelvin,
}
import java.nio.file.{Files, Path}
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

class ThermalSegmentBuilderSpec extends UnitSpec with Matchers {
  private val geometryFactory = new GeometryFactory()
  private val lineUuid = UUID.randomUUID()
  private val soilTypeUuid = UUID.randomUUID()
  private val lineInput: LineInput = {
    val li = mock(classOf[LineInput])
    when(li.getUuid).thenReturn(lineUuid)
    when(li.getId).thenReturn("testLine")
    li
  }
  private val cable = CigreT880LandCable33kV.cable
  private val limitTemperature = Celsius(90d)
  private val soilType = SoilType(
    soilTypeUuid,
    "testLoam",
    KelvinMetersPerWatt(0.5), // wet
    KelvinMetersPerWatt(2.5), // dry
    JoulesPerCubicMeterKelvin(1.5e6),
    Celsius(35d),
  )
  private val soilTypes: Seq[SoilType] = Seq(soilType)

  private def rectangle(
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double,
  ) = {
    val coords = Array(
      new JtsCoordinate(xMin, yMin),
      new JtsCoordinate(xMax, yMin),
      new JtsCoordinate(xMax, yMax),
      new JtsCoordinate(xMin, yMax),
      new JtsCoordinate(xMin, yMin),
    )
    geometryFactory.createPolygon(coords)
  }

  private def run(
      coordinates: Seq[(Double, Double)],
      cableDepth: Length,
      soilLayers: Seq[SoilLayer],
      neighbourLines: Seq[ThermalSegmentBuilder.NeighbourLineInfo] = Seq.empty,
      ppThreshold: Option[Double] = None,
  ): (
      Set[LineSegmentThermalModel],
      ListBuffer[ThermalSegmentBuilder.BuiltSegment],
  ) = {
    val generatedSegments = ListBuffer.empty[ThermalSegmentBuilder.BuiltSegment]
    val warnedPairs = scala.collection.mutable.Set.empty[(UUID, UUID)]
    val context = ThermalSegmentBuilder.SegmentBuildContext(
      lineInput,
      cable,
      coordinates,
      cableDepth,
      Meters(0.5),
      soilLayers,
      soilTypes,
      limitTemperature,
      generatedSegments,
      neighbourLines,
      ppThreshold,
      warnedPairs,
    )
    val segments = ThermalSegmentBuilder.splitAndAssignSegments(context)
    (segments, generatedSegments)
  }
  private def makeSegment(): ThermalSegmentBuilder.BuiltSegment = {
    val layer = SoilLayer(
      UUID.randomUUID(),
      rectangle(0.0, -0.1, 1.0, 0.1),
      Meters(0.0),
      Meters(-2.0),
      soilTypeUuid,
    )
    val (_, generated) = run(
      Seq((0.0, 0.0), (1.0, 0.0)),
      Meters(-1.0),
      Seq(layer),
    )
    generated.head
  }
  private def makeSegmentWithInfluence(
      influence: Seq[(UUID, Double)]
  ): ThermalSegmentBuilder.BuiltSegment = {
    val built = makeSegment()
    built.copy(segment = built.segment.copy(parallelInfluence = influence))
  }
  private def buildTestConfig(outputDir: String): SimonaConfig = {
    val config = ConfigFactory.parseString(
      s"""
        |simona.simulationName = "TestSim"
        |
        |simona.time.startDateTime = "2011-05-01T00:00:00Z"
        |simona.time.endDateTime = "2011-05-01T01:00:00Z"
        |
        |simona.input.grid.datasource.id = "csv"
        |simona.input.grid.datasource.csvParams = {
        |  directoryPath: "input/samples/vn_simona/fullGrid"
        |  isHierarchic: false
        |  csvSep: ","
        |}
        |
        |simona.output.base.dir = "$outputDir"
        |simona.output.sink.csv.csvSep = ","
        |simona.powerflow.newtonraphson.epsilon = [1E-12]
        |""".stripMargin
    )
    SimonaConfig(config)
  }
  "ThermalSegmentBuilder.splitAndAssignSegments" should {
    "split a straight segment at the boundary of two horizontally adjacent soil layers" in {
      val leftLayer = SoilLayer(
        UUID.randomUUID(),
        rectangle(0.0, -0.1, 0.5, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val rightLayer = SoilLayer(
        UUID.randomUUID(),
        rectangle(0.5, -0.1, 1.0, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, generated) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(leftLayer, rightLayer),
      )

      segments.size shouldBe 2
      generated.size shouldBe 2

      val sorted = generated.sortBy(_.start.longitude)
      sorted.head.start.longitude shouldBe 0.0
      sorted.head.end.longitude shouldBe 0.5
      sorted.last.start.longitude shouldBe 0.5
      sorted.last.end.longitude shouldBe 1.0
      generated.forall(_.segment.lineUuid == lineUuid) shouldBe true
    }

    "produce a single segment when one soil layer covers the entire straight segment" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-0.5, -0.1, 1.5, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
      )

      segments.size shouldBe 1
      val segment = segments.head

      segment.pointA.longitude shouldBe 0.0
      segment.pointA.latitude shouldBe 0.0
      segment.pointB.longitude shouldBe 1.0
      segment.pointB.latitude shouldBe 0.0
      segment.pointA.height shouldBe -1.0
      segment.pointB.height shouldBe -1.0
      segment.soilType.thermalResistivityWet.value shouldBe 0.5
      segment.soilType.specificHeatCapacity.value shouldBe 1.5e6
    }

    "split a line at its original support points" in {
      // One layer covering both straight parts; the line has three support
      // points, so two straight segments must be produced.
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(0.0, -0.1, 2.0, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, generated) = run(
        Seq((0.0, 0.0), (1.0, 0.0), (2.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
      )

      segments.size shouldBe 2
      generated.size shouldBe 2
    }

    "throw an exception if no soil layer covers a subsegment midpoint" in {
      // The layer is far away from the segment, so its midpoint is uncovered.
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(5.0, 5.0, 6.0, 6.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      an[GridAgentInitializationException] should be thrownBy {
        run(
          Seq((0.0, 0.0), (1.0, 0.0)),
          Meters(-1.0),
          Seq(layer),
        )
      }
    }

    "throw an exception if the cable depth is not covered vertically" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(0.0, -0.1, 1.0, 0.1),
        Meters(0.0),
        Meters(-0.5), // only covers [0, -0.5], cable is at -1.0 m
        soilTypeUuid,
      )

      an[GridAgentInitializationException] should be thrownBy {
        run(
          Seq((0.0, 0.0), (1.0, 0.0)),
          Meters(-1.0),
          Seq(layer),
        )
      }
    }

    "fill the per-segment model with real endpoints and soil parameters" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(0.0, -0.1, 1.0, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
      )

      segments.size shouldBe 1
      val segment = segments.head

      segment.pointA.longitude shouldBe 0.0
      segment.pointA.latitude shouldBe 0.0
      segment.pointB.longitude shouldBe 1.0
      segment.pointB.latitude shouldBe 0.0
      segment.pointA.height shouldBe -1.0
      segment.pointB.height shouldBe -1.0
      segment.soilType.thermalResistivityWet.value shouldBe 0.5
      segment.soilType.specificHeatCapacity.value shouldBe 1.5e6
    }

    "return no segments for lines with fewer than two support points" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(0.0, -0.1, 1.0, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, generated) = run(
        Seq((0.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
      )

      segments shouldBe empty
      generated shouldBe empty
    }

    "split a candidate line at a parallel neighbour within threshold and mark the middle segment" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 3
      val markedSegments = segments.filter(_.parallelInfluence.nonEmpty)
      markedSegments.size shouldBe 1
      val influence = markedSegments.head.parallelInfluence
      influence.exists(_._1 == neighbourUuid) shouldBe true
      val dist =
        influence
          .find(_._1 == neighbourUuid)
          .map(_._2)
          .getOrElse(fail("Neighbour UUID not found in influence list"))
      dist should be < 10.0
    }

    "split at a parallel neighbour from a different subgrid identically" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.2, 0.00004), (0.8, 0.00004)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 3
      val markedSegments = segments.filter(_.parallelInfluence.nonEmpty)
      markedSegments.size shouldBe 1
      val influence = markedSegments.head.parallelInfluence
      influence.map(_._1) should contain(neighbourUuid)
    }

    "produce no extra split when the neighbour is outside the threshold" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.0005), (0.7, 0.0005)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 1
      segments.forall(_.parallelInfluence.isEmpty) shouldBe true
    }

    "mark a crossing neighbour without distinguishing from parallel" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.5, -0.0005), (0.5, 0.0005)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      val markedSegments = segments.filter(_.parallelInfluence.nonEmpty)
      markedSegments should not be empty
      val allNeighbourUuids =
        markedSegments.flatMap(_.parallelInfluence).map(_._1).toSet
      allNeighbourUuids should contain(neighbourUuid)
    }

    "skip a parallel stretch shorter than the minimum overlap and produce no mark" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-0.0005, -0.0005, 0.0005, 0.0005),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.00004, 0.000005), (0.00006, 0.000005)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (0.0001, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(1.0),
      )

      segments.size shouldBe 1
      segments.forall(_.parallelInfluence.isEmpty) shouldBe true
    }

    "produce identical segments when the parallel-proximity flag is disabled" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = None,
      )

      segments.size shouldBe 1
      segments.forall(_.parallelInfluence.isEmpty) shouldBe true
    }

    "produce no split or mark for an undeployed neighbour within threshold" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = false,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 1
      segments.forall(_.parallelInfluence.isEmpty) shouldBe true
    }

    "mark a segment with multiple neighbour UUIDs when several are within threshold" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val neighbour1Uuid = UUID.randomUUID()
      val neighbour2Uuid = UUID.randomUUID()
      val neighbour1 = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbour1Uuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )
      val neighbour2 = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbour2Uuid,
        Seq((0.3, -0.00005), (0.7, -0.00005)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour1, neighbour2),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 3
      val markedSegments = segments.filter(_.parallelInfluence.nonEmpty)
      markedSegments should not be empty
      val influence = markedSegments.head.parallelInfluence
      influence.map(_._1) should contain(neighbour1Uuid)
      influence.map(_._1) should contain(neighbour2Uuid)
    }

    "exclude the candidate line itself from the neighbour set" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-1.0, -1.0, 2.0, 1.0),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )
      val selfNeighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        lineUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )

      val (segments, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(selfNeighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 1
      segments.forall(_.parallelInfluence.isEmpty) shouldBe true
    }
  }
  "ThermalSegmentBuilder.singleDeploymentFor" should {
    "return the single deployment if the line has exactly one" in {
      val deployment = mock(classOf[CableDeploymentInput])
      val result = ThermalSegmentBuilder.singleDeploymentFor(
        lineInput,
        Some(java.util.List.of(deployment)),
      )
      result should be(Some(deployment))
    }
    "return None if the line has no deployment" in {
      val result = ThermalSegmentBuilder.singleDeploymentFor(
        lineInput,
        None,
      )
      result shouldBe None
    }
    "throw an exception if the line has multiple deployments" in {
      val deployment1 = mock(classOf[CableDeploymentInput])
      val deployment2 = mock(classOf[CableDeploymentInput])
      val thrown = the[GridAgentInitializationException] thrownBy {
        ThermalSegmentBuilder.singleDeploymentFor(
          lineInput,
          Some(java.util.List.of(deployment1, deployment2)),
        )
      }
      thrown.getMessage should include(lineUuid.toString)
      thrown.getMessage should include("2")
    }
  }
  "ThermalSegmentBuilder.sampledProximityIndices" should {
    "return two in/out transition indices for a nearby parallel neighbour" in {
      val candidate = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.0, 0.0),
          new JtsCoordinate(1.0, 0.0),
        )
      )
      val indexedLine = new LengthIndexedLine(candidate)
      val segLength = candidate.getLength
      val neighbour = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.3, 0.00005),
          new JtsCoordinate(0.7, 0.00005),
        )
      )

      val idxs = ThermalSegmentBuilder.sampledProximityIndices(
        indexedLine,
        segLength,
        neighbour,
        10.0,
        geometryFactory,
      )

      // A mid-segment parallel stretch yields exactly an entry and an exit.
      idxs.size shouldBe 2
      idxs.head should be > 0.0
      idxs.head should be < segLength
      idxs.last should be > 0.0
      idxs.last should be < segLength
      // The stretch is roughly centred on the candidate line.
      idxs.head should be < idxs.last
    }

    "return no transitions when the neighbour is outside the threshold" in {
      val candidate = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.0, 0.0),
          new JtsCoordinate(1.0, 0.0),
        )
      )
      val indexedLine = new LengthIndexedLine(candidate)
      val segLength = candidate.getLength
      val neighbour = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.3, 0.0005),
          new JtsCoordinate(0.7, 0.0005),
        )
      )

      val idxs = ThermalSegmentBuilder.sampledProximityIndices(
        indexedLine,
        segLength,
        neighbour,
        10.0,
        geometryFactory,
      )
      idxs shouldBe empty
    }
    "resolve a parallel stretch on a geodesically long segment" in {
      val candidate = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.0, 0.0),
          new JtsCoordinate(1.0, 0.0),
        )
      )
      val indexedLine = new LengthIndexedLine(candidate)
      val segLength = candidate.getLength
      val neighbour = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.5, 0.00005),
          new JtsCoordinate(0.55, 0.00005),
        )
      )
      val idxs = ThermalSegmentBuilder.sampledProximityIndices(
        indexedLine,
        segLength,
        neighbour,
        10.0,
        geometryFactory,
      )
      idxs.size shouldBe 2
      idxs.head should be > 0.0
      idxs.head should be < segLength
      idxs.last should be > 0.0
      idxs.last should be < segLength
      idxs.head should be < idxs.last
    }
    "return no transitions for a sub-meter segment fully inside the threshold" in {
      val candidate = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.0, 0.0),
          new JtsCoordinate(0.000005, 0.0),
        )
      )
      val indexedLine = new LengthIndexedLine(candidate)
      val segLength = candidate.getLength
      val neighbour = geometryFactory.createLineString(
        Array(
          new JtsCoordinate(0.0, 0.000045),
          new JtsCoordinate(0.000005, 0.000045),
        )
      )
      val idxs = ThermalSegmentBuilder.sampledProximityIndices(
        indexedLine,
        segLength,
        neighbour,
        10.0,
        geometryFactory,
      )
      idxs shouldBe empty
    }
  }
  "ThermalSegmentBuilder.writeThermalSegmentsToOutput" should {
    "include the parallel-influence columns in the CSV header and rows" in {
      val tempDir = Files.createTempDirectory("test_thermal_out")
      try {
        val simonaConfig = buildTestConfig(tempDir.toString)
        val neighbourUuid = UUID.randomUUID()
        val built = makeSegmentWithInfluence(Seq(neighbourUuid -> 3.5))
        val segments = Seq(built)
        ThermalSegmentBuilder.writeThermalSegmentsToOutput(
          segments,
          simonaConfig,
        )
        val outPath = tempDir
          .resolve("TestSim")
          .resolve("rawOutputData")
          .resolve("thermal_line_segments.csv")
        val lines: Seq[String] =
          Files.readAllLines(outPath).asScala.toSeq
        val header = lines.head
        header should include("hasParallelInfluence")
        header should include("neighbourLineUuids")
        header should include("minLateralDistanceMeters")
        val row = lines(1)
        val fields = row.split(",", -1)
        fields(7) shouldBe "true"
        fields(8) shouldBe neighbourUuid.toString
        fields(9) shouldBe "3.5"
      } finally {
        Files
          .walk(tempDir)
          .sorted(java.util.Comparator.reverseOrder[Path]())
          .forEach(p => Files.deleteIfExists(p))
      }
    }
    "show false and empty neighbour columns when no parallel influence is present" in {
      val tempDir = Files.createTempDirectory("test_thermal_out")
      try {
        val simonaConfig = buildTestConfig(tempDir.toString)
        val built = makeSegment()
        val segments = Seq(built)
        ThermalSegmentBuilder.writeThermalSegmentsToOutput(
          segments,
          simonaConfig,
        )
        val outPath = tempDir
          .resolve("TestSim")
          .resolve("rawOutputData")
          .resolve("thermal_line_segments.csv")
        val lines: Seq[String] =
          Files.readAllLines(outPath).asScala.toSeq
        val row = lines(1)
        val fields = row.split(",", -1)
        fields(7) shouldBe "false"
        fields(8) shouldBe ""
        fields(9) shouldBe ""
      } finally {
        Files
          .walk(tempDir)
          .sorted(java.util.Comparator.reverseOrder[Path]())
          .forEach(p => Files.deleteIfExists(p))
      }
    }
    "write multiple neighbour UUIDs and distances in semicolon-separated order" in {
      val tempDir = Files.createTempDirectory("test_thermal_out")
      try {
        val simonaConfig = buildTestConfig(tempDir.toString)
        val neighbour1Uuid = UUID.randomUUID()
        val neighbour2Uuid = UUID.randomUUID()
        val built = makeSegmentWithInfluence(
          Seq(neighbour1Uuid -> 2.0, neighbour2Uuid -> 5.5)
        )
        val segments = Seq(built)
        ThermalSegmentBuilder.writeThermalSegmentsToOutput(
          segments,
          simonaConfig,
        )
        val outPath = tempDir
          .resolve("TestSim")
          .resolve("rawOutputData")
          .resolve("thermal_line_segments.csv")
        val lines: Seq[String] =
          Files.readAllLines(outPath).asScala.toSeq
        val row = lines(1)
        val fields = row.split(",", -1)
        fields(7) shouldBe "true"
        fields(8) shouldBe s"$neighbour1Uuid;$neighbour2Uuid"
        fields(9) shouldBe "2.0;5.5"
      } finally {
        Files
          .walk(tempDir)
          .sorted(java.util.Comparator.reverseOrder[Path]())
          .forEach(p => Files.deleteIfExists(p))
      }
    }
  }
}
