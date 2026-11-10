/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.input.connector.`type`.{CableMaterial, CableTypeInput, ConductorInput, LineTypeInput}
import edu.ie3.datamodel.models.input.connector.{CableDeploymentInput, LineInput}
import edu.ie3.datamodel.models.input.container.*
import edu.ie3.datamodel.models.input.{EmInput, NodeInput, SoilLayer, SoilType}
import edu.ie3.datamodel.models.voltagelevels.VoltageLevel
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.CigreT880LandCable33kV
import edu.ie3.simona.util.Coordinate
import edu.ie3.util.quantities.PowerSystemUnits.*
import edu.ie3.util.quantities.thermal.JoulesPerCubicMeterKelvin
import org.locationtech.jts.geom.{GeometryFactory, Coordinate as JtsCoordinate}
import org.locationtech.jts.linearref.LengthIndexedLine
import org.mockito.Mockito.{mock, when}
import org.scalatest.matchers.should.Matchers
import squants.Meters
import squants.space.{Length, Millimeters, SquareMeters}
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.*

import java.nio.file.{Files, Path}
import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.jdk.CollectionConverters.*

class ThermalSegmentBuilderSpec extends UnitSpec with Matchers {
  private val geometryFactory = new GeometryFactory()
  private val lineUuid = UUID.randomUUID()
  private val soilTypeUuid = UUID.randomUUID()
  private val secondSoilTypeUuid = UUID.randomUUID()
  private val lineInput: LineInput = {
    val li = mock(classOf[LineInput])
    when(li.getUuid).thenReturn(lineUuid)
    when(li.getId).thenReturn("testLine")
    li
  }
  private val cable = CigreT880LandCable33kV.cable
  private val limitTemperature = Celsius(90d)
  private val soilType = new SoilType(
    soilTypeUuid,
    "testLoam",
    Quantities.getQuantity(0.5, KELVIN_METRE_PER_WATT),
    Quantities.getQuantity(2.5, KELVIN_METRE_PER_WATT),
    Quantities.getQuantity(1.5e6, JOULE_PER_CUBIC_METRE_KELVIN),
    Quantities.getQuantity(35.0, CELSIUS),
  )
  private val secondSoilType = new SoilType(
    secondSoilTypeUuid,
    "testSand",
    Quantities.getQuantity(0.5, KELVIN_METRE_PER_WATT),
    Quantities.getQuantity(2.5, KELVIN_METRE_PER_WATT),
    Quantities.getQuantity(1.5e6, JOULE_PER_CUBIC_METRE_KELVIN),
    Quantities.getQuantity(35.0, CELSIUS),
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

  private def makeSoilLayer(
      xMin: Double,
      yMin: Double,
      xMax: Double,
      yMax: Double,
      zFromMeters: Double = 0.0,
      zToMeters: Double = -2.0,
      soilTypeId: UUID = soilTypeUuid,
  ): SoilLayer =
    new SoilLayer(
      UUID.randomUUID(),
      rectangle(xMin, yMin, xMax, yMax),
      Quantities.getQuantity(zFromMeters, METRE),
      Quantities.getQuantity(zToMeters, METRE),
      soilTypeId,
    )

  private def run(
      coordinates: Seq[(Double, Double)],
      cableDepth: Length,
      soilLayers: Seq[SoilLayer],
      neighbourLines: Seq[ThermalSegmentBuilder.NeighbourLineInfo] = Seq.empty,
      ppThreshold: Option[Double] = None,
      cableBed: Option[LineSegmentThermalModel.CableBed] = None,
      inputSoilTypes: Seq[SoilType] = soilTypes,
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
      inputSoilTypes,
      limitTemperature,
      generatedSegments,
      neighbourLines,
      ppThreshold,
      warnedPairs,
      cableBed,
    )
    val segments = ThermalSegmentBuilder.splitAndAssignSegments(context)
    (segments, generatedSegments)
  }
  private def makeContext(
      coordinates: Seq[(Double, Double)],
      soilLayers: Seq[SoilLayer],
      neighbourLines: Seq[ThermalSegmentBuilder.NeighbourLineInfo] = Seq.empty,
      ppThreshold: Option[Double] = None,
      cableBed: Option[LineSegmentThermalModel.CableBed] = None,
      inputSoilTypes: Seq[SoilType] = soilTypes,
  ): ThermalSegmentBuilder.SegmentBuildContext = {
    val generatedSegments =
      ListBuffer.empty[ThermalSegmentBuilder.BuiltSegment]
    val warnedPairs = scala.collection.mutable.Set.empty[(UUID, UUID)]
    ThermalSegmentBuilder.SegmentBuildContext(
      lineInput,
      cable,
      coordinates,
      Meters(-1.0),
      Meters(0.5),
      soilLayers,
      inputSoilTypes,
      limitTemperature,
      generatedSegments,
      neighbourLines,
      ppThreshold,
      warnedPairs,
      cableBed,
    )
  }
  private def straightLine(
      start: (Double, Double),
      end: (Double, Double),
  ): (org.locationtech.jts.geom.Geometry, LengthIndexedLine) = {
    val jtsLine = geometryFactory.createLineString(
      Array(
        new JtsCoordinate(start._1, start._2),
        new JtsCoordinate(end._1, end._2),
      )
    )
    (jtsLine, new LengthIndexedLine(jtsLine))
  }

  private def makeSegment(): ThermalSegmentBuilder.BuiltSegment = {
    val layer = makeSoilLayer(0.0, -0.1, 1.0, 0.1)
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
      val leftLayer = makeSoilLayer(0.0, -0.1, 0.5, 0.1)
      val rightLayer = makeSoilLayer(0.5, -0.1, 1.0, 0.1)

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
      val layer = makeSoilLayer(-0.5, -0.1, 1.5, 0.1, 0.0, -2.0)

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
      segment.soilType.getThermalResistivityWet.getValue
        .doubleValue() shouldBe 0.5
      segment.soilType.getVolumetricHeatCapacity.getValue
        .doubleValue() shouldBe 1.5e6
    }

    "split a line at its original support points" in {
      // One layer covering both straight parts; the line has three support
      // points, so two straight segments must be produced.
      val layer = makeSoilLayer(0.0, -0.1, 2.0, 0.1, 0.0, -2.0)

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
      val layer = makeSoilLayer(5.0, 5.0, 6.0, 6.0, 0.0, -2.0)

      an[GridAgentInitializationException] should be thrownBy {
        run(
          Seq((0.0, 0.0), (1.0, 0.0)),
          Meters(-1.0),
          Seq(layer),
        )
      }
    }

    "throw an exception if the cable depth is not covered vertically" in {
      val layer = makeSoilLayer(0.0, -0.1, 1.0, 0.1, 0.0, -0.5)

      an[GridAgentInitializationException] should be thrownBy {
        run(
          Seq((0.0, 0.0), (1.0, 0.0)),
          Meters(-1.0),
          Seq(layer),
        )
      }
    }

    "fill the per-segment model with real endpoints and soil parameters" in {
      val layer = makeSoilLayer(0.0, -0.1, 1.0, 0.1, 0.0, -2.0)

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
      segment.soilType.getThermalResistivityWet.getValue
        .doubleValue() shouldBe 0.5
      segment.soilType.getVolumetricHeatCapacity.getValue
        .doubleValue() shouldBe 1.5e6
    }

    "return no segments for lines with fewer than two support points" in {
      val layer = makeSoilLayer(0.0, -0.1, 1.0, 0.1, 0.0, -2.0)

      val (segments, generated) = run(
        Seq((0.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
      )

      segments shouldBe empty
      generated shouldBe empty
    }

    "split a candidate line at a parallel neighbour within threshold and mark the middle segment" in {
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
        markedSegments.flatMap(_.parallelInfluence).map(_._1)
      allNeighbourUuids should contain(neighbourUuid)
    }

    "skip a parallel stretch shorter than the minimum overlap and produce no mark" in {
      val layer = makeSoilLayer(-0.0005, -0.0005, 0.0005, 0.0005, 0.0, -2.0)
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
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
      val layer = makeSoilLayer(-1.0, -1.0, 2.0, 1.0, 0.0, -2.0)
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
  "ThermalSegmentBuilder.requireValue" should {
    "return the value when the option is defined" in {
      val result = ThermalSegmentBuilder.requireValue(Some(42), "missing")
      result shouldBe 42
    }
    "throw an exception with the given message when the option is empty" in {
      val thrown = the[GridAgentInitializationException] thrownBy {
        ThermalSegmentBuilder.requireValue(Option.empty[Int], "missing value")
      }
      thrown.getMessage shouldBe "missing value"
    }
  }
  "ThermalSegmentBuilder.requireQuantity" should {
    "return the quantity when the option is defined" in {
      val result =
        ThermalSegmentBuilder.requireQuantity(Some(Meters(1.5)), "missing")
      result shouldBe Meters(1.5)
    }
    "throw an exception with the given message when the option is empty" in {
      val thrown = the[GridAgentInitializationException] thrownBy {
        val emptyOpt: Option[Length] = None
        ThermalSegmentBuilder.requireQuantity(emptyOpt, "missing quantity")
      }
      thrown.getMessage shouldBe "missing quantity"
    }
  }
  "ThermalSegmentBuilder.computeSoilIndices" should {
    "return the boundary intersection indices for two adjacent soil layers" in {
      val leftLayer = makeSoilLayer(0.0, -0.1, 0.5, 0.1)
      val rightLayer = makeSoilLayer(0.5, -0.1, 1.0, 0.1)
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(leftLayer, rightLayer),
      )
      val (jtsLine, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))

      val indices =
        ThermalSegmentBuilder.computeSoilIndices(context, jtsLine, indexedLine)

      // Each layer contributes its entry and exit intersection with the line.
      // Left layer: indices 0.0 and 0.5. Right layer: indices 0.5 and 1.0.
      indices.distinct.sorted shouldBe Seq(0.0, 0.5, 1.0)
    }
    "return no indices when no soil boundary crosses the segment" in {
      val layer = makeSoilLayer(-0.5, -0.1, 1.5, 0.1)
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(layer),
      )
      val (jtsLine, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))

      val indices =
        ThermalSegmentBuilder.computeSoilIndices(context, jtsLine, indexedLine)

      indices shouldBe empty
    }
  }
  "ThermalSegmentBuilder.computePpIndices" should {
    "return no indices when the parallel-proximity threshold is not set" in {
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(makeSoilLayer(-1.0, -1.0, 2.0, 1.0)),
        neighbourLines = Seq(neighbour),
        ppThreshold = None,
      )
      val (jtsLine, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))
      val neighbourLine =
        geometryFactory.createLineString(
          Array(
            new JtsCoordinate(0.3, 0.00005),
            new JtsCoordinate(0.7, 0.00005),
          )
        )
      val rejected = scala.collection.mutable.Set.empty[UUID]

      val indices = ThermalSegmentBuilder.computePpIndices(
        context,
        Seq(neighbour -> neighbourLine),
        indexedLine,
        jtsLine.getLength,
        jtsLine,
        rejected,
      )

      indices shouldBe empty
    }
    "return the transition indices for a deployed neighbour within threshold" in {
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(makeSoilLayer(-1.0, -1.0, 2.0, 1.0)),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )
      val (jtsLine, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))
      val neighbourLine =
        geometryFactory.createLineString(
          Array(
            new JtsCoordinate(0.3, 0.00005),
            new JtsCoordinate(0.7, 0.00005),
          )
        )
      val rejected = scala.collection.mutable.Set.empty[UUID]

      val indices = ThermalSegmentBuilder.computePpIndices(
        context,
        Seq(neighbour -> neighbourLine),
        indexedLine,
        jtsLine.getLength,
        jtsLine,
        rejected,
      )

      indices.size shouldBe 2
      rejected shouldBe empty
    }
    "skip an undeployed neighbour and record a warning pair" in {
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = false,
      )
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(makeSoilLayer(-1.0, -1.0, 2.0, 1.0)),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )
      val (jtsLine, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))
      val neighbourLine =
        geometryFactory.createLineString(
          Array(
            new JtsCoordinate(0.3, 0.00005),
            new JtsCoordinate(0.7, 0.00005),
          )
        )
      val rejected = scala.collection.mutable.Set.empty[UUID]

      val indices = ThermalSegmentBuilder.computePpIndices(
        context,
        Seq(neighbour -> neighbourLine),
        indexedLine,
        jtsLine.getLength,
        jtsLine,
        rejected,
      )

      indices shouldBe empty
      context.warnedPairs should contain((lineUuid, neighbourUuid))
    }
    "reject a parallel stretch shorter than the minimum overlap" in {
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.00004, 0.000005), (0.00006, 0.000005)),
        hasDeployment = true,
      )
      val context = makeContext(
        Seq((0.0, 0.0), (0.0001, 0.0)),
        Seq(makeSoilLayer(-0.0005, -0.0005, 0.0005, 0.0005)),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(1.0),
      )
      val (jtsLine, indexedLine) =
        straightLine((0.0, 0.0), (0.0001, 0.0))
      val neighbourLine =
        geometryFactory.createLineString(
          Array(
            new JtsCoordinate(0.00004, 0.000005),
            new JtsCoordinate(0.00006, 0.000005),
          )
        )
      val rejected = scala.collection.mutable.Set.empty[UUID]

      val indices = ThermalSegmentBuilder.computePpIndices(
        context,
        Seq(neighbour -> neighbourLine),
        indexedLine,
        jtsLine.getLength,
        jtsLine,
        rejected,
      )

      indices shouldBe empty
      rejected should contain(neighbourUuid)
    }
  }
  "ThermalSegmentBuilder.buildSubsegments" should {
    "build one segment per index interval from the given split indices" in {
      val leftLayer = makeSoilLayer(0.0, -0.1, 0.5, 0.1)
      val rightLayer = makeSoilLayer(0.5, -0.1, 1.0, 0.1)
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(leftLayer, rightLayer),
      )
      val (jtsLine, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))

      val segments = ThermalSegmentBuilder.buildSubsegments(
        context,
        indexedLine,
        jtsLine.getLength,
        Seq(0.5),
        Seq.empty,
        scala.collection.mutable.Set.empty[UUID],
        Seq.empty,
        -1.0,
        scala.collection.mutable.Set.empty[UUID],
        ThermalSegmentBuilder.calcCableThermalCapacities(cable),
      )

      segments.size shouldBe 2
      context.generatedSegments.size shouldBe 2
      val sorted = context.generatedSegments.sortBy(_.start.longitude)
      sorted.head.end.longitude shouldBe 0.5
      sorted.last.start.longitude shouldBe 0.5
    }
    "mark segments with the parallel influence of non-rejected neighbours" in {
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(makeSoilLayer(-1.0, -1.0, 2.0, 1.0)),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )
      val (_, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))
      val neighbourLine =
        geometryFactory.createLineString(
          Array(
            new JtsCoordinate(0.3, 0.00005),
            new JtsCoordinate(0.7, 0.00005),
          )
        )

      val segments = ThermalSegmentBuilder.buildSubsegments(
        context,
        indexedLine,
        1.0,
        Seq.empty,
        Seq.empty,
        scala.collection.mutable.Set.empty[UUID],
        Seq(neighbour -> neighbourLine),
        -1.0,
        scala.collection.mutable.Set.empty[UUID],
        ThermalSegmentBuilder.calcCableThermalCapacities(cable),
      )

      segments.size shouldBe 1
      segments.head.parallelInfluence.map(_._1) should contain(neighbourUuid)
    }
    "exclude rejected neighbours from the parallel influence" in {
      val neighbourUuid = UUID.randomUUID()
      val neighbour = ThermalSegmentBuilder.NeighbourLineInfo(
        neighbourUuid,
        Seq((0.3, 0.00005), (0.7, 0.00005)),
        hasDeployment = true,
      )
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(makeSoilLayer(-1.0, -1.0, 2.0, 1.0)),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )
      val (_, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))
      val neighbourLine =
        geometryFactory.createLineString(
          Array(
            new JtsCoordinate(0.3, 0.00005),
            new JtsCoordinate(0.7, 0.00005),
          )
        )
      val rejected = scala.collection.mutable.Set(neighbourUuid)

      val segments = ThermalSegmentBuilder.buildSubsegments(
        context,
        indexedLine,
        1.0,
        Seq.empty,
        Seq.empty,
        rejected,
        Seq(neighbour -> neighbourLine),
        -1.0,
        scala.collection.mutable.Set.empty[UUID],
        ThermalSegmentBuilder.calcCableThermalCapacities(cable),
      )

      segments.size shouldBe 1
      segments.head.parallelInfluence shouldBe empty
    }
    "throw an exception if no soil layer covers a subsegment midpoint" in {
      val context = makeContext(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Seq(makeSoilLayer(5.0, 5.0, 6.0, 6.0)),
      )
      val (_, indexedLine) = straightLine((0.0, 0.0), (1.0, 0.0))

      an[GridAgentInitializationException] should be thrownBy {
        ThermalSegmentBuilder.buildSubsegments(
          context,
          indexedLine,
          1.0,
          Seq.empty,
          Seq.empty,
          scala.collection.mutable.Set.empty[UUID],
          Seq.empty,
          -1.0,
          scala.collection.mutable.Set.empty[UUID],
          ThermalSegmentBuilder.calcCableThermalCapacities(cable),
        )
      }
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
  "map cable bed fields from the deployment input" in {
    val deployment = new CableDeploymentInput(
      UUID.randomUUID(),
      lineUuid,
      "trefoil-touching",
      Quantities.getQuantity(1.0, METRE),
      Quantities.getQuantity(0.5, METRE),
      Quantities.getQuantity(0.6, METRE),
      Quantities.getQuantity(0.4, METRE),
      soilTypeUuid,
    )

    val bed = ThermalSegmentBuilder
      .cableBedForDeployment(deployment, soilTypes)
      .getOrElse(fail("Expected cable bed fields to be mapped."))

    bed.width shouldBe Meters(0.6)
    bed.height shouldBe Meters(0.4)
    bed.soilType shouldBe soilType
  }

  "leave cable bed unset when deployment fields are absent" in {
    val deployment = new CableDeploymentInput(
      UUID.randomUUID(),
      lineUuid,
      "trefoil-touching",
      Quantities.getQuantity(1.0, METRE),
      Quantities.getQuantity(0.5, METRE),
    )

    ThermalSegmentBuilder.cableBedForDeployment(
      deployment,
      soilTypes,
    ) shouldBe None
  }

  "reject cable bed fields with an unknown soil type UUID" in {
    val deployment = new CableDeploymentInput(
      UUID.randomUUID(),
      lineUuid,
      "trefoil-touching",
      Quantities.getQuantity(1.0, METRE),
      Quantities.getQuantity(0.5, METRE),
      Quantities.getQuantity(0.6, METRE),
      Quantities.getQuantity(0.4, METRE),
      UUID.randomUUID(),
    )

    an[GridAgentInitializationException] should be thrownBy {
      ThermalSegmentBuilder.cableBedForDeployment(deployment, soilTypes)
    }
  }

  "accept an enclosing bed for each supported cable formation" in {
    val bed = LineSegmentThermalModel.CableBed(
      Meters(1.2),
      Meters(0.5),
      soilType,
    )

    Seq(
      LayoutFormation.FlatDistance,
      LayoutFormation.FlatTouching,
      LayoutFormation.TrefoilTouching,
    ).foreach { formation =>
      noException should be thrownBy {
        ThermalSegmentBuilder.validateCableBedGeometry(
          cable.copy(layoutFormation = formation),
          Meters(0.5),
          Meters(-1.0),
          bed,
        )
      }
    }
  }

  "reject cable beds that do not enclose every cable" in {
    val bed = LineSegmentThermalModel.CableBed(
      Meters(0.1),
      Meters(0.1),
      soilType,
    )

    an[GridAgentInitializationException] should be thrownBy {
      ThermalSegmentBuilder.validateCableBedGeometry(
        cable,
        Meters(0.5),
        Meters(-1.0),
        bed,
      )
    }
  }

  "reject overlapping outer-soil layers with a cable bed" in {
    val bed = LineSegmentThermalModel.CableBed(
      Meters(1.2),
      Meters(0.5),
      soilType,
    )
    val layers = Seq(
      makeSoilLayer(0.0, -0.1, 1.0, 0.1),
      makeSoilLayer(0.0, -0.1, 1.0, 0.1),
    )

    an[GridAgentInitializationException] should be thrownBy {
      run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        layers,
        cableBed = Some(bed),
      )
    }
  }

  "reject adjacent outer-soil materials with a cable bed" in {
    val bed = LineSegmentThermalModel.CableBed(
      Meters(1.2),
      Meters(0.5),
      soilType,
    )
    val layers = Seq(
      makeSoilLayer(0.0, -0.1, 0.5, 0.1),
      makeSoilLayer(
        0.5,
        -0.1,
        1.0,
        0.1,
        soilTypeId = secondSoilTypeUuid,
      ),
    )

    an[GridAgentInitializationException] should be thrownBy {
      run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        layers,
        cableBed = Some(bed),
        inputSoilTypes = Seq(soilType, secondSoilType),
      )
    }
  }

  "reject cable beds whose top crosses ground level" in {
    val bed = LineSegmentThermalModel.CableBed(
      Meters(1.2),
      Meters(1.2),
      soilType,
    )

    an[GridAgentInitializationException] should be thrownBy {
      ThermalSegmentBuilder.validateCableBedGeometry(
        cable,
        Meters(0.5),
        Meters(-0.5),
        bed,
      )
    }
  }

  "reject vertically layered outer soil with a cable bed when only one layer covers cable depth" in {
    val bed = LineSegmentThermalModel.CableBed(
      Meters(1.2),
      Meters(0.5),
      soilType,
    )
    val layers = Seq(
      makeSoilLayer(0.0, -0.1, 1.0, 0.1, zFromMeters = 0.0, zToMeters = -0.8),
      makeSoilLayer(0.0, -0.1, 1.0, 0.1, zFromMeters = -0.8, zToMeters = -2.0),
    )

    an[GridAgentInitializationException] should be thrownBy {
      run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        layers,
        cableBed = Some(bed),
      )
    }
  }

  "ThermalSegmentBuilder.neighbourLineDistanceMeters" should {
    "return the geodesic distance between a line and a point at the same location" in {
      val line = geometryFactory.createLineString(
        Array(new JtsCoordinate(8.5, 51.5), new JtsCoordinate(8.6, 51.5))
      )
      val point = geometryFactory.createPoint(new JtsCoordinate(8.55, 51.5))

      val dist = ThermalSegmentBuilder.neighbourLineDistanceMeters(line, point)
      dist should be < 1.0
    }

    "return a non-zero distance for a distant neighbour" in {
      val line = geometryFactory.createLineString(
        Array(new JtsCoordinate(8.5, 51.5), new JtsCoordinate(8.6, 51.5))
      )
      val point = geometryFactory.createPoint(new JtsCoordinate(8.5, 51.51))

      val dist = ThermalSegmentBuilder.neighbourLineDistanceMeters(line, point)
      dist should be > 100.0
    }
  }

  "ThermalSegmentBuilder.crossSectionArea" should {
    "return the given area when one is provided" in {
      val result = ThermalSegmentBuilder.crossSectionArea(
        Millimeters(10),
        Millimeters(20),
        Some(SquareMeters(0.002)),
      )
      result.toSquareMeters shouldBe 0.002
    }

    "compute the annular area when no area is provided" in {
      val inner = Meters(0.01)
      val outer = Meters(0.03)
      val result = ThermalSegmentBuilder.crossSectionArea(
        inner,
        outer,
        None,
      )
      val expected = math.Pi / 4d * (0.03d * 0.03d - 0.01d * 0.01d)
      result.toSquareMeters should be(expected +- 1e-12)
    }

    "compute the circular area for a solid conductor with zero inner diameter" in {
      val result = ThermalSegmentBuilder.crossSectionArea(
        Meters(0.0),
        Meters(0.02),
        None,
      )
      val expected = math.Pi / 4d * 0.02d * 0.02d
      result.toSquareMeters should be(expected +- 1e-12)
    }
  }

  "ThermalSegmentBuilder.perLengthCapacity" should {
    "multiply volumetric heat capacity by cross-section area" in {
      val volCap = JoulesPerCubicMeterKelvin(1000.0)
      val result = ThermalSegmentBuilder.perLengthCapacity(
        volCap,
        Meters(0.0),
        Meters(0.02),
        None,
      )
      val expectedArea = math.Pi / 4d * 0.02d * 0.02d
      result.toJoulesPerMeterKelvin should be(
        (1000.0 * expectedArea) +- 1e-9
      )
    }

    "use the explicit area when provided" in {
      val volCap = JoulesPerCubicMeterKelvin(500.0)
      val result = ThermalSegmentBuilder.perLengthCapacity(
        volCap,
        Meters(0.01),
        Meters(0.03),
        Some(SquareMeters(0.001)),
      )
      result.toJoulesPerMeterKelvin should be(0.5 +- 1e-9)
    }
  }

  "ThermalSegmentBuilder.calcCableThermalCapacities" should {
    "return non-zero capacities for a cable with a screen" in {
      val result = ThermalSegmentBuilder.calcCableThermalCapacities(cable)
      result.cc.toJoulesPerMeterKelvin should be > 0.0
      result.cd.toJoulesPerMeterKelvin should be > 0.0
      result.cs.toJoulesPerMeterKelvin should be > 0.0
      result.cj.toJoulesPerMeterKelvin should be > 0.0
    }

    "return zero screen capacity for a cable without a screen" in {
      val cableNoScreen = cable.copy(screenLayer = None)
      val result =
        ThermalSegmentBuilder.calcCableThermalCapacities(cableNoScreen)
      result.cs.toJoulesPerMeterKelvin shouldBe 0.0
    }
  }

  "ThermalSegmentBuilder.validateCableTypesPresent" should {
    "throw an exception when no line has a cable type" in {
      val container = new SubGridContainer(
        "testGrid",
        0,
        new RawGridElements(
          Set(makeTestNode()).asJava,
          Set(makeLineWithoutCableType()).asJava,
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
        ),
        makeEmptySystemParticipants,
        new EnergyManagementUnits(java.util.Collections.emptySet[EmInput]()),
      )
      val thrown = the[GridAgentInitializationException] thrownBy {
        ThermalSegmentBuilder.validateCableTypesPresent(container)
      }
      thrown.getMessage should include("no cable types")
    }

    "not throw an exception when at least one line has a cable type" in {
      val cableType = makeCableTypeInput()
      val lineType = makeLineTypeInput(cableType)
      val line = makeLineWithCableType(lineType)
      val container = new SubGridContainer(
        "testGrid",
        0,
        new RawGridElements(
          Set(makeTestNode()).asJava,
          Set(line).asJava,
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
        ),
        makeEmptySystemParticipants,
        new EnergyManagementUnits(java.util.Collections.emptySet[EmInput]()),
      )
      noException should be thrownBy {
        ThermalSegmentBuilder.validateCableTypesPresent(container)
      }
    }
  }

  "ThermalSegmentBuilder.validateCableDeployments" should {
    "throw an exception when no deployments are provided" in {
      val cableType = makeCableTypeInput()
      val lineType = makeLineTypeInput(cableType)
      val line = makeLineWithCableType(lineType)
      val container = new SubGridContainer(
        "testGrid",
        0,
        new RawGridElements(
          Set(makeTestNode()).asJava,
          Set(line).asJava,
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
        ),
        makeEmptySystemParticipants,
        new EnergyManagementUnits(java.util.Collections.emptySet[EmInput]()),
      )
      val thrown = the[GridAgentInitializationException] thrownBy {
        ThermalSegmentBuilder.validateCableDeployments(
          container,
          Map.empty,
        )
      }
      thrown.getMessage should include("no cable deployment entries")
    }

    "throw an exception when a line with a cable type has no deployment" in {
      val cableType = makeCableTypeInput()
      val lineType = makeLineTypeInput(cableType)
      val line = makeLineWithCableType(lineType)
      val container = new SubGridContainer(
        "testGrid",
        0,
        new RawGridElements(
          Set(makeTestNode()).asJava,
          Set(line).asJava,
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
        ),
        makeEmptySystemParticipants,
        new EnergyManagementUnits(java.util.Collections.emptySet[EmInput]()),
      )
      val emptyMap: scala.collection.Map[UUID, java.util.List[
        CableDeploymentInput
      ]] = Map.empty
      val thrown = the[GridAgentInitializationException] thrownBy {
        ThermalSegmentBuilder.validateCableDeployments(
          container,
          emptyMap,
        )
      }
      thrown.getMessage should include("no cable deployment entries")
    }

    "not throw an exception when all lines with cable types have deployments" in {
      val cableType = makeCableTypeInput()
      val lineType = makeLineTypeInput(cableType)
      val lineUuid = UUID.randomUUID()
      val line = makeLineWithCableType(lineType, lineUuid)
      val deployment = new CableDeploymentInput(
        UUID.randomUUID(),
        lineUuid,
        "trefoil-touching",
        Quantities.getQuantity(1.0, METRE),
        Quantities.getQuantity(0.5, METRE),
      )
      val layer = makeSoilLayer(0.0, -0.1, 1.0, 0.1)
      val rawGrid = new RawGridElements(
        Set(makeTestNode()).asJava,
        Set(line).asJava,
        java.util.Collections.emptySet(),
        java.util.Collections.emptySet(),
        java.util.Collections.emptySet(),
        java.util.Collections.emptySet(),
        java.util.Map.of(lineUuid, Seq(deployment).asJava),
      )
      val container = new SubGridContainer(
        "testGrid",
        0,
        rawGrid,
        makeEmptySystemParticipants,
        new EnergyManagementUnits(java.util.Collections.emptySet[EmInput]()),
        new SoilData(Set(soilType).asJava, Set(layer).asJava),
      )
      val deployments: scala.collection.Map[UUID, java.util.List[
        CableDeploymentInput
      ]] = Map(lineUuid -> Seq(deployment).asJava)
      noException should be thrownBy {
        ThermalSegmentBuilder.validateCableDeployments(
          container,
          deployments,
        )
      }
    }
  }

  "ThermalSegmentBuilder.build" should {
    "return a BuildResult with soil layers and no segments when no cable type is present" in {
      val tempDir = Files.createTempDirectory("test_thermal_build")
      try {
        val layer = makeSoilLayer(0.0, -0.1, 1.0, 0.1)
        val container = new SubGridContainer(
          "testGrid",
          0,
          new RawGridElements(
            Set(makeTestNode()).asJava,
            Set(makeLineWithoutCableType()).asJava,
            java.util.Collections.emptySet(),
            java.util.Collections.emptySet(),
            java.util.Collections.emptySet(),
            java.util.Collections.emptySet(),
          ),
          makeEmptySystemParticipants,
          new EnergyManagementUnits(java.util.Collections.emptySet[EmInput]()),
          new SoilData(Set(soilType).asJava, Set(layer).asJava),
        )
        val simonaConfig = buildTestConfig(tempDir.toString)
        val result = ThermalSegmentBuilder.build(container, simonaConfig)
        result.soilLayers should contain(layer)
        result.thermalLineSegments shouldBe empty
      } finally {
        Files
          .walk(tempDir)
          .sorted(java.util.Comparator.reverseOrder[Path]())
          .forEach(p => Files.deleteIfExists(p))
      }
    }

    "generate thermal segments for a line with a cable type and deployment" in {
      val tempDir = Files.createTempDirectory("test_thermal_build2")
      try {
        val cableType = makeCableTypeInput()
        val lineType = makeLineTypeInput(cableType)
        val lineUuid = UUID.randomUUID()
        val line = makeLineWithCableType(lineType, lineUuid)
        val deployment = new CableDeploymentInput(
          UUID.randomUUID(),
          lineUuid,
          "trefoil-touching",
          Quantities.getQuantity(-1.0, METRE),
          Quantities.getQuantity(0.5, METRE),
        )
        val layer = makeSoilLayer(0.0, -0.1, 1.0, 0.1)
        val rawGrid = new RawGridElements(
          Set(makeTestNode()).asJava,
          Set(line).asJava,
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Collections.emptySet(),
          java.util.Map.of(lineUuid, Seq(deployment).asJava),
        )
        val container = new SubGridContainer(
          "testGrid",
          0,
          rawGrid,
          makeEmptySystemParticipants,
          new EnergyManagementUnits(java.util.Collections.emptySet[EmInput]()),
          new SoilData(Set(soilType).asJava, Set(layer).asJava),
        )
        val simonaConfig = buildTestConfig(tempDir.toString)
        val result = ThermalSegmentBuilder.build(container, simonaConfig)
        result.thermalLineSegments.size should be >= 1
        result.thermalLineSegments.forall(_.lineUuid == lineUuid) shouldBe true
      } finally {
        Files
          .walk(tempDir)
          .sorted(java.util.Comparator.reverseOrder[Path]())
          .forEach(p => Files.deleteIfExists(p))
      }
    }
  }

  private val testVoltageLevel =
    new VoltageLevel(
      "testVoltageLevel",
      Quantities.getQuantity(0.4, KILOVOLT),
    )

  private def makeTestNode(): NodeInput =
    new NodeInput(
      UUID.randomUUID(),
      "testNode",
      Quantities.getQuantity(1.0, PU),
      false,
      geometryFactory.createPoint(new JtsCoordinate(0.5, 0.0)),
      testVoltageLevel,
      0,
    )

  private def makeCableTypeInput(): CableTypeInput = {
    val conductor = new ConductorInput(
      UUID.randomUUID(),
      "conductor",
      CableMaterial.COPPER,
      Quantities.getQuantity(0.0001, SQUARE_METRE),
      Quantities.getQuantity(12.7, MILLIMETRE),
      false,
      Quantities.getQuantity(0.7, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(3.5e6, JOULE_PER_CUBIC_METRE_KELVIN),
      Quantities.getQuantity(0.0001267, SQUARE_METRE),
    )
    new CableTypeInput(
      UUID.randomUUID(),
      "testCableType",
      3,
      conductor,
      java.util.List.of(),
      null,
      java.util.List.of(),
      java.util.List.of(),
      java.util.List.of(),
      Quantities.getQuantity(90.0, CELSIUS),
      Quantities.getQuantity(50.0, HERTZ),
      1.0,
      1.0,
      Quantities.getQuantity(0.2, FARAD_PER_KILOMETRE),
      0.004,
      0.04,
      0.0,
    )
  }

  private def makeLineTypeInput(
      cableType: CableTypeInput
  ): LineTypeInput =
    new LineTypeInput(
      UUID.randomUUID(),
      "testLineType",
      Quantities.getQuantity(1.0, SIEMENS_PER_KILOMETRE),
      Quantities.getQuantity(0.0, SIEMENS_PER_KILOMETRE),
      Quantities.getQuantity(0.1, OHM_PER_KILOMETRE),
      Quantities.getQuantity(0.1, OHM_PER_KILOMETRE),
      Quantities.getQuantity(200.0, AMPERE),
      Quantities.getQuantity(0.4, KILOVOLT),
      java.util.Optional.of(cableType),
    )

  private def makeLineWithCableType(
      lineType: LineTypeInput,
      uuid: UUID = UUID.randomUUID(),
  ): LineInput = {
    val nodeA = makeTestNode()
    val nodeB = makeTestNode()
    val geoPosition = geometryFactory.createLineString(
      Array(new JtsCoordinate(0.0, 0.0), new JtsCoordinate(1.0, 0.0))
    )
    new LineInput(
      uuid,
      "testLineId",
      nodeA,
      nodeB,
      1,
      lineType,
      Quantities.getQuantity(1.0, METRE),
      geoPosition,
      null,
    )
  }

  private def makeLineWithoutCableType(): LineInput = {
    val nodeA = makeTestNode()
    val nodeB = makeTestNode()
    val lineType = new LineTypeInput(
      UUID.randomUUID(),
      "testLineTypeNoCable",
      Quantities.getQuantity(1.0, SIEMENS_PER_KILOMETRE),
      Quantities.getQuantity(0.0, SIEMENS_PER_KILOMETRE),
      Quantities.getQuantity(0.1, OHM_PER_KILOMETRE),
      Quantities.getQuantity(0.1, OHM_PER_KILOMETRE),
      Quantities.getQuantity(200.0, AMPERE),
      Quantities.getQuantity(0.4, KILOVOLT),
    )
    val geoPosition = geometryFactory.createLineString(
      Array(new JtsCoordinate(0.0, 0.0), new JtsCoordinate(1.0, 0.0))
    )
    new LineInput(
      UUID.randomUUID(),
      "testLineNoCable",
      nodeA,
      nodeB,
      1,
      lineType,
      Quantities.getQuantity(1.0, METRE),
      geoPosition,
      null,
    )
  }

  private def makeEmptySystemParticipants: SystemParticipants =
    new SystemParticipants(
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
      java.util.Collections.emptySet(),
    )
}
