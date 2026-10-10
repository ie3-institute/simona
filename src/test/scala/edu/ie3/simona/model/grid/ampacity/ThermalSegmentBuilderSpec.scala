/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.connector.LineInput
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.CigreT880LandCable33kV
import edu.ie3.simona.util.Coordinate
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

import java.util.UUID
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.Map as MutableMap

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
      ListBuffer[
        (LineSegmentThermalModel, (Double, Double), (Double, Double))
      ],
      MutableMap[UUID, Coordinate],
      scala.collection.mutable.Map[UUID, Seq[(UUID, Double)]],
  ) = {
    val generatedSegments = ListBuffer.empty[
      (LineSegmentThermalModel, (Double, Double), (Double, Double))
    ]
    val coordinatesBuffer = MutableMap.empty[UUID, Coordinate]
    val parallelInfluenceBuffer =
      scala.collection.mutable.Map.empty[UUID, Seq[(UUID, Double)]]
    val warnedPairs = scala.collection.mutable.Set.empty[(UUID, UUID)]
    val segments = ThermalSegmentBuilder.splitAndAssignSegments(
      lineInput,
      cable,
      coordinates,
      cableDepth,
      Meters(0.5),
      soilLayers,
      soilTypes,
      limitTemperature,
      generatedSegments,
      coordinatesBuffer,
      neighbourLines,
      ppThreshold,
      parallelInfluenceBuffer,
      warnedPairs,
    )
    (segments, generatedSegments, coordinatesBuffer, parallelInfluenceBuffer)
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

      val (segments, generated, _, _) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(leftLayer, rightLayer),
      )

      segments.size shouldBe 2
      generated.size shouldBe 2

      val sorted = generated.sortBy(_._2._1)
      // Left subsegment spans x in [0, 0.5], right one in [0.5, 1]
      sorted.head._2._1 shouldBe 0.0
      sorted.head._3._1 shouldBe 0.5
      sorted.last._2._1 shouldBe 0.5
      sorted.last._3._1 shouldBe 1.0

      // Each subsegment must reference the original line.
      generated.forall(_._1.lineUuid == lineUuid) shouldBe true
    }

    "produce a single segment when one soil layer covers the entire straight segment" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(-0.5, -0.1, 1.5, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, _, _, _) = run(
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

      val (segments, generated, _, _) = run(
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

      val (segments, _, _, _) = run(
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

      val (segments, generated, _, _) = run(
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 3
      parallelInfluence.size shouldBe 1
      val markedSegment = parallelInfluence.keys.head
      val influence = parallelInfluence(markedSegment)
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 3
      parallelInfluence.size shouldBe 1
      val influence = parallelInfluence.values.head
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 1
      parallelInfluence shouldBe empty
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      parallelInfluence should not be empty
      val allNeighbourUuids = parallelInfluence.values.flatten.map(_._1).toSet
      allNeighbourUuids should contain(neighbourUuid)
    }

    "skip a parallel stretch shorter than the minimum overlap and produce no mark" in {
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
        Seq((0.499995, 0.000005), (0.500005, 0.000005)),
        hasDeployment = true,
      )

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(1.0),
      )

      segments.size shouldBe 1
      parallelInfluence shouldBe empty
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = None,
      )

      segments.size shouldBe 1
      parallelInfluence shouldBe empty
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 1
      parallelInfluence shouldBe empty
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(neighbour1, neighbour2),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 3
      parallelInfluence should not be empty
      val markedSegment = parallelInfluence.keys.head
      val influence = parallelInfluence(markedSegment)
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

      val (segments, _, _, parallelInfluence) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
        neighbourLines = Seq(selfNeighbour),
        ppThreshold = Some(10.0),
      )

      segments.size shouldBe 1
      parallelInfluence shouldBe empty
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
  }
}
