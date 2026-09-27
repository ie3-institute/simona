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
  ): (
      Set[LineSegmentThermalModel],
      ListBuffer[
        (LineSegmentThermalModel, (Double, Double), (Double, Double))
      ],
      MutableMap[UUID, Coordinate],
  ) = {
    val generatedSegments = ListBuffer.empty[
      (LineSegmentThermalModel, (Double, Double), (Double, Double))
    ]
    val coordinatesBuffer = MutableMap.empty[UUID, Coordinate]
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
    )
    (segments, generatedSegments, coordinatesBuffer)
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

      val (segments, generated, _) = run(
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
        rectangle(0.0, -0.1, 1.0, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, generated, coordinates) = run(
        Seq((0.0, 0.0), (1.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
      )

      segments.size shouldBe 1
      generated.size shouldBe 1
      generated.head._2 shouldBe ((0.0, 0.0))
      generated.head._3 shouldBe ((1.0, 0.0))

      // Midpoint must be stored for weather registration.
      coordinates.size shouldBe 1
      val midpoint = coordinates.head._2
      midpoint.latitude shouldBe 0.0
      midpoint.longitude shouldBe 0.5
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

      val (segments, generated, _) = run(
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

      val (segments, _, _) = run(
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
      segment.soilResistivity.value shouldBe 0.5
      segment.soilCapacitance.value shouldBe 1.5e6
    }

    "return no segments for lines with fewer than two support points" in {
      val layer = SoilLayer(
        UUID.randomUUID(),
        rectangle(0.0, -0.1, 1.0, 0.1),
        Meters(0.0),
        Meters(-2.0),
        soilTypeUuid,
      )

      val (segments, generated, _) = run(
        Seq((0.0, 0.0)),
        Meters(-1.0),
        Seq(layer),
      )

      segments shouldBe empty
      generated shouldBe empty
    }
  }
}
