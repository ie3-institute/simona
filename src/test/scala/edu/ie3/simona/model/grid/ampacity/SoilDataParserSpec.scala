/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.matchers.QuantityMatchers
import squants.thermal.Celsius
import squants.{Kelvin, Meters, Temperature}
import edu.ie3.util.scala.quantities.{
  KelvinMetersPerWatt,
  KilowattHoursPerKelvinCubicMeters,
  SpecificHeatCapacity,
  ThermalResistivity,
}
import org.scalatest.matchers.should.Matchers
import org.locationtech.jts.geom.Geometry

import java.nio.file.{Files, Paths}
import java.util.UUID
import org.locationtech.jts.geom.{Coordinate, GeometryFactory}

class SoilDataParserSpec extends UnitSpec with Matchers with QuantityMatchers {

  given Temperature = Kelvin(1e-3)
  given SpecificHeatCapacity = KilowattHoursPerKelvinCubicMeters(1e-8)
  given ThermalResistivity = KelvinMetersPerWatt(1e-8)
  given Double = 1e-8

  "SoilDataParser" should {
    val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
    val gf = new GeometryFactory()
    def square(x: Double, y: Double, s: Double = 0.0001) = {
      val coords = Array(
        new Coordinate(x - s, y - s),
        new Coordinate(x + s, y - s),
        new Coordinate(x + s, y + s),
        new Coordinate(x - s, y + s),
        new Coordinate(x - s, y - s),
      )
      gf.createPolygon(coords)
    }

    val g = square(0.0, 0.0)

    "read soil types from CSV" in {
      val content =
        s"uuid,id,thermalResistivityWet,thermalResistivityDry,specificHeatCapacity,criticalTemperature\n${uuid},loam,0.30,0.40,0.0015,35.0\n"
      val tmp = Files.createTempFile("soil_types", ".csv")
      Files.writeString(tmp, content)

      val res = SoilDataParser.readSoilTypes(tmp)
      assert(res.isSuccess)
      val types = res.get
      assert(types.size == 1)
      val t = types.head
      assert(t.uuid == uuid)
      assert(t.id == "loam")
      assert(t.criticalTemperature == Celsius(35d))
    }

    "read soil layers from resources and compute thicknesses" in {
      val typesUrl =
        getClass.getResource("/edu/ie3/simona/service/soilLayers/soilTypes.csv")
      typesUrl should not be (null)
      val typesPath = Paths.get(typesUrl.toURI)

      val layersUrl = getClass.getResource(
        "/edu/ie3/simona/service/soilLayers/soilLayers.csv"
      )
      layersUrl should not be (null)
      val layersPath = Paths.get(layersUrl.toURI)

      val typesRes = SoilDataParser.readSoilTypes(typesPath)
      typesRes.isSuccess shouldBe true
      val types = typesRes.get
      types should not be empty
      types.head.id should be("loam")

      val layersRes = SoilDataParser.readSoilLayers(layersPath)
      layersRes.isSuccess shouldBe true
      val layers = layersRes.get
      layers should not be empty
      layers.head.thickness.toMeters should be(2.0)

      val typeIds = types.map(_.uuid).toSet
      val referenced = layers.map(_.soilType).toSet
      referenced.subsetOf(typeIds) shouldBe true

      val totals = SoilDataParser.totalThicknessBySoilType(layers)
      totals.keySet should contain allElementsOf referenced
    }

    "read soil layers from inline CSV and compute thickness" in {
      val content =
        """f07aa67c-43f5-4706-967a-5d0613a94701,"{""type"":""Polygon"",""coordinates"":[[[7.40383,51.49129],[7.40562,51.49130],[7.40560,51.49106],[7.40377,51.49105],[7.40383,51.49129]]]}",0.0,-0.4,32b43a78-7721-431d-b1c2-56975a123670"""

      val tmp = Files.createTempFile("soilLayers", ".csv")
      Files.writeString(tmp, content)

      val res = SoilDataParser.readSoilLayers(tmp)
      assert(res.isSuccess)
      val layers = res.get
      assert(layers.size == 1)

      layers.head.thickness shouldBe Meters(0.4)
    }

    "check for overlaps of soil layers" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), uuid),
        SoilLayer(uuid, g, Meters(-0.5), Meters(-2.0), uuid), // overlap
      )
      intercept[RuntimeException] {
        SoilDataParser.validateNonOverlappingPerCoordinate(layers)
      }

    }
    "check for gaps between soil layers" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-0.5), uuid),
        SoilLayer(uuid, g, Meters(-1.0), Meters(-2.0), uuid), // gap
      )
      intercept[RuntimeException] {
        SoilDataParser.validateNoGapsPerCoordinate(layers)
      }
    }
    "check for coverage of soil layers" in {
      val g2 = square(1.0, 1.0)
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), uuid),
        SoilLayer(uuid, g, Meters(-1.0), Meters(-2.0), uuid),
        SoilLayer(uuid, g2, Meters(0.0), Meters(-0.5), uuid),
        SoilLayer(
          uuid,
          g2,
          Meters(-0.5),
          Meters(-1.5),
          uuid,
        ), // missing coverage to -2.0 m
      )
      val expected: Map[org.locationtech.jts.geom.Geometry, (Double, Double)] =
        Map(g -> (-2.0, 0.0), g2 -> (-2.0, 0.0))
      intercept[RuntimeException] {
        SoilDataParser.validateCoverageAgainstRanges(layers, expected)
      }
    }

    "throw exception in case of errors" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), uuid),
        SoilLayer(uuid, g, Meters(-0.5), Meters(-2.0), uuid), // overlap
      )
      val expected: Map[Geometry, (Double, Double)] =
        Map(g -> (-2.0, 0.0))

      intercept[RuntimeException] {
        SoilDataParser.validateAll(layers, expectedRanges = expected)
      }
    }

    "not throw exception when everything is fine" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), uuid),
        SoilLayer(uuid, g, Meters(-1.0), Meters(-2.0), uuid),
      )
      val expected: Map[Geometry, (Double, Double)] =
        Map(g -> (-2.0, 0.0))

      noException shouldBe thrownBy {
        SoilDataParser.validateAll(layers, expectedRanges = expected)
      }
    }

  }
}
