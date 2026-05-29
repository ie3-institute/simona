/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.test.common.UnitSpec
import squants.thermal.Celsius
import squants.{Kelvin, Temperature}
import edu.ie3.util.scala.quantities.{
  KelvinMetersPerWatt,
  KilowattHoursPerKelvinCubicMeters,
  SpecificHeatCapacity,
  ThermalResistivity,
}
import org.scalatest.matchers.should.Matchers

import java.nio.file.Files
import java.util.UUID

class SoilDataParserSpec extends UnitSpec with Matchers {

  given Temperature = Kelvin(1e-3)
  given SpecificHeatCapacity = KilowattHoursPerKelvinCubicMeters(1e-8)
  given ThermalResistivity = KelvinMetersPerWatt(1e-8)
  given Double = 1e-8

  "SoilDataParser" should {
    val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
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

    "read soil layers from CSV and compute thickness" in {
      val content = s"x,y,zFrom,zTo,soilTypeUuid\n1.0,2.0,-0.1,-0.5,${uuid}\n"
      val tmp = Files.createTempFile("soil_layers", ".csv")
      Files.writeString(tmp, content)

      val res = SoilDataParser.readSoilLayers(tmp)
      assert(res.isSuccess)
      val layers = res.get
      assert(layers.size == 1)
      layers.head.thickness shouldBe 0.4
    }

    "check for overloaps of soil layers" in {

      val layers = Seq(
        SoilLayer(0.0, 0.0, 0.0, -1.0, uuid),
        SoilLayer(0.0, 0.0, -0.5, -2.0, uuid), // overlap
      )
      intercept[RuntimeException] {
        SoilDataParser.validateNonOverlappingPerCoordinate(layers)
      }

    }
    "check for gaps between soil layers" in {
      val layers = Seq(
        SoilLayer(1.0, 1.0, 0.0, -0.5, uuid),
        SoilLayer(1.0, 1.0, -1.0, -2.0, uuid), // gap
      )
      intercept[RuntimeException] {
        SoilDataParser.validateNoGapsPerCoordinate(layers)
      }
    }
    "check for coverage of soil layers" in {
      val layers = Seq(
        SoilLayer(0.0, 0.0, 0.0, -1.0, uuid),
        SoilLayer(0.0, 0.0, -1.0, -2.0, uuid),
        SoilLayer(1.0, 1.0, 0.0, -0.5, uuid),
        SoilLayer(1.0, 1.0, -0.5, -1.5, uuid), // missing coverage to -2.0 m
      )
      val expected = Map((0.0, 0.0) -> (-2.0, 0.0), (1.0, 1.0) -> (-2.0, 0.0))
      intercept[RuntimeException] {
        SoilDataParser.validateCoverageAgainstRanges(layers, expected)
      }
    }

    "throw exception in case of errors" in {
      val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
      val layers = Seq(
        SoilLayer(0.0, 0.0, 0.0, -1.0, uuid),
        SoilLayer(0.0, 0.0, -0.5, -2.0, uuid), // overlap
      )
      val expected = Map((0.0, 0.0) -> (-2.0, 0.0))

      intercept[RuntimeException] {
        SoilDataParser.validateAll(layers, expectedRanges = expected)
      }
    }

    "not throw exception when everything is fine" in {
      val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
      val layers = Seq(
        SoilLayer(0.0, 0.0, 0.0, -1.0, uuid),
        SoilLayer(0.0, 0.0, -1.0, -2.0, uuid),
      )
      val expected = Map((0.0, 0.0) -> (-2.0, 0.0))

      noException shouldBe thrownBy {
        SoilDataParser.validateAll(layers, expectedRanges = expected)
      }
    }

  }
}
