/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.CsvDataSource
import edu.ie3.simona.exceptions.GridInconsistencyException
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.{
  KelvinMetersPerWatt,
  KilowattHoursPerCubicMeterKelvin,
  ThermalCapacitance,
  ThermalResistivity,
}
import org.locationtech.jts.geom.{Coordinate, Geometry, GeometryFactory}
import squants.thermal.Celsius
import squants.{Kelvin, Meters, Temperature}

import java.nio.file.{Files, Path, Paths}
import java.util.UUID

class SoilDataParserSpec extends UnitSpec {

  given Temperature = Kelvin(1e-3)
  given ThermalCapacitance = KilowattHoursPerCubicMeterKelvin(1e-8)
  given ThermalResistivity = KelvinMetersPerWatt(1e-8)
  given Double = 1e-8

  "SoilDataParser" should {
    val uuid = UUID.fromString("123e4567-e89b-12d3-a456-426614174000")
    val typeUuid = UUID.fromString("32b43a78-7721-431d-b1c2-56975a123670")
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
    def sourceFor(path: Path): CsvDataSource =
      new CsvDataSource(
        ",",
        Option(path.getParent).getOrElse(Paths.get(".")),
        new FileNamingStrategy(),
      )

    def soilType(tuuid: UUID, name: String) =
      SoilType(
        tuuid,
        name,
        KelvinMetersPerWatt(0.30),
        KelvinMetersPerWatt(0.40),
        KilowattHoursPerCubicMeterKelvin(0.0015),
        Celsius(15d),
      )

    "read soil types from CSV" in {
      val content =
        s"uuid,id,thermal_resistivity_wet,thermal_resistivity_dry,specific_heat_capacity,critical_temperature_difference\n${uuid},loam,0.30,0.40,0.0015,15.0\n"
      val tmp = Files.createTempFile("soil_types", ".csv")
      Files.writeString(tmp, content)

      val types =
        SoilDataParser.readSoilTypes(sourceFor(tmp), tmp).success.value
      types should have size 1
      val t = types.head
      t.uuid shouldBe uuid
      t.id shouldBe "loam"
      t.criticalTemperatureDifference shouldBe Celsius(15d)
    }

    "reject soil types file with incorrect header case" in {
      val content =
        s"UUID,id,thermal_resistivity_wet,thermal_resistivity_dry,specific_heat_capacity,critical_temperature_difference\n${uuid},loam,0.30,0.40,0.0015,15.0\n"
      val tmp = Files.createTempFile("soil_types_bad_header", ".csv")
      Files.writeString(tmp, content)

      val res = SoilDataParser.readSoilTypes(tmp)
      assert(res.isFailure)
    }

    "select the wet or dry thermal resistivity based on the critical temperature difference" in {
      val t = soilType(uuid, "loam")

      // Below the critical difference: wet resistivity.
      t.currentThermalResistivity(Celsius(0.0)) shouldBe KelvinMetersPerWatt(
        0.30
      )
      t.currentThermalResistivity(Celsius(14.9)) shouldBe KelvinMetersPerWatt(
        0.30
      )
      // At the critical difference: dry resistivity.
      t.currentThermalResistivity(Celsius(15.0)) shouldBe KelvinMetersPerWatt(
        0.40
      )
      // Above the critical difference: dry resistivity.
      t.currentThermalResistivity(Celsius(50.0)) shouldBe KelvinMetersPerWatt(
        0.40
      )
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

      val types =
        SoilDataParser
          .readSoilTypes(sourceFor(typesPath), typesPath)
          .success
          .value
      types should not be empty
      types.head.id should be("loam")

      val layers =
        SoilDataParser
          .readSoilLayers(sourceFor(layersPath), layersPath)
          .success
          .value
      layers should not be empty

      val typeIds = types.map(_.uuid).toSet
      val referenced = layers.map(_.soilType).toSet
      referenced.subsetOf(typeIds) shouldBe true

      val totals = SoilDataParser.totalThicknessBySoilType(layers)
      totals.keySet should contain allElementsOf referenced
    }

    "read soil layers from inline CSV and compute thickness" in {
      val content =
        """uuid,geometry,z_from,z_to,soil_type
          |f07aa67c-43f5-4706-967a-5d0613a94701,"{""type"":""Polygon"",""coordinates"":[[[7.40383,51.49129],[7.40562,51.49130],[7.40560,51.49106],[7.40377,51.49105],[7.40383,51.49129]]]}",0.0,-0.4,32b43a78-7721-431d-b1c2-56975a123670
          |""".stripMargin

      val tmp = Files.createTempFile("soilLayers", ".csv")
      Files.writeString(tmp, content)

      val layers =
        SoilDataParser.readSoilLayers(sourceFor(tmp), tmp).success.value
      layers should have size 1
      layers.head.thickness shouldBe Meters(0.4)
    }

    "associate layers with their soil types" in {
      val knownType = soilType(typeUuid, "loam")
      val matched = SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), typeUuid)
      val missing = SoilLayer(
        UUID.randomUUID(),
        g,
        Meters(0.0),
        Meters(-1.0),
        UUID.randomUUID(),
      )

      val res = SoilDataParser.associateLayersWithTypes(
        Seq(matched, missing),
        Seq(knownType),
      )

      res should have size 2
      res.find(_._1 == matched).value._2 shouldBe Some(knownType)
      res.find(_._1 == missing).value._2 shouldBe None
    }

    "compute the total thickness per soil type" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), typeUuid),
        SoilLayer(uuid, g, Meters(-1.0), Meters(-2.0), typeUuid),
        SoilLayer(
          UUID.randomUUID(),
          g,
          Meters(0.0),
          Meters(-0.5),
          UUID.randomUUID(),
        ),
      )

      val totals = SoilDataParser.totalThicknessBySoilType(layers)
      totals should have size 2
      totals(typeUuid) shouldBe Meters(2.0)
    }

    "check for overlaps of soil layers" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), typeUuid),
        SoilLayer(uuid, g, Meters(-0.5), Meters(-2.0), typeUuid), // overlap
      )
      intercept[GridInconsistencyException] {
        SoilDataParser.validateNonOverlappingPerCoordinate(layers)
      }
    }

    "check for gaps between soil layers" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-0.5), typeUuid),
        SoilLayer(uuid, g, Meters(-1.0), Meters(-2.0), typeUuid), // gap
      )
      intercept[GridInconsistencyException] {
        SoilDataParser.validateNoGapsPerCoordinate(layers)
      }
    }

    "check for coverage of soil layers" in {
      val g2 = square(1.0, 1.0)
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), typeUuid),
        SoilLayer(uuid, g, Meters(-1.0), Meters(-2.0), typeUuid),
        SoilLayer(UUID.randomUUID(), g2, Meters(0.0), Meters(-0.5), typeUuid),
        SoilLayer(
          UUID.randomUUID(),
          g2,
          Meters(-0.5),
          Meters(-1.5),
          typeUuid,
        ), // missing coverage to -2.0 m
      )
      val expected: Map[Geometry, (Double, Double)] =
        Map(g -> (-2.0, 0.0), g2 -> (-2.0, 0.0))
      intercept[GridInconsistencyException] {
        SoilDataParser.validateCoverageAgainstRanges(layers, expected)
      }
    }

    "report layers that reference unknown soil types" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-2.0), typeUuid)
      )
      val expected: Map[Geometry, (Double, Double)] = Map(g -> (-2.0, 0.0))
      // The only known soil type has a different UUID than the one referenced
      // by the layer, so the reference is missing.
      val knownTypes = Seq(soilType(UUID.randomUUID(), "loam"))

      val ex = intercept[GridInconsistencyException] {
        SoilDataParser.validateAll(
          layers,
          expectedRanges = expected,
          types = knownTypes,
        )
      }
      ex.getMessage should include("missing soil types")
    }

    "throw exception in case of errors" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), typeUuid),
        SoilLayer(uuid, g, Meters(-0.5), Meters(-2.0), typeUuid), // overlap
      )
      val expected: Map[Geometry, (Double, Double)] = Map(g -> (-2.0, 0.0))

      intercept[GridInconsistencyException] {
        SoilDataParser.validateAll(layers, expectedRanges = expected)
      }
    }

    "not throw exception when everything is fine" in {
      val layers = Seq(
        SoilLayer(uuid, g, Meters(0.0), Meters(-1.0), typeUuid),
        SoilLayer(uuid, g, Meters(-1.0), Meters(-2.0), typeUuid),
      )
      val expected: Map[Geometry, (Double, Double)] = Map(g -> (-2.0, 0.0))
      val knownTypes = Seq(soilType(typeUuid, "loam"))

      noException shouldBe thrownBy {
        SoilDataParser.validateAll(
          layers,
          expectedRanges = expected,
          types = knownTypes,
        )
      }
    }

  }
}
