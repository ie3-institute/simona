/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.participant.PvModel.{PvState, RadiationData}
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexType,
}
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec, WeatherTestData}
import edu.ie3.util.quantities.PowerSystemUnits.*
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW, zeroKWh}
import edu.ie3.util.scala.quantities.{
  ApparentPower,
  Kilovoltamperes,
  WattsPerSquareMeter,
}
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import org.scalatest.GivenWhenThen
import squants.energy.{Power, WattHours, Watts}
import squants.time.Hours
import squants.Energy
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units.*

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.SortedMap

class PvModelSpec
    extends UnitSpec
    with GivenWhenThen
    with DefaultTestData
    with WeatherTestData {

  // testing tolerances
  private given Power = Watts(1e-6)
  private given ApparentPower = Kilovoltamperes(1e-6)
  private given Energy = WattHours(1e-6)

  // build the NodeInputModel (which defines the location of the pv input model)
  // the NodeInputModel needs a GeoReference for the Pv to work
  val geometryFactory = new GeometryFactory()
  val p: Point = geometryFactory.createPoint(new Coordinate(13.2491, 53.457909))
  val nodeInput = new NodeInput(
    UUID.fromString("85f8b517-8a2d-4c20-86c6-3ff3c5823e6d"),
    "NodeInputModel for PvModel Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    getQuantity(1, PU),
    false,
    p,
    GermanVoltageLevelUtils.MV_20KV,
    11,
  )

  // build the PvInputModel
  val pvInput = new PvInput(
    UUID.fromString("adb4eb23-1dd6-4406-a5e7-02e1e4c9dead"),
    "Pv Model Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited,
    nodeInput,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.9)}"),
    null,
    0.20000000298023224,
    getQuantity(-8.926613807678223, DEGREE_GEOM),
    getQuantity(97, PERCENT),
    getQuantity(41.01871871948242, DEGREE_GEOM),
    0.8999999761581421,
    1,
    getQuantity(10, KILOVOLTAMPERE),
    0.8999999761581421,
  )

  // build the PvModel
  val pvModel: PvModel = PvModel.Factory(pvInput).create()

  "A PV Model" should {

    "have sMax set to be 10% higher than its sRated" in {
      When("sMax is calculated")
      val actualSMax = pvModel.sMax
      val expectedSMax = pvModel.sRated * 1.1

      Then("result should match the test data")
      actualSMax should approximate(expectedSMax)
    }

    "handle singular weather data by storing it into state" in {
      val oldState = PvState(0L, defaultSimulationStart)

      val actualState =
        pvModel.handleInput(oldState, Seq(weatherData), onePU)

      actualState.tick shouldEqual oldState.tick
      actualState.dateTime shouldEqual defaultSimulationStart
      actualState.radiationData shouldEqual SortedMap(
        0L -> RadiationData(
          diffIrradiance = weatherData.diffIrr,
          dirIrradiance = weatherData.dirIrr,
        )
      )
    }

    "handle weather series data by storing it into state" in {
      val oldState = PvState(0L, defaultSimulationStart)

      val actualState =
        pvModel.handleInput(oldState, Seq(weatherSeriesData), onePU)

      actualState.tick shouldEqual oldState.tick
      actualState.dateTime shouldEqual defaultSimulationStart
      actualState.radiationData shouldEqual weatherSeriesData.series.map {
        case (tick, data: WeatherData) =>
          tick -> RadiationData(data)
      }
    }

    val startDate = ZonedDateTime.parse("2025-07-15T10:00:00+01:00")
    // dir. irr., diff. irr., hour, expected power
    val radiationToResult = Seq(
      (400.0, 40.0, 10, -3526.5055027),
      (500.0, 60.0, 11, -4475.9787502),
      (100.0, 10.0, 12, -871.793398),
      (200.0, 50.0, 13, -1883.7032106),
    )

    "calculate active power output depending on radiation and time" in {
      val testCases = Table(
        ("dirIrr", "diffIrr", "hour", "expectedPower"),
        radiationToResult*
      )

      forAll(testCases) {
        (dirIrr: Double, diffIrr: Double, hour: Int, expectedPower: Double) =>
          val state = PvState(
            tick = 0L,
            dateTime = startDate.withHour(hour),
            dirIrradiance = WattsPerSquareMeter(dirIrr),
            diffIrradiance = WattsPerSquareMeter(diffIrr),
          )
          val (operatingPoint, nextTick) =
            pvModel.determineOperatingPoint(state)

          operatingPoint.activePower should approximate(Watts(expectedPower))
          nextTick shouldBe None
      }
    }

    "calculate forecast flex power series correctly" in {
      val (radiationData, expectedResults) =
        radiationToResult.zipWithIndex.map {
          case ((dirIrr, diffIrr, _, expectedPower), i) =>
            val tick = i * 3600L
            val data = RadiationData(
              dirIrradiance = WattsPerSquareMeter(dirIrr),
              diffIrradiance = WattsPerSquareMeter(diffIrr),
            )
            val expectedResult = Watts(expectedPower)

            (tick -> data, tick -> expectedResult)
        }.unzip

      val state = PvState(
        tick = 0L,
        dateTime = startDate,
        radiationData = radiationData.to(SortedMap),
      )

      val flexOptions =
        pvModel
          .flexModels(FlexType.EnergyBoundaries)
          .determineFlexOptions(
            state,
            DataTimeType.CurrentAndForecast(
              forecastLength = Hours(4),
              forecastResolution = Hours(1),
            ),
          )

      flexOptions match {
        case EnergyBoundariesFlexOptions(boundaries) =>
          boundaries should have size 1

          val energyLimits = boundaries.headOption.value.energyLimits
          energyLimits should have size expectedResults.size + 1

          expectedResults
            // adding dummy value so that last energy is tested
            .appended(14400L -> zeroKW)
            .foldLeft(zeroKWh) { case (expectedEnergy, (tick, expectedPower)) =>
              energyLimits(tick).getUpper should approximate(expectedEnergy)
              energyLimits(tick).getLower should approximate(expectedEnergy)

              expectedEnergy + expectedPower * Hours(1)
            }
        case unexpected => fail(s"Received unexpected flex options $unexpected")
      }

    }

  }

}
