/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.`type`.WecTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.{
  ReactivePowerCharacteristic,
  WecCharacteristicInput,
}
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.participant.WecModel.{AirWeatherData, WecState}
import edu.ie3.simona.ontology.messages.flex.{
  EnergyBoundariesFlexOptions,
  FlexType,
}
import edu.ie3.simona.service.Data.SecondaryData.WeatherData
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.test.common.{UnitSpec, WeatherTestData}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKW, zeroKWh}
import squants.energy.{Energy, Power, WattHours, Watts}
import squants.motion.{MetersPerSecond, Pascals}
import squants.thermal.Celsius
import squants.time.Hours
import squants.{Dimensionless, Each}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{METRE, PERCENT, SQUARE_METRE}

import java.util.UUID
import scala.collection.immutable.SortedMap

class WecModelSpec extends UnitSpec with WeatherTestData {

  // testing tolerances
  private given Power = Watts(1e-6)
  private given Energy = WattHours(1e-6)
  private given Dimensionless = Each(1e-12)
  private given Double = 1e-9

  val nodeInput = new NodeInput(
    UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
    "NodeInput",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    Quantities.getQuantity(1, PowerSystemUnits.PU),
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.LV,
    -1,
  )

  val typeInput = new WecTypeInput(
    UUID.randomUUID(),
    "WecTypeInput",
    Quantities.getQuantity(1000, PowerSystemUnits.EURO),
    Quantities.getQuantity(1000, PowerSystemUnits.EURO_PER_MEGAWATTHOUR),
    Quantities.getQuantity(1200.0, PowerSystemUnits.KILOVOLTAMPERE),
    0.95,
    new WecCharacteristicInput(
      "cP:{(0.0,0.0), (1.0,0.0), (2.0,0.115933516), (3.0,0.286255595), (4.0,0.39610618), " +
        "(5.0,0.430345211), (6.0,0.45944023), (7.0,0.479507331), (8.0,0.492113623), " +
        "(9.0,0.500417188), (10.0,0.488466547), (11.0,0.420415054), (12.0,0.354241299), " +
        "(13.0,0.288470591), (14.0,0.230965702), (15.0,0.18778367), (16.0,0.154728976), " +
        "(17.0,0.128998552), (18.0,0.108671106), (19.0,0.09239975), (20.0,0.079221236), " +
        "(21.0,0.068434282), (22.0,0.059520087), (23.0,0.052089249), (24.0,0.045845623), " +
        "(25.0,0.040561273), (26.0,0.036058824), (27.0,0.032198846), (28.0,0.016618264), " +
        "(29.0,0.010330976), (30.0,0.006091519), (31.0,0.003331177), (32.0,0.001641637), " +
        "(33.0,0.000705423), (34.0,0.000196644), (35.0,0.0), (36.0,0.0), (37.0,0.0), (38.0,0.0), " +
        "(39.0,0.0), (40.0,0.0), (41.0,0.0), (42.0,0.0), (43.0,0.0), (44.0,0.0), (45.0,0.0), " +
        "(46.0,0.0), (47.0,0.0), (48.0,0.0), (49.0,0.0), (50.0,0.0)}"
    ),
    Quantities.getQuantity(15, PERCENT),
    Quantities.getQuantity(5281.0, SQUARE_METRE),
    Quantities.getQuantity(20, METRE),
  )

  val inputModel = new WecInput(
    UUID.randomUUID(),
    "WecTypeInput",
    nodeInput,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.00,0.95)}"),
    null,
    typeInput,
  )

  "WecModel" should {

    "be build properly by using factory" in {
      val wecModel = WecModel.Factory(inputModel).create()
      wecModel.uuid shouldBe inputModel.getUuid
      wecModel.cosPhiRated should approximate(typeInput.getCosPhiRated)
      wecModel.sRated.toVoltamperes shouldBe (typeInput.getsRated.toSystemUnit.getValue
        .doubleValue() +- 1e-5)
    }

    "handle singular weather data by storing it into state" in {
      val wecModel = WecModel.Factory(inputModel).create()
      val oldState = WecState(0L)

      val actualState =
        wecModel.handleInput(oldState, Seq(weatherData), onePU)

      actualState.tick shouldEqual oldState.tick
      actualState.weatherData shouldEqual SortedMap(
        0L -> AirWeatherData(
          windVelocity = weatherData.windVel,
          temperature = weatherData.temp,
        )
      )
    }

    "handle weather series data by storing it into state" in {
      val wecModel = WecModel.Factory(inputModel).create()
      val oldState = WecState(0L)

      val actualState =
        wecModel.handleInput(oldState, Seq(weatherSeriesData), onePU)

      actualState.tick shouldEqual oldState.tick
      actualState.weatherData shouldEqual weatherSeriesData.series.map {
        case (tick, data: WeatherData) =>
          tick -> AirWeatherData(data)
      }
    }

    "determine Betz coefficient correctly" in {
      val wecModel = WecModel.Factory(inputModel).create()

      val testCases = Table(
        ("velocity", "expectedBetzResult"),
        (2.0, 0.115933516),
        (2.5, 0.2010945555),
        (18.0, 0.108671106),
        (27.0, 0.032198846),
        (34.0, 0.000196644),
        (40.0, 0.0),
      )

      forAll(testCases) { case (velocity: Double, expectedBetzResult: Double) =>
        val windVel = MetersPerSecond(velocity)
        val betzFactor = wecModel.determineBetzCoefficient(windVel)

        betzFactor should approximate(Each(expectedBetzResult))
      }
    }

    val speedToResult = Seq(
      (1.0, 0.0),
      (2.0, -2948.809585),
      (3.0, -24573.413204),
      (7.0, -522922.232571),
      (9.0, -1140000.0),
      (13.0, -1140000.0),
      (15.0, -1140000.0),
      (19.0, -1140000.0),
      (23.0, -1140000.0),
      (27.0, -1140000.0),
      (34.0, -24573.396388),
      (40.0, 0.0),
    )

    "calculate active power output depending on velocity" in {
      val wecModel = WecModel.Factory(inputModel).create()
      val testCases = Table(
        ("velocity", "expectedPower"),
        speedToResult*
      )

      forAll(testCases) { (velocity: Double, expectedPower: Double) =>
        val state = WecState(
          tick = 0L,
          windVelocity = MetersPerSecond(velocity),
          temperature = Celsius(20),
          airPressure = Some(Pascals(101325d)),
        )
        val (operatingPoint, nextTick) =
          wecModel.determineOperatingPoint(state)

        operatingPoint.activePower should approximate(Watts(expectedPower))
        nextTick shouldBe None
      }
    }

    "calculate forecast flex power series correctly" in {
      val wecModel = WecModel.Factory(inputModel).create()

      val (weatherData, expectedResults) = speedToResult.zipWithIndex.map {
        case ((velocity, expectedPower), i) =>
          val tick = i * 3600L
          val data = AirWeatherData(
            windVelocity = MetersPerSecond(velocity),
            temperature = Celsius(20),
            airPressure = Some(Pascals(101325d)),
          )
          val expectedResult = Watts(expectedPower)

          (tick -> data, tick -> expectedResult)
      }.unzip

      val state = WecState(
        tick = 0L,
        weatherData = weatherData.to(SortedMap),
      )

      val flexOptions =
        wecModel
          .flexModels(FlexType.EnergyBoundaries)
          .determineFlexOptions(
            state,
            DataTimeType.CurrentAndForecast(
              forecastLength = Hours(12),
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
            .appended(43200L -> zeroKW)
            .foldLeft(zeroKWh) { case (expectedEnergy, (tick, expectedPower)) =>
              energyLimits(tick).getUpper should approximate(expectedEnergy)
              energyLimits(tick).getLower should approximate(expectedEnergy)

              expectedEnergy + expectedPower * Hours(1)
            }
        case unexpected => fail(s"Received unexpected flex options $unexpected")
      }

    }

    "calculate air density correctly" in {
      val wecModel = WecModel.Factory(inputModel).create()
      val testCases = Seq(
        (-15.0, 100129.44, 1.3512151548083537),
        (-5.0, 99535.96, 1.2931147269065832),
        (0.0, 99535.96, 1.2694443127219486),
        (5.0, 100129.44, 1.25405785444464),
        (20.0, 100129.44, 1.1898897909390296),
        (25.0, 100427.25, 1.1734149214121568),
        (37.0, 100427.25, 1.1280143763309192),
        // test cases where no air pressure is given
        (0.0, -1.0, 1.2041),
        (5.0, -1.0, 1.2041),
        (40.0, -1.0, 1.2041),
      )

      testCases.foreach { case (temperature, pressure, densityResult) =>
        val temperatureV = Celsius(temperature)
        val pressureV =
          if pressure > 0 then Some(Pascals(pressure)) else Option.empty

        val airDensity = wecModel
          .calculateAirDensity(temperatureV, pressureV)
          .toKilogramsPerCubicMeter

        airDensity should approximate(densityResult)
      }
    }

    "calculate active power output depending on temperature" in {
      val wecModel = WecModel.Factory(inputModel).create()
      val testCases = Table(
        ("temperature", "expectedPower"),
        (35.0, -23377.238620),
        (20.0, -24573.413204),
        (-25.0, -29029.603388),
      )

      forAll(testCases) { (temperature: Double, expectedPower: Double) =>
        val state = WecState(
          tick = 0L,
          windVelocity = MetersPerSecond(3.0),
          temperature = Celsius(temperature),
          airPressure = Some(Pascals(101325d)),
        )
        val (operatingPoint, nextTick) =
          wecModel.determineOperatingPoint(state)

        operatingPoint.activePower should approximate(Watts(expectedPower))
        nextTick shouldBe None
      }
    }
  }
}
