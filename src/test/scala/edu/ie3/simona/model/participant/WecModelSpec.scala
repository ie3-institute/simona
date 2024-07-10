/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.`type`.WecTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.{ReactivePowerCharacteristic, WecCharacteristicInput}
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.participant.WecModel.WecRelevantData
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.Watts
import squants.mass.{Density, KilogramsPerCubicMeter}
import squants.motion.{MetersPerSecond, Pascals}
import squants.thermal.Celsius
import squants.{Each, Power}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{METRE, PERCENT, SQUARE_METRE}

import java.util.UUID

class WecModelSpec extends UnitSpec with DefaultTestData {

  private implicit val densityTolerance: Density = KilogramsPerCubicMeter(1e-5)
  private implicit val powerTolerance: Power = Watts(1e-5)

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
    false,
  )

  def buildWecModel(): WecModel = {
    WecModel.apply(
      inputModel,
      1,
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z"),
    )
  }

  "WecModel" should {

    "check build method of companion object" in {
      val wecModel = buildWecModel()
      wecModel.uuid shouldBe inputModel.getUuid
      wecModel.id shouldBe inputModel.getId
      wecModel.rotorArea.toSquareMeters shouldBe (typeInput.getRotorArea.toSystemUnit.getValue
        .doubleValue() +- 1e-5)
      wecModel.cosPhiRated shouldBe typeInput.getCosPhiRated
      wecModel.sRated.toWatts shouldBe (typeInput.getsRated.toSystemUnit.getValue
        .doubleValue() +- 1e-5)
      wecModel.betzCurve shouldBe WecModel.WecCharacteristic.apply(
        inputModel.getType.getCpCharacteristic
      )
    }

    "determine Betz coefficient correctly" in {
      val wecModel = buildWecModel()
      val velocities = Seq(2.0, 2.5, 18.0, 27.0, 34.0, 40.0)
      val expectedBetzResults = Seq(0.115933516, 0.2010945555, 0.108671106,
        0.032198846, 0.000196644, 0.0)
      velocities.zip(expectedBetzResults).foreach {
        case (velocity, betzResult) =>
          val windVel = MetersPerSecond(velocity)
          val betzFactor = wecModel.determineBetzCoefficient(windVel)
          val expected = Each(betzResult)
          betzFactor shouldEqual expected
      }
    }

    "calculate active power output depending on velocity" in {
      val wecModel = buildWecModel()
      val velocities =
        Seq(1.0, 2.0, 3.0, 7.0, 9.0, 13.0, 15.0, 19.0, 23.0, 27.0, 34.0, 40.0)
      val expectedPowers =
        Seq(0, -2948.80958, -24573.41320, -522922.23257, -1140000, -1140000,
          -1140000, -1140000, -1140000, -1140000, -24573.39638, 0)

      velocities.zip(expectedPowers).foreach { case (velocity, power) =>
        val wecData = new WecRelevantData(
          MetersPerSecond(velocity),
          Celsius(20),
          Some(Pascals(101325d)),
        )
        val result =
          wecModel.calculateActivePower(ModelState.ConstantState, wecData)
        val expectedPower = Watts(power)
        math.abs(
          result.toWatts - expectedPower.toWatts
        ) should be < powerTolerance.toWatts
      }
    }

    "calculate air density correctly" in {
      val wecModel = buildWecModel()
      val testCases = Seq(
        (-15.0, 100129.44, 1.35121),
        (-5.0, 99535.96, 1.29311),
        (0.0, 99535.96, 1.26944),
        (5.0, 100129.44, 1.25405),
        (20.0, 100129.44, 1.18988),
        (25.0, 100427.25, 1.17341),
        (37.0, 100427.25, 1.12801),
        // test cases where no air pressure is given
        (0.0, -1.0, 1.2041),
        (5.0, -1.0, 1.2041),
        (40.0, -1.0, 1.2041),
      )

      testCases.foreach { case (temperature, pressure, densityResult) =>
        val temperatureV = Celsius(temperature)
        val pressureV =
          if (pressure > 0) Some(Pascals(pressure)) else Option.empty

        val airDensity = wecModel
          .calculateAirDensity(temperatureV, pressureV)
          .toKilogramsPerCubicMeter
        math.abs(
          airDensity - densityResult
        ) should be < densityTolerance.toKilogramsPerCubicMeter
      }
    }

    "calculate active power output depending on temperature" in {
      val wecModel = buildWecModel()
      val temperatures = Seq(35.0, 20.0, -25.0)
      val expectedPowers = Seq(-23377.23862, -24573.41320, -29029.60338)

      temperatures.zip(expectedPowers).foreach { case (temperature, power) =>
        val wecData = new WecRelevantData(
          MetersPerSecond(3.0),
          Celsius(temperature),
          Some(Pascals(101325d)),
        )
        val result =
          wecModel.calculateActivePower(ModelState.ConstantState, wecData)
        result.toWatts shouldBe power.doubleValue() +- powerTolerance.toWatts
      }
    }
  }
}
