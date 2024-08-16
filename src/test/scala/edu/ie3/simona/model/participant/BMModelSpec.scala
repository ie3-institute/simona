/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.`type`.BmTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.EuroPerKilowatthour
import squants.energy.{Kilowatts, Megawatts}
import squants.market.EUR
import squants.thermal.Celsius
import squants.{Power, Temperature}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID

/** Test class that tries to cover all special cases of the current
  * implementation of the {@link BMModel}
  *
  * Test results have been calculated on paper using equations from wiki:
  * https://wiki.ie3.e-technik.tu-dortmund.de/!simona/model:bm_model
  */

class BMModelSpec extends UnitSpec {

  implicit val powerTolerance: Power = Megawatts(1e-4)
  implicit val power2Tolerance: Power = Kilowatts(1e-4)
  implicit val usageTolerance: Double = 1e-12

  val bmType: BmTypeInput = new BmTypeInput(
    UUID.fromString("bc06e089-03cd-481e-9e28-228266a148a4"),
    "BM Model Test Type 1",
    Quantities.getQuantity(0, edu.ie3.util.quantities.PowerSystemUnits.EURO),
    Quantities.getQuantity(
      0.05d,
      edu.ie3.util.quantities.PowerSystemUnits.EURO_PER_KILOWATTHOUR,
    ),
    Quantities.getQuantity(
      5,
      edu.ie3.util.quantities.PowerSystemUnits.PERCENT_PER_HOUR,
    ),
    Quantities.getQuantity(
      190,
      edu.ie3.util.quantities.PowerSystemUnits.KILOVOLTAMPERE,
    ),
    1d,
    Quantities.getQuantity(100d, tech.units.indriya.unit.Units.PERCENT),
  )

  def buildBmModel(): BMModel = {
    new BMModel(
      UUID.fromString("1b332f94-03e4-4abe-b142-8fceca689c53"),
      "BM Model Test",
      OperationInterval(0L, 86400L),
      QControl(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
      Kilowatts(190),
      bmType.getCosPhiRated,
      "MockNode",
      isCostControlled = true,
      EUR(bmType.getOpex.getValue.doubleValue()),
      EuroPerKilowatthour(0.51d),
      0.05,
    )
  }

  "A BMModel" should {
    "calculate K1 correctly" in {
      val bmModel = buildBmModel()

      val testCases = Table(
        ("Time", "K1 Solution"),
        ("2019-01-04T05:15:00+01:00[Europe/Berlin]", 1d), // Friday
        ("2019-01-07T05:15:00+01:00[Europe/Berlin]", 1d), // Monday
        ("2019-01-05T05:15:00+01:00[Europe/Berlin]", 0.96d), // Saturday, 5:15AM
        ("2019-01-05T15:59:00+01:00[Europe/Berlin]", 0.995d), // Sunday, 3:59PM
      )

      testCases.foreach { case (time, k1Sol) =>
        val k1Calc = bmModel.calculateK1(ZonedDateTime.parse(time))
        k1Calc should be(k1Sol)
      }
    }
  }

  "calculate K2 correctly" in {
    val bmModel = buildBmModel()

    val testCases = Table(
      ("Time", "K2 Solution"),
      (
        "2019-05-29T05:15:00+02:00[Europe/Berlin]",
        1.03d,
      ), // Day 149 of the year
      (
        "2019-05-30T05:15:00+02:00[Europe/Berlin]",
        0.61d,
      ), // Day 150 of the year
      (
        "2019-08-31T05:15:00+02:00[Europe/Berlin]",
        0.61d,
      ), // Day 243 of the year
      ("2019-09-01T05:15:00+02:00[Europe/Berlin]", 1.03d), // Day 244 of the year
    )

    testCases.foreach { case (time, k2Sol) =>
      val k2Calc = bmModel.calculateK2(ZonedDateTime.parse(time))
      k2Calc should be(k2Sol)
    }
  }

  "calculate PTh correctly" in {
    val bmModel = buildBmModel()

    val testCases = Table(
      ("Temperature", "K1", "K2", "PTh Sol"),
      (19.28, 1d, 1d, 5.62d), // independent of temp
      (30d, 2d, 3d, 33.72d),
      (19.2799999d, 1d, 1d, 5.6147201076d), // dependent on temp
      (15d, 1.01d, 0.61d, 6.296542d), // somewhat realistic
    )

    testCases.foreach { case (temp, k1, k2, pThSol) =>
      val pThCalc = bmModel.calculatePTh(Celsius(temp), k1, k2)
      pThCalc should approximate(Megawatts(pThSol))(powerTolerance)
    }
  }

  "calculate usage correctly" in {
    val bmModel = buildBmModel()

    val testCases = Table(
      ("PTh", "Usage Solution"),
      (43.14d, 1d), // exactly maximum heat
      (50d, 1d), // more than maximum, cap to 1
      (20d, 0.463606861382d), // less than max
      (0d, 0d), // zero
    )

    testCases.foreach { case (pTh, usageSol) =>
      val usageCalc = bmModel.calculateUsage(Megawatts(pTh))
      usageCalc should be(usageSol +- usageTolerance)
    }
  }

  "calculate efficiency correctly" in {
    val bmModel = buildBmModel()

    val testCases = Table(
      ("Usage", "Efficiency Sol"),
      (1d, 1d),
      (0d, 0.724d),
      (0.75d, 0.98425d),
      (0.86446317d, 0.993848918615d),
    )

    testCases.foreach { case (usage, effSol) =>
      val effCalc = bmModel.calculateEff(usage)
      effCalc should be(effSol +- 0.000000001)
    }
  }

  "calculate electrical output correctly" in {

    val testCases = Table(
      ("FeedInTariff", "Usage", "Efficiency", "PEl Sol"),
      (0.051d, 1d, 1d, -190d), // tariff greater than opex => full power
      (
        0.04d,
        0.75d,
        0.98425d,
        -140.255625d,
      ), // tariff too little, only serve heat demand
      (0.04d, 1d, 1d, -190d), // tariff too little, but max heat demand
    )

    testCases.foreach { case (feedInTariff, usage, eff, pElSol) =>
      val bmModel = new BMModel(
        UUID.fromString("8fbaf82d-5170-4636-bd7a-790eccbea880"),
        "BM Model Test",
        OperationInterval(0L, 86400L),
        QControl(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
        Kilowatts(190),
        bmType.getCosPhiRated,
        "MockNode",
        isCostControlled = true,
        EUR(bmType.getOpex.getValue.doubleValue()),
        EuroPerKilowatthour(feedInTariff),
        0.05,
      )

      val pElCalc = bmModel.calculateElOutput(usage, eff)
      pElCalc.value should be(Kilowatts(pElSol).value +- 1e-4)
    }
  }

  "apply load gradient correctly" in {
    val bmModel = buildBmModel()

    val testCases = Table(
      ("Last Power", "PEl", "PEl Sol"),
      (
        Kilowatts(-100d), // Last Power
        Kilowatts(-120d), // PEl
        Kilowatts(-109.5d), // PEl Solution
      ), // increase of power, more than load gradient allows
      (
        Kilowatts(-50d), // Last Power
        Kilowatts(-55d), // PEl
        Kilowatts(-55d), // PEl Solution
      ), // increase, within load gradient
      (
        Kilowatts(-50d), // Last Power
        Kilowatts(-41d), // PEl
        Kilowatts(-41d), // PEl Solution
      ), // decrease, within load gradient
      (
        Kilowatts(-30d), // Last Power
        Kilowatts(-15d), // PEl
        Kilowatts(-20.5d), // PEl Solution
      ), // decrease, more than load gradient
    )

    testCases.foreach { case (lastPower, pEl, pElSol) =>
      bmModel._lastPower = Some(lastPower)
      val pElCalc = bmModel.applyLoadGradient(pEl)
      pElCalc should approximate(pElSol)(power2Tolerance)
    }
  }

  "calculate P correctly" in {

    val testCases = Table(
      ("time", "temp", "costControlled", "lastPower", "powerSol"),
      (
        "2019-01-05T05:15:00+01:00[Europe/Berlin]",
        Celsius(10),
        true,
        Kilowatts(-40.0),
        Kilowatts(-49.5),
      ),
      (
        "2019-01-04T05:15:00+01:00[Europe/Berlin]",
        Celsius(10),
        true,
        Kilowatts(-80.0),
        Kilowatts(-89.5),
      ),
      (
        "2019-01-04T05:15:00+01:00[Europe/Berlin]",
        Celsius(-20),
        true,
        Kilowatts(-182.0),
        Kilowatts(-190),
      ),
      (
        "2019-01-04T05:15:00+01:00[Europe/Berlin]",
        Celsius(-7),
        true,
        Kilowatts(-182.0),
        Kilowatts(-190),
      ),
      (
        "2019-01-04T05:15:00+01:00[Europe/Berlin]",
        Celsius(-7),
        false,
        Kilowatts(-150.0),
        Kilowatts(-152.16900643778735),
      ),
      (
        "2019-07-07T10:15:00+02:00[Europe/Berlin]",
        Celsius(19),
        true,
        Kilowatts(-10.0),
        Kilowatts(-19.5),
      ),
      (
        "2019-07-05T05:15:00+02:00[Europe/Berlin]",
        Celsius(20),
        true,
        Kilowatts(-20.0),
        Kilowatts(-29.5),
      ),
      (
        "2019-07-06T10:15:00+02:00[Europe/Berlin]",
        Celsius(20),
        true,
        Kilowatts(0.0),
        Kilowatts(-9.5),
      ),
      (
        "2019-07-05T05:15:00+02:00[Europe/Berlin]",
        Celsius(22),
        true,
        Kilowatts(-22.0),
        Kilowatts(-31.5),
      ),
    )

    forAll(testCases) {
      (
          time: String,
          temp: Temperature,
          costControlled: Boolean,
          lastPower: Power,
          powerSol: Power,
      ) =>
        val dateTime = ZonedDateTime.parse(time)
        val relevantData = new BMModel.BMCalcRelevantData(dateTime, temp)

        val bmModel = new BMModel(
          UUID.fromString("08a8134d-04b7-45de-a937-9a55fab4e1af"),
          "BM Model Test",
          OperationInterval(0L, 86400L),
          QControl(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
          Kilowatts(190),
          bmType.getCosPhiRated(),
          "MockNode",
          costControlled,
          EUR(bmType.getOpex.getValue.doubleValue()),
          EuroPerKilowatthour(0.51d),
          0.05,
        )

        bmModel._lastPower = Some(lastPower)
        val powerCalc =
          bmModel.calculateActivePower(ConstantState, relevantData)
        powerCalc should be(powerSol)
    }
  }
}
