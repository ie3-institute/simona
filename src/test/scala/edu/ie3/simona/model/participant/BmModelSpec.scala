/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.BmModel
import edu.ie3.simona.model.participant2.BmModel.BmState
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.{EuroPerKilowatthour, Kilovoltamperes}
import squants.energy.{Kilowatts, Megawatts}
import squants.market.EUR
import squants.thermal.Celsius
import squants.{Power, Temperature}

import java.time.ZonedDateTime
import java.util.UUID

/** Test class that tries to cover all special cases of the current
  * implementation of the [[BmModel]]
  *
  * Test results have been calculated on paper using equations from
  * documentation.
  */
class BmModelSpec extends UnitSpec {

  implicit val powerTolerance: Power = Kilowatts(1e-4)
  implicit val usageTolerance: Double = 1e-12

  private val bmModel = new BmModel(
    UUID.fromString("1b332f94-03e4-4abe-b142-8fceca689c53"),
    "BM Model Test",
    Kilovoltamperes(190),
    1d,
    QControl(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
    isCostControlled = true,
    EUR(0.05),
    EuroPerKilowatthour(0.51d),
    0.05,
  )

  "A BmModel" should {

    "calculate K1 correctly" in {
      val testCases = Table(
        ("Time", "K1 Solution"),
        ("2019-01-04T05:15:00+01:00[Europe/Berlin]", 1d), // Friday
        ("2019-01-07T05:15:00+01:00[Europe/Berlin]", 1d), // Monday
        ("2019-01-05T05:15:00+01:00[Europe/Berlin]", 0.96d), // Saturday, 5:15AM
        ("2019-01-05T15:59:00+01:00[Europe/Berlin]", 0.995d), // Sunday, 3:59PM
      )

      testCases.foreach { case (time, k1Sol) =>
        val k1Calc = BmModel.calculateK1(ZonedDateTime.parse(time))
        k1Calc should be(k1Sol)
      }
    }

    "calculate K2 correctly" in {
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
        (
          "2019-09-01T05:15:00+02:00[Europe/Berlin]",
          1.03d,
        ), // Day 244 of the year
      )

      testCases.foreach { case (time, k2Sol) =>
        val k2Calc = BmModel.calculateK2(ZonedDateTime.parse(time))
        k2Calc should be(k2Sol)
      }
    }

    "calculate PTh correctly" in {
      val testCases = Table(
        ("Temperature", "K1", "K2", "PTh Sol"),
        (19.28, 1d, 1d, 5.62d), // independent of temp
        (30d, 2d, 3d, 33.72d),
        (19.2799999d, 1d, 1d, 5.6147201076d), // dependent on temp
        (15d, 1.01d, 0.61d, 6.296542d), // somewhat realistic
      )

      testCases.foreach { case (temp, k1, k2, pThSol) =>
        val pThCalc = BmModel.calculatePTh(Celsius(temp), k1, k2)
        pThCalc should approximate(Megawatts(pThSol))
      }
    }

    "calculate usage correctly" in {
      val testCases = Table(
        ("PTh", "Usage Solution"),
        (43.14d, 1d), // exactly maximum heat
        (50d, 1d), // more than maximum, cap to 1
        (20d, 0.463606861382d), // less than max
        (0d, 0d), // zero
      )

      testCases.foreach { case (pTh, usageSol) =>
        val usageCalc = BmModel.calculateUsage(Megawatts(pTh))
        usageCalc should be(usageSol +- usageTolerance)
      }
    }

    "calculate efficiency correctly" in {
      val testCases = Table(
        ("Usage", "Efficiency Sol"),
        (1d, 1d),
        (0d, 0.724d),
        (0.75d, 0.98425d),
        (0.86446317d, 0.993848918615d),
      )

      testCases.foreach { case (usage, effSol) =>
        val effCalc = BmModel.calculateEff(usage)
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
          -140.25562499999998d,
        ), // tariff too little, only serve heat demand
        (0.04d, 1d, 1d, -190d), // tariff too little, but max heat demand
      )

      testCases.foreach { case (feedInTariff, usage, eff, pElSol) =>
        val caseModel =
          bmModel.copy(feedInTariff = EuroPerKilowatthour(feedInTariff))

        val pElCalc = caseModel.calculateElOutput(usage, eff)
        pElCalc should approximate(Kilowatts(pElSol))
      }
    }

    "apply load gradient correctly" in {
      val testCases = Table(
        ("Last Power", "PEl", "PEl Sol"),
        (
          Kilowatts(-100d),
          Kilowatts(-120d),
          Kilowatts(-109.5d),
        ), // increase of power, more than load gradient allows
        (
          Kilowatts(-50d),
          Kilowatts(-55d),
          Kilowatts(-55d),
        ), // increase, within load gradient
        (
          Kilowatts(-50d),
          Kilowatts(-41d),
          Kilowatts(-41d),
        ), // decrease, within load gradient
        (
          Kilowatts(-30d),
          Kilowatts(-15d),
          Kilowatts(-20.5d),
        ), // decrease, more than load gradient
      )

      testCases.foreach { case (lastPower, pEl, pElSol) =>
        val pElCalc = bmModel.applyLoadGradient(Some(lastPower), pEl)
        pElCalc should approximate(pElSol)
      }
    }

    "calculate P correctly" in {
      val testCases = Table(
        ("time", "temp", "costControlled", "lastPower", "powerSol"),
        // weekend day in heating season, power increase capped by load gradient
        (
          "2019-01-05T05:15:00+01:00[Europe/Berlin]",
          Celsius(10.0),
          true,
          Kilowatts(-40.0),
          Kilowatts(-49.5),
        ),
        // working day in heating season, power decrease capped by load gradient
        (
          "2019-01-04T05:15:00+01:00[Europe/Berlin]",
          Celsius(10.0),
          true,
          Kilowatts(-80.0),
          Kilowatts(-70.5),
        ),
        // peak load boiler activated, max output because cost < revenues
        (
          "2019-01-04T05:15:00+01:00[Europe/Berlin]",
          Celsius(-20.0),
          true,
          Kilowatts(-182.0),
          Kilowatts(-190.0),
        ),
        // close to peak load, max output because cost < revenues
        (
          "2019-01-04T05:15:00+01:00[Europe/Berlin]",
          Celsius(-7.0),
          true,
          Kilowatts(-182.0),
          Kilowatts(-190.0),
        ),
        // close to peak load, not cost controlled but just serving heat demand
        (
          "2019-01-04T05:15:00+01:00[Europe/Berlin]",
          Celsius(-7.0),
          false,
          Kilowatts(-150.0),
          Kilowatts(-152.16900643778735),
        ),
        // weekend day outside heating season, increase not capped
        (
          "2019-07-07T10:15:00+02:00[Europe/Berlin]",
          Celsius(19.0),
          true,
          Kilowatts(-10.0),
          Kilowatts(-12.099949463243976),
        ),
        // working day outside heating season, decrease not capped
        (
          "2019-07-05T05:15:00+02:00[Europe/Berlin]",
          Celsius(20.0),
          true,
          Kilowatts(-20.0),
          Kilowatts(-11.70638561892377),
        ),
        // weekend day outside heating season, increase capped
        (
          "2019-07-06T10:15:00+02:00[Europe/Berlin]",
          Celsius(20.0),
          true,
          Kilowatts(0.0),
          Kilowatts(-9.5),
        ),
        // working day outside heating season, decrease capped
        (
          "2019-07-05T05:15:00+02:00[Europe/Berlin]",
          Celsius(22.0),
          true,
          Kilowatts(-22.0),
          Kilowatts(-12.5),
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
          val caseModel = bmModel.copy(
            isCostControlled = costControlled,
            feedInTariff = EuroPerKilowatthour(0.051d),
          )

          val dateTime = ZonedDateTime.parse(time)

          // Tick is not relevant
          val state = BmState(0, dateTime, Celsius(temp), Some(lastPower))

          val (operatingPoint, nextTick) =
            caseModel.determineOperatingPoint(state)

          operatingPoint.activePower should approximate(powerSol)
          nextTick shouldBe None
      }
    }
  }
}
