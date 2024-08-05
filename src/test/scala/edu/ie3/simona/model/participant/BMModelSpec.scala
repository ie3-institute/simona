/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.system.`type`.BmTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.scala.OperationInterval
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import squants.energy.{Kilowatts, Megawatts}
import squants.market.EUR
import squants.thermal.Celsius
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID

/**
 * Test class that tries to cover all special cases of the current implementation of the {@link BMModel}
 *
 * Test results have been calculated on paper using equations from wiki: https://wiki.ie3.e-technik.tu-dortmund.de/!simona/model:bm_model
 */
class BMModelSpec extends AnyFlatSpec with Matchers {

  val nodeInput: NodeInput = _
  val bmType: BmTypeInput = new BmTypeInput(
    UUID.fromString("bc06e089-03cd-481e-9e28-228266a148a4"),
    "BM Model Test Type 1",
    Quantities.getQuantity(0, EUR),
    Quantities.getQuantity(0.05, EuroPerKilowattHour),
    Quantities.getQuantity(5, edu.ie3.util.quantities.PowerSystemUnits.PERCENT_PER_HOUR),
    Quantities.getQuantity(190, edu.ie3.util.quantities.PowerSystemUnits.KILOVOLTAMPERE),
    1d,
    Quantities.getQuantity(100d, tech.units.indriya.unit.Units.PERCENT)
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
      EuroPerKilowattHour(0.051d),
      0.05
    )
  }

  "BMModel" should "calculate K1 correctly" in {
    val bmModel = buildBmModel()

    val testCases = Seq(
      ("2019-01-04T05:15:00+01:00[Europe/Berlin]", 1d), // Friday
      ("2019-01-07T05:15:00+01:00[Europe/Berlin]", 1d), // Monday
      ("2019-01-05T05:15:00+01:00[Europe/Berlin]", 0.96d), // Saturday, 5:15AM
      ("2019-01-05T15:59:00+01:00[Europe/Berlin]", 0.995d) // Sunday, 3:59PM
    )

    testCases.foreach { case (time, k1Sol) =>
      val k1Calc = bmModel.calculateK1(ZonedDateTime.parse(time))
      k1Calc shouldBe k1Sol
    }
  }

  it should "calculate K2 correctly" in {
    val bmModel = buildBmModel()

    val testCases = Seq(
      ("2019-05-29T05:15:00+02:00[Europe/Berlin]", 1.03d), // Day 149 of the year
      ("2019-05-30T05:15:00+02:00[Europe/Berlin]", 0.61d), // Day 150 of the year
      ("2019-08-31T05:15:00+02:00[Europe/Berlin]", 0.61d), // Day 243 of the year
      ("2019-09-01T05:15:00+02:00[Europe/Berlin]", 1.03d) // Day 244 of the year
    )

    testCases.foreach { case (time, k2Sol) =>
      val k2Calc = bmModel.calculateK2(ZonedDateTime.parse(time))
      k2Calc shouldBe k2Sol
    }
  }

  it should "calculate PTh correctly" in {
    val bmModel = buildBmModel()

    val testCases = Seq(
      (19.28, 1d, 1d, 5.62d), // independent of temp
      (30d, 2d, 3d, 33.72d),
      (19.2799999d, 1d, 1d, 5.6147201076d), // dependent on temp
      (15d, 1.01d, 0.61d, 6.296542d) // somewhat realistic
    )

    testCases.foreach { case (temp, k1, k2, pThSol) =>
      val pThCalc = bmModel.calculatePTh(Celsius(temp), k1, k2)
      pThCalc should be (Megawatts(pThSol) +- Megawatts(0.0001))
    }
  }

  it should "calculate usage correctly" in {
    val bmModel = buildBmModel()

    val testCases = Seq(
      (43.14d, 1d), // exactly maximum heat
      (50d, 1d), // more than maximum, cap to 1
      (20d, 0.463606861382d), // less than max
      (0d, 0d) // zero
    )

    testCases.foreach { case (pTh, usageSol) =>
      val usageCalc = bmModel.calculateUsage(Megawatts(pTh))
      usageCalc should be (usageSol +- 0.00000001)
    }
  }

  it should "calculate efficiency correctly" in {
    val bmModel = buildBmModel()

    val testCases = Seq(
      (1d, 1d),
      (0d, 0.724d),
      (0.75d, 0.98425d),
      (0.86446317d, 0.993848918615d)
    )

    testCases.foreach { case (usage, effSol) =>
      val effCalc = bmModel.calculateEff(usage)
      effCalc should be (effSol +- 0.000000001)
    }
  }

  it should "calculate electrical output correctly" in {
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
      EuroPerKilowattHour(feedInTariff),
      0.05
    )

    val testCases = Seq(
      (0.051d, 1d, 1d, -190d), // tariff greater than opex => full power
      (0.04d, 0.75d, 0.98425d, -140.255625d), // tariff too little, only serve heat demand
      (0.04d, 1d, 1d, -190d) // tariff too little, but max heat demand
    )

    testCases.foreach { case (feedInTariff, usage, eff, pElSol) =>
      val pElCalc = bmModel.calculateElOutput(usage, eff)
      pElCalc should be (Kilowatts(pElSol) +- Kilowatts(0.0001))
    }
  }

  it should "apply load gradient correctly" in {
    val bmModel = buildBmModel()

    val testCases = Seq(
      (-100d, -120d, -109.5d), // increase of power, more than load gradient allows
      (-50d, -55d, -55d), // increase, within load gradient
      (-50d, -41d, -41d), // decrease, within load gradient
      (-30d, -15d, -20.5d) // decrease, more than load gradient
    )

    testCases.foreach { case (lastPower, pEl, pElSol) =>
      bmModel._lastPower = Some(Kilowatts(lastPower))
      val pElCalc = bmModel.applyLoadGradient(Kilowatts(pEl))
      pElCalc should be (Kilowatts(pElSol))
    }
  }

  it should "calculate P correctly" in {
    val bmModel = new BMModel(
      UUID.fromString("08e44067-5d1e-4a6e-97ef-d8bc9c8c256a"),
      "BM Model Test",
      OperationInterval(0L, 86400L),
      QControl(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
      Kilowatts(190),
      bmType.getCosPhiRated,
      "MockNode",
      isCostControlled = true,
      EUR(bmType.getOpex.getValue.doubleValue()),
      EuroPerKilowattHour(feedInTariff),
      0.05
    )

    val testCases = Seq(
      (Megawatts(20), Megawatts(5), Megawatts(-15)),
      (Megawatts(40), Megawatts(10), Megawatts(-30)),
      (Megawatts(60), Megawatts(20), Megawatts(-40)),
      (Megawatts(80), Megawatts(30), Megawatts(-50))
    )

    testCases.foreach { case (heatDemand, power, pSol) =>
      val pCalc = bmModel.calculateP(heatDemand, power)
      pCalc should be (pSol)
    }
  }
}