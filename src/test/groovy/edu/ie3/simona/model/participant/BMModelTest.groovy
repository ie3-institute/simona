/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.type.BmTypeInput
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.interfaces.EnergyPrice
import edu.ie3.util.scala.OperationInterval
import scala.Some
import spock.lang.Shared
import spock.lang.Specification
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.unit.Units.CELSIUS
import static tech.units.indriya.unit.Units.PERCENT

/**
 * Test class that tries to cover all special cases of the current implementation of the {@link BMModel}
 *
 * Test results have been calculated on paper using equations from wiki: https://wiki.ie3.e-technik.tu-dortmund.de/!simona/model:bm_model
 */
class BMModelTest extends Specification {

  @Shared
  NodeInput nodeInput
  @Shared
  BmTypeInput bmType
  @Shared
  double scalingFactor = 1.0d

  def setupSpec() {
    // build the NodeInputModel
    nodeInput = Mock(NodeInput)

    // build the BMTypesInputModel
    bmType = new BmTypeInput(
        UUID.fromString("bc06e089-03cd-481e-9e28-228266a148a4"),
        "BM Model Test Type 1",
        Quantities.getQuantity(0, EURO),
        Quantities.getQuantity(0.05d, EURO_PER_KILOWATTHOUR),
        Quantities.getQuantity(5d, PERCENT_PER_HOUR),
        Quantities.getQuantity(190, KILOVOLTAMPERE),
        1d,
        Quantities.getQuantity(100d, PERCENT)
        )
  }

  def getStandardModel() {
    return new BMModel(
        UUID.fromString("1b332f94-03e4-4abe-b142-8fceca689c53"),
        "BM Model Test",
        OperationInterval.apply(0L, 86400L),
        scalingFactor,
        QControl.apply(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
        Quantities.getQuantity(190, KILOVOLTAMPERE),
        bmType.getCosPhiRated(),
        "MockNode",
        true,
        bmType.getOpex() as ComparableQuantity<EnergyPrice>,
        Quantities.getQuantity(0.051d, EURO_PER_KILOWATTHOUR),
        Quantities.getQuantity(5d, PERCENT_PER_HOUR))
  }

  def "Test calculateK1"() {
    given:
    BMModel bmModel = getStandardModel()

    when:
    def k1Calc = bmModel.calculateK1(ZonedDateTime.parse(time))

    then:
    k1Calc == k1Sol

    where:
    time                                       || k1Sol
    '2019-01-04T05:15:00+01:00[Europe/Berlin]' || 1d       // Friday
    '2019-01-07T05:15:00+01:00[Europe/Berlin]' || 1d       // Monday
    '2019-01-05T05:15:00+01:00[Europe/Berlin]' || 0.96d    // Saturday, 5:15AM
    '2019-01-05T15:59:00+01:00[Europe/Berlin]' || 0.995d   // Sunday, 3:59PM
  }

  def "Test calculateK2"() {
    given:
    BMModel bmModel = getStandardModel()

    when:
    def k2Calc = bmModel.calculateK2(ZonedDateTime.parse(time))

    then:
    k2Calc == k2Sol

    where:
    time                                       || k2Sol
    '2019-05-29T05:15:00+02:00[Europe/Berlin]' || 1.03d     // Day 149 of the year
    '2019-05-30T05:15:00+02:00[Europe/Berlin]' || 0.61d     // Day 150 of the year
    '2019-08-31T05:15:00+02:00[Europe/Berlin]' || 0.61d     // Day 243 of the year
    '2019-09-01T05:15:00+02:00[Europe/Berlin]' || 1.03d     // Day 244 of the year
  }

  def "Test calculatePTh"() {
    given:
    BMModel bmModel = getStandardModel()

    when:
    def pThCalc = bmModel.calculatePTh(Quantities.getQuantity(temp, CELSIUS), k1, k2)

    then: "compare in watts"
    Math.abs(pThCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(pThSol, MEGAWATT).toSystemUnit().getValue().doubleValue()) < 0.0001

    where:
    temp        | k1    | k2    || pThSol
    19.28d      | 1d    | 1d    || 5.62d          // independent of temp
    30d         | 2d    | 3d    || 33.72d
    19.2799999d | 1d    | 1d    || 5.6147201076d  // dependent on temp
    15d         | 1.01d | 0.61d || 6.296542d      // somewhat realistic
  }

  def "Test calculateUsage"() {
    given:
    BMModel bmModel = getStandardModel()

    when:
    def usageCalc = bmModel.calculateUsage(Quantities.getQuantity(pTh, MEGAWATT))

    then:
    Math.abs(usageCalc - usageSol) < 0.00000001

    where:
    pTh      || usageSol
    43.14d   || 1d               // exactly maximum heat
    50d      || 1d               // more than maximum, cap to 1
    20d      || 0.463606861382d  // less than max
    0d       || 0d               // zero
  }

  def "Test calculateEff"() {
    given:
    BMModel bmModel = getStandardModel()

    when:
    def effCalc = bmModel.calculateEff(usage)

    then:
    Math.abs(effCalc - effSol) < 0.000000001

    where:
    usage        || effSol
    1d           || 1d
    0d           || 0.724d
    0.75d        || 0.98425d
    0.86446317d  || 0.993848918615d
  }

  def "Test calculateElOutput"() {
    when:
    BMModel bmModel = new BMModel(
        UUID.fromString("8fbaf82d-5170-4636-bd7a-790eccbea880"),
        "BM Model Test",
        OperationInterval.apply(0L, 86400L),
        scalingFactor,
        QControl.apply(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
        Quantities.getQuantity(190, KILOVOLTAMPERE),
        bmType.getCosPhiRated(),
        "MockNode",
        true,
        bmType.getOpex() as ComparableQuantity<EnergyPrice>,
        Quantities.getQuantity(feedInTariff, EURO_PER_KILOWATTHOUR),
        Quantities.getQuantity(5d, PERCENT_PER_HOUR))

    def pElCalc = bmModel.calculateElOutput(usage, eff)

    then: "compare in watts"
    Math.abs(pElCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(pElSol, KILOWATT).toSystemUnit().getValue().doubleValue()) < 0.0001

    where:
    feedInTariff | usage  | eff      || pElSol
    0.051d       | 1d     | 1d       || -190d         // tariff greater than opex => full power
    0.04d        | 0.75d  | 0.98425d || -140.255625d  // tariff too little, only serve heat demand
    0.04d        | 1d     | 1d       || -190d         // tariff too little, but max heat demand
  }

  def "Test applyLoadGradient"() {
    given:
    BMModel bmModel = getStandardModel()
    bmModel._lastPower = new Some(Quantities.getQuantity(lastPower, KILOWATT))

    when:
    def pElCalc = bmModel.applyLoadGradient(Quantities.getQuantity(pEl, KILOWATT))

    then:
    pElCalc == Quantities.getQuantity(pElSol, KILOWATT)

    where:
    lastPower  | pEl    || pElSol
    -100d      | -120d  || -109.5d  // increase of power, more than load gradient allows
    -50d       | -55d   || -55d     // increase, within load gradient
    -50d       | -41d   || -41d     // decrease, within load gradient
    -30d       | -15    || -20.5d   // decrease, more than load gradient
  }

  def "Test calculateP"() {
    given: "date, time, a temperature and last power output and the built model"
    // construct date and time from string
    ZonedDateTime dateTime = ZonedDateTime.parse(time)

    /* Prepare the calculation relevant data */
    BMModel.BMCalcRelevantData relevantData = new BMModel.BMCalcRelevantData(dateTime, Quantities.getQuantity(temp, CELSIUS))

    BMModel bmModel = new BMModel(
        UUID.fromString("08a8134d-04b7-45de-a937-9a55fab4e1af"),
        "BM Model Test",
        OperationInterval.apply(0L, 86400L),
        scalingFactor,
        QControl.apply(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
        Quantities.getQuantity(190, KILOVOLTAMPERE),
        bmType.getCosPhiRated(),
        "MockNode",
        costControlled,
        bmType.getOpex() as ComparableQuantity<EnergyPrice>,
        Quantities.getQuantity(0.051d, EURO_PER_KILOWATTHOUR),
        Quantities.getQuantity(5d, PERCENT_PER_HOUR))

    // modify data store: add last output power, one hour in the past
    bmModel._lastPower = new Some(Quantities.getQuantity(lastPower, KILOWATT))

    when: "the power from the grid is calculated"
    def powerCalc = bmModel.calculateActivePower(new BMModel.BmState(), relevantData)

    then: "compare in kilowatts"
    Math.abs(powerCalc.toKilowatts() - powerSol.doubleValue()) < 1e-12

    where:
    time                                       | temp | costControlled | lastPower || powerSol
    '2019-01-05T05:15:00+01:00[Europe/Berlin]' | 10   | true           | -40.0     || -49.5           // weekend day in heating season, power increase capped by load gradient
    '2019-01-04T05:15:00+01:00[Europe/Berlin]' | 10   | true           | -80.0     || -70.5           // working day in heating season, power decrease capped by load gradient
    '2019-01-04T05:15:00+01:00[Europe/Berlin]' | -20  | true           | -182.0    || -190            // peek load boiler activated, max output because cost < revenues
    '2019-01-04T05:15:00+01:00[Europe/Berlin]' | -7   | true           | -182.0    || -190            // close to peak load, max output because cost < revenues
    '2019-01-04T05:15:00+01:00[Europe/Berlin]' | -7   | false          | -150.0    || -152.16900643778735  // close to peak load, not cost controlled but just serving heat demand
    '2019-07-07T10:15:00+02:00[Europe/Berlin]' | 19   | true           | -10.0     || -12.099949463243976    // weekend day outside heating season, increase not capped
    '2019-07-05T05:15:00+02:00[Europe/Berlin]' | 20   | true           | -20.0     || -11.70638561892377   // working day outside heating season, decrease not capped
    '2019-07-06T10:15:00+02:00[Europe/Berlin]' | 20   | true           | -0.0      || -9.5            // weekend day outside heating season, increase capped
    '2019-07-05T05:15:00+02:00[Europe/Berlin]' | 22   | true           | -22.0     || -12.5           // working day outside heating season, decrease capped
  }
}