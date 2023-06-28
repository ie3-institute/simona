/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static edu.ie3.datamodel.models.StandardUnits.*
import static edu.ie3.util.quantities.QuantityUtil.equals
import static edu.ie3.simona.model.participant.WecModel.WecRelevantData
import static tech.units.indriya.quantity.Quantities.getQuantity

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.characteristic.WecCharacteristicInput
import edu.ie3.datamodel.models.input.system.type.WecTypeInput
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.EmptyQuantity
import spock.lang.Shared
import spock.lang.Specification
import spock.lang.Unroll



class WecModelTest extends Specification {

  @Shared
  WecInput inputModel
  @Shared
  static final Double TOLERANCE = 1e-5

  def setupSpec() {
    def nodeInput = new NodeInput(
        UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
        "NodeInput",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        getQuantity(1d, PU),
        false,
        NodeInput.DEFAULT_GEO_POSITION,
        GermanVoltageLevelUtils.LV,
        -1)

    def typeInput = new WecTypeInput(
        UUID.randomUUID(),
        "WecTypeInput",
        getQuantity(1000, EURO),
        getQuantity(1000, ENERGY_PRICE),
        getQuantity(1200, S_RATED),
        0.95,
        //Using the characteristics of the Enercon E-82 wind turbine.
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
        getQuantity(15, EFFICIENCY),
        getQuantity(5281, ROTOR_AREA),
        getQuantity(20, HUB_HEIGHT))

    inputModel = new WecInput(
        UUID.randomUUID(),
        "WecInput",
        new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
        OperationTime.notLimited(),
        nodeInput,
        CosPhiFixed.CONSTANT_CHARACTERISTIC,
        typeInput,
        false)
  }

  def buildWecModel() {
    return WecModel.apply(inputModel, 1,
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00"),
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01 01:00:00"))
  }

  @Unroll
  def "Check build method of companion object"() {
    when:
    def wecModel = buildWecModel()
    then:
    wecModel.uuid() == inputModel.getUuid()
    wecModel.id() == inputModel.getId()
    wecModel.scalingFactor() == 1
    wecModel.sRated() == inputModel.type.sRated
    wecModel.cosPhiRated() == inputModel.type.cosPhiRated
    wecModel.rotorArea() == inputModel.type.rotorArea
    wecModel.betzCurve() == new WecModel.WecCharacteristic$().apply(inputModel.type.cpCharacteristic)
  }

  @Unroll
  def "Check active power output depending on velocity #velocity m/s"() {
    given:
    def wecModel = buildWecModel()
    def wecData = new WecRelevantData(getQuantity(velocity, METRE_PER_SECOND),
        getQuantity(20, CELSIUS), getQuantity(101325, PASCAL))

    when:
    def result = wecModel.calculateActivePower(wecData).to(WATT)
    def expected = getQuantity(power, WATT)

    then:
    equals(result, expected, TOLERANCE)

    where:
    velocity || power
    1.0      || 0
    2.0      || -2948.80958
    3.0      || -24573.41320
    7.0      || -522922.23257
    9.0      || -1140000
    13.0     || -1140000
    15.0     || -1140000
    19.0     || -1140000
    23.0     || -1140000
    27.0     || -1140000
    34.0     || -24573.39638
    40.0     || 0
  }

  @Unroll
  def "Check active power output depending on temperature #temperature Celsius"() {
    given:
    def wecModel = buildWecModel()
    def wecData = new WecRelevantData(getQuantity(3.0, METRE_PER_SECOND),
        getQuantity(temperature, CELSIUS), getQuantity(101325, PASCAL))

    when:
    def result = wecModel.calculateActivePower(wecData).to(WATT)
    def expected = getQuantity(power, WATT)

    then:
    equals(result, expected, TOLERANCE)

    where:
    temperature || power
    35          || -23377.23862
    20          || -24573.41320
    -25         || -29029.60338
  }

  @Unroll
  def "Check determineBetzCoefficient method with wind velocity #velocity m/s:"() {
    given:
    def wecModel = buildWecModel()
    def windVel = getQuantity(velocity, WIND_VELOCITY)

    when:
    def betzFactor = wecModel.determineBetzCoefficient(windVel)
    def expected = getQuantity(betzResult, PU)

    then:
    betzFactor == expected

    where:
    velocity || betzResult
    2        || 0.115933516
    2.5      || 0.2010945555
    18       || 0.108671106
    27       || 0.032198846
    34       || 0.000196644
    40       || 0.0
  }

  @Unroll
  def "Check calculateAirDensity method with temperature #temperature degrees Celsius and air pressure #pressure Pascal:"() {
    given:
    def wecModel = buildWecModel()
    def temperatureV = getQuantity(temperature, CELSIUS)
    def pressureV = EmptyQuantity.of(PASCAL)

    when:
    if (pressure > 0) {
      pressureV = getQuantity(pressure, PASCAL)
    }
    def airDensity = wecModel.calculateAirDensity(temperatureV, pressureV).to(KILOGRAM_PER_CUBIC_METRE)
    def expected = getQuantity(densityResult, KILOGRAM_PER_CUBIC_METRE)

    then:
    equals(airDensity, expected, TOLERANCE)

    where:
    temperature | pressure  || densityResult
    -15         | 100129.44 || 1.35121
    -5          | 99535.96  || 1.29311
    0           | 99535.96  || 1.26944
    5           | 100129.44 || 1.25405
    20          | 100129.44 || 1.18988
    25          | 100427.25 || 1.17341
    37          | 100427.25 || 1.12801
    // test case, where no air pressure is given (see WecModel.calculateAirDensity)
    0           | -1.0      || 1.2041
    5           | -1.0      || 1.2041
    40          | -1.0      || 1.2041
  }
}
