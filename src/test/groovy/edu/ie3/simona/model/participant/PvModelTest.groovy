/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.quantity.Quantities.getQuantity
import squants.Dimensionless
import squants.Each$
import squants.energy.Kilowatts$
import squants.space.Degrees$
import squants.space.Radians$
import squants.space.SquareMeters$
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.scala.quantities.Irradiation
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.Megavars$
import edu.ie3.util.scala.quantities.Sq
import edu.ie3.util.scala.quantities.WattHoursPerSquareMeter$
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Point
import scala.Option
import spock.lang.Shared
import spock.lang.Specification
import squants.space.Angle
import java.time.ZonedDateTime

/**
 * Test class that tries to cover all special cases of the current implementation of the PvModel
 *
 * Some of these test cases are taken from the examples of
 * Duffie, J. A., & Beckman, W. A. (2013). Solar engineering of thermal processes (4th ed.). Hoboken, N.J.: John Wiley & Sons.
 *
 * The page examples can be found using the page number provided in each test. Results may differ slightly from the
 * book as we sometimes use different formulas. Furthermore, sometimes the example might be slightly adapted to fit our needs.
 *
 */

class PvModelTest extends Specification {

  @Shared
  PvModel pvModel

  @Shared
  GeometryFactory geometryFactory

  def setupSpec() {

    // build the NodeInputModel (which defines the location of the pv input model)
    /// the NodeInputModel needs a GeoReference for the Pv to work
    geometryFactory = new GeometryFactory()
    Point p = geometryFactory.createPoint(
        new Coordinate(13.2491, 53.457909))
    NodeInput nodeInput = new NodeInput(
        UUID.fromString("85f8b517-8a2d-4c20-86c6-3ff3c5823e6d"),
        "NodeInputModel for PvModel Test",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        getQuantity(1, PU),
        false,
        p,
        GermanVoltageLevelUtils.MV_20KV,
        11
        )


    // build the PvInputModel
    PvInput pvInput = new PvInput(
        UUID.fromString("adb4eb23-1dd6-4406-a5e7-02e1e4c9dead"),
        "Pv Model Test",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        nodeInput,
        new CosPhiFixed("cosPhiFixed:{(0.0,0.9)}"),
        0.20000000298023224,
        getQuantity(-8.926613807678223, DEGREE_GEOM),
        getQuantity(97, PERCENT),
        getQuantity(41.01871871948242, DEGREE_GEOM),
        0.8999999761581421,
        1,
        false,
        getQuantity(10, KILOVOLTAMPERE),
        0.8999999761581421
        )

    // build the PvModel
    double scalingFactor = 1.0d
    pvModel = PvModel.apply(
        pvInput.uuid,
        pvInput.id,
        OperationInterval.apply(0L, 86400L),
        scalingFactor,
        QControl.apply(pvInput.qCharacteristics),
        Sq.create(pvInput.sRated.to(KILOWATT).value.doubleValue(), Kilowatts$.MODULE$),
        pvInput.cosPhiRated,
        Sq.create(pvInput.node.geoPosition.y, Degrees$.MODULE$),
        Sq.create(pvInput.node.geoPosition.x, Degrees$.MODULE$),
        pvInput.albedo,
        Sq.create(pvInput.etaConv.to(PU).value.doubleValue(), Each$.MODULE$),
        Sq.create(pvInput.azimuth.to(RADIAN).value.doubleValue(), Radians$.MODULE$),
        Sq.create(pvInput.elevationAngle.value.doubleValue(), Radians$.MODULE$),
        Sq.create(1d, SquareMeters$.MODULE$)
        )
  }

  def "A PvModel should have sMax set to be 10% higher than its sRated"() {
    expect:
    pvModel.sMax().toKilowatts() == ((pvModel.sRated().toKilowatts() * 1.1))
  }

  def "A PvModel should provide reactive power up to 110% of it's rated apparent power"() {
    given: "default adjusted voltage"
    Dimensionless adjustedVoltage = Sq.create(1, Each$.MODULE$) // needed for method call but not applicable for cosphi_p

    when: "the reactive power is calculated"
    def qCalc = pvModel.calculateReactivePower(Sq.create(pVal, Kilowatts$.MODULE$), adjustedVoltage)

    then:
    qCalc =~ Sq.create(qSoll, Megavars$.MODULE$)

    where:
    pVal || qSoll
    9.5d  || 0.004601059995959599d // above sRated (no q limitation)
    11d   || 0d        // above sMax (limit q becomes active)
  }

  def "Calculate day angle J"() {
    when:
    Angle jCalc = pvModel.calcAngleJ(ZonedDateTime.parse(time))

    then:
    jCalc =~ Sq.create(jSol, Radians$.MODULE$)

    where:
    time                                       || jSol
    '2019-01-05T05:15:00+01:00[Europe/Berlin]' || 0.06885682528415985d
    '2016-10-31T12:15:00+01:00[Europe/Berlin]' || 5.23311872159614873d // leap year => day = 305
    '2017-10-31T12:15:00+01:00[Europe/Berlin]' || 5.21590451527510877d // regular year => day = 304
    '2011-06-21T13:00:00+02:00[Europe/Berlin]' || 2.9436292808978335d // regular year => day = 172
    '2011-04-05T16:00:00+02:00[Europe/Berlin]' || 1.6181353941777565d // regular year => day = 95
    '2011-09-21T00:00:00+02:00[Europe/Berlin]' || 4.5273362624335105d // regular year => day = 264
    '2011-03-21T00:00:00+01:00[Europe/Berlin]' || 1.359922299362157d // regular year => day = 80
  }

  def "Calculate declination angle delta"() {
    when:
    Angle dayAngleQuantity = Sq.create(j, Radians$.MODULE$)
    Angle deltaCalc = pvModel.calcSunDeclinationDelta(dayAngleQuantity)

    then:
    deltaCalc =~ Sq.create(deltaSol, Radians$.MODULE$)

    where:
    j                  || deltaSol
    0d                 || -0.402449d           // Jan 1st
    2.943629280897834d || 0.40931542032971796d   // Jun 21 (maximum: 23.45°)
    6.024972212363987d || -0.4069636112528855d   // Dec 21 (minimum: -23.45°)
    4.52733626243351d  || 0.01790836361732633d   // Sep 21 (0°)
    1.359922299362157d || -0.0011505915019577827d   // Mar 21 (0°)
  }

  def "Calculate hour angle omega"() {
    when:
    ZonedDateTime dateTime = ZonedDateTime.parse(time)
    Angle dayAngleQuantity = Sq.create(j, Radians$.MODULE$)
    Angle longitudeQuantity = Sq.create(longitude, Radians$.MODULE$)

    Angle omegaCalc = pvModel.calcHourAngleOmega(dateTime, dayAngleQuantity, longitudeQuantity)

    then:
    omegaCalc =~ Sq.create(omegaSol, Radians$.MODULE$)

    where:
    time                                       | j                   | longitude   || omegaSol
    '2019-01-01T05:00:00+01:00[Europe/Berlin]' | 0d                  | 0.16d       || -1.9465030168609223d   // long: ~9.17°E
    '2019-01-01T10:05:00+01:00[Europe/Berlin]' | 0d                  | 0.16d       || -0.6156894622152458d   // different time: 10:05
    '2019-01-01T12:00:00+01:00[Europe/Berlin]' | 0d                  | 0.16d       || -0.11390730226687622d  // 12:00
    '2019-01-01T14:00:00+01:00[Europe/Berlin]' | 0d                  | 0.16d       || 0.40969147333142264d   // 14:00
    '2019-01-01T17:30:00+01:00[Europe/Berlin]' | 0d                  | 0.16d       || 1.3259893306284447d    // 17:30
    '2019-03-21T05:00:00+01:00[Europe/Berlin]' | 1.359922299362157d  | 0.16d       || -1.9677750757840207d   // different j (different date)
    '2019-01-01T05:00:00+01:00[Europe/Berlin]' | 0d                  | 0.175d      || -1.9315030168609224d   // different long, ~10°E
    '2011-06-21T11:00:00+02:00[Europe/Berlin]' | 2.9436292808978d    | 0.2337d     || -0.2960273936975511d   // long of Berlin (13.39E),
    '2011-06-21T12:00:00+02:00[Europe/Berlin]' | 2.9436292808978d    | 0.2337d     || -0.034228005898401644d // long of Berlin (13.39E),
    '2011-06-21T13:00:00+02:00[Europe/Berlin]' | 2.9436292808978d    | 0.2337d     || 0.2275713819007478d    // long of Berlin (13.39E),
    '2011-06-21T14:00:00+02:00[Europe/Berlin]' | 2.9436292808978d    | 0.2337d     || 0.4893707696998972d    // long of Berlin (13.39E),
    '2011-06-21T15:00:00+02:00[Europe/Berlin]' | 2.9436292808978d    | 0.2337d     || 0.7511701574990467d    // long of Berlin (13.39E),
    '2011-04-05T16:00:00+02:00[Europe/Berlin]' | 1.6181353941777565d | 0.2337d     || 1.0062695999127786d    // long of Berlin (13.39E),
    '2011-06-21T12:00:00+02:00[Europe/Berlin]' | 2.9436292808978d    | 0.5449d     || 0.2769719941015987d    // long of Cairo (31.22E),
  }

  def "Calculate sunset angle omegaSS"() {
    when:
    Angle latitudeQuantity = Sq.create(latitude, Radians$.MODULE$)
    Angle deltaQuantity = Sq.create(delta, Radians$.MODULE$)

    Angle omegaSSCalc = pvModel.calcSunsetAngleOmegaSS(latitudeQuantity, deltaQuantity)

    then:
    omegaSSCalc =~ Sq.create(omegaSSSol, Radians$.MODULE$)

    where:
    latitude     | delta        || omegaSSSol
    0.9d         | -0.402449d   || 1.0045975406286176d  // lat: ~51.57°N
    0.935d       | -0.402449d   || 0.956011693657339d   // different lat: ~53.57°N
    0.9d         | 0.017908d    || 1.5933675693198284d  // different delta
    0.157952297d | 0.384670567d || 1.635323424114512d  // //Example 2.2 Goswami Priciples of Solar Engineering
  }

  def "Calculate solar altitude angle alphaS"() {
    when:
    Angle omegaQuantity = Sq.create(omega, Radians$.MODULE$)
    Angle deltaQuantity = Sq.create(delta, Radians$.MODULE$)
    Angle latitudeQuantity = Sq.create(latitude, Radians$.MODULE$)

    Angle alphaSCalc = pvModel.calcSolarAltitudeAngleAlphaS(omegaQuantity, deltaQuantity, latitudeQuantity)

    then:
    alphaSCalc =~ Sq.create(alphaSSol, Radians$.MODULE$)

    where:
    omega              | delta               | latitude        || alphaSSol
    1.946503016860923d | -0.402449d          | 0.9d            || -0.5429594681352444d   // delta: Jan 1st, lat: ~51.57°N
    1.967775075784021d | -0.001150591501958d | 0.9d            || -0.24363984335678648d   // delta: March 21st
    1.946503016860923d | -0.402449d          | 0.935d          || -0.5417322854819461d   // delta: Jan 1st, lat: ~53.57°N
    1.256637061d       | -0.402449d          | 0.698d          || -0.033897520990303694d   // omega: 82°, delta: Jan 1st, lat: ~39.99°N
    0.409691473331422d | -0.402449d          | 0.9d            || 0.21956610107293822d    // omega: 14:00, delta: Jan 1st
    -0.85019406d       | -0.00720875d        | 0.9128072d      || 0.40911138927659646d  // omega: -48.71° = 09:00, delta: March 21st, lat: Berlin
    +0.22425484d       | +0.40899596d        | 0.9128072d      || 1.0386092658376944d   // omega: +12.84° = 14:00 MESZ = 13:00 MEZ, delta: June 21st, lat: Berlin
    -0.81703281d       | -0.00720875d        | 0.54628806d     || 0.619982384489836d    // omega: -36.9809° = 09:00, delta: March 21st, lat: Cairo
    -0.00438329d       | +0.40899596d        | 0.54628806d     || 1.4334492081530734d   // omega: -0.25° = 12:00, delta: June 21st, lat: Cairo
    +0.0126074d        | -0.40842934d        | 0.54628806d     || 0.6160025701438165d   // omega: +0.7223° = 12:00, delta: Dez 21st, lat: Cairo
    -0.78639785d       | +0.1549651d         | 0.54628806d     || 0.7430566034615067d   // omega: -45.05° = 09:00, delta: Sep 1st, lat: Cairo
    +1.04619786d       | 0.1549651d          | 0.54628806d     || 0.5270965151470974d   // omega: +59.943° = 16:00, delta: Sep 1st, lat: Cairo
    0d                 | -0.305432619d       | 0.518013722d    || 0.7473499857948969d   // omega: 0 = Solar Noon, delta: February 01st, lat/lon: Gainsville (29.68 N, 82.27 W) //Example 2.1a Goswami Priciples of Solar Engineering
    -1.374970385d      | +0.380755678d       | 0.157952297d    || 0.2391202791125743d   // omega: -78.78° = 7:00 a.m., delta: June 01st, lat/lon: Tocumen Panama (9.05 N, 79.37 W) //Example 2.2a Goswami Priciples of Solar Engineering
    0d                 | -0.268780705d       | -0.616101226d   || 1.2234758057948967d   // omega: 0° = Solar noon., delta: November 01st, lat/lon: Canberra Australia (35.3 S, 149.1 E) //Example 2.3b Goswami Priciples of Solar Engineering
    Math.toRadians(-37.5d)        | Math.toRadians(-14d)         | Math.toRadians(43d)     || Math.toRadians(23.4529893659531784299686037109330117049955654837550d)  // '2011-02-13T09:30:00' from Duffie
    Math.toRadians(97.5d)         | Math.toRadians(23.1d)        | Math.toRadians(43d)     || Math.toRadians(10.356151317506402829742934977890382350725031728508d) // '2011-07-01T06:30:00' from Duffie
    // Reference: Quaschning, Regenerative Energiesysteme figure 2.15 and figure 2.16   // gammaS@Quaschning = alphaS@SIMONA !
    Math.toRadians(-47.15114406)  | Math.toRadians(23.4337425d)  | Math.toRadians(52.3d)   || Math.toRadians(44.12595614280154d)    // Berlin (13.2E 52.3N) '2011-06-21T09:00:00' MEZ
    Math.toRadians(-32.15114394d) | Math.toRadians(23.4337425d)  | Math.toRadians(52.3d)   || Math.toRadians(52.15790489243239d)    // Berlin (13.2E 52.3N) '2011-06-21T10:00:00' MEZ
    Math.toRadians(-17.15114381d) | Math.toRadians(23.4337425d)  | Math.toRadians(52.3d)   || Math.toRadians(58.29851278388936d)    // Berlin (13.2E 52.3N) '2011-06-21T11:00:00' MEZ
    Math.toRadians(-2.151143686d) | Math.toRadians(23.4337425d)  | Math.toRadians(52.3d)   || Math.toRadians(61.086849596117524d)   // Berlin (13.2E 52.3N) '2011-06-21T12:00:00' MEZ
    Math.toRadians(12.84885587d)  | Math.toRadians(23.4337425d)  | Math.toRadians(52.3d)   || Math.toRadians(59.50792770681503d)    // Berlin (13.2E 52.3N) '2011-06-21T13:00:00' MEZ
    Math.toRadians(27.84885599d)  | Math.toRadians(23.4337425d)  | Math.toRadians(52.3d)   || Math.toRadians(54.170777340509574d)   // Berlin (13.2E 52.3N) '2011-06-21T14:00:00' MEZ
    Math.toRadians(58.28178946d)  | Math.toRadians(7.79402247d)  | Math.toRadians(52.3d)   || Math.toRadians(25.203526133755485d)   // Berlin (13.2E 52.3N) '2011-09-04T16:00:00' MEZ
    Math.toRadians(0.948855924d)  | Math.toRadians(23.4337425d)  | Math.toRadians(30.1d)   || Math.toRadians(83.28023248078853d)   // Cairo  (31.3E 30.1N)  '2011-06-21T12:00:00' MEZ+1h
  }

  def "Calculate zenith angle thetaZ"() {
    when:
    Angle alphaSQuantity = Sq.create(alphaS, Radians$.MODULE$)

    Angle thetaZCalc = pvModel.calcZenithAngleThetaZ(alphaSQuantity)

    then:
    thetaZCalc =~ Sq.create(thetaZSol, Radians$.MODULE$)

    where:
    alphaS             || thetaZSol
    0d                 || 1.5707963267948966d     // 0°
    0.785398163397448d || 0.7853981633974486d     // 45°
    1.570796326794897d || -4.440892098500626E-16d  // 90°
  }

  def "Calculate air mass"() {
    when:
    Angle thetaZQuantity = Sq.create(thetaZ, Radians$.MODULE$)

    double airMassCalc = pvModel.calcAirMass(thetaZQuantity)

    then:
    airMassCalc =~ airMassSol

    where:
    thetaZ             || airMassSol
    0d                 || 1d                  // 0°
    0.785398163397448d || 1.41321748045965d   // 45°
    1.570796326794897d || 37.640108631323025d // 90°
  }

  def "Calculate extraterrestrial radiation IO"() {
    when:
    Angle dayAngleQuantity = Sq.create(j, Radians$.MODULE$)

    Irradiation I0Calc = pvModel.calcExtraterrestrialRadiationI0(dayAngleQuantity)

    then:
    I0Calc =~ Sq.create(I0Sol, WattHoursPerSquareMeter$.MODULE$)

    where:
    j                  || I0Sol
    0d                 || 1414.91335d           // Jan 1st
    2.943629280897834d || 1322.494291080537598d // Jun 21st
    4.52733626243351d  || 1355.944773587800003d // Sep 21st
  }

  def "Calculate the angle of incidence thetaG"() {

    "Calculate the angle of incidence of beam radiation on a surface located at a Latitude" +
        "at a certain hour angle (solar time) on a given declination (date) if the surface " +
        "is tilted by a certain slope from the horizontal and pointed to a certain panel azimuth " +
        "west of south."

    "== Calculate the angle of incidence thetaG =="
    given:
    // Declination Angle delta of the sun at solar noon
    Angle deltaRad = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Latitude in Radian
    Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)
    //Hour Angle
    Angle omegaRad = Sq.create(Math.toRadians(omegaDeg), Radians$.MODULE$)
    //Inclination Angle of the surface
    Angle gammaERad = Sq.create(Math.toRadians(gammaEDeg), Radians$.MODULE$)
    //Surface azimuth
    Angle alphaERad = Sq.create(Math.toRadians(alphaEDeg), Radians$.MODULE$)

    when:
    Angle thetaG = pvModel.calcAngleOfIncidenceThetaG(deltaRad, latitudeInRad, gammaERad, alphaERad, omegaRad)

    then:
    "- should calculate the angle of incidence thetaG "
    thetaG.toDegrees() =~ Sq.create(thetaGOut, Degrees$.MODULE$).toDegrees()

    where: "the following parameters are given"
    latitudeInDeg | deltaIn         | omegaDeg       | gammaEDeg  | alphaEDeg  || thetaGOut
    43d           | -14d            | -22.5d         | 45d        | 15d        || 35.176193345578606393727080835951995075234213360724d  // Duffie
    51.516667d    | +18.4557514d    | -15.00225713d  | 30d        | +0d        || 14.420271449960715d                     // Iqbal
    51.516667d    | +18.4557514d    | -15.00225713d  | 90d        | +0d        || 58.65287310017624d                     // Iqbal
    35.0d         | +23.2320597d    | +30.00053311d  | 45d        | 10d        || 39.62841449023577d                      // Kalogirou - Solar Energy Engineering Example 2.7  ISBN 978-0-12-374501-9; DOI https://doi.org/10.1016/B978-0-12-374501-9.X0001-5
    35.0d         | +23.2320597d    | +30.00053311d  | 45d        | 90d        || 18.946300807438607d                     // Kalogirou - Solar Energy Engineering Example 2.7 changed to 90° panel azimuth to WEST
    35.0d         | +23.2320597d    | +74.648850625d | 45d        | 90d        || 21.95480347380729d                     // Kalogirou - Solar Energy Engineering Example 2.7  90° panel azimuth to WEST at 17:00
    35.0d         | +23.2320597d    | +74.648850625d | 45d        | -90d       || 109.00780288303966d                     // Kalogirou - Solar Energy Engineering Example 2.7  90° panel azimuth to EAST at 17:00
    27.96d        | -17.51d         | -11.1d         | 30d        | +10d       || 22.384603601536398d                     // Goswami Priciples of Solar Engineering Example 2.7a
    -35.3d        | -17.51d         | -4.2d          | 30d        | +170d      || 14.882390116876563d                     // Goswami Priciples of Solar Engineering Example 2.7b
  }

  def "Testing the equality of zenith angle of a horizontal surface and thetaG of a sloped surface"() {

    "Iqbal Figure 1.6.2 - the angle of incidence of a surface sloped by angle beta at " +
        "latitude phi should be same as the zenith angle of an unsloped surface" +
        "positioned at latitude phi - beta " +
        ""

    given:
    "- using pre-calculated parameters"
    //Latitude in Radian
    Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)
    // Declination Angle delta of the sun at solar noon
    Angle delta = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Hour Angle
    Angle omega = Sq.create(Math.toRadians(omegaIn), Radians$.MODULE$)
    //Inclination Angle of the surface
    Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    //Sun's azimuth
    Angle alphaE = Sq.create(Math.toRadians(azimuth), Radians$.MODULE$)

    // should calculate the angle of incidence thetaG
    when:
    Angle thetaG = pvModel.calcAngleOfIncidenceThetaG(delta, latitudeInRad, gammaE, alphaE, omega)

    then:
    thetaG.toDegrees() =~ thetaOut

    where: "the following parameters are given"
    latitudeInDeg | deltaIn      | omegaIn       | slope  | azimuth || thetaOut
    45d            | -7.15       | -82.5d        | 60d    | 0       || 80.94904834048776d  // thetaG
    15d            | -7.15       | -82.5d        | 30d    | 0       || 80.94904834048776d  // same test but 15° South with 15° less sloped surface
    0d             | -7.15       | -82.5d        | 15d    | 0       || 80.94904834048776d  // same test but 15° South with 15° less sloped surface
    52.3d          | 23.4337425  | 2.15114395d   | 0d     | 0       || 28.91315041538251d  // Berlin 21.06. 12:00 => thetaG = 90 - alphaS
    70.3d          | 23.4337425  | 2.15114395d   | 18d    | 0       || 28.91315041538251d  // same test but 18° North with 18° sloped surface
  }

  def "Calculate Rb (cos(thetaG)/cos(thetaZ))"() {

    "On March 4 at a latitude of 45◦ and a surface slope of 60◦ determine Rb at 6:30 AM and Rb,ave " +
        "for the hour 6 to 7 AM."

    given:
    "- using pre-calculated parameters"
    //Latitude in Radian
    Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)
    // Declination Angle delta of the sun at solar noon
    Angle delta = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Hour Angle
    Angle omega = Sq.create(Math.toRadians(omegaIn), Radians$.MODULE$)
    //Inclination Angle of the surface
    Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    //Sun's azimuth
    Angle alphaE = Sq.create(Math.toRadians(azimuth), Radians$.MODULE$)

    expect:
    "- should calculate the angle of incidence thetaG and zenith angle thetaZ of beam radiation on a surface correctly"

    pvModel.calcAngleOfIncidenceThetaG(delta, latitudeInRad, gammaE, alphaE, omega).toDegrees() =~ thetaOut


    where: "the following parameters are given"
    latitudeInDeg | deltaIn | omegaIn | slope | azimuth || thetaOut
    45            | -7.15   | -82.5   | 60    | 0       || 80.94904834048776  // thetaG
    45            | -7.15   | -82.5   | 0     | 0       || 89.79565474295107  // thetaZ
    40            | -11.6   | -82.5   | 60    | 0       || 79.11011928744357
    40            | -11.6   | 82.5    | 60    | 0       || 79.11011928744357
    40            | -11.6   | -78.0   | 60    | 0       || 74.92072065185143
    40            | -11.6   | 78.0    | 60    | 0       || 74.92072065185143
  }

  def "Calculate the estimate beam radiation eBeamS"() {

    "Using the Perez diffuse model, estimate the beam, diffuse, and ground-reflected components of solar " +
        "radiation and the total radiation on a surface sloped 60◦ toward the south at a latitude of 40◦ N " +
        "for the hour 9 to 10 AM on February 20. Here I = 1.04 MJ/m2 and ρg = 0.60."
    //Reference p.95
    //https://www.sku.ac.ir/Datafiles/BookLibrary/45/John%20A.%20Duffie,%20William%20A.%20Beckman(auth.)-Solar%20Engineering%20of%20Thermal%20Processes,%20Fourth%20Edition%20(2013).pdf

    given:
    //Inclination of the Pv system in degrees (tilted from the horizontal)
    Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    //Inclination of the Pv system in degrees (Inclined in a compass direction)(South 0◦; West 90◦; East -90◦)
    Angle alphaE = Sq.create(Math.toRadians(azimuth), Radians$.MODULE$)
    //Latitude in Radian
    Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)
    // 1 MJ/m^2 = 277,778 Wh/m^2
    // 0.244 MJ/m^2 = 67.777778 Wh/m^2
    //Beam Radiation on horizontal surface
    Irradiation eBeamH = Sq.create(67.777778d, WattHoursPerSquareMeter$.MODULE$)
    // Declination Angle delta of the sun at solar noon
    Angle delta = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Hour Angle
    Angle omega = Sq.create(Math.toRadians(omegaIn), Radians$.MODULE$)
    //Incidence Angle
    Angle thetaG = Sq.create(Math.toRadians(thetaGIn), Radians$.MODULE$)
    //Sunset Angle
    Angle omegaSS = pvModel.calcSunsetAngleOmegaSS(latitudeInRad, delta)
    //Sunrise Angle (Sunset Angle * (-1))
    Angle omegaSR = omegaSS.$times(-1d)
    //omega1 and omega2
    Option<scala.Tuple2<Angle, Angle>> omegas = pvModel.calculateBeamOmegas(thetaG, omega, omegaSS, omegaSR)

    expect:
    "- should calculate the beam contribution,"

    pvModel.calcBeamRadiationOnSlopedSurface(eBeamH, omegas, delta, latitudeInRad, gammaE, alphaE) =~ Sq.create(eBeamSSol, WattHoursPerSquareMeter$.MODULE$)


    where: "the following parameters are given"
    latitudeInDeg | slope | azimuth | deltaIn | omegaIn | thetaGIn || eBeamSSol
    40d           | 0d    | 0d      | -11.6d  | -37.5d  | 37.0d    || 67.777778d             // flat surface => eBeamS = eBeamH
    40d           | 60d   | 0d      | -11.6d  | -37.5d  | 37.0d    || 112.84217113154841369d // 2011-02-20T09:00:00
    40d           | 60d   | 0d      | -11.6d  | -78.0d  | 75.0d    || 210.97937494450755d    // sunrise
    40d           | 60d   | 0d      | -11.6d  | 62.0d   | 76.0d    || 199.16566536224116d    // sunset
    40d           | 60d   | 0d      | -11.6d  | 69.0d   | 89.9d    || 245.77637766673405d    // sunset, cut off
    40d           | 60d   | 0d      | -11.6d  | 75.0d   | 89.9d    || 0d                     // no sun
    40d           | 60d   | -90.0d  | -11.6d  | 60.0d   | 91.0d    || 0d                     // no direct beam
  }

  def "Calculate the estimate diffuse radiation eDifS"() {

    given:
    //Inclination of the Pv system in degrees (tilted from the horizontal)
    Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    // 1 MJ/m^2 = 277,778 Wh/m^2
    // 0.244 MJ/m^2 = 67.777778 Wh/m^2
    //Beam Radiation on horizontal surface
    Irradiation eBeamH = Sq.create(67.777778d, WattHoursPerSquareMeter$.MODULE$)
    // 0.796 MJ/m^2 = 221,111288 Wh/m^2
    //Diffuse beam Radiation on horizontal surface
    Irradiation eDifH = Sq.create(221.111288d, WattHoursPerSquareMeter$.MODULE$)
    //Incidence Angle
    Angle thetaG = Sq.create(Math.toRadians(thetaGIn), Radians$.MODULE$)
    //Zenith Angle
    Angle thetaZ = Sq.create(Math.toRadians(thetaZIn), Radians$.MODULE$)
    // Extraterrestrial radiation
    Irradiation I0Quantity = Sq.create(I0, WattHoursPerSquareMeter$.MODULE$)

    expect:
    "- should calculate the beam diffusion"
    // == 0,7792781569074354 MJ/m^2

    def epsilon = pvModel.calcEpsilon(eDifH, eBeamH, thetaZ) // epsilon(Duffie) = 1,28451252
    def epsilonOld = pvModel.calcEpsilonOld(eDifH, eBeamH, thetaZ)
    def firstFraction = pvModel.firstFraction(eDifH, eBeamH, thetaZ)

    def diffuseradiation = pvModel.calcDiffuseRadiationOnSlopedSurfacePerez(eDifH, eBeamH, airMass, I0Quantity, thetaZ, thetaG, gammaE)
    diffuseradiation =~ Sq.create(eDifSSol, WattHoursPerSquareMeter$.MODULE$)

    where: "the following parameters are given"
    thetaGIn | thetaZIn | slope | airMass           | I0                  || eDifSSol
    37.0     | 62.2     | 60    | 2.144d            | 1395.8445d          || 220.83351d
  }

  def "Calculate the ground reflection eRefS"() {

    given:
    "- Beam radiation and diffuse bean radiation on horizontal surface"
    //Inclination of the Pv system in degrees (tilted from the horizontal)
    Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    // 1 MJ/m^2 = 277,778 Wh/m^2
    // 0.244 MJ/m^2 = 67.777778 Wh/m^2
    //Beam Radiation on horizontal surface
    Irradiation eBeamH = Sq.create(67.777778d, WattHoursPerSquareMeter$.MODULE$)
    // 0.769 MJ/m^2 = 213,61111 Wh/m^2
    //Diffuse beam Radiation on horizontal surface
    Irradiation eDifH = Sq.create(213.61111d, WattHoursPerSquareMeter$.MODULE$)

    expect:
    "- should calculate the ground reflection correctly"
    // == 0,15194999952 MJ/m^2 (Book: 0.156 MJ/m^2)

    pvModel.calcReflectedRadiationOnSlopedSurface(eBeamH, eDifH, gammaE, albedo) =~ Sq.create(eRefSSol, WattHoursPerSquareMeter$.MODULE$)


    where: "the following parameters are given"
    slope | albedo || eRefSSol
    60    | 0.60   || 42.20833319999999155833336d // '2011-02-20T09:00:00'
  }
}
