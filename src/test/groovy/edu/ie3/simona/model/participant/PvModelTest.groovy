/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.interfaces.Irradiation
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.Kilovars
import edu.ie3.util.scala.quantities.Sq
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Point
import scala.Option
import spock.lang.Shared
import spock.lang.Specification
import squants.*
import squants.energy.*
import squants.space.*
import tech.units.indriya.ComparableQuantity

import javax.measure.Quantity
import javax.measure.quantity.Angle
import java.time.ZonedDateTime

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.quantity.Quantities.getQuantity

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

  static final double TESTING_TOLERANCE = 1e-10

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
        pvInput.getUuid(),
        pvInput.getId(),
        OperationInterval.apply(0L, 86400L),
        scalingFactor,
        QControl.apply(pvInput.getqCharacteristics()),
        Sq.create(pvInput.getsRated().to(KILOWATT).getValue().doubleValue(), Kilowatts$.MODULE$),
        pvInput.getCosPhiRated(),
        pvInput.getNode().getGeoPosition().getY(),
        pvInput.getNode().getGeoPosition().getX(),
        pvInput.getAlbedo(),
        Sq.create(pvInput.getEtaConv().to(PU).getValue().doubleValue(), Each$.MODULE$),
        Sq.create(pvInput.getAzimuth().to(RADIAN).getValue().doubleValue(), Radians$.MODULE$),
        Sq.create(pvInput.getElevationAngle().getValue().doubleValue(), Radians$.MODULE$),
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
    (Math.abs(qCalc.toKilovars() - qSoll)).toDouble() < 0.0001

    where:
    pVal || qSoll
    9.5d  || 4.601059d // above sRated (no q limitation)
    11d   || 0d        // above sMax (limit q becomes active)
  }

  def "Calculate day angle J"() {
    when:
    squants.space.Angle jCalc = pvModel.calcJ(ZonedDateTime.parse(time))

    then:
    Math.abs(jCalc.toRadians() - jSol) < 1e-15

    where:
    time                                       || jSol
    '2019-01-05T05:15:00+01:00[Europe/Berlin]' || 0.06885682528415985d
    '2016-10-31T12:15:00+01:00[Europe/Berlin]' || 5.23311872159614873d // leap year => day = 305
    '2017-10-31T12:15:00+01:00[Europe/Berlin]' || 5.21590451527510877d // regular year => day = 304
  }

  def "Calculate declination angle delta"() {
    when:
    squants.space.Angle dayAngleQuantity = Sq.create(j, Radians$.MODULE$)
    squants.space.Angle deltaCalc = pvModel.calcSunDeclinationDelta(dayAngleQuantity)

    then:
    Math.abs(deltaCalc.toRadians() - deltaSol) < 1e-15

    where:
    j                  || deltaSol
    0d                 || -0.402449d           // Jan 1st
    2.943629280897834d || 0.409315420329718d   // Jun 21 (maximum: 23.45°)
    6.024972212363987d || -0.406963611252886d   // Dec 21 (minimum: -23.45°)
    4.52733626243351d  || 0.017908363617326d   // Sep 21 (0°)
    1.359922299362157d || -0.001150591501958d   // Mar 21 (0°)
  }

  def "Calculate hour angle omega"() {
    when:
    ZonedDateTime dateTime = ZonedDateTime.parse(time)
    squants.space.Angle dayAngleQuantity = Sq.create(j, Radians$.MODULE$)
    squants.space.Angle longitudeQuantity = Sq.create(longitude, Radians$.MODULE$)

    squants.space.Angle omegaCalc = pvModel.calcHourAngleOmega(dateTime, dayAngleQuantity, longitudeQuantity)

    then:
    Math.abs(omegaCalc.toRadians() - omegaSol) < 1e-15

    where:
    time                                       | j                  | longitude || omegaSol
    '2019-01-01T05:00:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || -1.946503016860923d  // long: ~9.17°E
    '2019-01-01T10:05:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || -0.615689462215246d  // different time: 10:05
    '2019-01-01T12:00:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || -0.113907302266876d  // 12:00
    '2019-01-01T14:00:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || 0.409691473331422d  // 14:00
    '2019-01-01T17:30:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || 1.325989330628445d  // 17:30
    '2019-03-21T05:00:00+01:00[Europe/Berlin]' | 1.359922299362157d | 0.16d     || -1.967775075784021d  // different j (different date)
    '2019-01-01T05:00:00+01:00[Europe/Berlin]' | 0d                 | 0.175d    || -1.931503016860923d  // different long, ~10°E
  }

  def "Calculate sunset angle omegaSS"() {
    when:
    squants.space.Angle latitudeQuantity = Sq.create(latitude, Radians$.MODULE$)
    squants.space.Angle deltaQuantity = Sq.create(delta, Radians$.MODULE$)

    squants.space.Angle omegaSSCalc = pvModel.calcSunsetAngleOmegaSS(latitudeQuantity, deltaQuantity)

    then:
    Math.abs(omegaSSCalc.toRadians() - omegaSSSol) < 1e-15

    where:
    latitude | delta      || omegaSSSol
    0.9d     | -0.402449d || 1.004597540628618d  // lat: ~51.57°N
    0.935d   | -0.402449d || 0.956011693657339d  // different lat: ~53.57°N
    0.9d     | 0.017908d  || 1.593367569319828d  // different delta
  }

  def "Calculate solar altitude angle alphaS"() {
    when:
    squants.space.Angle omegaQuantity = Sq.create(omega, Radians$.MODULE$)
    squants.space.Angle deltaQuantity = Sq.create(delta, Radians$.MODULE$)
    squants.space.Angle latitudeQuantity = Sq.create(latitude, Radians$.MODULE$)

    squants.space.Angle alphaSCalc = pvModel.calcSolarAltitudeAngleAlphaS(omegaQuantity, deltaQuantity, latitudeQuantity)

    then:
    Math.abs(alphaSCalc.toRadians() - alphaSSol) < 1e-15

    where:
    omega              | delta               | latitude || alphaSSol
    1.946503016860923d | -0.402449d          | 0.9d     || -0.542959468135244d  // delta: Jan 1st, lat: ~51.57°N
    1.967775075784021d | -0.001150591501958d | 0.9d     || -0.243639843356786d  // delta: March 21st
    1.946503016860923d | -0.402449d          | 0.935d   || -0.541732285481946d  // delta: Jan 1st, lat: ~53.57°N
    1.256637061d       | -0.402449d          | 0.698d   || -0.033897520990304d  // omega: 82°, delta: Jan 1st, lat: ~53.57°N
    0.409691473331422d | -0.402449d          | 0.9d     || 0.219566101072938d  // omega: 14:00, delta: Jan 1st
  }

  def "Calculate zenith angle thetaZ"() {
    when:
    squants.space.Angle alphaSQuantity = Sq.create(alphaS, Radians$.MODULE$)

    squants.space.Angle thetaZCalc = pvModel.calcZenithAngleThetaZ(alphaSQuantity)

    then:
    Math.abs(thetaZCalc.toRadians() - thetaZSol) < 1e-15

    where:
    alphaS             || thetaZSol
    0d                 || 1.570796326794897d  // 0°
    0.785398163397448d || 0.785398163397449d  // 45°
    1.570796326794897d || 0d                  // 90°
  }

  def "Calculate air mass"() {
    when:
    squants.space.Angle thetaZQuantity = Sq.create(thetaZ, Radians$.MODULE$)

    double airMassCalc = pvModel.calcAirMass(thetaZQuantity)

    then:
    Math.abs(airMassCalc - airMassSol) < 1e-15

    where:
    thetaZ             || airMassSol
    0d                 || 1d                  // 0°
    0.785398163397448d || 1.41321748045965d   // 45°
    1.570796326794897d || 37.640108631323025d // 90°
  }

  def "Calculate extraterrestrial radiation IO"() {
    when:
    squants.space.Angle dayAngleQuantity = Sq.create(j, Radians$.MODULE$)

    Energy I0Calc = pvModel.calcExtraterrestrialRadiationI0(dayAngleQuantity)

    then:
    Math.abs(I0Calc.toWattHours() - I0Sol) < 1e-15

    where:
    j                  || I0Sol
    0d                 || 1414.91335d           // Jan 1st
    2.943629280897834d || 1322.494291080537598d // Jun 21st
    4.52733626243351d  || 1355.944773587800003d // Sep 21st
  }

  def "Calculate the angle of incidence thetaG"() {

    "Calculate the angle of incidence of beam radiation on a surface located at Madison (lat: 43◦ N), Wisconsin, " +
        "at 10:30 (solar time) on February 13 if the surface is tilted 45◦ from the horizontal and pointed 15◦ " +
        "west of south."

    "== Calculate the angle of incidence thetaG =="
    when:
    // Declination Angle delta of the sun at solar noon
    squants.space.Angle delta = Sq.create(Math.toRadians(-14), Radians$.MODULE$)
    //Latitude in Radian
    squants.space.Angle latitudeInRad = Sq.create(Math.toRadians(43d), Radians$.MODULE$)
    //Hour Angle
    squants.space.Angle omega = Sq.create(Math.toRadians(-22.5), Radians$.MODULE$)
    //Inclination Angle of the surface
    squants.space.Angle gammaE = Sq.create(Math.toRadians(45), Radians$.MODULE$)
    //Sun's azimuth
    squants.space.Angle alphaE = Sq.create(Math.toRadians(15), Radians$.MODULE$)

    then:
    Math.abs(
        pvModel.calcAngleOfIncidenceThetaG(delta, latitudeInRad, gammaE, alphaE, omega).toDegrees() - 35.176193345578606394
        ) < 1e-15
  }

  def "Calculate the solar altitude (azimuth) angle alphaS"() {

    "Calculate the solar azimuth angle for φ = 43◦ at a) 9:30 AM on February 13 and b) 6:30 PM on July 1."

    "== Calculate solar altitude (azimuth) angle =="
    given:
    // Declination Angle delta of the sun at solar noon
    squants.space.Angle delta = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Hour Angle
    squants.space.Angle omega = Sq.create(Math.toRadians(omegaIn), Radians$.MODULE$)
    //Latitude in Radian
    squants.space.Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)

    expect:
    "- should calculate the solar altitude correctly and"
    Math.abs(
        pvModel.calcSolarAltitudeAngleAlphaS(omega, delta, latitudeInRad).toDegrees() - alphaSOut
        ) < 1e-15

    where: "the following parameters are given"
    latitudeInDeg | deltaIn | omegaIn || alphaSOut
    43d           | -14d    | -37.5d  || 23.4529893659531784299686037109330117049955654837550  // '2011-02-13T09:30:00'
    43d           | 23.1d   | 97.5d   || 10.356151317506402829742934977890382350725031728508 // '2011-07-01T06:30:00'
  }

  def "Calculate Rb (cos(thetaG)/cos(thetaZ))"() {

    "On March 4 at a latitude of 45◦ and a surface slope of 60◦ determine Rb at 6:30 AM and Rb,ave " +
        "for the hour 6 to 7 AM."

    given:
    "- using pre-calculated parameters"
    //Latitude in Radian
    squants.space.Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)
    // Declination Angle delta of the sun at solar noon
    squants.space.Angle delta = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Hour Angle
    squants.space.Angle omega = Sq.create(Math.toRadians(omegaIn), Radians$.MODULE$)
    //Inclination Angle of the surface
    squants.space.Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    //Sun's azimuth
    squants.space.Angle alphaE = Sq.create(Math.toRadians(azimuth), Radians$.MODULE$)

    expect:
    "- should calculate the angle of incidence thetaG and zenith angle thetaZ of beam radiation on a surface correctly"
    Math.abs(
        pvModel.calcAngleOfIncidenceThetaG(delta, latitudeInRad, gammaE, alphaE, omega).toDegrees() - thetaOut
        ) < 1e-15

    where: "the following parameters are given"
    latitudeInDeg | deltaIn | omegaIn | slope | azimuth || thetaOut
    45            | -7.15   | -82.5   | 60    | 0       || 80.949048340487770372059710314128606931745693791068  // thetaG
    45            | -7.15   | -82.5   | 0     | 0       || 89.795654742951077439582101317248278436936658210074  // thetaZ
    40            | -11.6   | -82.5   | 60    | 0       || 79.110119287443572813858647674599902596082559960114
    40            | -11.6   | 82.5    | 60    | 0       || 79.110119287443572813858647674599902596082559960114
    40            | -11.6   | -78.0   | 60    | 0       || 74.920720651851429547034443178659599205063808040294
    40            | -11.6   | 78.0    | 60    | 0       || 74.920720651851429547034443178659599205063808040294
  }

  def "Calculate the estimate beam radiation eBeamS"() {

    "Using the Perez diffuse model, estimate the beam, diffuse, and ground-reflected components of solar " +
        "radiation and the total radiation on a surface sloped 60◦ toward the south at a latitude of 40◦ N " +
        "for the hour 9 to 10 AM on February 20. Here I = 1.04 MJ/m2 and ρg = 0.60."
    //Reference p.95
    //https://www.sku.ac.ir/Datafiles/BookLibrary/45/John%20A.%20Duffie,%20William%20A.%20Beckman(auth.)-Solar%20Engineering%20of%20Thermal%20Processes,%20Fourth%20Edition%20(2013).pdf

    given:
    //Inclination of the Pv system in degrees (tilted from the horizontal)
    squants.space.Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    //Inclination of the Pv system in degrees (Inclined in a compass direction)(South 0◦; West 90◦; East -90◦)
    squants.space.Angle alphaE = Sq.create(Math.toRadians(azimuth), Radians$.MODULE$)
    //Latitude in Radian
    squants.space.Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)
    // 1 MJ/m^2 = 277,778 Wh/m^2
    // 0.244 MJ/m^2 = 67.777778 Wh/m^2
    //Beam Radiation on horizontal surface
    Quantity<Irradiation> eBeamH = Sq.create(67.777778, WATTHOUR_PER_SQUAREMETRE)
    // Declination Angle delta of the sun at solar noon
    squants.space.Angle delta = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Hour Angle
    squants.space.Angle omega = Sq.create(Math.toRadians(omegaIn), Radians$.MODULE$)
    //Incidence Angle
    squants.space.Angle thetaG = Sq.create(Math.toRadians(thetaGIn), Radians$.MODULE$)
    //Sunset Angle
    squants.space.Angle omegaSS = pvModel.calcSunsetAngleOmegaSS(latitudeInRad, delta)
    //Sunrise Angle (Sunset Angle * (-1))
    squants.space.Angle omegaSR = omegaSS * (-1)
    //omega1 and omega2
    Option<scala.Tuple2<ComparableQuantity<Angle>, ComparableQuantity<Angle>>> omegas = pvModel.calculateBeamOmegas(thetaG, omega, omegaSS, omegaSR)

    expect:
    "- should calculate the beam contribution,"
    Math.abs(
        pvModel.calcBeamRadiationOnSlopedSurface(eBeamH, omegas, delta, latitudeInRad, gammaE, alphaE).toWattHours() - eBeamSSol
        ) < 1e-15

    where: "the following parameters are given"
    latitudeInDeg | slope | azimuth | deltaIn | omegaIn | thetaGIn || eBeamSSol
    40d           | 0d    | 0d      | -11.6d  | -37.5d  | 37.0d    || 67.777778d            // flat surface => eBeamS = eBeamH
    40d           | 60d   | 0d      | -11.6d  | -37.5d  | 37.0d    || 112.84217113154841369d// 2011-02-20T09:00:00
    40d           | 60d   | 0d      | -11.6d  | -78.0d  | 75.0d    || 210.97937494450753662d// sunrise
    40d           | 60d   | 0d      | -11.6d  | 62.0d   | 76.0d    || 199.165665362241176d  // sunset
    40d           | 60d   | 0d      | -11.6d  | 69.0d   | 89.9d    || 245.776377666734070d  // sunset, cut off
    40d           | 60d   | 0d      | -11.6d  | 75.0d   | 89.9d    || 0d                    // no sun
    40d           | 60d   | -90.0d  | -11.6d  | 60.0d   | 91.0d    || 0d                    // no direct beam
  }

  def "Calculate the estimate diffuse radiation eDifS"() {

    given:
    //Inclination of the Pv system in degrees (tilted from the horizontal)
    squants.space.Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    // 1 MJ/m^2 = 277,778 Wh/m^2
    // 0.244 MJ/m^2 = 67.777778 Wh/m^2
    //Beam Radiation on horizontal surface
    Energy eBeamH = Sq.create(67.777778, WattHours$.MODULE$)
    // 0.769 MJ/m^2 = 213,61111 Wh/m^2
    //Diffuse beam Radiation on horizontal surface
    Energy eDifH = Sq.create(213.61111, WattHours$.MODULE$)
    //Incidence Angle
    squants.space.Angle thetaG = Sq.create(Math.toRadians(thetaGIn), Radians$.MODULE$)
    //Zenith Angle
    squants.space.Angle thetaZ = Sq.create(Math.toRadians(thetaZIn), Radians$.MODULE$)
    // Extraterrestrial radiation
    Energy I0Quantity = Sq.create(I0, WattHours$.MODULE$)

    expect:
    "- should calculate the beam diffusion"
    // == 0,7792781569074354 MJ/m^2
    Math.abs(
        pvModel.calcDiffuseRadiationOnSlopedSurfacePerez(eDifH, eBeamH, airMass, I0Quantity, thetaZ, thetaG, gammaE).toWattHours() - eDifSSol
        ) < 1e-15

    where: "the following parameters are given"
    thetaGIn | thetaZIn | slope | airMass           | I0                  || eDifSSol
    37.0     | 62.2     | 60    | 2.13873080095658d | 1399.0077631849722d || 216.46615469650985d
  }

  def "Calculate the ground reflection eRefS"() {

    given:
    "- Beam radiation and diffuse bean radiation on horizontal surface"
    //Inclination of the Pv system in degrees (tilted from the horizontal)
    squants.space.Angle gammaE = Sq.create(Math.toRadians(slope), Radians$.MODULE$)
    // 1 MJ/m^2 = 277,778 Wh/m^2
    // 0.244 MJ/m^2 = 67.777778 Wh/m^2
    //Beam Radiation on horizontal surface
    Energy eBeamH = Sq.create(67.777778, WattHours$.MODULE$)
    // 0.769 MJ/m^2 = 213,61111 Wh/m^2
    //Diffuse beam Radiation on horizontal surface
    Energy eDifH = Sq.create(213.61111, WattHours$.MODULE$)

    expect:
    "- should calculate the ground reflection correctly"
    // == 0,15194999952 MJ/m^2 (Book: 0.156 MJ/m^2)
    Math.abs(
        pvModel.calcReflectedRadiationOnSlopedSurface(eBeamH, eDifH, gammaE, albedo).toWattHours() - eRefSSol
        ) < 1e-15

    where: "the following parameters are given"
    slope | albedo || eRefSSol
    60    | 0.60   || 42.20833319999999155833336d // '2011-02-20T09:00:00'
  }
}
