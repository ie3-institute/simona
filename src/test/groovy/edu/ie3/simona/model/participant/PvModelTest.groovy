/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import squants.Dimensionless
import squants.Each$
import squants.energy.Kilowatts$
import squants.space.Radians$
import squants.space.SquareMeters$

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.quantity.Quantities.getQuantity

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
        pvInput.node.geoPosition.y,
        pvInput.node.geoPosition.x,
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
    time                                       | j                  | longitude || omegaSol
    '2019-01-01T05:00:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || -1.9465030168609223d  // long: ~9.17°E
    '2019-01-01T10:05:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || -0.6156894622152458d  // different time: 10:05
    '2019-01-01T12:00:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || -0.11390730226687622d  // 12:00
    '2019-01-01T14:00:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || 0.40969147333142264d  // 14:00
    '2019-01-01T17:30:00+01:00[Europe/Berlin]' | 0d                 | 0.16d     || 1.3259893306284447d  // 17:30
    '2019-03-21T05:00:00+01:00[Europe/Berlin]' | 1.359922299362157d | 0.16d     || -1.9677750757840207d  // different j (different date)
    '2019-01-01T05:00:00+01:00[Europe/Berlin]' | 0d                 | 0.175d    || -1.9315030168609224d  // different long, ~10°E
  }

  def "Calculate sunset angle omegaSS"() {
    when:
    Angle latitudeQuantity = Sq.create(latitude, Radians$.MODULE$)
    Angle deltaQuantity = Sq.create(delta, Radians$.MODULE$)

    Angle omegaSSCalc = pvModel.calcSunsetAngleOmegaSS(latitudeQuantity, deltaQuantity)

    then:
    omegaSSCalc =~ Sq.create(omegaSSSol, Radians$.MODULE$)

    where:
    latitude | delta      || omegaSSSol
    0.9d     | -0.402449d || 1.0045975406286176d  // lat: ~51.57°N
    0.935d   | -0.402449d || 0.956011693657339d   // different lat: ~53.57°N
    0.9d     | 0.017908d  || 1.5933675693198284d  // different delta
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
    omega              | delta               | latitude || alphaSSol
    1.946503016860923d | -0.402449d          | 0.9d     || -0.5429594681352444d  // delta: Jan 1st, lat: ~51.57°N
    1.967775075784021d | -0.001150591501958d | 0.9d     || -0.24363984335678648d  // delta: March 21st
    1.946503016860923d | -0.402449d          | 0.935d   || -0.5417322854819461d  // delta: Jan 1st, lat: ~53.57°N
    1.256637061d       | -0.402449d          | 0.698d   || -0.033897520990303694d  // omega: 82°, delta: Jan 1st, lat: ~53.57°N
    0.409691473331422d | -0.402449d          | 0.9d     || 0.21956610107293822d  // omega: 14:00, delta: Jan 1st
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

    "Calculate the angle of incidence of beam radiation on a surface located at Madison (lat: 43◦ N), Wisconsin, " +
        "at 10:30 (solar time) on February 13 if the surface is tilted 45◦ from the horizontal and pointed 15◦ " +
        "west of south."

    "== Calculate the angle of incidence thetaG =="
    when:
    // Declination Angle delta of the sun at solar noon
    Angle delta = Sq.create(Math.toRadians(-14), Radians$.MODULE$)
    //Latitude in Radian
    Angle latitudeInRad = Sq.create(Math.toRadians(43d), Radians$.MODULE$)
    //Hour Angle
    Angle omega = Sq.create(Math.toRadians(-22.5), Radians$.MODULE$)
    //Inclination Angle of the surface
    Angle gammaE = Sq.create(Math.toRadians(45), Radians$.MODULE$)
    //Sun's azimuth
    Angle alphaE = Sq.create(Math.toRadians(15), Radians$.MODULE$)

    then:

    pvModel.calcAngleOfIncidenceThetaG(delta, latitudeInRad, gammaE, alphaE, omega).toDegrees() =~ 35.176193345578604
  }

  def "Calculate the solar altitude (azimuth) angle alphaS"() {

    "Calculate the solar azimuth angle for φ = 43◦ at a) 9:30 AM on February 13 and b) 6:30 PM on July 1."

    "== Calculate solar altitude (azimuth) angle =="
    given:
    // Declination Angle delta of the sun at solar noon
    Angle delta = Sq.create(Math.toRadians(deltaIn), Radians$.MODULE$)
    //Hour Angle
    Angle omega = Sq.create(Math.toRadians(omegaIn), Radians$.MODULE$)
    //Latitude in Radian
    Angle latitudeInRad = Sq.create(Math.toRadians(latitudeInDeg), Radians$.MODULE$)

    expect:
    "- should calculate the solar altitude correctly and"
    pvModel.calcSolarAltitudeAngleAlphaS(omega, delta, latitudeInRad).toDegrees() =~ alphaSOut


    where: "the following parameters are given"
    latitudeInDeg | deltaIn | omegaIn || alphaSOut
    43d           | -14d    | -37.5d  || 23.45298936595318  // '2011-02-13T09:30:00'
    43d           | 23.1d   | 97.5d   || 10.356151317506402 // '2011-07-01T06:30:00'
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
    // 0.769 MJ/m^2 = 213,61111 Wh/m^2
    //Diffuse beam Radiation on horizontal surface
    Irradiation eDifH = Sq.create(213.61111d, WattHoursPerSquareMeter$.MODULE$)
    //Incidence Angle
    Angle thetaG = Sq.create(Math.toRadians(thetaGIn), Radians$.MODULE$)
    //Zenith Angle
    Angle thetaZ = Sq.create(Math.toRadians(thetaZIn), Radians$.MODULE$)
    // Extraterrestrial radiation
    Irradiation I0Quantity = Sq.create(I0, WattHoursPerSquareMeter$.MODULE$)

    expect:
    "- should calculate the beam diffusion"
    // == 0,7792781569074354 MJ/m^2

    pvModel.calcDiffuseRadiationOnSlopedSurfacePerez(eDifH, eBeamH, airMass, I0Quantity, thetaZ, thetaG, gammaE) =~ Sq.create(eDifSSol, WattHoursPerSquareMeter$.MODULE$)

    where: "the following parameters are given"
    thetaGIn | thetaZIn | slope | airMass           | I0                  || eDifSSol
    37.0     | 62.2     | 60    | 2.13873080095658d | 1399.0077631849722d || 216.46615469650982d
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
