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
import edu.ie3.util.quantities.QuantityUtil
import edu.ie3.util.quantities.interfaces.Irradiation
import edu.ie3.util.scala.OperationInterval
import org.locationtech.jts.geom.Coordinate
import org.locationtech.jts.geom.GeometryFactory
import org.locationtech.jts.geom.Point
import scala.Option
import spock.lang.Shared
import spock.lang.Specification
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.Quantity
import javax.measure.quantity.Angle
import javax.measure.quantity.Dimensionless
import javax.measure.quantity.Power
import java.time.ZonedDateTime

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.quantity.Quantities.getQuantity
import static tech.units.indriya.unit.Units.*

/**
 * Test class that tries to cover all special cases of the current implementation of the PVModel
 *
 * Some of these test cases are taken from the examples of
 * Duffie, J. A., & Beckman, W. A. (2013). Solar engineering of thermal processes (4th ed.). Hoboken, N.J.: John Wiley & Sons.
 *
 * The page examples can be found using the page number provided in each test. Results may differ slightly from the
 * book as we sometimes use different formulas. Furthermore, sometimes the example might be slightly adapted to fit our needs.
 *
 */

class PVModelTest extends Specification {

	@Shared
	PVModel pvModel

	@Shared
	GeometryFactory geometryFactory

	def setupSpec() {

		// build the NodeInputModel (which defines the location of the pv input model)
		/// the NodeInputModel needs a GeoReference for the PV to work
		geometryFactory = new GeometryFactory()
		Point p = geometryFactory.createPoint(
				new Coordinate(13.2491, 53.457909))
		NodeInput nodeInput = new NodeInput(
				UUID.fromString("85f8b517-8a2d-4c20-86c6-3ff3c5823e6d"),
				"NodeInputModel for PVModel Test",
				OperatorInput.NO_OPERATOR_ASSIGNED,
				OperationTime.notLimited(),
				getQuantity(1, PU),
				false,
				p,
				GermanVoltageLevelUtils.MV_20KV,
				11
				)


		// build the PVInputModel
		PvInput pvInput = new PvInput(
				UUID.fromString("adb4eb23-1dd6-4406-a5e7-02e1e4c9dead"),
				"PV Model Test",
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

		// build the PVModel
		double scalingFactor = 1.0d
		pvModel = PVModel.apply(
				pvInput.getUuid(),
				pvInput.getId(),
				OperationInterval.apply(0L, 86400L),
				scalingFactor,
				QControl.apply(pvInput.getqCharacteristics()),
				pvInput.getsRated() as ComparableQuantity<Power>,
				pvInput.getCosPhiRated(),
				pvInput.getNode().getGeoPosition().getY(),
				pvInput.getNode().getGeoPosition().getX(),
				pvInput.getAlbedo(),
				pvInput.getEtaConv() as ComparableQuantity<Dimensionless>,
				getQuantity(Math.toRadians(pvInput.getAzimuth().getValue().doubleValue()), RADIAN),
				getQuantity(Math.toRadians(pvInput.getHeight().getValue().doubleValue()), RADIAN),
				getQuantity(1d, SQUARE_METRE)
				)
	}

	def "A PVModel should have sMax set to be 10% higher than its sRated"() {
		expect:
		pvModel.sMax() == (pvModel.sRated() * 1.1)
	}

	def "A PVModel should provide reactive power up to 110% of it's rated apparent power"() {
		given: "default adjusted voltage"
		Quantity adjustedVoltage = getQuantity(1, PU) // needed for method call but not applicable for cosphi_p

		when: "the reactive power is calculated"
		def qCalc = pvModel.calculateReactivePower(getQuantity(pVal, KILOWATT), adjustedVoltage)

		then:
		Math.abs(qCalc.subtract(getQuantity(qSoll, KILOVAR)).getValue().doubleValue()) < 0.0001

		where:
		pVal || qSoll
		9.5  || 4.601059 // above sRated (no q limitation)
		11   || 0        // above sMax (limit q becomes active)
	}

	def "Calculate day angle J"() {
		when:
		Quantity<Angle> jCalc = pvModel.calcJ(ZonedDateTime.parse(time))

		then:
		Math.abs(jCalc.getValue().doubleValue() - jSol) < 1e-15

		where:
		time                                       || jSol
		'2019-01-05T05:15:00+01:00[Europe/Berlin]' || 0.06885682528415985d
		'2016-10-31T12:15:00+01:00[Europe/Berlin]' || 5.23311872159614873d // leap year => day = 305
		'2017-10-31T12:15:00+01:00[Europe/Berlin]' || 5.21590451527510877d // regular year => day = 304
	}

	def "Calculate declination angle delta"() {
		when:
		Quantity<Angle> dayAngleQuantity = getQuantity(j, RADIAN)
		Quantity<Angle> deltaCalc = pvModel.calcSunDeclinationDelta(dayAngleQuantity)

		then:
		Math.abs(deltaCalc.getValue().doubleValue() - deltaSol) < 1e-15

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
		Quantity<Angle> dayAngleQuantity = getQuantity(j, RADIAN)
		Quantity<Angle> longitudeQuantity = getQuantity(longitude, RADIAN)

		Quantity<Angle> omegaCalc = pvModel.calcHourAngleOmega(dateTime, dayAngleQuantity, longitudeQuantity)

		then:
		Math.abs(omegaCalc.getValue().doubleValue() - omegaSol) < 1e-15

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
		Quantity<Angle> latitudeQuantity = getQuantity(latitude, RADIAN)
		Quantity<Angle> deltaQuantity = getQuantity(delta, RADIAN)

		Quantity<Angle> omegaSSCalc = pvModel.calcSunsetAngleOmegaSS(latitudeQuantity, deltaQuantity)

		then:
		Math.abs(omegaSSCalc.getValue().doubleValue() - omegaSSSol) < 1e-15

		where:
		latitude | delta      || omegaSSSol
		0.9d     | -0.402449d || 1.004597540628618d  // lat: ~51.57°N
		0.935d   | -0.402449d || 0.956011693657339d  // different lat: ~53.57°N
		0.9d     | 0.017908d  || 1.593367569319828d  // different delta
	}

	def "Calculate solar altitude angle alphaS"() {
		when:
		Quantity<Angle> omegaQuantity = getQuantity(omega, RADIAN)
		Quantity<Angle> deltaQuantity = getQuantity(delta, RADIAN)
		Quantity<Angle> latitudeQuantity = getQuantity(latitude, RADIAN)

		Quantity<Angle> alphaSCalc = pvModel.calcSolarAltitudeAngleAlphaS(omegaQuantity, deltaQuantity, latitudeQuantity)

		then:
		Math.abs(alphaSCalc.getValue().doubleValue() - alphaSSol) < 1e-15

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
		Quantity<Angle> alphaSQuantity = getQuantity(alphaS, RADIAN)

		Quantity<Angle> thetaZCalc = pvModel.calcZenithAngleThetaZ(alphaSQuantity)

		then:
		Math.abs(thetaZCalc.getValue().doubleValue() - thetaZSol) < 1e-15

		where:
		alphaS             || thetaZSol
		0d                 || 1.570796326794897d  // 0°
		0.785398163397448d || 0.785398163397449d  // 45°
		1.570796326794897d || 0d                  // 90°
	}

	def "Calculate air mass"() {
		when:
		Quantity<Angle> thetaZQuantity = getQuantity(thetaZ, RADIAN)

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
		Quantity<Angle> dayAngleQuantity = getQuantity(j, RADIAN)

		Quantity<Irradiation> I0Calc = pvModel.calcExtraterrestrialRadiationI0(dayAngleQuantity)

		then:
		Math.abs(I0Calc.getValue().doubleValue() - I0Sol) < 1e-15

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
		Quantity<Angle> delta = getQuantity(Math.toRadians(-14), RADIAN)
		//Latitude in Radian
		Quantity<Angle> latitudeInRad = getQuantity(Math.toRadians(43d), RADIAN)
		//Hour Angle
		Quantity<Angle> omega = getQuantity(Math.toRadians(-22.5), RADIAN)
		//Inclination Angle of the surface
		Quantity<Angle> gammaE = getQuantity(Math.toRadians(45), RADIAN)
		//Sun's azimuth
		Quantity<Angle> alphaE = getQuantity(Math.toRadians(15), RADIAN)

		then:
		QuantityUtil.isEquivalentAbs(pvModel.calcAngleOfIncidenceThetaG(delta, latitudeInRad, gammaE, alphaE, omega).to(DEGREE_GEOM),
				getQuantity(35.176193345578606393727080835951995075234213360724, DEGREE_GEOM))
	}

	def "Calculate the solar altitude (azimuth) angle alphaS"() {

		"Calculate the solar azimuth angle for φ = 43◦ at a) 9:30 AM on February 13 and b) 6:30 PM on July 1."

		"== Calculate solar altitude (azimuth) angle =="
		given:
		// Declination Angle delta of the sun at solar noon
		Quantity<Angle> delta = getQuantity(Math.toRadians(deltaIn), RADIAN)
		//Hour Angle
		Quantity<Angle> omega = getQuantity(Math.toRadians(omegaIn), RADIAN)
		//Latitude in Radian
		Quantity<Angle> latitudeInRad = getQuantity(Math.toRadians(latitudeInDeg), RADIAN)

		expect:
		"- should calculate the solar altitude correctly and"
		QuantityUtil.isEquivalentAbs(pvModel.calcSolarAltitudeAngleAlphaS(omega, delta, latitudeInRad).to(DEGREE_GEOM),
				getQuantity(alphaSOut, DEGREE_GEOM))

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
		Quantity<Angle> latitudeInRad = getQuantity(Math.toRadians(latitudeInDeg), RADIAN)
		// Declination Angle delta of the sun at solar noon
		Quantity<Angle> delta = getQuantity(Math.toRadians(deltaIn), RADIAN)
		//Hour Angle
		Quantity<Angle> omega = getQuantity(Math.toRadians(omegaIn), RADIAN)
		//Inclination Angle of the surface
		Quantity<Angle> gammaE = getQuantity(Math.toRadians(slope), RADIAN)
		//Sun's azimuth
		Quantity<Angle> alphaE = getQuantity(Math.toRadians(azimuth), RADIAN)

		expect:
		"- should calculate the angle of incidence thetaG and zenith angle thetaZ of beam radiation on a surface correctly"
		QuantityUtil.isEquivalentAbs(pvModel.calcAngleOfIncidenceThetaG(delta, latitudeInRad, gammaE, alphaE, omega).to(DEGREE_GEOM),
				getQuantity(thetaOut, DEGREE_GEOM))

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
		//Inclination of the PV system in degrees (tilted from the horizontal)
		Quantity<Angle> gammaE = getQuantity(Math.toRadians(slope), RADIAN)
		//Inclination of the PV system in degrees (Inclined in a compass direction)(South 0◦; West 90◦; East -90◦)
		Quantity<Angle> alphaE = getQuantity(Math.toRadians(azimuth), RADIAN)
		//Latitude in Radian
		Quantity<Angle> latitudeInRad = getQuantity(Math.toRadians(latitudeInDeg), RADIAN)
		// 1 MJ/m^2 = 277,778 Wh/m^2
		// 0.244 MJ/m^2 = 67.777778 Wh/m^2
		//Beam Radiation on horizontal surface
		Quantity<Irradiation> eBeamH = getQuantity(67.777778, WATTHOUR_PER_SQUAREMETRE)
		// Declination Angle delta of the sun at solar noon
		Quantity<Angle> delta = getQuantity(Math.toRadians(deltaIn), RADIAN)
		//Hour Angle
		Quantity<Angle> omega = getQuantity(Math.toRadians(omegaIn), RADIAN)
		//Incidence Angle
		Quantity<Angle> thetaG = getQuantity(Math.toRadians(thetaGIn), RADIAN)
		//Sunset Angle
		Quantity<Angle> omegaSS = pvModel.calcSunsetAngleOmegaSS(latitudeInRad, delta)
		//Sunrise Angle (Sunset Angle * (-1))
		Quantity<Angle> omegaSR = omegaSS * (-1)
		//omega1 and omega2
		Option<scala.Tuple2<ComparableQuantity<Angle>, ComparableQuantity<Angle>>> omegas = pvModel.calculateBeamOmegas(thetaG, omega, omegaSS, omegaSR)

		expect:
		"- should calculate the beam contribution,"
		QuantityUtil.isEquivalentAbs(pvModel.calcBeamRadiationOnSlopedSurface(eBeamH, omegas, delta, latitudeInRad, gammaE, alphaE),
				Quantities.getQuantity(eBeamSSol, WATTHOUR_PER_SQUAREMETRE))

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
		//Inclination of the PV system in degrees (tilted from the horizontal)
		Quantity<Angle> gammaE = getQuantity(Math.toRadians(slope), RADIAN)
		// 1 MJ/m^2 = 277,778 Wh/m^2
		// 0.244 MJ/m^2 = 67.777778 Wh/m^2
		//Beam Radiation on horizontal surface
		Quantity<Irradiation> eBeamH = getQuantity(67.777778, WATTHOUR_PER_SQUAREMETRE)
		// 0.769 MJ/m^2 = 213,61111 Wh/m^2
		//Diffuse beam Radiation on horizontal surface
		Quantity<Irradiation> eDifH = getQuantity(213.61111, WATTHOUR_PER_SQUAREMETRE)
		//Incidence Angle
		Quantity<Angle> thetaG = getQuantity(Math.toRadians(thetaGIn), RADIAN)
		//Zenith Angle
		Quantity<Angle> thetaZ = getQuantity(Math.toRadians(thetaZIn), RADIAN)
		// Extraterrestrial radiation
		Quantity<Irradiation> I0Quantity = getQuantity(I0, WATTHOUR_PER_SQUAREMETRE)

		expect:
		"- should calculate the beam diffusion"
		// == 0,7792781569074354 MJ/m^2
		QuantityUtil.isEquivalentAbs(pvModel.calcDiffuseRadiationOnSlopedSurfacePerez(eDifH, eBeamH, airMass, I0Quantity, thetaZ, thetaG, gammaE),
				getQuantity(eDifSSol, WATTHOUR_PER_SQUAREMETRE))

		where: "the following parameters are given"
		thetaGIn | thetaZIn | slope | airMass           | I0                  || eDifSSol
		37.0     | 62.2     | 60    | 2.13873080095658d | 1399.0077631849722d || 216.46615469650985d
	}

	def "Calculate the ground reflection eRefS"() {

		given:
		"- Beam radiation and diffuse bean radiation on horizontal surface"
		//Inclination of the PV system in degrees (tilted from the horizontal)
		Quantity<Angle> gammaE = getQuantity(Math.toRadians(slope), RADIAN)
		// 1 MJ/m^2 = 277,778 Wh/m^2
		// 0.244 MJ/m^2 = 67.777778 Wh/m^2
		//Beam Radiation on horizontal surface
		Quantity<Irradiation> eBeamH = getQuantity(67.777778, WATTHOUR_PER_SQUAREMETRE)
		// 0.769 MJ/m^2 = 213,61111 Wh/m^2
		//Diffuse beam Radiation on horizontal surface
		Quantity<Irradiation> eDifH = getQuantity(213.61111, WATTHOUR_PER_SQUAREMETRE)

		expect:
		"- should calculate the ground reflection correctly"
		// == 0,15194999952 MJ/m^2 (Book: 0.156 MJ/m^2)
		QuantityUtil.isEquivalentAbs(pvModel.calcReflectedRadiationOnSlopedSurface(eBeamH, eDifH, gammaE, albedo),
				Quantities.getQuantity(eRefSSol, WATTHOUR_PER_SQUAREMETRE))

		where: "the following parameters are given"
		slope | albedo || eRefSSol
		60    | 0.60   || 42.20833319999999155833336d // '2011-02-20T09:00:00'
	}
}
