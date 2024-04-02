/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.quantities.{
  Irradiation,
  Megavars,
  ReactivePower,
  WattHoursPerSquareMeter,
}
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import org.scalatest.GivenWhenThen
import squants.Each
import squants.energy.{Kilowatts, Power}
import squants.space.{Angle, Degrees, Radians}
import tech.units.indriya.quantity.Quantities.getQuantity
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import scala.math.toRadians

/** Test class that tries to cover all special cases of the current
  * implementation of the PvModel
  *
  * Some of these test cases are taken from the examples of Duffie, J. A., &
  * Beckman, W. A. (2013). Solar engineering of thermal processes (4th ed.).
  * Hoboken, N.J.: John Wiley & Sons.
  *
  * The page examples can be found using the page number provided in each test.
  * Results may differ slightly from the book as we sometimes use different
  * formulas. Furthermore, sometimes the example might be slightly adapted to
  * fit our needs.
  */
class PvModelSpec extends UnitSpec with GivenWhenThen with DefaultTestData {

  // build the NodeInputModel (which defines the location of the pv input model)
  // the NodeInputModel needs a GeoReference for the Pv to work
  val geometryFactory = new GeometryFactory()
  val p: Point = geometryFactory.createPoint(new Coordinate(13.2491, 53.457909))
  val nodeInput = new NodeInput(
    UUID.fromString("85f8b517-8a2d-4c20-86c6-3ff3c5823e6d"),
    "NodeInputModel for PvModel Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    getQuantity(1, PU),
    false,
    p,
    GermanVoltageLevelUtils.MV_20KV,
    11,
  )

  // build the PvInputModel
  val pvInput = new PvInput(
    UUID.fromString("adb4eb23-1dd6-4406-a5e7-02e1e4c9dead"),
    "Pv Model Test",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited,
    nodeInput,
    new CosPhiFixed("cosPhiFixed:{(0.0,0.9)}"),
    null,
    0.20000000298023224,
    getQuantity(-8.926613807678223, DEGREE_GEOM),
    getQuantity(97, PERCENT),
    getQuantity(41.01871871948242, DEGREE_GEOM),
    0.8999999761581421,
    1,
    false,
    getQuantity(10, KILOVOLTAMPERE),
    0.8999999761581421,
  )

  // build the PvModel
  val pvModel: PvModel = PvModel(
    pvInput,
    scalingFactor = 1.0d,
    defaultSimulationStart,
    defaultSimulationEnd,
  )

  private implicit val angleTolerance: Angle = Radians(1e-10)
  private implicit val irradiationTolerance: Irradiation =
    WattHoursPerSquareMeter(1e-10)
  private implicit val powerTolerance: Power = Kilowatts(1e-10)
  private implicit val reactivePowerTolerance: ReactivePower = Megavars(1e-10)

  "A PV Model" should {
    "have sMax set to be 10% higher than its sRated" in {
      When("sMax is calculated")
      val actualSMax = pvModel.sMax
      val expectedSMax = pvModel.sRated * 1.1

      Then("result should match the test data")
      actualSMax should approximate(expectedSMax)
    }

    "provide reactive power up to 110% of its rated apparent power" in {
      val testCases = Table(
        ("pVal", "qSol"),
        (9.5d, 0.004601059995959599d), // above sRated (no q limitation)
        (11d, 0d), // above sMax (limit q becomes active)
      )

      forAll(testCases) { (pVal, qSol) =>
        Given("default adjusted voltage of 1 p.u.")
        val adjustedVoltage = 1

        When("the reactive power is calculated")
        val qCalc =
          pvModel.calculateReactivePower(Kilowatts(pVal), Each(adjustedVoltage))

        Then("result should be capped if apparent power exceeds limit")
        qCalc should approximate(Megavars(qSol))
      }
    }

    "calculate the day angle J correctly" in {
      val testCases = Table(
        ("time", "jSol"),
        ("2019-01-05T05:15:00+01:00", 0.06885682528415985d),
        (
          "2016-10-31T12:15:00+01:00",
          5.23311872159614873d,
        ), // leap year => day = 305
        (
          "2017-10-31T12:15:00+01:00",
          5.21590451527510877d,
        ), // regular year => day = 304
        (
          "2011-06-21T13:00:00+02:00",
          2.9436292808978335d,
        ), // regular year => day = 172
        (
          "2011-04-05T16:00:00+02:00",
          1.6181353941777565d,
        ), // regular year => day = 95
        (
          "2011-09-21T00:00:00+02:00",
          4.5273362624335105d,
        ), // regular year => day = 264
        (
          "2011-03-21T00:00:00+01:00",
          1.359922299362157d,
        ), // regular year => day = 80
      )

      forAll(testCases) { (time, jSol) =>
        When("the day angle is calculated")
        val jCalc = pvModel.calcAngleJ(ZonedDateTime.parse(time))

        Then("result should match the test data")
        jCalc should approximate(Radians(jSol))
      }
    }

    "calculate the declination angle delta correctly" in {
      val testCases = Table(
        ("j", "deltaSol"),
        (0d, -0.402449d), // Jan 1st
        (2.943629280897834d, 0.40931542032971796d), // Jun 21 (maximum: 23.45°)
        (6.024972212363987d, -0.4069636112528855d), // Dec 21 (minimum: -23.45°)
        (4.52733626243351d, 0.01790836361732633d), // Sep 21 (0°)
        (1.359922299362157d, -0.0011505915019577827d), // Mar 21 (0°)
      )

      forAll(testCases) { (j, deltaSol) =>
        When("the declination angle is calculated")
        val deltaCalc = pvModel.calcSunDeclinationDelta(Radians(j))

        Then("result should match the test data")
        deltaCalc should approximate(Radians(deltaSol))
      }
    }

    "calculate the hour angle omega correctly" in {
      val testCases = Table(
        ("time", "j", "longitude", "omegaSol"),
        (
          "2019-01-01T05:00:00+01:00",
          0d,
          0.16d,
          -1.9465030168609223d,
        ), // long: ~9.17°E
        (
          "2019-01-01T10:05:00+01:00",
          0d,
          0.16d,
          -0.6156894622152458d,
        ), // different time: 10:05
        (
          "2019-01-01T12:00:00+01:00",
          0d,
          0.16d,
          -0.11390730226687622d,
        ), // 12:00
        (
          "2019-01-01T14:00:00+01:00",
          0d,
          0.16d,
          0.40969147333142264d,
        ), // 14:00
        (
          "2019-01-01T17:30:00+01:00",
          0d,
          0.16d,
          1.3259893306284447d,
        ), // 17:30
        (
          "2019-03-21T05:00:00+01:00",
          1.359922299362157d,
          0.16d,
          -1.9677750757840207d,
        ), // different j (different date)
        (
          "2019-01-01T05:00:00+01:00",
          0d,
          0.175d,
          -1.9315030168609224d,
        ), // different long, ~10°E
        (
          "2011-06-21T11:00:00+02:00",
          2.9436292808978d,
          0.2337d,
          -0.2960273936975511d,
        ), // long of Berlin (13.39E)
        (
          "2011-06-21T12:00:00+02:00",
          2.9436292808978d,
          0.2337d,
          -0.034228005898401644d,
        ), // long of Berlin (13.39E)
        (
          "2011-06-21T13:00:00+02:00",
          2.9436292808978d,
          0.2337d,
          0.2275713819007478d,
        ), // long of Berlin (13.39E)
        (
          "2011-06-21T14:00:00+02:00",
          2.9436292808978d,
          0.2337d,
          0.4893707696998972d,
        ), // long of Berlin (13.39E)
        (
          "2011-06-21T15:00:00+02:00",
          2.9436292808978d,
          0.2337d,
          0.7511701574990467d,
        ), // long of Berlin (13.39E)
        (
          "2011-04-05T16:00:00+02:00",
          1.6181353941777565d,
          0.2337d,
          1.0062695999127786d,
        ), // long of Berlin (13.39E)
        (
          "2011-06-21T12:00:00+02:00",
          2.9436292808978d,
          0.5449d,
          0.2769719941015987d,
        ), // long of Cairo (31.22E)
      )

      forAll(testCases) { (time, j, longitude, omegaSol) =>
        When("the hour angle is calculated")
        val omegaCalc = pvModel.calcHourAngleOmega(
          ZonedDateTime.parse(time),
          Radians(j),
          Radians(longitude),
        )

        Then("result should match the test data")
        omegaCalc should approximate(Radians(omegaSol))
      }
    }

    "calculate the sunset angle omegaSS correctly" in {
      val testCases = Table(
        ("latitude", "delta", "omegaSSSol"),
        (0.9d, -0.402449d, 1.0045975406286176d), // lat: ~51.57°N
        (0.935d, -0.402449d, 0.956011693657339d), // different lat: ~53.57°N
        (0.9d, 0.017908d, 1.5933675693198284d), // different delta
        (
          0.157952297d,
          0.384670567d,
          1.635323424114512d,
        ), // Example 2.2 Goswami Principles of Solar Engineering
      )

      forAll(testCases) { (latitude, delta, omegaSSSol) =>
        When("the sunset angle is calculated")
        val omegaSSCalc =
          pvModel.calcSunsetAngleOmegaSS(Radians(latitude), Radians(delta))

        Then("result should match the test data")
        omegaSSCalc should approximate(Radians(omegaSSSol))
      }
    }

    "calculate the solar altitude angle alphaS correctly" in {
      val testCases = Table(
        ("omega", "delta", "latitude", "alphaSSol"),
        (
          1.946503016860923d,
          -0.402449d,
          0.9d,
          -0.5429594681352444d,
        ), // Jan 1st, lat: ~51.57°N
        (
          1.967775075784021d,
          -0.001150591501958d,
          0.9d,
          -0.24363984335678648d,
        ), // March 21st
        (
          1.946503016860923d,
          -0.402449d,
          0.935d,
          -0.5417322854819461d,
        ), // Jan 1st, lat: ~53.57°N
        (
          1.256637061d,
          -0.402449d,
          0.698d,
          -0.033897520990303694d,
        ), // omega: 82°, Jan 1st, lat: ~39.99°N
        (
          0.409691473331422d,
          -0.402449d,
          0.9d,
          0.21956610107293822d,
        ), // omega: 14:00, Jan 1st
        (
          -0.85019406d,
          -0.00720875d,
          0.9128072d,
          0.40911138927659646d,
        ), // omega: -48.71° = 09:00, March 21st, lat: Berlin
        (
          0.22425484d,
          0.40899596d,
          0.9128072d,
          1.0386092658376944d,
        ), // omega: +12.84° = 14:00 MESZ = 13:00 MEZ, June 21st, lat: Berlin
        (
          -0.81703281d,
          -0.00720875d,
          0.54628806d,
          0.619982384489836d,
        ), // omega: -36.9809° = 09:00, March 21st, lat: Cairo
        (
          -0.00438329d,
          0.40899596d,
          0.54628806d,
          1.4334492081530734d,
        ), // omega: -0.25° = 12:00, June 21st, lat: Cairo
        (
          0.0126074d,
          -0.40842934d,
          0.54628806d,
          0.6160025701438165d,
        ), // omega: +0.7223° = 12:00, Dez 21st, lat: Cairo
        (
          -0.78639785d,
          0.1549651d,
          0.54628806d,
          0.7430566034615067d,
        ), // omega: -45.05° = 09:00, Sep 1st, lat: Cairo
        (
          1.04619786d,
          0.1549651d,
          0.54628806d,
          0.5270965151470974d,
        ), // omega: +59.943° = 16:00, Sep 1st, lat: Cairo
        (
          0d,
          -0.305432619d,
          0.518013722d,
          0.7473499857948969d,
        ), // omega: 0 = Solar Noon, Feb 01st, lat/lon: Gainsville (29.68 N, 82.27 W)
        (
          -1.374970385d,
          0.380755678d,
          0.157952297d,
          0.2391202791125743d,
        ), // omega: -78.78° = 7:00 a.m., June 01st, lat/lon: Tocumen Panama (9.05 N, 79.37 W)
        (
          0d,
          -0.268780705d,
          -0.616101226d,
          1.2234758057948967d,
        ), // omega: 0° = Solar noon., Nov 01st, lat/lon: Canberra Australia (35.3 S, 149.1 E)
        (
          toRadians(-37.5d),
          toRadians(-14d),
          toRadians(43d),
          toRadians(23.4529893659531784299686037109330117049955654837550d),
        ), // '2011-02-13T09:30:00' from Duffie
        (
          toRadians(97.5d),
          toRadians(23.1d),
          toRadians(43d),
          toRadians(10.356151317506402829742934977890382350725031728508d),
        ), // '2011-07-01T06:30:00' from Duffie
        // Reference: Quaschning, Regenerative Energiesysteme figure 2.15 and figure 2.16   // gammaS@Quaschning = alphaS@SIMONA !
        (
          toRadians(-47.15114406),
          toRadians(23.4337425d),
          toRadians(52.3d),
          toRadians(44.12595614280154d),
        ), // Berlin (13.2E 52.3N) '2011-06-21T09:00:00' MEZ
        (
          toRadians(-32.15114394d),
          toRadians(23.4337425d),
          toRadians(52.3d),
          toRadians(52.15790489243239d),
        ), // Berlin (13.2E 52.3N) '2011-06-21T10:00:00' MEZ
        (
          toRadians(-17.15114381d),
          toRadians(23.4337425d),
          toRadians(52.3d),
          toRadians(58.29851278388936d),
        ), // Berlin (13.2E 52.3N) '2011-06-21T11:00:00' MEZ
        (
          toRadians(-2.151143686d),
          toRadians(23.4337425d),
          toRadians(52.3d),
          toRadians(61.086849596117524d),
        ), // Berlin (13.2E 52.3N) '2011-06-21T12:00:00' MEZ
        (
          toRadians(12.84885587d),
          toRadians(23.4337425d),
          toRadians(52.3d),
          toRadians(59.50792770681503d),
        ), // Berlin (13.2E 52.3N) '2011-06-21T13:00:00' MEZ
        (
          toRadians(27.84885599d),
          toRadians(23.4337425d),
          toRadians(52.3d),
          toRadians(54.170777340509574d),
        ), // Berlin (13.2E 52.3N) '2011-06-21T14:00:00' MEZ
        (
          toRadians(58.28178946d),
          toRadians(7.79402247d),
          toRadians(52.3d),
          toRadians(25.203526133755485d),
        ), // Berlin (13.2E 52.3N) '2011-09-04T16:00:00' MEZ
        (
          toRadians(0.948855924d),
          toRadians(23.4337425d),
          toRadians(30.1d),
          toRadians(83.28023248078853d),
        ), // Cairo  (31.3E 30.1N)  '2011-06-21T12:00:00' MEZ+1h
      )

      forAll(testCases) { (omega, delta, latitude, alphaSSol) =>
        When("the solar altitude angle is calculated")
        val alphaSCalc = pvModel.calcSolarAltitudeAngleAlphaS(
          Radians(omega),
          Radians(delta),
          Radians(latitude),
        )

        Then("result should match the test data")
        alphaSCalc should approximate(Radians(alphaSSol))
      }
    }

    "calculate the zenith angle thetaZ correctly" in {
      val testCases = Table(
        ("alphaS", "thetaZSol"),
        (0d, 1.5707963267948966d), // 0°
        (0.785398163397448d, 0.7853981633974486d), // 45°
        (1.570796326794897d, -4.440892098500626e-16d), // 90°
      )

      forAll(testCases) { (alphaS, thetaZSol) =>
        When("the zenith angle is calculated")
        val thetaZCalc = pvModel.calcZenithAngleThetaZ(Radians(alphaS))

        Then("result should match the test data")
        thetaZCalc should approximate(Radians(thetaZSol))
      }
    }

    "calculate the air mass correctly" in {
      val testCases = Table(
        ("thetaZ", "airMassSol"),
        (0d, 1d), // 0°
        (0.785398163397448d, 1.41321748045965d), // 45°
        (1.570796326794897d, 37.640108631323025d), // 90°
      )

      forAll(testCases) { (thetaZ, airMassSol) =>
        When("the air mass is calculated")
        val airMassCalc = pvModel.calcAirMass(Radians(thetaZ))

        Then("result should match the test data")
        airMassCalc shouldEqual airMassSol +- 1e-10 // the "approximate" function does not work for doubles, therefore the "shouldEqual" function is used
      }
    }

    "calculate the extraterrestrial radiation I0 correctly" in {
      val testCases = Table(
        ("j", "I0Sol"),
        (0d, 1414.91335d), // Jan 1st
        (2.943629280897834d, 1322.494291080537598d), // Jun 21st
        (4.52733626243351d, 1355.944773587800003d), // Sep 21st
      )

      forAll(testCases) { (j, I0Sol) =>
        When("the extraterrestrial radiation is calculated")
        val I0Calc = pvModel.calcExtraterrestrialRadiationI0(Radians(j))

        Then("result should match the test data")
        I0Calc should approximate(WattHoursPerSquareMeter(I0Sol))
      }
    }

    "calculate the angle of incidence thetaG correctly" in {
      val testCases = Table(
        (
          "latitudeDeg",
          "deltaDeg",
          "omegaDeg",
          "gammaEDeg",
          "alphaEDeg",
          "thetaGOut",
        ),
        (43d, -14d, -22.5d, 45d, 15d,
          35.176193345578606393727080835951995075234213360724d), // Duffie
        (
          51.516667d,
          +18.4557514d,
          -15.00225713d,
          30d,
          +0d,
          14.420271449960715d,
        ), // Iqbal
        (
          51.516667d,
          +18.4557514d,
          -15.00225713d,
          90d,
          +0d,
          58.65287310017624d,
        ), // Iqbal
        (
          35.0d,
          +23.2320597d,
          +30.00053311d,
          45d,
          10d,
          39.62841449023577d,
        ), // Kalogirou - Solar Energy Engineering Example 2.7  ISBN 978-0-12-374501-9; DOI https://doi.org/10.1016/B978-0-12-374501-9.X0001-5
        (
          35.0d,
          +23.2320597d,
          +30.00053311d,
          45d,
          90d,
          18.946300807438607d,
        ), // Kalogirou - Solar Energy Engineering Example 2.7 changed to 90° panel azimuth to WEST
        (
          35.0d,
          +23.2320597d,
          +74.648850625d,
          45d,
          90d,
          21.95480347380729d,
        ), // Kalogirou - Solar Energy Engineering Example 2.7  90° panel azimuth to WEST at 17:00
        (
          35.0d,
          +23.2320597d,
          +74.648850625d,
          45d,
          -90d,
          109.00780288303966d,
        ), // Kalogirou - Solar Energy Engineering Example 2.7  90° panel azimuth to EAST at 17:00
        (
          27.96d,
          -17.51d,
          -11.1d,
          30d,
          +10d,
          22.384603601536398d,
        ), // Goswami Principles of Solar Engineering Example 2.7a
        (
          -35.3d,
          -17.51d,
          -4.2d,
          30d,
          +170d,
          14.882390116876563d,
        ), // Goswami Principles of Solar Engineering Example 2.7b
        (40d, -11.6d, 82.5d, 60d, 0d, 79.11011928744357d),
        (40d, -11.6d, -82.5d, 60d, 0d,
          79.11011928744357d), // inverse hour angle
        (40d, -11.6d, 78.0d, 60d, 0d, 74.92072065185143d),
        (40d, -11.6d, -78.0d, 60d, 0d,
          74.92072065185143d), // inverse hour angle
        (45d, -7.15d, -82.5d, 0d, 0d,
          89.79565474295107d), // Duffie Solar Engineering of Thermal Processes example 2.14.1

      )

      /** Calculate the angle of incidence of beam radiation on a surface
        * located at a latitude at a certain hour angle (solar time) on a given
        * declination (date) if the surface is tilted by a certain slope from
        * the horizontal and pointed to a certain panel azimuth west of south.
        */

      forAll(testCases) {
        (latitudeDeg, deltaDeg, omegaDeg, gammaEDeg, alphaEDeg, thetaGOut) =>
          Given("using the input data")

          When("the angle of incidence is calculated")
          val thetaG = pvModel.calcAngleOfIncidenceThetaG(
            Degrees(deltaDeg),
            Degrees(latitudeDeg),
            Degrees(gammaEDeg),
            Degrees(alphaEDeg),
            Degrees(omegaDeg),
          )

          Then("result should match the test data")
          thetaG should approximate(Degrees(thetaGOut))
      }
    }

    "deliver correct angles of incidence at different latitudes and slope angles" in {
      val testCases = Table(
        (
          "latitudeDeg",
          "deltaDeg",
          "omegaDeg",
          "gammaEDeg",
          "alphaEDeg",
          "thetaGOut",
        ),
        (45d, -7.15d, -82.5d, 60d, 0d, 80.94904834048776d), // thetaG
        (15d, -7.15, -82.5d, 30d, 0d,
          80.94904834048776d), // same test but 30° South with 30° less sloped surface
        (0d, -7.15, -82.5d, 15d, 0d,
          80.94904834048776d), // same test but 15° South with 15° less sloped surface
        (-15d, -7.15, -82.5d, 0d, 0d,
          80.94904834048776d), // same test but 15° South with 15° less sloped surface
        (-30d, -7.15, -82.5d, 15d, 180d,
          80.94904834048776d), // same test but 15° South with 15° more sloped surface (Surface is now facing north, since it is in the southern hemisphere, therefore the surface azimuth is 180°)
        (52.3d, 23.4337425, 2.15114395d, 0d, 0d,
          28.91315041538251d), // Berlin 21.06. 12:00 => thetaG = 90 - alphaS
        (70.3d, 23.4337425, 2.15114395d, 18d, 0d,
          28.91315041538251d), // same test but 18° North with 18° sloped surface

      )

      /** Iqbal Figure 1.6.2 - the angle of incidence of a surface sloped by
        * angle beta (gammaE) at latitude phi should be same as the angle of
        * incidence of an "unsloped" (horizontal) surface (where the angle of
        * incidence is equal to the zenith angle of the sun) positioned at
        * latitude phi - beta. Note that this is only true if the surface is
        * facing directly north or south.
        */
      forAll(testCases) {
        (latitudeDeg, deltaDeg, omegaDeg, gammaEDeg, alphaEDeg, thetaGOut) =>
          Given("using pre-calculated parameters")

          When("the angle of incidence is calculated")
          val thetaG = pvModel.calcAngleOfIncidenceThetaG(
            Degrees(deltaDeg),
            Degrees(latitudeDeg),
            Degrees(gammaEDeg),
            Degrees(alphaEDeg),
            Degrees(omegaDeg),
          )

          Then("the result should match the pre-calculated data")
          thetaG should approximate(Degrees(thetaGOut))
      }
    }

    "calculate the estimate beam radiation eBeamS correctly" in {
      val testCases = Table(
        (
          "latitudeDeg",
          "gammaEDeg",
          "alphaEDeg",
          "deltaDeg",
          "omegaDeg",
          "thetaGDeg",
          "eBeamSSol",
        ),
        (40d, 0d, 0d, -11.6d, -37.5d, 37.0d,
          67.777778d), // flat surface => eBeamS = eBeamH
        (40d, 60d, 0d, -11.6d, -37.5d, 37.0d,
          112.84217113154841369d), // 2011-02-20T09:00:00
        (40d, 60d, 0d, -11.6d, -78.0d, 75.0d, 210.97937494450755d), // sunrise
        (40d, 60d, 0d, -11.6d, 62.0d, 76.0d, 199.16566536224116d), // sunset
        (40d, 60d, 0d, -11.6d, 69.0d, 89.9d,
          245.77637766673405d), // sunset, cut off
        (40d, 60d, 0d, -11.6d, 75.0d, 89.9d, 0d), // no sun
        (40d, 60d, -90.0d, -11.6d, 60.0d, 91.0d, 0d), // no direct beam
      )

      /** For a given hour angle, the estimate beam radiation on a sloped
        * surface is calculated for the next 60 minutes. Reference p.95
        * https://www.sku.ac.ir/Datafiles/BookLibrary/45/John%20A.%20Duffie,%20William%20A.%20Beckman(auth.)-Solar%20Engineering%20of%20Thermal%20Processes,%20Fourth%20Edition%20(2013).pdf
        */
      forAll(testCases) {
        (
            latitudeDeg,
            gammaEDeg,
            alphaEDeg,
            deltaDeg,
            omegaDeg,
            thetaGDeg,
            eBeamSSol,
        ) =>
          Given("using the input data")
          // Beam Radiation on a horizontal surface
          val eBeamH =
            67.777778d // 1 MJ/m^2 = 277,778 Wh/m^2 -> 0.244 MJ/m^2 = 67.777778 Wh/m^2
          val omegaSS = pvModel.calcSunsetAngleOmegaSS(
            Degrees(latitudeDeg),
            Degrees(deltaDeg),
          ) // Sunset angle
          val omegaSR = -omegaSS // Sunrise angle
          val omegas = pvModel.calculateBeamOmegas(
            Degrees(thetaGDeg),
            Degrees(omegaDeg),
            omegaSS,
            omegaSR,
          ) // omega1 and omega2

          When("the beam radiation is calculated")
          val eBeamSCalc = pvModel.calcBeamRadiationOnSlopedSurface(
            WattHoursPerSquareMeter(eBeamH),
            omegas,
            Degrees(deltaDeg),
            Degrees(latitudeDeg),
            Degrees(gammaEDeg),
            Degrees(alphaEDeg),
          )

          Then("result should match the test data")
          eBeamSCalc should approximate(WattHoursPerSquareMeter(eBeamSSol))
      }
    }

    "calculate the estimate diffuse radiation eDifS correctly" in {
      val testCases = Table(
        ("thetaGDeg", "thetaZDeg", "gammaEDeg", "airMass", "I0", "eDifSSol"),
        (37.0d, 62.2d, 60d, 2.13873080095658d, 1399.0077631849722d,
          216.46615469650982d),
      )

      forAll(testCases) {
        (thetaGDeg, thetaZDeg, gammaEDeg, airMass, I0, eDifSSol) =>
          // Reference p.95
          // https://www.sku.ac.ir/Datafiles/BookLibrary/45/John%20A.%20Duffie,%20William%20A.%20Beckman(auth.)-Solar%20Engineering%20of%20Thermal%20Processes,%20Fourth%20Edition%20(2013).pdf
          Given("using the input data")
          // Beam Radiation on horizontal surface
          val eBeamH =
            67.777778d // 1 MJ/m^2 = 277,778 Wh/m^2 -> 0.244 MJ/m^2 = 67.777778 Wh/m^2
          // Diffuse Radiation on a horizontal surface
          val eDifH = 213.61111d // 0.769 MJ/m^2 = 213,61111 Wh/m^2

          When("the diffuse radiation is calculated")
          val eDifSCalc = pvModel.calcDiffuseRadiationOnSlopedSurfacePerez(
            WattHoursPerSquareMeter(eDifH),
            WattHoursPerSquareMeter(eBeamH),
            airMass,
            WattHoursPerSquareMeter(I0),
            Degrees(thetaZDeg),
            Degrees(thetaGDeg),
            Degrees(gammaEDeg),
          )

          Then("result should match the test data")
          eDifSCalc should approximate(WattHoursPerSquareMeter(eDifSSol))
      }
    }

    "calculate the ground reflection eRefS" in {
      val testCases = Table(
        ("gammaEDeg", "albedo", "eRefSSol"),
        (60d, 0.60d, 42.20833319999999155833336d), // '2011-02-20T09:00:00'
      )

      forAll(testCases) { (gammaEDeg, albedo, eRefSSol) =>
        Given("using the input data")
        // Beam Radiation on horizontal surface
        val eBeamH =
          67.777778d // 1 MJ/m^2 = 277,778 Wh/m^2 -> 0.244 MJ/m^2 = 67.777778 Wh/m^2
        // Diffuse Radiation on a horizontal surface
        val eDifH = 213.61111d // 0.769 MJ/m^2 = 213,61111 Wh/m^2

        When("the ground reflection is calculated")
        val eRefSCalc = pvModel.calcReflectedRadiationOnSlopedSurface(
          WattHoursPerSquareMeter(eBeamH),
          WattHoursPerSquareMeter(eDifH),
          Degrees(gammaEDeg),
          albedo,
        )

        Then("result should match the test data")
        eRefSCalc should approximate(WattHoursPerSquareMeter(eRefSSol))
      }
    }
  }
}
