/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.result.system.{
  PvResult,
  SystemParticipantResult,
}
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelInput,
  ModelState,
}
import edu.ie3.simona.model.participant2.PvModel.PvState
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.ServiceType
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities._
import squants._
import squants.space.{Degrees, SquareMeters}
import squants.time.Minutes
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import scala.math._

class PvModel private (
    override val uuid: UUID,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val lat: Angle,
    private val lon: Angle,
    private val albedo: Double,
    private val etaConv: Dimensionless,
    private val alphaE: Angle,
    private val gammaE: Angle,
    private val moduleSurface: Area = SquareMeters(1d),
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      PvState,
    ]
    with ParticipantSimpleFlexibility[PvState]
    with LazyLogging {

  /** Override sMax as the power output of a pv unit could become easily up to
    * 10% higher than the sRated value found in the technical sheets
    */
  val sMax: ApparentPower = sRated * 1.1

  /** Permissible maximum active power feed in (therefore negative) */
  protected val pMax: Power = sMax.toActivePower(cosPhiRated) * -1

  /** Reference yield at standard testing conditions (STC) */
  private val yieldSTC = WattsPerSquareMeter(1000d)

  private val activationThreshold =
    sRated.toActivePower(cosPhiRated) * 0.001 * -1

  override val initialState: ModelInput => PvState = { input =>
    val weatherData = getWeatherData(input.receivedData)
    PvState(
      input.currentTick,
      input.currentSimulationTime,
      ???,
      weatherData.diffIrr,
      weatherData.dirIrr,
    )
  }

  override def determineState(
      lastState: PvState,
      operatingPoint: ActivePowerOperatingPoint,
      input: ModelInput,
  ): PvState = initialState(input)

  private def getWeatherData(receivedData: Seq[Data]): WeatherData = {
    receivedData
      .collectFirst { case weatherData: WeatherData =>
        weatherData
      }
      .getOrElse {
        throw new CriticalFailureException(
          s"Expected WeatherData, got $receivedData"
        )
      }
  }

  /** Calculate the active power behaviour of the model
    *
    * @param state
    *   The current state including weather data
    * @return
    *   Active power
    */
  override def determineOperatingPoint(
      state: PvState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    // === Weather Base Data  === //
    /* The pv model calculates the power in-feed based on the solar irradiance that is received over a specific
     * time frame (which actually is the solar irradiation). Hence, a multiplication with the time frame within
     * this irradiance is received is required. */
    val duration: Time = Seconds(state.weatherDataFrameLength)

    // eBeamH and eDifH needs to be extract to their double values in some places
    // hence a conversion to watt-hour per square meter is required, to avoid
    // invalid double value extraction!
    val eBeamH =
      state.dirIrradiance * duration
    val eDifH =
      state.diffIrradiance * duration

    // === Beam Radiation Parameters  === //
    val angleJ = calcAngleJ(state.dateTime)
    val delta = calcSunDeclinationDelta(angleJ)

    val omega = calcHourAngleOmega(state.dateTime, angleJ, lon)

    val omegaSS = calcSunsetAngleOmegaSS(lat, delta)
    val omegaSR = calcSunriseAngleOmegaSR(omegaSS)

    val alphaS = calcSolarAltitudeAngleAlphaS(omega, delta, lat)
    val thetaG =
      calcAngleOfIncidenceThetaG(delta, lat, gammaE, alphaE, omega)

    val omegas = calculateBeamOmegas(thetaG, omega, omegaSS, omegaSR)

    // === Beam Radiation ===//
    val eBeamS = calcBeamRadiationOnSlopedSurface(
      eBeamH,
      omegas,
      delta,
      lat,
      gammaE,
      alphaE,
    )

    // === Diffuse Radiation Parameters ===//
    val thetaZ = calcZenithAngleThetaZ(alphaS)
    val airMass = calcAirMass(thetaZ)
    val extraterrestrialRadiationI0 = calcExtraterrestrialRadiationI0(angleJ)

    // === Diffuse Radiation ===//
    val eDifS = calcDiffuseRadiationOnSlopedSurfacePerez(
      eDifH,
      eBeamH,
      airMass,
      extraterrestrialRadiationI0,
      thetaZ,
      thetaG,
      gammaE,
    )

    // === Reflected Radiation ===//
    val eRefS =
      calcReflectedRadiationOnSlopedSurface(eBeamH, eDifH, gammaE, albedo)

    // === Total Radiation ===//
    val eTotal = eDifS + eBeamS + eRefS

    val irraditionSTC = yieldSTC * duration
    val power = calcOutput(
      eTotal,
      state.dateTime,
      irraditionSTC,
    )

    (ActivePowerOperatingPoint(power), None)
  }

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  /** Calculates the position of the earth in relation to the sun (day angle)
    * for the provided time
    *
    * @param time
    *   the time
    * @return
    *   day angle J
    */
  def calcAngleJ(time: ZonedDateTime): Angle = {
    val day = time.getDayOfYear // day of the year
    val j = 2d * Math.PI * ((day - 1d) / 365)
    Radians(j)
  }

  /** Calculates the declination angle delta of the sun at solar noon (i.e.,
    * when the sun is on the local meridian) with respect to the plane of the
    * equator. Formula taken from Spencer, J.W. "Fourier series representation
    * of the position of the sun". Appl. Opt. 1971, 10, 2569–2571
    *
    * @param angleJ
    *   day angle J
    * @return
    *   declination angle
    */
  def calcSunDeclinationDelta(
      angleJ: Angle
  ): Angle = {
    val jInRad = angleJ.toRadians
    Radians(
      0.006918 -
        0.399912 * cos(jInRad) +
        0.070257 * sin(jInRad) -
        0.006758 * cos(2d * jInRad) +
        0.000907 * sin(2d * jInRad) -
        0.002697 * cos(3d * jInRad) +
        0.00148 * sin(3d * jInRad)
    )
  }

  /** Calculates the hour angle omega which represents the angular displacement
    * of the sun east or west of the local meridian due to rotation of the earth
    * on its axis at 15◦ per hour; morning negative, afternoon positive.
    *
    * @param time
    *   the requested time (which is transformed to solar time)
    * @param angleJ
    *   day angle J
    * @param longitude
    *   longitude of the position
    * @return
    *   hour angle omega
    */
  def calcHourAngleOmega(
      time: ZonedDateTime,
      angleJ: Angle,
      longitude: Angle,
  ): Angle = {
    val jInRad = angleJ.toRadians
    val lambda = longitude.toDegrees
    val et = Minutes(
      0.0066 + 7.3525 * cos(jInRad + 1.4992378274631293) + 9.9359 * cos(
        2d * jInRad + 1.9006635554218247
      ) + 0.3387 * cos(3d * jInRad + 1.8360863730980346)
    )

    val lmt = Minutes(time.getHour * 60d + time.getMinute - 4d * (15d - lambda))
    val st = lmt + et

    Radians((st.toHours - 12).toRadians * 15d)
  }

  /** Calculates the sunset hour angle omegaSS which represents the omega value
    * when the sun sets. The sunrise hour angle omegaSR is the negative of
    * omegaSS.
    *
    * @param latitude
    *   latitude of the position
    * @param delta
    *   sun declination angle
    * @return
    *   sunset angle omegaSS
    */
  def calcSunsetAngleOmegaSS(
      latitude: Angle,
      delta: Angle,
  ): Angle = {
    val latInRad = latitude.toRadians
    val deltaInRad = delta.toRadians

    Radians(acos(-tan(latInRad) * tan(deltaInRad)))
  }

  /** Calculates the sunrise hour angle omegaSR given omegaSS.
    */
  private val calcSunriseAngleOmegaSR =
    (omegaSS: Angle) => omegaSS * -1

  /** Calculates the solar altitude angle alphaS which represents the angle
    * between the horizontal and the line to the sun, that is, the complement of
    * the zenith angle.
    *
    * @param omega
    *   hour angle
    * @param delta
    *   sun declination angle
    * @param latitude
    *   latitude of the position
    * @return
    *   solar altitude angle alphaS
    */
  def calcSolarAltitudeAngleAlphaS(
      omega: Angle,
      delta: Angle,
      latitude: Angle,
  ): Angle = {
    val latInRad = latitude.toRadians
    val deltaInRad = delta.toRadians
    val omegaInRad = omega.toRadians
    val sinAlphaS =
      min(
        max(
          cos(omegaInRad) * cos(latInRad) * cos(deltaInRad) +
            sin(latInRad) * sin(deltaInRad),
          -1,
        ),
        1,
      )
    Radians(asin(sinAlphaS))
  }

  /** Calculates the zenith angle thetaG which represents the angle between the
    * vertical and the line to the sun, that is, the angle of incidence of beam
    * radiation on a horizontal surface.
    *
    * @param alphaS
    *   sun altitude angle
    * @return
    *   the zenith angle
    */
  def calcZenithAngleThetaZ(
      alphaS: Angle
  ): Angle = {
    val alphaSInRad = alphaS.toRadians

    // the zenith angle is defined as 90° - gammaS in Radian
    Radians(Pi / 2 - abs(alphaSInRad))
  }

  /** Calculates the ratio of the mass of atmosphere through which beam
    * radiation passes to the mass it would pass through if the sun were at the
    * zenith (i.e., directly overhead).
    *
    * @param thetaZ
    *   zenith angle
    * @return
    *   air mass
    */
  def calcAirMass(thetaZ: Angle): Double = {
    val thetaZInRad = thetaZ.toRadians

    // radius of the earth in kilometers
    val re = 6371d
    // approx. effective height of the atmosphere
    val yAtm = 9d

    // Ratio re / yAtm between the earth radius and the atmosphere height
    val airMassRatio = re / yAtm
    sqrt(
      pow(airMassRatio * cos(thetaZInRad), 2d) + 2d * airMassRatio + 1d
    ) - airMassRatio * cos(thetaZInRad)
  }

  /** Calculates the extraterrestrial radiation, that is, the radiation that
    * would be received in the absence of the atmosphere.
    *
    * @param angleJ
    *   day angle J
    * @return
    *   extraterrestrial radiation I0
    */
  def calcExtraterrestrialRadiationI0(
      angleJ: Angle
  ): Irradiation = {
    val jInRad = angleJ.toRadians

    // eccentricity correction factor
    val e0 = 1.000110 +
      0.034221 * cos(jInRad) +
      0.001280 * sin(jInRad) +
      0.000719 * cos(2d * jInRad) +
      0.000077 * sin(2d * jInRad)

    // solar constant in W/m2
    val Gsc = WattHoursPerSquareMeter(1367) // solar constant
    Gsc * e0
  }

  /** Calculates the angle of incidence thetaG of beam radiation on a surface
    *
    * @param delta
    *   sun declination angle
    * @param latitude
    *   latitude of the position
    * @param gammaE
    *   slope angle (the angle between the plane of the surface in question and
    *   the horizontal)
    * @param alphaE
    *   surface azimuth angle (the deviation of the projection on a horizontal
    *   plane of the normal to the surface from the local meridian, with zero
    *   due south, east negative, and west positive)
    * @param omega
    *   hour angle
    * @return
    *   angle of incidence thetaG
    */
  def calcAngleOfIncidenceThetaG(
      delta: Angle,
      latitude: Angle,
      gammaE: Angle,
      alphaE: Angle,
      omega: Angle,
  ): Angle = {
    val deltaInRad = delta.toRadians
    val omegaInRad = omega.toRadians
    val gammaInRad = gammaE.toRadians
    val alphaEInRad = alphaE.toRadians
    val latInRad = latitude.toRadians

    Radians(
      acos(
        sin(deltaInRad) * sin(latInRad) * cos(gammaInRad) -
          sin(deltaInRad) * cos(latInRad) * sin(gammaInRad) * cos(alphaEInRad) +
          cos(deltaInRad) * cos(latInRad) * cos(gammaInRad) * cos(omegaInRad) +
          cos(deltaInRad) * sin(latInRad) * sin(gammaInRad) *
          cos(alphaEInRad) * cos(omegaInRad) +
          cos(deltaInRad) * sin(gammaInRad) * sin(alphaEInRad) * sin(omegaInRad)
      )
    )
  }

  /** Calculates omega1 and omega2, which are parameters for
    * calcBeamRadiationOnSlopedSurface
    *
    * @param thetaG
    *   angle of incidence
    * @param omega
    *   hour angle
    * @param omegaSS
    *   sunset angle
    * @param omegaSR
    *   sunrise angle
    * @return
    *   omega1 and omega encapsulated in an Option, if applicable. None
    *   otherwise
    */
  def calculateBeamOmegas(
      thetaG: Angle,
      omega: Angle,
      omegaSS: Angle,
      omegaSR: Angle,
  ): Option[(Angle, Angle)] = {
    val thetaGInRad = thetaG.toRadians
    val omegaSSInRad = omegaSS.toRadians
    val omegaSRInRad = omegaSR.toRadians

    val omegaOneHour = toRadians(15d)
    val omegaHalfHour = omegaOneHour / 2d

    val omega1InRad = omega.toRadians // requested hour
    val omega2InRad = omega1InRad + omegaOneHour // requested hour plus 1 hour

    // (thetaG < 90°): sun is visible
    // (thetaG > 90°), otherwise: sun is behind the surface  -> no direct radiation
    if (
      thetaGInRad < toRadians(90)
      // omega1 and omega2: sun has risen and has not set yet
      && omega2InRad > omegaSRInRad + omegaHalfHour
      && omega1InRad < omegaSSInRad - omegaHalfHour
    ) {

      val (finalOmega1, finalOmega2) =
        if (omega1InRad < omegaSRInRad) {
          // requested time earlier than sunrise
          (omegaSRInRad, omegaSRInRad + omegaOneHour)
        } else if (omega2InRad > omegaSSInRad) {
          // sunset earlier than requested time
          (omegaSSInRad - omegaOneHour, omegaSSInRad)
        } else {
          (omega1InRad, omega2InRad)
        }

      Some(Radians(finalOmega1), Radians(finalOmega2))
    } else
      None
  }

  /** Calculates the beam radiation on a sloped surface
    *
    * @param eBeamH
    *   beam radiation on a horizontal surface
    * @param omegas
    *   omega1 and omega2
    * @param delta
    *   sun declination angle
    * @param latitude
    *   latitude of the position
    * @param gammaE
    *   slope angle (the angle between the plane of the surface in question and
    *   the horizontal)
    * @param alphaE
    *   surface azimuth angle (the deviation of the projection on a horizontal
    *   plane of the normal to the surface from the local meridian, with zero
    *   due south, east negative, and west positive)
    * @return
    *   the beam radiation on the sloped surface
    */
  def calcBeamRadiationOnSlopedSurface(
      eBeamH: Irradiation,
      omegas: Option[(Angle, Angle)],
      delta: Angle,
      latitude: Angle,
      gammaE: Angle,
      alphaE: Angle,
  ): Irradiation = {

    omegas match {
      case Some((omega1, omega2)) =>
        val deltaInRad = delta.toRadians
        val gammaEInRad = gammaE.toRadians
        val alphaEInRad = alphaE.toRadians
        val latInRad = latitude.toRadians

        val omega1InRad = omega1.toRadians
        val omega2InRad = omega2.toRadians

        val a = ((sin(deltaInRad) * sin(latInRad) * cos(gammaEInRad)
          - sin(deltaInRad) * cos(latInRad) * sin(gammaEInRad) * cos(
            alphaEInRad
          ))
          * (omega2InRad - omega1InRad)
          + (cos(deltaInRad) * cos(latInRad) * cos(gammaEInRad)
            + cos(deltaInRad) * sin(latInRad) * sin(gammaEInRad) * cos(
              alphaEInRad
            ))
          * (sin(omega2InRad) - sin(omega1InRad))
          - (cos(deltaInRad) * sin(gammaEInRad) * sin(alphaEInRad))
          * (cos(omega2InRad) - cos(omega1InRad)))

        val b = ((cos(latInRad) * cos(deltaInRad)) * (sin(omega2InRad) - sin(
          omega1InRad
        ))
          + (sin(latInRad) * sin(deltaInRad)) * (omega2InRad - omega1InRad))

        // in rare cases (close to sunrise) r can become negative (although very small)
        val r = max(a / b, 0d)
        eBeamH * r
      case None => WattHoursPerSquareMeter(0d)
    }
  }

  /** Calculates the diffuse radiation on a sloped surface based on the model of
    * Perez et al.
    *
    * <p>Formula taken from Perez, R., P. Ineichen, R. Seals, J. Michalsky, and
    * R. Stewart, "Modeling Daylight Availability and Irradiance Components from
    * Direct and Global Irradiance". Solar Energy, 44, 271 (1990).
    *
    * @param eDifH
    *   diffuse radiation on a horizontal surface
    * @param eBeamH
    *   beam radiation on a horizontal surface
    * @param airMass
    *   the air mass
    * @param extraterrestrialRadiationI0
    *   extraterrestrial radiation
    * @param thetaZ
    *   zenith angle
    * @param thetaG
    *   angle of incidence
    * @param gammaE
    *   slope angle (the angle between the plane of the surface in question and
    *   the horizontal)
    * @return
    *   the diffuse radiation on the sloped surface
    */
  def calcDiffuseRadiationOnSlopedSurfacePerez(
      eDifH: Irradiation,
      eBeamH: Irradiation,
      airMass: Double,
      extraterrestrialRadiationI0: Irradiation,
      thetaZ: Angle,
      thetaG: Angle,
      gammaE: Angle,
  ): Irradiation = {
    val thetaZInRad = thetaZ.toRadians
    val thetaGInRad = thetaG.toRadians
    val gammaEInRad = gammaE.toRadians

    // == brightness index delta  ==//
    val delta = eDifH * airMass / extraterrestrialRadiationI0

    // == cloud index epsilon  ==//
    // if we have no clouds,  the epsilon bin is 8, as epsilon bin for an epsilon in [6.2, inf.[ = 8
    val x = if (eDifH.value.doubleValue > 0) {
      // if we have diffuse radiation on horizontal surface we have to consider
      // the clearness parameter epsilon, which then gives us an epsilon bin x

      // Beam radiation is required on a plane normal to the beam direction (normal incidence),
      // thus dividing by cos theta_z
      var epsilon = ((eDifH + eBeamH / cos(thetaZInRad)) / eDifH +
        (5.535d * 1.0e-6) * pow(
          thetaZ.toDegrees,
          3,
        )) / (1d + (5.535d * 1.0e-6) * pow(
        thetaZ.toDegrees,
        3,
      ))

      // get the corresponding bin if epsilon is smaller than 6.2
      if (epsilon < 6.2) { // define the bins based on Perez
        val discreteSkyClearnessCategories = Array(
          Array(1, 1.065),
          Array(1.065, 1.230),
          Array(1.230, 1.500),
          Array(1.500, 1.950),
          Array(1.950, 2.800),
          Array(2.800, 4.500),
          Array(4.500, 6.200),
        )
        // adapt the epsilon as we have no bin < 1
        epsilon = max(1, epsilon)

        // get the corresponding bin
        val finalEpsilon = epsilon

        discreteSkyClearnessCategories.indices
          .find { i =>
            (finalEpsilon -
              discreteSkyClearnessCategories(i)(0) >= 0) &&
            (finalEpsilon -
              discreteSkyClearnessCategories(i)(1) < 0)
          }
          .map(_ + 1)
          .getOrElse(8)
      } else {
        // epsilon in [6.2, inf.[
        8
      }
    } else {
      // if we have no clouds, the epsilon bin is 8,
      // as the epsilon bin for an epsilon in [6.2, inf.[ is 8
      8
    }

    // calculate the f_ij components based on the epsilon bin
    val f11 = -0.0161 * pow(x, 3) + 0.1840 * pow(x, 2) - 0.3806 * x + 0.2324
    val f12 = 0.0134 * pow(x, 4) - 0.1938 * pow(x, 3) + 0.8410 * pow(
      x,
      2,
    ) - 1.4018 * x + 1.3579
    val f13 = 0.0032 * pow(x, 3) - 0.0280 * pow(x, 2) - 0.0056 * x - 0.0385
    val f21 = -0.0048 * pow(x, 3) + 0.0536 * pow(x, 2) - 0.1049 * x + 0.0034
    val f22 = 0.0012 * pow(x, 3) - 0.0067 * pow(x, 2) + 0.0091 * x - 0.0269
    val f23 = 0.0052 * pow(x, 3) - 0.0971 * pow(x, 2) + 0.2856 * x - 0.1389

    // calculate circumsolar brightness coefficient f1 and horizon brightness coefficient f2
    val f1 = max(0, f11 + f12 * delta + f13 * thetaZInRad)
    val f2 = f21 + f22 * delta + f23 * thetaZInRad
    val aPerez = max(0, cos(thetaGInRad))
    val bPerez = max(cos(1.4835298641951802), cos(thetaZInRad))

    // finally calculate the diffuse radiation on an inclined surface
    eDifH * (
      ((1 + cos(gammaEInRad)) / 2) * (1 - f1) +
        (f1 * (aPerez / bPerez)) +
        (f2 * sin(gammaEInRad))
    )
  }

  /** Calculates the reflected radiation on a sloped surface
    *
    * @param eBeamH
    *   beam radiation on a horizontal surface
    * @param eDifH
    *   diffuse radiation on a horizontal surface
    * @param gammaE
    *   slope angle (the angle between the plane of the surface in question and
    *   the horizontal)
    * @param albedo
    *   albedo / "composite" ground reflection
    * @return
    *   the reflected radiation on the sloped surface eRefS
    */
  def calcReflectedRadiationOnSlopedSurface(
      eBeamH: Irradiation,
      eDifH: Irradiation,
      gammaE: Angle,
      albedo: Double,
  ): Irradiation = {
    val gammaEInRad = gammaE.toRadians
    (eBeamH + eDifH) * (albedo * 0.5 * (1 - cos(gammaEInRad)))
  }

  private def generatorCorrectionFactor(
      time: ZonedDateTime,
      gammaE: Angle,
  ): Double = {
    val gammaEValInDeg = gammaE.toDegrees

    val genCorr = new Array[Array[Double]](4)
    genCorr(0) = Array(0.69, 0.73, 0.81, 0.83, 0.84, 0.84, 0.9, 0.84, 0.84,
      0.82, 0.75, 0.66) // 30°
    genCorr(1) = Array(0.8, 0.83, 0.84, 0.85, 0.86, 0.86, 0.86, 0.86, 0.86,
      0.84, 0.82, 0.77) // 45°
    genCorr(2) = Array(0.84, 0.85, 0.86, 0.86, 0.85, 0.85, 0.85, 0.85, 0.86,
      0.86, 0.85, 0.84) // 60°
    genCorr(3) = Array(0.86, 0.86, 0.85, 0.84, 0.82, 0.81, 0.81, 0.82, 0.84,
      0.85, 0.86, 0.86) // 90°

    val genCorrKey: Int = gammaEValInDeg match {
      case gamma if gamma < 38 => 0
      case gamma if gamma < 53 => 1
      case gamma if gamma < 75 => 2
      case _                   => 3
    }

    genCorr(genCorrKey)(time.getMonth.getValue - 1)
  }

  private def temperatureCorrectionFactor(time: ZonedDateTime): Double = {
    val tempCorr =
      Array(1.06, 1.04, 1.01, 0.98, 0.94, 0.93, 0.94, 0.93, 0.96, 1, 1.04, 1.06)

    tempCorr(time.getMonth.getValue - 1)
  }

  private def calcOutput(
      eTotalInWhPerSM: Irradiation,
      time: ZonedDateTime,
      irradiationSTC: Irradiation,
  ): Power = {
    val genCorr = generatorCorrectionFactor(time, gammaE)
    val tempCorr = temperatureCorrectionFactor(time)
    /* The actual yield of this sum of available panels. As the solar irradiance summed up over the total panel surface
     * area. The yield also takes care of generator and temperature correction factors as well as the converter's
     * efficiency */
    val actYield =
      eTotalInWhPerSM * moduleSurface.toSquareMeters * etaConv.toEach * (genCorr * tempCorr)

    /* Calculate the foreseen active power output without boundary condition adaptions */
    val proposal = pRated * -1 * (
      actYield / irradiationSTC
    )

    /* Do sanity check, if the proposed feed in is above the estimated maximum to be apparent active power of the plant */
    if (proposal < pMax)
      logger.warn(
        "The fed in active power is higher than the estimated maximum active power of this plant ({} < {}). " +
          "Did you provide wrong weather input data?",
        proposal,
        pMax,
      )

    /* If the output is marginally small, suppress the output, as we are likely to be in night and then only produce incorrect output */
    if (proposal.compareTo(activationThreshold) > 0)
      DefaultQuantities.zeroMW
    else proposal
  }

  override def createResults(
      state: PvState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      new PvResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new PvResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

  override def getRequiredSecondaryServices: Iterable[ServiceType] =
    Iterable(ServiceType.WeatherService)

}

object PvModel {

  /** Holds all relevant data for a pv model calculation
    *
    * @param tick
    *   The current tick
    * @param dateTime
    *   date and time of the <b>ending</b> of time frame to calculate
    * @param weatherDataFrameLength
    *   the duration in ticks (= seconds) the provided irradiance is received by
    *   the pv panel
    * @param diffIrradiance
    *   diffuse solar irradiance
    * @param dirIrradiance
    *   direct solar irradiance
    */
  final case class PvState(
      override val tick: Long,
      dateTime: ZonedDateTime,
      weatherDataFrameLength: Long,
      diffIrradiance: Irradiance,
      dirIrradiance: Irradiance,
  ) extends ModelState

  def apply(
      inputModel: PvInput
  ): PvModel =
    new PvModel(
      inputModel.getUuid,
      Kilovoltamperes(
        inputModel.getsRated
          .to(PowerSystemUnits.KILOVOLTAMPERE)
          .getValue
          .doubleValue
      ),
      inputModel.getCosPhiRated,
      QControl(inputModel.getqCharacteristics),
      Degrees(inputModel.getNode.getGeoPosition.getY),
      Degrees(inputModel.getNode.getGeoPosition.getX),
      inputModel.getAlbedo,
      Each(
        inputModel.getEtaConv
          .to(PowerSystemUnits.PU)
          .getValue
          .doubleValue
      ),
      Radians(
        inputModel.getAzimuth
          .to(RADIAN)
          .getValue
          .doubleValue
      ),
      Radians(
        inputModel.getElevationAngle
          .to(RADIAN)
          .getValue
          .doubleValue
      ),
    )

}
