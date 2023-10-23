/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.PvModel.PvRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities._
import squants._
import squants.energy.{Kilowatts, Megawatts}
import squants.space.SquareMeters
import squants.time.Minutes
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import java.util.stream.IntStream
import scala.math._

final case class PvModel private (
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
    private val lat: Double,
    private val lon: Double,
    private val albedo: Double,
    private val etaConv: Dimensionless,
    private val alphaE: Angle,
    private val gammaE: Angle,
    private val moduleSurface: Area = SquareMeters(1d)
) extends SystemParticipant[PvRelevantData, ApparentPower](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated
    )
    with ApparentPowerParticipant[PvRelevantData] {

  /** Override sMax as the power output of a pv unit could become easily up to
    * 10% higher than the sRated value found in the technical sheets
    */
  override protected val sMax: Power = sRated * 1.1

  /** Permissible maximum active power feed in (therefore negative) */
  protected val pMax: Power = sMax * cosPhiRated * -1d

  /** Reference yield at standard testing conditions (STC) */
  private val yieldSTC = WattsPerSquareMeter(1000d)

  private val activationThreshold = Megawatts(
    sRated.toMegawatts * cosPhiRated * 0.001 * -1d
  )

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      data: PvRelevantData
  ): Power = {
    // === Pv Panel Base Data  === //
    val latInRad = Radians(lat.toRadians) // latitude of location
    val lonInRad = Radians(lon.toRadians) // longitude of location

    // === Weather Base Data  === //
    /* The pv model calculates the power in-feed based on the solar irradiance that is received over a specific
     * time frame (which actually is the solar irradiation). Hence, a multiplication with the time frame within
     * this irradiance is received is required. */

    val duration: Time = Seconds(data.weatherDataFrameLength)

    // eBeamH and eDifH needs to be extract to their double values in some places
    // hence a conversion to watt-hour per square meter is required, to avoid
    // invalid double value extraction!
    val eBeamH =
      data.dirIrradiance * duration
    val eDifH =
      data.diffIrradiance * duration

    // === Beam Radiation Parameters  === //
    val angleJ = calcAngleJ(data.dateTime)
    val delta = calcSunDeclinationDelta(angleJ)

    val omega = calcHourAngleOmega(data.dateTime, angleJ, lonInRad)

    val omegaSS = calcSunsetAngleOmegaSS(latInRad, delta)
    val omegaSR = calcSunriseAngleOmegaSR(omegaSS)

    val alphaS = calcSolarAltitudeAngleAlphaS(omega, delta, latInRad)
    val thetaG =
      calcAngleOfIncidenceThetaG(delta, latInRad, gammaE, alphaE, omega)

    val omegas = calculateBeamOmegas(thetaG, omega, omegaSS, omegaSR)

    // === Beam Radiation ===//
    val eBeamS = calcBeamRadiationOnSlopedSurface(
      eBeamH,
      omegas,
      delta,
      latInRad,
      gammaE,
      alphaE
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
      gammaE
    )

    // === Reflected Radiation ===//
    val eRefS =
      calcReflectedRadiationOnSlopedSurface(eBeamH, eDifH, gammaE, albedo)

    // === Total Radiation ===//
    val eTotal = eDifS + eBeamS + eRefS

    val irraditionSTC = yieldSTC * duration
    calcOutput(
      eTotal,
      data.dateTime,
      irraditionSTC
    ) * scalingFactor
  }

  /** Calculates the position of the earth in relation to the sun (day angle)
    * for the provided time
    *
    * @param time
    *   the time
    * @return
    *   day angle J in radians
    */
  private def calcAngleJ(time: ZonedDateTime): Angle = {
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
    *   day angle in radians
    * @return
    *   declination angle in radians
    */
  private def calcSunDeclinationDelta(
      angleJ: Angle
  ): Angle = {
    val jInRad = angleJ.toRadians
    Radians(
      0.006918 - 0.399912 * cos(jInRad) + 0.070257 * sin(
        jInRad
      ) - 0.006758 * cos(
        2d * jInRad
      ) + 0.000907 * sin(2d * jInRad) - 0.002697 * cos(
        3d * jInRad
      ) + 0.00148 * sin(
        3d * jInRad
      )
    )
  }

  /** Calculates the hour angle omega which represents the angular displacement
    * of the sun east or west of the local meridian due to rotation of the earth
    * on its axis at 15◦ per hour; morning negative, afternoon positive.
    *
    * @param time
    *   the requested time (which is transformed to solar time)
    * @param angleJ
    *   day angle in radians
    * @param longitudeInRad
    *   longitude of the position in radians
    * @return
    *   hour angle omega in radians
    */
  private def calcHourAngleOmega(
      time: ZonedDateTime,
      angleJ: Angle,
      longitudeInRad: Angle
  ): Angle = {
    val jInRad = angleJ.toRadians
    val lambda = longitudeInRad.toDegrees
    val et = Minutes(
      0.0066 + 7.3525 * cos(jInRad + 1.4992378274631293) + 9.9359 * cos(
        2d * jInRad + 1.9006635554218247
      ) + 0.3387 * cos(3d * jInRad + 1.8360863730980346)
    )

    val lmt = Minutes(time.getHour * 60d + time.getMinute - 4d * (15d - lambda))
    val st = lmt + et
    val stValue = st.toHours

    Radians((stValue - 12).toRadians * 15d)
  }

  /** Calculates the sunset hour angle omegaSS which represents the omega value
    * when the sun sets. The sunrise hour angle omegaSR is the negative of
    * omegaSS.
    *
    * @param latitudeInRad
    *   latitude of the position in radians
    * @param delta
    *   sun declination angle in radians
    * @return
    *   sunset angle omegaSS in radians
    */
  private def calcSunsetAngleOmegaSS(
      latitudeInRad: Angle,
      delta: Angle
  ): Angle = {
    val latInRad = latitudeInRad.toRadians
    val deltaValue = delta.toRadians

    Radians(acos(-tan(latInRad) * tan(deltaValue)))
  }

  /** Calculates the sunrise hour angle omegaSR given omegaSS.
    */
  private val calcSunriseAngleOmegaSR =
    (omegaSS: Angle) => omegaSS * (-1)

  /** Calculates the solar altitude angle alphaS which represents the angle
    * between the horizontal and the line to the sun, that is, the complement of
    * the zenith angle.
    *
    * @param omega
    *   hour angle in radians
    * @param delta
    *   sun declination angle in radians
    * @param latitudeInRad
    *   latitude of the position in radians
    * @return
    *   solar altitude angle alphaS in radians
    */
  private def calcSolarAltitudeAngleAlphaS(
      omega: Angle,
      delta: Angle,
      latitudeInRad: Angle
  ): Angle = {
    val latInRad = latitudeInRad.toRadians
    val deltaValue = delta.toRadians
    val omegaValue = omega.toRadians
    val sinAlphaS =
      min(
        max(
          cos(omegaValue) * cos(latInRad) * cos(deltaValue) + sin(
            latInRad
          ) * sin(
            deltaValue
          ),
          -1
        ),
        1
      )
    Radians(asin(sinAlphaS))
  }

  /** Calculates the zenith angle thetaG which represents the angle between the
    * vertical and the line to the sun, that is, the angle of incidence of beam
    * radiation on a horizontal surface.
    *
    * @param alphaS
    *   sun altitude angle in radians
    * @return
    *   the zenith angle in radians
    */
  private def calcZenithAngleThetaZ(
      alphaS: Angle
  ): Angle = {
    val alphaSValue = alphaS.toRadians

    // the zenith angle is defined as 90° - gammaS in Radian
    Radians(Pi / 2 - abs(alphaSValue))
  }

  /** Calculates the ratio of the mass of atmosphere through which beam
    * radiation passes to the mass it would pass through if the sun were at the
    * zenith (i.e., directly overhead).
    *
    * @param thetaZ
    *   zenith angle in radians
    * @return
    *   air mass
    */
  private def calcAirMass(thetaZ: Angle): Double = {
    val thetaZValue = thetaZ.toRadians

    // radius of the earth in kilometers
    val re = 6371d
    // approx. effective height of the atmosphere
    val yAtm = 9d

    // Ratio re / yAtm between the earth radius and the atmosphere height
    val airMassRatio = re / yAtm
    sqrt(
      pow(airMassRatio * cos(thetaZValue), 2d) + 2d * airMassRatio + 1d
    ) - airMassRatio * cos(
      thetaZValue
    )
  }

  /** Calculates the extraterrestrial radiation, that is, the radiation that
    * would be received in the absence of the atmosphere.
    *
    * @param angleJ
    *   day angle in radians
    * @return
    *   extraterrestrial radiation I0
    */
  private def calcExtraterrestrialRadiationI0(
      angleJ: Angle
  ): Irradiation = {
    val jInRad = angleJ.toRadians

    // eccentricity correction factor
    val e0 = 1.000110 + 0.034221 * cos(jInRad) + 0.001280 * sin(
      jInRad
    ) + 0.000719 * cos(
      2d * jInRad
    ) + 0.000077 * sin(2d * jInRad)

    // solar constant in W/m2
    val Gsc = WattHoursPerSquareMeter(1367) // solar constant
    Gsc * e0
  }

  /** Calculates the angle of incidence thetaG of beam radiation on a surface
    *
    * @param delta
    *   sun declination angle in radians
    * @param latitudeInRad
    *   latitude of the position in radians
    * @param gammaE
    *   slope angle (the angle between the plane of the surface in question and
    *   the horizontal) in radians
    * @param alphaE
    *   surface azimuth angle (the deviation of the projection on a horizontal
    *   plane of the normal to the surface from the local meridian, with zero
    *   due south, east negative, and west positive) in radians
    * @param omega
    *   hour angle in radians
    * @return
    *   angle of incidence thetaG in radians
    */
  private def calcAngleOfIncidenceThetaG(
      delta: Angle,
      latitudeInRad: Angle,
      gammaE: Angle,
      alphaE: Angle,
      omega: Angle
  ): Angle = {
    val deltaValue = delta.toRadians
    val omegaValue = omega.toRadians
    val gammaEValue = gammaE.toRadians
    val alphaEValue = alphaE.toRadians
    val latInRad = latitudeInRad.toRadians

    Radians(
      acos(
        sin(deltaValue) * sin(latInRad) * cos(gammaEValue) - sin(
          deltaValue
        ) * cos(
          latInRad
        ) * sin(gammaEValue) * cos(alphaEValue) + cos(deltaValue) * cos(
          latInRad
        ) * cos(gammaEValue) * cos(omegaValue) + cos(deltaValue) * sin(
          latInRad
        ) * sin(
          gammaEValue
        ) * cos(alphaEValue) * cos(omegaValue) + cos(deltaValue) * sin(
          gammaEValue
        ) * sin(alphaEValue) * sin(omegaValue)
      )
    )
  }

  /** Calculates omega1 and omega2, which are parameters for
    * calcBeamRadiationOnSlopedSurface
    *
    * @param thetaG
    *   angle of incidence in radians
    * @param omega
    *   hour angle in radians
    * @param omegaSS
    *   sunset angle in radians
    * @param omegaSR
    *   sunrise angle in radians
    * @return
    *   omega1 and omega encapsulated in an Option, if applicable. None
    *   otherwise
    */
  private def calculateBeamOmegas(
      thetaG: Angle,
      omega: Angle,
      omegaSS: Angle,
      omegaSR: Angle
  ): Option[(Angle, Angle)] = {
    val thetaGValue = thetaG.toRadians
    val omegaSSValue = omegaSS.toRadians
    val omegaSRValue = omegaSR.toRadians

    val omegaOneHour = toRadians(15d)
    val omegaHalfHour = omegaOneHour / 2d

    var omega1Value = omega.toRadians // requested hour
    var omega2Value = omega1Value + omegaOneHour // requested hour plus 1 hour

    // (thetaG < 90°): sun is visible
    // (thetaG > 90°), otherwise: sun is behind the surface  -> no direct radiation
    if (
      thetaGValue < toRadians(90)
      // omega1 and omega2: sun has risen and has not set yet
      && omega2Value > omegaSRValue + omegaHalfHour
      && omega1Value < omegaSSValue - omegaHalfHour
    ) {

      if (omega1Value < omegaSRValue) {
        // requested time earlier than sunrise?
        omega1Value = omegaSRValue
        omega2Value = omegaSRValue + omegaOneHour
      }

      if (omega2Value > omegaSSValue) {
        // sunset earlier than requested time?
        omega1Value = omegaSSValue - omegaOneHour
        omega2Value = omegaSSValue
      }

      Option(Radians(omega1Value), Radians(omega2Value))
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
    *   sun declination angle in radians
    * @param latitudeInRad
    *   latitude of the position in radians
    * @param gammaE
    *   slope angle (the angle between the plane of the surface in question and
    *   the horizontal) in radians
    * @param alphaE
    *   surface azimuth angle (the deviation of the projection on a horizontal
    *   plane of the normal to the surface from the local meridian, with zero
    *   due south, east negative, and west positive) in radians
    * @return
    *   the beam radiation on the sloped surface
    */
  private def calcBeamRadiationOnSlopedSurface(
      eBeamH: Irradiation,
      omegas: Option[(Angle, Angle)],
      delta: Angle,
      latitudeInRad: Angle,
      gammaE: Angle,
      alphaE: Angle
  ): Irradiation = {

    omegas match {
      case Some((omega1, omega2)) =>
        val deltaValue = delta.toRadians
        val gammaEValue = gammaE.toRadians
        val alphaEValue = alphaE.toRadians
        val latInRad = latitudeInRad.toRadians

        val omega1Value = omega1.toRadians
        val omega2Value = omega2.toRadians

        val a = ((sin(deltaValue) * sin(latInRad) * cos(gammaEValue)
          - sin(deltaValue) * cos(latInRad) * sin(gammaEValue) * cos(
            alphaEValue
          ))
          * (omega2Value - omega1Value)
          + (cos(deltaValue) * cos(latInRad) * cos(gammaEValue)
            + cos(deltaValue) * sin(latInRad) * sin(gammaEValue) * cos(
              alphaEValue
            ))
          * (sin(omega2Value) - sin(omega1Value))
          - (cos(deltaValue) * sin(gammaEValue) * sin(alphaEValue))
          * (cos(omega2Value) - cos(omega1Value)))

        val b = ((cos(latInRad) * cos(deltaValue)) * (sin(omega2Value) - sin(
          omega1Value
        ))
          + (sin(latInRad) * sin(deltaValue)) * (omega2Value - omega1Value))

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
    *   the horizontal) in radians
    * @return
    *   the diffuse radiation on the sloped surface
    */
  private def calcDiffuseRadiationOnSlopedSurfacePerez(
      eDifH: Irradiation,
      eBeamH: Irradiation,
      airMass: Double,
      extraterrestrialRadiationI0: Irradiation,
      thetaZ: Angle,
      thetaG: Angle,
      gammaE: Angle
  ): Irradiation = {
    val thetaZValue = thetaZ.toRadians
    val thetaGValue = thetaG.toRadians
    val gammaEValue = gammaE.toRadians

    // == brightness index beta  ==//
    val beta = eDifH * airMass / extraterrestrialRadiationI0

    // == cloud index epsilon  ==//
    // if we have no clouds,  the epsilon bin is 8, as epsilon bin for an epsilon in [6.2, inf.[ = 8
    var x = 8

    if (eDifH.value.doubleValue > 0) {
      // if we have diffuse radiation on horizontal surface we have to check if we have another epsilon due to clouds get the epsilon
      var epsilon = ((eDifH + eBeamH) / eDifH +
        (5.535d * 1.0e-6) * pow(
          thetaZValue,
          3
        )) / (1d + (5.535d * 1.0e-6) * pow(
        thetaZValue,
        3
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
          Array(4.500, 6.200)
        )
        // adapt the epsilon as we have no bin < 1
        epsilon = max(1, epsilon)

        // get the corresponding bin
        val finalEpsilon = epsilon

        x = IntStream
          .range(0, discreteSkyClearnessCategories.length)
          .filter((i: Int) =>
            (finalEpsilon - discreteSkyClearnessCategories(i)(
              0
            ) >= 0) && (finalEpsilon - discreteSkyClearnessCategories(
              i
            )(1) < 0)
          )
          .findFirst
          .getAsInt + 1
      }
    }

    // calculate the f_ij components based on the epsilon bin
    val f11 = -0.0161 * pow(x, 3) + 0.1840 * pow(x, 2) - 0.3806 * x + 0.2324
    val f12 = 0.0134 * pow(x, 4) - 0.1938 * pow(x, 3) + 0.8410 * pow(
      x,
      2
    ) - 1.4018 * x + 1.3579
    val f13 = 0.0032 * pow(x, 3) - 0.0280 * pow(x, 2) - 0.0056 * x - 0.0385
    val f21 = -0.0048 * pow(x, 3) + 0.0536 * pow(x, 2) - 0.1049 * x + 0.0034
    val f22 = 0.0012 * pow(x, 3) - 0.0067 * pow(x, 2) + 0.0091 * x - 0.0269
    val f23 = 0.0052 * pow(x, 3) - 0.0971 * pow(x, 2) + 0.2856 * x - 0.1389

    // calculate circuumsolar brightness coefficient f1 and horizon brightness coefficient f2
    val f1 = max(0, f11 + f12 * beta + f13 * thetaZValue)
    val f2 = f21 + f22 * beta + f23 * thetaZValue
    val aPerez = max(0, cos(thetaGValue))
    val bPerez = max(cos(1.4835298641951802), cos(thetaZValue))

    // finally calculate the diffuse radiation on an inclined surface
    eDifH * (
      ((1 + cos(
        gammaEValue
      )) / 2) * (1 - f1) + (f1 * (aPerez / bPerez)) + (f2 * sin(
        gammaEValue
      ))
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
    *   the horizontal) in radians
    * @param albedo
    *   albedo / "composite" ground reflection
    * @return
    *   the reflected radiation on the sloped surface eRefS
    */
  private def calcReflectedRadiationOnSlopedSurface(
      eBeamH: Irradiation,
      eDifH: Irradiation,
      gammaE: Angle,
      albedo: Double
  ): Irradiation = {
    val gammaEValue = gammaE.toRadians
    (eBeamH + eDifH) * (albedo * 0.5 * (1 - cos(gammaEValue)))
  }

  /** gammaE in radians
    */
  private def generatorCorrectionFactor(
      time: ZonedDateTime,
      gammaE: Angle
  ): Double = {
    val gammaEValInDe = gammaE.toDegrees

    val genCorr = new Array[Array[Double]](4)
    genCorr(0) = Array(0.69, 0.73, 0.81, 0.83, 0.84, 0.84, 0.9, 0.84, 0.84,
      0.82, 0.75, 0.66) // 30°
    genCorr(1) = Array(0.8, 0.83, 0.84, 0.85, 0.86, 0.86, 0.86, 0.86, 0.86,
      0.84, 0.82, 0.77) // 45°
    genCorr(2) = Array(0.84, 0.85, 0.86, 0.86, 0.85, 0.85, 0.85, 0.85, 0.86,
      0.86, 0.85, 0.84) // 60°
    genCorr(3) = Array(0.86, 0.86, 0.85, 0.84, 0.82, 0.81, 0.81, 0.82, 0.84,
      0.85, 0.86, 0.86) // 90°

    val genCorrKey: Int = gammaEValInDe match {
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
      irradiationSTC: Irradiation
  ): Power = {
    val genCorr = generatorCorrectionFactor(time, gammaE)
    val tempCorr = temperatureCorrectionFactor(time)
    /* The actual yield of this sum of available panels. As the solar irradiance summed up over the total panel surface
     * area. The yield also takes care of generator and temperature correction factors as well as the converter's
     * efficiency */
    val actYield =
      eTotalInWhPerSM * moduleSurface.toSquareMeters * etaConv.toEach * (genCorr * tempCorr)

    /* Calculate the foreseen active power output without boundary condition adaptions */
    val proposal = Megawatts(sRated.toMegawatts) * (-1) * (
      actYield / irradiationSTC
    ) * cosPhiRated

    /* Do sanity check, if the proposed feed in is above the estimated maximum to be apparent active power of the plant */
    if (proposal < pMax)
      logger.warn(
        "The fed in active power is higher than the estimated maximum active power of this plant ({} < {}). " +
          "Did you provide wrong weather input data?",
        proposal,
        pMax
      )

    /* If the output is marginally small, suppress the output, as we are likely to be in night and then only produce incorrect output */
    if (proposal.compareTo(activationThreshold) > 0)
      DefaultQuantities.zeroMW
    else proposal
  }
}

case object PvModel {

  /** Class that holds all relevant data for a pv model calculation
    *
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
  final case class PvRelevantData(
      dateTime: ZonedDateTime,
      weatherDataFrameLength: Long,
      diffIrradiance: Irradiance,
      dirIrradiance: Irradiance
  ) extends CalcRelevantData

  def apply(
      inputModel: PvInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): PvModel = {
    /* Determine the operation interval */
    val operationInterval: OperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        inputModel.getOperationTime
      )

    // moduleSurface and yieldSTC are left out for now
    val model = apply(
      inputModel.getUuid,
      inputModel.getId,
      operationInterval,
      scalingFactor,
      QControl(inputModel.getqCharacteristics),
      Kilowatts(
        inputModel.getsRated
          .to(PowerSystemUnits.KILOWATT)
          .getValue
          .doubleValue
      ),
      inputModel.getCosPhiRated,
      inputModel.getNode.getGeoPosition.getY,
      inputModel.getNode.getGeoPosition.getX,
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
      )
    )

    model.enable()

    model
  }

  /** Default factory method to create an PvModel instance. This constructor
    * ensures, that the angles passed in are converted to radian as required by
    * the model.
    *
    * @param uuid
    *   the unique id of the model
    * @param id
    *   the human readable id
    * @param operationInterval
    *   the operation interval of the model
    * @param scalingFactor
    *   the scaling factor of the power output
    * @param qControl
    *   the q control this model is using
    * @param sRated
    *   the rated apparent power of the model
    * @param cosPhiRated
    *   the rated cosine phi of the model
    * @param lat
    *   the latitude of the model
    * @param lon
    *   the longitude of the mode l
    * @param albedo
    *   the albedo of the model
    * @param etaConv
    *   the converter efficiency
    * @param alphaE
    *   the sun azimuth angle of the pv panel
    * @param gammaE
    *   the slope angle of the pv panel
    * @param moduleSurface
    *   the model surface size
    * @return
    */
  def apply(
      uuid: UUID,
      id: String,
      operationInterval: OperationInterval,
      scalingFactor: Double,
      qControl: QControl,
      sRated: Power,
      cosPhiRated: Double,
      lat: Double,
      lon: Double,
      albedo: Double,
      etaConv: Dimensionless,
      alphaE: Angle,
      gammaE: Angle,
      moduleSurface: Area = SquareMeters(1d)
  ): PvModel = {
    val model = new PvModel(
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhiRated,
      lat,
      lon,
      albedo,
      etaConv,
      alphaE,
      gammaE,
      moduleSurface
    )

    model.enable()

    model
  }

}
