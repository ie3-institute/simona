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
import edu.ie3.simona.model.participant2.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant2.PvModel.PvState
import edu.ie3.simona.model.participant2.SolarIrradiationCalculation._
import edu.ie3.simona.model.participant2.control.QControl
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroWPerSM
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  DimensionlessToSimona,
  PowerConversionSimona,
  RadiansConversionSimona,
}
import edu.ie3.util.scala.quantities._
import squants._
import squants.space.{Degrees, SquareMeters}

import java.time.ZonedDateTime
import java.util.UUID

class PvModel private (
    override val uuid: UUID,
    override val id: String,
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
    * 10% higher than the sRated value found in the technical sheets.
    */
  val sMax: ApparentPower = sRated * 1.1

  /** Permissible maximum active power feed in (therefore negative). */
  protected val pMax: Power = sMax.toActivePower(cosPhiRated) * -1

  /** Reference yield at standard testing conditions (STC). */
  private val yieldSTC = WattsPerSquareMeter(1000d)

  private val activationThreshold =
    sRated.toActivePower(cosPhiRated) * 0.001 * -1

  override def determineState(
      lastState: PvState,
      operatingPoint: ActivePowerOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): PvState =
    lastState.copy(tick = tick, dateTime = simulationTime)

  override def handleInput(
      state: PvState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): PvState =
    receivedData
      .collectFirst { case weatherData: WeatherData =>
        weatherData
      }
      .map(newData =>
        state.copy(
          diffIrradiance = newData.diffIrr,
          dirIrradiance = newData.dirIrr,
        )
      )
      .getOrElse(state)

  /** Calculate the active power behaviour of the model.
    *
    * @param state
    *   The current state including weather data.
    * @return
    *   The active power.
    */
  override def determineOperatingPoint(
      state: PvState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    // Irradiance on a horizontal surface
    val gBeamH = state.dirIrradiance
    val gDifH = state.diffIrradiance

    // === Beam irradiance parameters  === //
    val angleJ = calcAngleJ(state.dateTime)
    val delta = calcSunDeclinationDelta(angleJ)

    val omega = calcHourAngleOmega(state.dateTime, angleJ, lon)

    val omegaSS = calcSunsetAngleOmegaSS(lat, delta)
    val omegaSR = calcSunriseAngleOmegaSR(omegaSS)

    val alphaS = calcSolarAltitudeAngleAlphaS(omega, delta, lat)
    val thetaG =
      calcAngleOfIncidenceThetaG(delta, lat, gammaE, alphaE, omega)

    val omegas = calculateBeamOmegas(thetaG, omega, omegaSS, omegaSR)

    // === Beam irradiance ===//
    val gBeamS = calcBeamIrradianceOnSlopedSurface(
      gBeamH,
      omegas,
      delta,
      lat,
      gammaE,
      alphaE,
    )

    // === Diffuse irradiance parameters ===//
    val thetaZ = calcZenithAngleThetaZ(alphaS)
    val airMass = calcAirMass(thetaZ)
    val g0 = calcExtraterrestrialRadianceG0(angleJ)

    // === Diffuse irradiance ===//
    val gDifS = calcDiffuseIrradianceOnSlopedSurfacePerez(
      gDifH,
      gBeamH,
      airMass,
      g0,
      thetaZ,
      thetaG,
      gammaE,
    )

    // === Reflected irradiance ===//
    val gRefS =
      calcReflectedIrradianceOnSlopedSurface(gBeamH, gDifH, gammaE, albedo)

    // === Total irradiance ===//
    val gTotal = gDifS + gBeamS + gRefS

    val power = calcOutput(
      gTotal,
      state.dateTime,
      yieldSTC,
    )

    (ActivePowerOperatingPoint(power), None)
  }

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  private def calcOutput(
      gTotal: Irradiance,
      time: ZonedDateTime,
      irradianceSTC: Irradiance,
  ): Power = {
    val genCorr = generatorCorrectionFactor(time, gammaE)
    val tempCorr = temperatureCorrectionFactor(time)
    /* The actual yield of this sum of available panels. As the solar irradiance summed up over the total panel surface
     * area. The yield also takes care of generator and temperature correction factors as well as the converter's
     * efficiency */
    val actYield =
      gTotal * moduleSurface.toSquareMeters * etaConv.toEach * (genCorr * tempCorr)

    /* Calculate the foreseen active power output without boundary condition adaptions */
    val proposal =
      sRated.toActivePower(cosPhiRated) * -1 * (actYield / irradianceSTC)

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

}

object PvModel {

  /** Holds all relevant data for a pv model calculation.
    *
    * @param tick
    *   The current tick.
    * @param dateTime
    *   The date and time of the <b>ending</b> of time frame to calculate.
    * @param diffIrradiance
    *   The diffuse solar irradiance on a horizontal surface.
    * @param dirIrradiance
    *   The direct solar irradiance on a horizontal surface.
    */
  final case class PvState(
      override val tick: Long,
      dateTime: ZonedDateTime,
      diffIrradiance: Irradiance,
      dirIrradiance: Irradiance,
  ) extends ModelState

  final case class Factory(
      input: PvInput
  ) extends ParticipantModelFactory[PvState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.WeatherService)

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): PvState =
      PvState(
        tick,
        simulationTime,
        zeroWPerSM,
        zeroWPerSM,
      )

    override def create(): PvModel =
      new PvModel(
        input.getUuid,
        input.getId,
        input.getsRated.toApparent,
        input.getCosPhiRated,
        QControl(input.getqCharacteristics),
        Degrees(input.getNode.getGeoPosition.getY),
        Degrees(input.getNode.getGeoPosition.getX),
        input.getAlbedo,
        input.getEtaConv.toSquants,
        input.getAzimuth.toSquants,
        input.getElevationAngle.toSquants,
      )

  }

}
