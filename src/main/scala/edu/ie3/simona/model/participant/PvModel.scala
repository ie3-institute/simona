/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.result.system.{
  PvResult,
  SystemParticipantResult,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  OperationChangeIndicator,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.PvModel.{PvState, RadiationData}
import edu.ie3.simona.model.participant.SolarIrradiationCalculation.*
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.flex.PowerSeriesMathFlexModel
import edu.ie3.simona.ontology.messages.flex.FlexType
import edu.ie3.simona.service.Data.PrimaryData.{
  ComplexPower,
  PrimaryDataWithComplexPower,
}
import edu.ie3.simona.service.Data.SecondaryData.{
  WeatherData,
  WeatherSeriesData,
}
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.quantities.QuantityUtils.{asMegaVar, asMegaWatt}
import edu.ie3.util.scala.quantities.*
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  toApparent,
  toSquants,
}
import squants.*
import squants.space.{Degrees, SquareMeters}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.SortedMap

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

  override val flexModels: Map[FlexType, ParticipantFlexModel[PvState]] =
    Map(
      FlexType.PowerLimit -> ParticipantInflexiblePowerLimitFlexModel(this),
      FlexType.MathProgramming -> PowerSeriesMathFlexModel(
        this,
        _.toStateSeries,
      ),
    )

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
      .collectFirst {
        case weatherData: WeatherData =>
          SortedMap(state.tick -> RadiationData(weatherData))
        case WeatherSeriesData(series) =>
          series.map { case (tick, weatherData) =>
            tick -> RadiationData(weatherData)
          }
      }
      .map(newData => state.copy(radiationData = newData))
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
    val (_, radiationData) = state.radiationData
      .maxBefore(state.tick + 1)
      .getOrElse(
        throw new CriticalFailureException(
          s"No radiation data available for current tick ${state.tick}"
        )
      )

    // Irradiance on a horizontal surface
    val gBeamH = radiationData.dirIrradiance
    val gDifH = radiationData.diffIrradiance

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

  override def determineOperatingPoint(
      state: PvState,
      setPower: Power,
  ): (ActivePowerOperatingPoint, OperationChangeIndicator) =
    (ActivePowerOperatingPoint(setPower), OperationChangeIndicator())

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
    if proposal < pMax then
      logger.warn(
        "The fed in active power is higher than the estimated maximum active power of this plant ({} < {}). " +
          "Did you provide wrong weather input data?",
        proposal,
        pMax,
      )

    /* If the output is marginally small, suppress the output, as we are likely to be in night and then only produce incorrect output */
    if proposal.compareTo(activationThreshold) > 0 then DefaultQuantities.zeroMW
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
      data: PrimaryDataWithComplexPower[?],
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
    * @param radiationData
    *   A map of tick to radiation data. todo
    */
  final case class PvState(
      override val tick: Long,
      dateTime: ZonedDateTime,
      radiationData: SortedMap[Long, RadiationData] = SortedMap.empty,
  ) extends ModelState {

    def toStateSeries: SortedMap[Long, PvState] = {
      radiationData.map { case (dataTick, _) =>
        val tickDiff = dataTick - tick
        val tickState = PvState(
          dataTick,
          dateTime.plusSeconds(tickDiff),
          radiationData,
        )

        dataTick -> tickState
      }
    }
  }

  /** Radiation data for a specific point in simulation time.
    *
    * @param diffIrradiance
    *   The diffuse solar irradiance on a horizontal surface.
    * @param dirIrradiance
    *   The direct solar irradiance on a horizontal surface.
    */
  final case class RadiationData(
      diffIrradiance: Irradiance,
      dirIrradiance: Irradiance,
  )

  object RadiationData {
    def apply(weatherData: WeatherData): RadiationData =
      RadiationData(
        diffIrradiance = weatherData.diffIrr,
        dirIrradiance = weatherData.dirIrr,
      )
  }

  final case class Factory(
      input: PvInput
  ) extends ParticipantModelFactory[PvState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable(ServiceType.WeatherService)

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): PvState =
      PvState(tick, simulationTime)

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
