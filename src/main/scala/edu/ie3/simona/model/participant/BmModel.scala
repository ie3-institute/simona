/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.BmInput
import edu.ie3.datamodel.models.result.system.{
  BmResult,
  SystemParticipantResult,
}
import edu.ie3.simona.model.participant.BmModel._
import edu.ie3.simona.model.participant.ParticipantFlexibility.ParticipantSimpleFlexibility
import edu.ie3.simona.model.participant.ParticipantModel.{
  ActivePowerOperatingPoint,
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.Data.PrimaryData
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.simona.service.{Data, ServiceType}
import edu.ie3.util.quantities.QuantityUtils.{asMegaWatt, asMegaVar}
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroCelsius, zeroKW}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  EnergyPriceToSimona,
  PowerConversionSimona,
}
import edu.ie3.util.scala.quantities.{ApparentPower, EnergyPrice}
import squants.energy.Megawatts
import squants.thermal.Temperature
import squants.{Dimensionless, Power}

import java.time.ZonedDateTime
import java.util.UUID
import scala.math._

final case class BmModel(
    override val uuid: UUID,
    override val id: String,
    override val sRated: ApparentPower,
    override val cosPhiRated: Double,
    override val qControl: QControl,
    private val isCostControlled: Boolean,
    private val opex: EnergyPrice,
    private val feedInTariff: EnergyPrice,
    private val loadGradient: Double,
) extends ParticipantModel[
      ActivePowerOperatingPoint,
      BmState,
    ]
    with ParticipantSimpleFlexibility[BmState] {

  override def determineState(
      lastState: BmState,
      operatingPoint: ActivePowerOperatingPoint,
      tick: Long,
      simulationTime: ZonedDateTime,
  ): BmState =
    lastState.copy(
      tick = tick,
      dateTime = simulationTime,
      lastPEl = Some(operatingPoint.activePower),
    )

  override def handleInput(
      state: BmState,
      receivedData: Seq[Data],
      nodalVoltage: Dimensionless,
  ): BmState =
    receivedData
      .collectFirst { case weatherData: WeatherData =>
        weatherData
      }
      .map(newData => state.copy(temperature = newData.temp))
      .getOrElse(state)

  override def determineOperatingPoint(
      state: BmState
  ): (ActivePowerOperatingPoint, Option[Long]) = {
    // Calculate heat demand //
    val (k1, k2) = (calculateK1(state.dateTime), calculateK2(state.dateTime))
    val pTh = calculatePTh(state.temperature, k1, k2)

    // Usage: heat-demand in relation to max. heat-demand
    val usage = calculateUsage(pTh)

    // Efficiency (dependent on usage)
    val eff = calculateEff(usage)

    // Electrical output //
    val pEl = calculateElOutput(usage, eff)

    // Application of load gradient
    val outputPower = applyLoadGradient(state.lastPEl, pEl)

    (ActivePowerOperatingPoint(outputPower), None)
  }

  /** Calculates electrical output from usage and efficiency.
    *
    * @param usage
    *   The usage.
    * @param eff
    *   The efficiency.
    * @return
    *   The electrical output power.
    */
  def calculateElOutput(
      usage: Double,
      eff: Double,
  ): Power = {
    val currOpex = opex / eff
    val avgOpex = (currOpex + opex) / 2

    if (isCostControlled && avgOpex < feedInTariff)
      pRated * -1
    else
      pRated * usage * eff * -1
  }

  /** Applies the load gradient to the electrical output.
    *
    * @param pEl
    *   The electrical output.
    * @return
    *   The electrical output after load gradient has been applied.
    */
  def applyLoadGradient(
      lastPEl: Option[Power],
      pEl: Power,
  ): Power = {
    lastPEl match {
      case None => pEl
      case Some(lastPowerVal) =>
        val pElDeltaMaxAbs = pRated * loadGradient

        pEl - lastPowerVal match {
          case pElDelta if pElDelta > pElDeltaMaxAbs =>
            lastPowerVal + pElDeltaMaxAbs
          case pElDelta if pElDelta < (pElDeltaMaxAbs * -1) =>
            lastPowerVal - pElDeltaMaxAbs
          case _ =>
            pEl
        }
    }
  }

  override def zeroPowerOperatingPoint: ActivePowerOperatingPoint =
    ActivePowerOperatingPoint.zero

  override def createResults(
      state: BmState,
      lastOperatingPoint: Option[ActivePowerOperatingPoint],
      currentOperatingPoint: ActivePowerOperatingPoint,
      complexPower: ComplexPower,
      dateTime: ZonedDateTime,
  ): Iterable[SystemParticipantResult] =
    Iterable(
      new BmResult(
        dateTime,
        uuid,
        complexPower.p.toMegawatts.asMegaWatt,
        complexPower.q.toMegavars.asMegaVar,
      )
    )

  override def createPrimaryDataResult(
      data: PrimaryData.PrimaryDataWithComplexPower[_],
      dateTime: ZonedDateTime,
  ): SystemParticipantResult =
    new BmResult(
      dateTime,
      uuid,
      data.p.toMegawatts.asMegaWatt,
      data.q.toMegavars.asMegaVar,
    )

}

object BmModel {

  final case class Factory(
      input: BmInput
  ) extends ParticipantModelFactory[BmState] {

    override def getRequiredSecondaryServices: Iterable[ServiceType] =
      Iterable.empty

    override def getInitialState(
        tick: Long,
        simulationTime: ZonedDateTime,
    ): BmState = BmState(tick, simulationTime, zeroCelsius, Option(zeroKW))

    override def create(): BmModel = {
      val bmType = input.getType
      val loadGradient = bmType.getActivePowerGradient
        .to(StandardUnits.ACTIVE_POWER_GRADIENT)
        .getValue
        .doubleValue()

      new BmModel(
        input.getUuid,
        input.getId,
        bmType.getsRated.toApparent,
        bmType.getCosPhiRated,
        QControl(input.getqCharacteristics()),
        input.isCostControlled,
        bmType.getOpex.toSquants,
        input.getFeedInTariff.toSquants,
        loadGradient,
      )
    }
  }

  /** Data, that is needed for model calculations with the biomass model.
    *
    * @param tick
    *   The current tick.
    * @param dateTime
    *   The current date and time.
    * @param temperature
    *   The current temperature.
    * @param lastPEl
    *   The power output of last cycle. Needed for load gradient.
    */
  final case class BmState(
      override val tick: Long,
      dateTime: ZonedDateTime,
      temperature: Temperature,
      lastPEl: Option[Power],
  ) extends ModelState

  /** Calculates first time-dependent factor for heat demand.
    *
    * @param time
    *   The date and time.
    * @return
    *   The factor k1.
    */
  def calculateK1(time: ZonedDateTime): Double = {
    val weekendCorr = Vector(0.98, 0.985, 0.982, 0.982, 0.97, 0.96, 0.95, 0.93,
      0.925, 0.95, 0.98, 1.01, 1.018, 1.01, 1.01, 0.995, 1, 0.995, 0.99, 0.985,
      0.99, 0.98, 0.975, 0.99)

    time.getDayOfWeek.getValue match {
      case x if x > 5 =>
        weekendCorr(
          time.getHour
        ) // correction factor for each hour of Saturday and Sunday
      case _ => 1
    }
  }

  /** Calculates second time-dependent factor for heat demand.
    *
    * @param time
    *   The date and time.
    * @return
    *   The factor k2
    */
  def calculateK2(time: ZonedDateTime): Double = {
    time.getDayOfYear match {
      case x if x < 150 || x > 243 =>
        1.03 // correction factor in heating season
      case _ => 0.61 // correction factor outside heating season
    }
  }

  /** Calculates heat demand.
    *
    * @param temp
    *   The temperature.
    * @param k1
    *   The factor k1.
    * @param k2
    *   The factor k2.
    * @return
    *   The heat demand.
    */
  def calculatePTh(
      temp: Temperature,
      k1: Double,
      k2: Double,
  ): Power = {
    // linear regression: Heat-demand in relation to temperature (above 19.28°C: independent of temperature)
    val pTh = temp.toCelsiusScale match {
      case x if x < 19.28 => (-1.076 * x + 26.36) * k1 * k2
      case _              => 5.62 * k1 * k2
    }

    Megawatts(pTh)
  }

  /** Calculates usage from heat demand, using fixed maximum heat.
    *
    * @param pTh
    *   The heat demand.
    * @return
    *   The usage.
    */
  def calculateUsage(pTh: Power): Double = {
    // if demand exceeds capacity -> activate peak load boiler (no effect on electrical output)
    val maxHeat = Megawatts(43.14)
    val usageUnchecked = pTh / maxHeat

    if (usageUnchecked < 1) usageUnchecked else 1
  }

  /** Calculates efficiency from usage. Efficiency is based on a regression
    * which might lead to values > 1 -> valid cap (see docs for details).
    *
    * @param usage
    *   The usage.
    * @return
    *   The efficiency.
    */
  def calculateEff(usage: Double): Double =
    min(0.18 * pow(usage, 3) - 0.595 * pow(usage, 2) + 0.692 * usage + 0.724, 1)

}
