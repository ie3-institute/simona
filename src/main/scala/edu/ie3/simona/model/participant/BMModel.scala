/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.participant.BMModel.BMCalcRelevantData
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.quantities.interfaces.{DimensionlessRate, EnergyPrice}
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units._

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power, Temperature}
import scala.math._

/** This class represents a single biomass plant
  */
final case class BMModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    scalingFactor: Double,
    qControl: QControl,
    sRated: ComparableQuantity[Power],
    cosPhi: Double,
    private val node: String,
    private val isCostControlled: Boolean,
    private val opex: ComparableQuantity[EnergyPrice],
    private val feedInTariff: ComparableQuantity[EnergyPrice],
    private val loadGradient: ComparableQuantity[DimensionlessRate]
) extends SystemParticipant[BMCalcRelevantData](
      uuid,
      id,
      operationInterval,
      scalingFactor,
      qControl,
      sRated,
      cosPhi
    ) {

  /** Saves power output of last cycle. Needed for load gradient
    */
  private var _lastPower: Option[ComparableQuantity[Power]] = None

  override def calculatePower(
      tick: Long,
      voltage: ComparableQuantity[Dimensionless],
      data: BMCalcRelevantData
  ): ApparentPower = {
    val result = super.calculatePower(tick, voltage, data)
    _lastPower = Some(result.p)

    result
  }

  /** Calculate the active power behaviour of the model
    *
    * @param data
    *   Further needed, secondary data
    * @return
    *   Active power
    */
  override protected def calculateActivePower(
      data: BMCalcRelevantData
  ): ComparableQuantity[Power] = {
    // Calculate heat demand //
    val (k1, k2) = (calculateK1(data.date), calculateK2(data.date))
    val pTh = calculatePTh(data.temperature, k1, k2)

    // Usage: heat-demand in relation to max. heat-demand
    val usage = calculateUsage(pTh)

    // Efficiency (dependent on usage)
    val eff = calculateEff(usage)

    // Electrical output //
    val pEl = calculateElOutput(usage, eff)

    // Application of load gradient, return power output
    applyLoadGradient(pEl)
  }

  /** Calculates first time dependent factor for heat demand
    * @param time
    *   the time
    * @return
    *   factor k1
    */
  private def calculateK1(time: ZonedDateTime): Double = {
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

  /** Calculates second time dependent factor for heat demand
    * @param time
    *   the time
    * @return
    *   factor k2
    */
  private def calculateK2(time: ZonedDateTime): Double = {
    time.getDayOfYear match {
      case x if x < 150 || x > 243 =>
        1.03 // correction factor in heating season
      case _ => 0.61 // correction factor outside heating season
    }
  }

  /** Calculates heat demand
    * @param temp
    *   the temperature
    * @param k1
    *   factor k1
    * @param k2
    *   factor k2
    * @return
    *   heat demand in Megawatt
    */
  private def calculatePTh(
      temp: ComparableQuantity[Temperature],
      k1: Double,
      k2: Double
  ): ComparableQuantity[Power] = {
    // linear regression: Heat-demand in relation to temperature (above 19.28°C: independent of temperature)
    val pTh = temp.getValue.doubleValue match {
      case x if x < 19.28 => (-1.076 * x + 26.36) * k1 * k2
      case _              => 5.62 * k1 * k2
    }

    Quantities.getQuantity(pTh, MEGAWATT)
  }

  /** Calculates usage from heat demand, using fixed maximum heat
    * @param pTh
    *   heat demand
    * @return
    *   usage
    */
  private def calculateUsage(pTh: ComparableQuantity[Power]): Double = {
    // if demand exceeds capacity -> activate peak load boiler (no effect on electrical output)
    val maxHeat = Quantities.getQuantity(43.14, MEGAWATT)
    val usageUnchecked = pTh.divide(maxHeat).getValue.doubleValue

    if (usageUnchecked < 1) usageUnchecked else 1
  }

  /** Calculates efficiency from usage. Efficiency is based on a regression
    * which might lead to values > 1 -> valid cap (see docs for details)
    * @param usage
    *   the usage
    * @return
    *   efficiency
    */
  private def calculateEff(usage: Double): Double =
    min(0.18 * pow(usage, 3) - 0.595 * pow(usage, 2) + 0.692 * usage + 0.724, 1)

  /** Calculates electrical output from usage and efficiency
    * @param usage
    *   the usage
    * @param eff
    *   the efficiency
    * @return
    *   electrical output as Power
    */
  private def calculateElOutput(
      usage: Double,
      eff: Double
  ): ComparableQuantity[Power] = {
    val currOpex = opex.divide(eff)
    val avgOpex = currOpex.add(opex).divide(2)

    if (isCostControlled && avgOpex.isLessThan(feedInTariff))
      sRated.multiply(cosPhi).multiply(-1)
    else
      sRated.multiply(usage * eff * cosPhi).multiply(-1)
  }

  /** Applies the load gradient to the electrical output
    * @param pEl
    *   electrical output
    * @return
    *   electrical output after load gradient has been applied
    */
  private def applyLoadGradient(
      pEl: ComparableQuantity[Power]
  ): ComparableQuantity[Power] = {
    _lastPower match {
      case None => pEl
      case Some(lastPowerVal) =>
        val pElDeltaMaxAbs = sRated
          .multiply(loadGradient)
          .multiply(Quantities.getQuantity(1d, HOUR))
          .multiply(cosPhi)
          .asType(classOf[Power])

        pEl.subtract(lastPowerVal) match {
          case pElDelta if pElDelta.isGreaterThan(pElDeltaMaxAbs) =>
            lastPowerVal.add(pElDeltaMaxAbs)
          case pElDelta if pElDelta.isLessThan(pElDeltaMaxAbs.multiply(-1)) =>
            lastPowerVal.subtract(pElDeltaMaxAbs)
          case _ =>
            pEl
        }
    }
  }
}

case object BMModel {

  /** Data, that is needed for model calculations with the biomass model
    *
    * @param date
    *   Date of the tick
    * @param temperature
    *   The current temperature
    */
  final case class BMCalcRelevantData(
      date: ZonedDateTime,
      temperature: ComparableQuantity[Temperature]
  ) extends CalcRelevantData
}
