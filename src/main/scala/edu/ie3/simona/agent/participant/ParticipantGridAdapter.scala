/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.ParticipantGridAdapter.*
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.service.Data.PrimaryData.ComplexPower
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroMVAr, zeroMW}
import edu.ie3.util.scala.quantities.{Megavars, QuantityUtil, ReactivePower}
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.energy.Megawatts
import squants.{Dimensionless, Each, Energy, Power}

import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success}

/** Stores resulting power values from the participant model and provides
  * averaged values to the [[GridAgent]]. Also receives the resulting nodal
  * voltage, which might be used for model calculations.
  *
  * When the power value is requested by the [[GridAgent]], an average (weighted
  * by time) of the recent registered power values (from the time of the last
  * request on) is determined. This is done in consideration of the voltage
  * value calculated by the [[GridAgent]], which is valid for the past time
  * frame. For the new time frame starting with the current tick, the voltage is
  * preliminarily used, until the next communication with the grid establishes a
  * proper new voltage valid for the new time frame.
  *
  * @param gridAgent
  *   The actor reference to the [[GridAgent]].
  * @param expectedRequestTick
  *   The tick at which next power request is expected.
  * @param nodalVoltage
  *   The most recent nodal voltage in p.u.
  * @param tickToPower
  *   Map storing the power values from which averages can be derived.
  * @param avgPowerResult
  *   The calculated average power for the current request window.
  * @param requestVoltageDeviationTolerance
  *   The tolerance for differences in voltage when deciding whether to
  *   recalculate reactive power.
  */
final case class ParticipantGridAdapter(
    gridAgent: ActorRef[GridAgent.Request],
    nodalVoltage: Dimensionless,
    private val expectedRequestTick: Long,
    private val tickToPower: SortedMap[Long, ComplexPower],
    avgPowerResult: Option[AvgPowerResult],
)(private implicit val requestVoltageDeviationTolerance: Dimensionless) {

  /** Whether a power request is expected and has not yet arrived, thus is
    * awaited, for the given tick.
    *
    * @param currentTick
    *   The current tick.
    * @return
    *   Whether a power request is awaited for the given tick.
    */
  def isPowerRequestAwaited(currentTick: Long): Boolean = {
    expectedRequestTick == currentTick
  }

  /** Store a power value that has been determined by the model for the given
    * tick.
    *
    * @param power
    *   The power value determined by the model.
    * @param tick
    *   The current tick.
    * @return
    *   An adapted [[ParticipantGridAdapter]] that stores given value.
    */
  def storePowerValue(
      power: ComplexPower,
      tick: Long,
  ): ParticipantGridAdapter =
    copy(tickToPower = tickToPower.updated(tick, power))

  /** Handles a power request by making sure an average power value has been
    * calculated, taking into account the new voltage value.
    *
    * @param newVoltage
    *   The updated voltage valid for the recent time window ending at the
    *   current tick.
    * @param currentTick
    *   The current tick.
    * @param reactivePowerFuncOpt
    *   A model function that determines reactive power given nodal voltage and
    *   active power.
    * @param log
    *   A logger.
    * @return
    *   An adapted [[ParticipantGridAdapter]] that holds the determined average
    *   power.
    */
  def handlePowerRequest(
      newVoltage: Dimensionless,
      currentTick: Long,
      reactivePowerFuncOpt: Option[
        Dimensionless => Power => ReactivePower
      ],
      log: Logger,
  ): ParticipantGridAdapter = {
    if currentTick != expectedRequestTick then
      throw new CriticalFailureException(
        s"Power request expected for $expectedRequestTick, but not for current tick $currentTick"
      )

    val result = (avgPowerResult match {
      case Some(cache @ AvgPowerResult(windowStart, windowEnd, voltage, _, _))
          if windowEnd == currentTick =>
        // Results have been calculated for the same tick...
        if voltage =~ newVoltage then {
          // ... and same voltage, return cached result
          Left(cache)
        } else {
          // ... and different voltage, results have to be re-calculated with same params
          Right(windowStart, windowEnd)
        }
      case Some(AvgPowerResult(_, windowEnd, _, _, _)) =>
        // Results have been calculated for a former tick, take former windowEnd as the new windowStart
        Right(windowEnd, currentTick)
      case None =>
        // No results have been calculated whatsoever, calculate from simulation start (0)
        Right(0L, currentTick)
    }).fold(
      cachedResult => cachedResult.copy(newResult = false),
      { case (windowStart: Long, windowEnd: Long) =>
        val avgPower = averageApparentPower(
          tickToPower,
          windowStart,
          windowEnd,
          reactivePowerFuncOpt.map(_.apply(newVoltage)),
          log,
        )
        AvgPowerResult(
          windowStart,
          windowEnd,
          newVoltage,
          avgPower,
          newResult = true,
        )
      },
    )

    val reducedMap = reduceTickToPowerMap(tickToPower, result.windowStart)

    copy(
      nodalVoltage = newVoltage,
      tickToPower = reducedMap,
      avgPowerResult = Some(result),
    )
  }

  /** Updates this grid adapter with the given next request tick.
    *
    * @param nextRequestTick
    *   The next tick at which power is requested.
    * @return
    *   The updated grid adapter.
    */
  def updateNextRequestTick(nextRequestTick: Long): ParticipantGridAdapter =
    copy(expectedRequestTick = nextRequestTick)

}

object ParticipantGridAdapter {

  /** Result of an average power calculation.
    *
    * @param windowStart
    *   The tick at which the time frame starts.
    * @param windowEnd
    *   The tick at which the time frame ends.
    * @param voltage
    *   The voltage used for calculating the average power.
    * @param avgPower
    *   The calculated average power.
    * @param newResult
    *   Whether the calculated power has been calculated anew or reused from
    *   past calculation.
    */
  final case class AvgPowerResult(
      windowStart: Long,
      windowEnd: Long,
      voltage: Dimensionless,
      avgPower: ComplexPower,
      newResult: Boolean,
  )

  def apply(
      gridAgentRef: ActorRef[GridAgent.Request],
      expectedRequestTick: Long,
      requestVoltageDeviationTolerance: Dimensionless,
  ): ParticipantGridAdapter =
    new ParticipantGridAdapter(
      gridAgent = gridAgentRef,
      nodalVoltage = Each(1d),
      expectedRequestTick = expectedRequestTick,
      tickToPower = SortedMap.empty,
      avgPowerResult = None,
    )(using
      requestVoltageDeviationTolerance = requestVoltageDeviationTolerance
    )

  private def reduceTickToPowerMap(
      tickToPower: SortedMap[Long, ComplexPower],
      windowStart: Long,
  ): SortedMap[Long, ComplexPower] = {
    // keep the last entry at or before windowStart
    val lastTickBeforeWindowStart =
      tickToPower.rangeUntil(windowStart + 1).lastOption

    // remove all entries before or at windowStart
    val reducedMap = tickToPower.rangeFrom(windowStart + 1)

    // combine both
    lastTickBeforeWindowStart.map(reducedMap + _).getOrElse(reducedMap)
  }

  /** Determine the average apparent power within the given time frame.
    *
    * @param tickToPower
    *   Mapping from tick to power value.
    * @param windowStart
    *   First tick (inclusive) of the time frame.
    * @param windowEnd
    *   Last tick (exclusive) of the time frame.
    * @param activeToReactivePowerFuncOpt
    *   The optional function determining reactive power given an active power.
    * @return
    *   The averaged complex power.
    */
  private def averageApparentPower(
      tickToPower: Map[Long, ComplexPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        Power => ReactivePower
      ] = None,
      log: Logger,
  ): ComplexPower = {
    val p = QuantityUtil.average[Power, Energy](
      tickToPower.map { case (tick, pd) =>
        tick -> pd.p
      },
      windowStart,
      windowEnd,
    ) match {
      case Success(pSuccess) =>
        pSuccess
      case Failure(exception) =>
        log.warn(
          "Unable to determine average active power. Apply 0 instead.",
          exception,
        )
        zeroMW
    }

    val q = QuantityUtil.average[Power, Energy](
      tickToPower.map { case (tick, pd) =>
        activeToReactivePowerFuncOpt match {
          case Some(qFunc) =>
            // NOTE: The type conversion to Megawatts is done to satisfy the methods type constraints
            // and is undone after unpacking the results
            tick -> Megawatts(qFunc(pd.p).toMegavars)
          case None => tick -> Megawatts(pd.q.toMegavars)
        }
      },
      windowStart,
      windowEnd,
    ) match {
      case Success(pSuccess) =>
        Megavars(pSuccess.toMegawatts)
      case Failure(exception) =>
        log.warn(
          "Unable to determine average reactive power. Apply 0 instead.",
          exception,
        )
        zeroMVAr
    }

    ComplexPower(p, q)
  }
}
