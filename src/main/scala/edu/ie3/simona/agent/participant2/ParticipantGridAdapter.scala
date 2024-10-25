/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant2.ParticipantGridAdapter._
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroMVAr, zeroMW}
import edu.ie3.util.scala.quantities.{Megavars, QuantityUtil, ReactivePower}
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.energy.Megawatts
import squants.{Dimensionless, Each, Energy, Power}

import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success}

/** Provides (average) power values to grid agent
  *
  * @param gridAgent
  * @param expectedRequestTick
  *   Tick at which next power request is expected
  * @param nodalVoltage
  * @param tickToPower
  *   power values
  * @param avgPowerResult
  */
final case class ParticipantGridAdapter(
    gridAgent: ActorRef[GridAgent.Request],
    nodalVoltage: Dimensionless,
    expectedRequestTick: Long,
    tickToPower: SortedMap[Long, ApparentPower],
    avgPowerResult: Option[AvgPowerResult],
) {

  def isPowerRequestExpected(currentTick: Long): Boolean = {
    expectedRequestTick == currentTick
  }

  def storePowerValue(
      power: ApparentPower,
      tick: Long,
  ): ParticipantGridAdapter =
    copy(tickToPower = tickToPower.updated(tick, power))

  def handlePowerRequest(
      newVoltage: Dimensionless,
      currentTick: Long,
      activeToReactivePowerFuncOpt: Option[
        Dimensionless => Power => ReactivePower
      ],
      log: Logger,
  ): ParticipantGridAdapter = {
    if (currentTick != expectedRequestTick)
      throw new CriticalFailureException(
        s"Power request expected for $expectedRequestTick, but not for current tick $currentTick"
      )

    implicit val voltageTolerance: Dimensionless = Each(
      1e-3
    ) // todo requestVoltageDeviationThreshold

    val result = (avgPowerResult match {
      case Some(cache @ AvgPowerResult(windowStart, windowEnd, voltage, _, _))
          if windowEnd == currentTick =>
        // Results have been calculated for the same tick...
        if (voltage =~ newVoltage) {
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
        Right(0, currentTick)
    }).fold(
      cachedResult => cachedResult.copy(newResult = false),
      { case (windowStart: Long, windowEnd: Long) =>
        val avgPower = averageApparentPower(
          tickToPower,
          windowStart,
          windowEnd,
          activeToReactivePowerFuncOpt.map(_.apply(newVoltage)),
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

  def updateNextRequestTick(nextRequestTick: Long): ParticipantGridAdapter =
    copy(expectedRequestTick = nextRequestTick)

}

object ParticipantGridAdapter {

  final case class AvgPowerResult(
      windowStart: Long,
      windowEnd: Long,
      voltage: Dimensionless,
      avgPower: ApparentPower,
      newResult: Boolean,
  )

  def apply(
      gridAgentRef: ActorRef[GridAgent.Request],
      expectedRequestTick: Long,
  ): ParticipantGridAdapter =
    new ParticipantGridAdapter(
      gridAgent = gridAgentRef,
      nodalVoltage = Each(1d),
      expectedRequestTick = expectedRequestTick,
      tickToPower = SortedMap.empty,
      avgPowerResult = None,
    )

  private def reduceTickToPowerMap(
      tickToPower: SortedMap[Long, ApparentPower],
      windowStart: Long,
  ): SortedMap[Long, ApparentPower] = {
    // keep the last entry at or before windowStart
    val lastTickBeforeWindowStart =
      tickToPower.rangeUntil(windowStart + 1).lastOption

    // throw out all entries before or at windowStart
    val reducedMap = tickToPower.rangeFrom(windowStart + 1)

    // combine both
    lastTickBeforeWindowStart.map(reducedMap + _).getOrElse(reducedMap)
  }

  /** Determine the average apparent power within the given tick window
    *
    * @param tickToPower
    *   Mapping from data tick to actual data
    * @param windowStart
    *   First, included tick of the time window
    * @param windowEnd
    *   Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    *   An Option on a function, that transfers the active into reactive power
    * @return
    *   The averaged apparent power
    */
  private def averageApparentPower(
      tickToPower: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        Power => ReactivePower
      ] = None,
      log: Logger,
  ): ApparentPower = {
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
            tick -> Megawatts(qFunc(pd.toApparentPower.p).toMegavars)
          case None => tick -> Megawatts(pd.toApparentPower.q.toMegavars)
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

    ApparentPower(p, q)
  }
}
