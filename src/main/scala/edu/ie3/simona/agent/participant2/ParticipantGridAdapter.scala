/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.simona.agent.em.FlexCorrespondenceStore.WithTime
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant2.ParticipantGridAdapter.averageApparentPower
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroMVAr, zeroMW}
import edu.ie3.util.scala.quantities.{Megavars, QuantityUtil, ReactivePower}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.event.LoggingAdapter
import org.slf4j.Logger
import squants.energy.Megawatts
import squants.{Dimensionless, Energy, Power}

import scala.collection.immutable.SortedMap
import scala.util.{Failure, Success}

/** Provides (average) power values to grid agent
  *
  * @param gridAgent
  * @param expectedRequestTick
  *   Tick at which next power request is expected
  * @param voltage
  * @param lastRequestTick
  *   Tick of the last request
  * @param tickToPower
  *   power values
  * @param currentAvgPower
  */
final case class ParticipantGridAdapter(
    gridAgent: ActorRef[GridAgent.Request],
    expectedRequestTick: Long,
    voltage: Dimensionless,
    lastRequestTick: Long = 0,
    tickToPower: SortedMap[Long, ApparentPower],
    currentAvgPower: WithTime[ApparentPower],
) {

  def isPowerRequestExpected(currentTick: Long): Boolean = {
    expectedRequestTick == currentTick
  }

  def storePowerValue(
      power: ApparentPower,
      tick: Long,
  ): ParticipantGridAdapter =
    copy(tickToPower = tickToPower + (tick, power))
  // power of the current tick is irrelevant

  def updateAveragePower(
      currentTick: Long,
      log: Logger,
  ): ParticipantGridAdapter = {
    val averagePower =
      averageApparentPower(tickToPower, lastRequestTick, currentTick, ???, log)

    // keep the last entry because we do not know
    // if the next entry will necessarily be at the
    // current tick
    val lastTickAndPower = tickToPower.maxByOption { case (tick, _) =>
      tick
    }

    copy(
      currentAvgPower = WithTime(averagePower, currentTick),
      tickToPower = SortedMap.from(lastTickAndPower),
      lastRequestTick = currentTick,
    )
  }

}

object ParticipantGridAdapter {

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
  def averageApparentPower(
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
