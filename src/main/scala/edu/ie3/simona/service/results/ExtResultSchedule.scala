/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.results

import edu.ie3.simona.service.results.ExtResultProvider.ResultResponseMessage

import java.util.UUID

final case class ExtResultSchedule(
    scheduleMap: Map[Long, Set[UUID]] = Map.empty,
    unscheduledList: Set[UUID] = Set.empty,
) {
  def shiftPastTicksToCurrentTick(
      currentTick: Long
  ): ExtResultSchedule = {
    // Sammle alle Sets von Keys, deren Schlüssel kleiner als currentTick
    val (toMerge, remaining) = scheduleMap.partition { case (tick, _) =>
      tick < currentTick
    }

    // Kombiniere die Sets zu einem einzigen Set
    val mergedSet = toMerge.values.flatten.toSet

    // Aktualisiere den scheduleMap mit dem neuen Set für currentTick
    val updatedScheduleMap = remaining.updated(
      currentTick,
      scheduleMap.getOrElse(currentTick, Set.empty) ++ mergedSet,
    )

    // Rückgabe eines neuen ExtResultSchedule mit dem aktualisierten scheduleMap
    copy(
      scheduleMap = updatedScheduleMap
    )
  }

  def getExpectedKeys(tick: Long): Set[UUID] = {
    scheduleMap.getOrElse(
      tick,
      Set(),
    ) ++ unscheduledList
  }

  private def getScheduledKeys(tick: Long): Set[UUID] = {
    scheduleMap.getOrElse(tick, Set[UUID]())
  }

  def handleActivation(tick: Long): ExtResultSchedule = {
    copy(
      scheduleMap = scheduleMap.-(tick)
    )
  }

  def handleActivationWithRequest(
      tick: Long,
      keys: Iterable[UUID],
  ): ExtResultSchedule = {
    val remainingKeys =
      scheduleMap.get(tick).map(_.diff(keys.toSet)).getOrElse(Set.empty)
    if (remainingKeys.isEmpty) {
      copy(
        scheduleMap = scheduleMap.-(tick)
      )
    } else {
      copy(
        scheduleMap = scheduleMap.updated(tick, remainingKeys)
      )
    }
  }

  def handleResult(
      msg: ResultResponseMessage,
      nextTick: Long,
  ): ExtResultSchedule = {
    copy(
      scheduleMap = scheduleMap.updated(
        nextTick,
        getScheduledKeys(nextTick) + msg.result.getInputModel,
      )
    )
  }
}
