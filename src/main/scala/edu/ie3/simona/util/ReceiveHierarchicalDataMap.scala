/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

import org.slf4j.{Logger, LoggerFactory}

final case class ReceiveHierarchicalDataMap[K, V](
    withExpected: Boolean,
    structure: Map[K, Set[K]],
    allKeys: Set[K],
    expectedKeys: Set[K],
    receivedData: Map[K, V],
) {
  private val log: Logger =
    LoggerFactory.getLogger(ReceiveHierarchicalDataMap.getClass)

  def allCompleted: Boolean = allKeys.forall(isComplete)

  def hasCompletedKeys: Boolean = structure.keySet.exists(isComplete)

  def isComplete(key: K): Boolean = structure
      .get(key)
      .map(_.intersect(expectedKeys))
      .forall(_.forall(receivedData.contains))

  def updateStructure(
      key: Option[K],
      subKey: K,
  ): ReceiveHierarchicalDataMap[K, V] = {
    log.warn(s"Added agent '$subKey' with parent '$key'.")

    val (updatedStructure, updatedKeys): (Map[K, Set[K]], Set[K]) = key match {
      case Some(parent) =>
        structure.get(parent) match {
          case Some(subKeys) =>
            val allSubKeys = subKeys + subKey

            (
              structure ++ Map(parent -> allSubKeys),
              allKeys + subKey,
            )
          case None =>
            (
              structure ++ Map(parent -> Set(subKey)),
              allKeys ++ List(parent, subKey),
            )
        }

      case None =>
        (
          structure ++ Map(subKey -> Set.empty),
          allKeys + subKey,
        )
    }

    copy(
      structure = updatedStructure,
      allKeys = updatedKeys,
    )
  }

  def addExpectedKey(key: K): ReceiveHierarchicalDataMap[K, V] =
    copy(expectedKeys = expectedKeys + key)

  def addExpectedKeys(keys: Set[K]): ReceiveHierarchicalDataMap[K, V] =
    copy(expectedKeys = expectedKeys ++ keys)

  def addSubKeysToExpectedKeys(keys: Set[K]): ReceiveHierarchicalDataMap[K, V] =
    copy(expectedKeys = expectedKeys ++ keys.flatMap(structure.get).flatten)

  def addData(
      key: K,
      value: V,
  ): ReceiveHierarchicalDataMap[K, V] = {

    if (!allKeys.contains(key))
      throw new RuntimeException(
        s"Received value $value for key $key, but no data has been expected for this key."
      )

    copy(
      expectedKeys = expectedKeys.excl(key),
      receivedData = receivedData.updated(key, value),
    )
  }

  def getFinishedData: (Map[K, V], ReceiveHierarchicalDataMap[K, V]) = {
    val dataMap = if (expectedKeys.nonEmpty) {
      structure.keySet
        .filter(isComplete)
        .flatMap(key => structure(key))
        .map(key => key -> receivedData(key))
        .toMap
    } else receivedData

    val updated = receivedData.removedAll(dataMap.keys)

    (dataMap, copy(receivedData = updated))
  }

  def getFinishedDataHierarchical
      : (Map[K, Set[K]], Map[K, V], ReceiveHierarchicalDataMap[K, V]) = {
    val (dataMap, updated) = getFinishedData

    val structureMap = structure.keySet
      .filter(isComplete)
      .map(parent => parent -> structure(parent))
      .toMap

    (structureMap, dataMap, updated)
  }

}

object ReceiveHierarchicalDataMap {

  def empty[K, V]: ReceiveHierarchicalDataMap[K, V] =
    ReceiveHierarchicalDataMap(
      withExpected = true,
      Map.empty,
      Set.empty,
      Set.empty,
      Map.empty,
    )

  def empty[K, V](
      withExpected: Boolean = true
  ): ReceiveHierarchicalDataMap[K, V] = ReceiveHierarchicalDataMap(
    withExpected,
    Map.empty,
    Set.empty,
    Set.empty,
    Map.empty,
  )

}
