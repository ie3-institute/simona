/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.util

final case class ReceiveHierarchicalDataMap[K, V](
    private val allKeys: Set[K],
    private val expectedKeys: Set[K],
    structure: Map[K, Set[K]],
    receivedData: Map[K, V],
) {

  def hasCompletedKeys: Boolean = structure.keySet.exists(isComplete)

  def isComplete: Boolean = expectedKeys.isEmpty

  private def isComplete(key: K): Boolean = structure
    .get(key)
    .map(_.intersect(expectedKeys))
    .forall(_.forall(receivedData.contains))

  def updateStructure(
      key: Option[K],
      subKey: K,
  ): ReceiveHierarchicalDataMap[K, V] = {
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

      case None if !structure.contains(subKey) =>
        (
          structure ++ Map(subKey -> Set.empty),
          allKeys + subKey,
        )
      case _ =>
        // we already added the subkey as parent
        // therefore, no changes are needed
        (structure, allKeys)
    }

    copy(
      structure = updatedStructure,
      allKeys = updatedKeys,
    )
  }

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

  def getExpectedKeys: Set[K] = expectedKeys

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

  def apply[K, V](expected: Set[K]): ReceiveHierarchicalDataMap[K, V] =
    ReceiveHierarchicalDataMap(
      Set.empty,
      expected,
      Map.empty,
      Map.empty,
    )

  def empty[K, V]: ReceiveHierarchicalDataMap[K, V] =
    ReceiveHierarchicalDataMap(
      Set.empty,
      Set.empty,
      Map.empty,
      Map.empty,
    )

}
