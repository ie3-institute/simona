package edu.ie3.simona.util

import org.slf4j.{Logger, LoggerFactory}

final case class ReceiveHierarchicalDataMap[K, V](
                                   structure: Map[K, Set[K]],
                                   allKeys: Set[K],
                                   receivedData: Map[K, V]
                                                 ) {
  private val log: Logger = LoggerFactory.getLogger(ReceiveHierarchicalDataMap.getClass)


  def hasCompletedKeys: Boolean = structure.keySet.exists(isComplete)

  def isComplete(key: K): Boolean =
    structure.get(key).forall(_.forall(receivedData.contains))


  def updateStructure(
                     key: Option[K],
                     subKey: K
                     ): ReceiveHierarchicalDataMap[K, V] = {
    log.warn(s"Parent '$key' with sub '$subKey'.")

    val (updatedStructure, updatedKeys): (Map[K, Set[K]], Set[K]) = key match {
      case Some(parent) =>

        structure.get(parent) match {
              case Some(subKeys) =>
                val allSubKeys = subKeys + subKey

                (
                  structure ++ Map(parent -> allSubKeys),
                  allKeys + subKey
                )
              case None =>

                (
                  structure ++ Map(parent -> Set(subKey)),
                  allKeys ++ List(parent, subKey)
                )
            }

      case None =>
        (
          structure ++ Map(subKey -> Set.empty),
          allKeys + subKey
          )
    }

    copy(
      structure = updatedStructure,
      allKeys = updatedKeys
    )
  }


  def addData(
             key: K,
             value: V,
             ): ReceiveHierarchicalDataMap[K, V] = {

    if (!allKeys.contains(key))
      throw new RuntimeException(
        s"Received value $value for key $key, but no data has been expected for this key."
      )

    copy(receivedData = receivedData.updated(key, value))
  }


  def getFinishedData: (Map[K, V], ReceiveHierarchicalDataMap[K, V]) = {
   val dataMap = structure.keySet.filter(isComplete).flatMap(key => structure(key))
      .map(key => key -> receivedData(key))
      .toMap

    val updated = receivedData.removedAll(dataMap.keys)

    (dataMap, copy(receivedData = updated))
  }

}

object ReceiveHierarchicalDataMap {

  def empty[K, V]: ReceiveHierarchicalDataMap[K, V] = ReceiveHierarchicalDataMap(
    Map.empty,
    Set.empty,
    Map.empty
  )

}