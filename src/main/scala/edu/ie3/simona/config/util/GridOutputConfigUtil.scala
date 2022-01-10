/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config.util

import edu.ie3.datamodel.models.result.{NodeResult, ResultEntity}
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  SwitchResult,
  Transformer2WResult,
  Transformer3WResult
}
import edu.ie3.simona.config.SimonaConfig.GridOutputConfig

import scala.collection.mutable

final case class GridOutputConfigUtil(subConfig: GridOutputConfig) {

  /** Determine the set of result entity classes for later consideration based
    * on the grid output configuration
    *
    * @return
    *   Set of result entity classes
    */
  def simulationResultEntitiesToConsider: Set[Class[_ <: ResultEntity]] = {
    val entities = mutable.Set.empty[Class[_ <: ResultEntity]]

    if (subConfig.nodes)
      entities += classOf[NodeResult]
    if (subConfig.lines)
      entities += classOf[LineResult]
    if (subConfig.switches)
      entities += classOf[SwitchResult]
    if (subConfig.transformers2w)
      entities += classOf[Transformer2WResult]
    if (subConfig.transformers3w)
      entities += classOf[Transformer3WResult]

    entities.toSet
  }
}
