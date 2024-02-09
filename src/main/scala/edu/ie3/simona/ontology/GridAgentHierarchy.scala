/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology

sealed trait GridAgentHierarchy

/** Describes the hierarchical structure of
  * [[edu.ie3.simona.agent.grid.GridAgent]] s in a simulation, whereas
  * [[edu.ie3.simona.ontology.GridAgentHierarchy.Centrum]] might be existing
  * multiple times e.g. when multiple medium voltage levels are present
  */
object GridAgentHierarchy {

  case object Superior extends GridAgentHierarchy

  case object Centrum extends GridAgentHierarchy

  case object Inferior extends GridAgentHierarchy

}
