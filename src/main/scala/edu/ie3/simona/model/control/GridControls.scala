/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.control

/** Collection of grid-related control strategies
  *
  * @param transformerControlGroups
  *   Transformer control groups
  */
final case class GridControls(
    transformerControlGroups: Set[TransformerControlGroupModel]
)

object GridControls {

  /** Represents an empty GridControls group
    */
  def empty: GridControls = GridControls(Set.empty)
}
