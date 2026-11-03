/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

/** Describes the geometric layout formation in which the cables of a line
  * segment are deployed.
  *
  * The formation determines the arrangement of the conductor axes relative to
  * each other and to the ground surface. It therefore drives the selection of
  * the corresponding external thermal resistance and sheath loss calculations.
  */
sealed trait LayoutFormation

object LayoutFormation {

  /** Three single-core cables laid side by side with an axial distance between
    * their axes.
    */
  case object FlatDistance extends LayoutFormation

  /** Three single-core cables laid side by side, touching each other.
    */
  case object FlatTouching extends LayoutFormation

  /** Three single-core cables arranged in a touching trefoil.
    */
  case object TrefoilTouching extends LayoutFormation

  /** Three single-core cables arranged in a non-touching trefoil.
    */
  case object TrefoilNotTouching extends LayoutFormation

  private val byName: Map[String, LayoutFormation] = Map(
    "flat-distance" -> FlatDistance,
    "flat-touching" -> FlatTouching,
    "trefoil-touching" -> TrefoilTouching,
    "trefoil-not-touching" -> TrefoilNotTouching,
  )

  /** Parses a layout formation from its string representation as used in the
    * power system data model.
    *
    * @param formation
    *   The string representation of the layout formation.
    * @return
    *   The matching [[LayoutFormation]].
    * @throws IllegalArgumentException
    *   if the string does not correspond to a known layout formation.
    */
  def fromString(formation: String): LayoutFormation =
    byName.getOrElse(
      formation.trim.toLowerCase,
      throw new IllegalArgumentException(
        s"Unknown layout formation: $formation"
      ),
    )
}
