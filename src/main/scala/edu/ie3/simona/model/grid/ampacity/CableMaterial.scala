/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity
sealed trait CableMaterial
object CableMaterial {
  case object Copper extends CableMaterial
  case object Aluminium extends CableMaterial
  case object XLPE extends CableMaterial
  case object PE extends CableMaterial
  case object PVC extends CableMaterial
  case object ScTape extends CableMaterial
  case object SemiCondScreen extends CableMaterial
  case object NoneCableMaterial extends CableMaterial

  def fromString(s: String): CableMaterial = s.toLowerCase match {
    case "copper" | "cooper" => Copper
    case "aluminium"         => Aluminium
    case "xlpe"              => XLPE
    case "pe"                => PE
    case "pvc"               => PVC
    case "cooperwoventape"   => ScTape
    case "semicondscreen"    => SemiCondScreen
    case "none"              => NoneCableMaterial
    case other =>
      throw new IllegalArgumentException(s"Unknown material: $other")
  }
}
