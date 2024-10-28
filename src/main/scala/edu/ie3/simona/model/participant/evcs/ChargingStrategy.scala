/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.evcs

/** Enumeration that represents all implemented charging strategies
  */
object ChargingStrategy extends Enumeration {
  val MAX_POWER, CONSTANT_POWER, GRID_ORIENTED, MARKET_ORIENTED = Value

  def apply(token: String): ChargingStrategy.Value =
    "[-_]".r.replaceAllIn(token.trim.toLowerCase, "") match {
      case "maxpower"                 => MAX_POWER
      case "constantpower"            => CONSTANT_POWER
      case "gridorientedscheduling"   => GRID_ORIENTED
      case "marketorientedscheduling" => MARKET_ORIENTED
      case malformed =>
        throw new RuntimeException(
          s"The token '$malformed' cannot be parsed to charging strategy."
        )
    }
}
