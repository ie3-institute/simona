/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

trait ThermalThreshold {
  val tick: Long
}
case class SimpleThermalThreshold(tick: Long) extends ThermalThreshold
