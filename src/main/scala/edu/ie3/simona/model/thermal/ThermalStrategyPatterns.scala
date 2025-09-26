/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power

private sealed trait FeedInStrategy {
  def apply(
      qDot: Power
  ): (Power, Power) // (house, heatStorage)
}

private object HouseOnlyStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power) =
    (qDot, zeroKW)
}

private object HeatStorageOnlyStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power) =
    (zeroKW, qDot)
}

private object NoOperationStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power) =
    (zeroKW, zeroKW)
}
