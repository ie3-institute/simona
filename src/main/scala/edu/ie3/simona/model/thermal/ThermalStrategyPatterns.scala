/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power

/** Trait to provide a feed-in strategy for handling thermal infeed (qDot).
  */
private sealed trait FeedInStrategy {
  def apply(
      qDot: Power
  ): (Power, Power, Power) // (house, heatStorage, waterStorage)
}

private object SplitHouseWaterStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power, Power) =
    (qDot / 2, zeroKW, qDot / 2)
}

private object HouseOnlyStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power, Power) =
    (qDot, zeroKW, zeroKW)
}

private object WaterStorageOnlyStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power, Power) =
    (zeroKW, zeroKW, qDot)
}

private object HeatStorageOnlyStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power, Power) =
    (zeroKW, qDot, zeroKW)
}

private object NoOperationStrategy extends FeedInStrategy {
  override def apply(qDot: Power): (Power, Power, Power) =
    (zeroKW, zeroKW, zeroKW)
}
