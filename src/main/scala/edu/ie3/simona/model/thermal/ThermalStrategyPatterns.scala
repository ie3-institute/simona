/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power

import java.security.InvalidParameterException

/** Trait to provide a feed-in strategy for handling thermal infeed (qDot).
  */
private sealed trait FeedInStrategy {
  def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
  ): (Power, Power) // (house, heatStorage)
}

private object HouseOnlyStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
  ): (Power, Power) =
    (qDot, zeroKW)
}

private object HeatStorageFirstStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
  ): (Power, Power) = {
    val pThermalMaxHeatStorage = heatStorage
      .getOrElse(
        throw new InvalidParameterException(
          "Could not find heatStorage but expected one."
        )
      )
      .pThermalMax
    if qDot > pThermalMaxHeatStorage
    then {
      val remainingQDot = qDot - pThermalMaxHeatStorage
      (remainingQDot, pThermalMaxHeatStorage)
    } else (zeroKW, qDot)
  }
}

private object NoOperationStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
  ): (Power, Power) =
    (zeroKW, zeroKW)
}
