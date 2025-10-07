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
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) // (house, heatStorage, waterStorage)
}

private object SplitHouseWaterStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) = {
    val pThermalMaxWaterStorage = waterStorage
      .getOrElse(
        throw new InvalidParameterException(
          "Could not find heatStorage but expected one."
        )
      )
      .pThermalMax

    // Check if water storage can handle half of qDot
    if qDot / 2 > pThermalMaxWaterStorage
    then {
      val remainingQDot = qDot - pThermalMaxWaterStorage
      (remainingQDot, zeroKW, pThermalMaxWaterStorage)
    } else (qDot / 2, zeroKW, qDot / 2)
  }
}

private object HouseOnlyStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) =
    (qDot, zeroKW, zeroKW)
}

private object WaterStorageFirstStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) = {
    val pThermalMaxWaterStorage = waterStorage
      .getOrElse(
        throw new InvalidParameterException(
          "Could not find heatStorage but expected one."
        )
      )
      .pThermalMax

    // Check if water storage can handle qDot
    if qDot > pThermalMaxWaterStorage
    then {
      val remainingQDot = qDot - pThermalMaxWaterStorage
      (remainingQDot, zeroKW, pThermalMaxWaterStorage)
    } else (zeroKW, zeroKW, qDot)
  }
}

private object HeatStorageFirstStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) = {
    val pThermalMaxHeatStorage = heatStorage
      .getOrElse(
        throw new InvalidParameterException(
          "Could not find heatStorage but expected one."
        )
      )
      .pThermalMax

    // Check if heat storage can handle qDot
    if qDot > pThermalMaxHeatStorage
    then {
      val remainingQDot = qDot - pThermalMaxHeatStorage
      (remainingQDot, pThermalMaxHeatStorage, zeroKW)
    } else (zeroKW, qDot, zeroKW)
  }
}

private object NoOperationStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) =
    (zeroKW, zeroKW, zeroKW)
}
