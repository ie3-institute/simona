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

  protected def getMaxPower(
      storage: Option[? <: { def pThermalMax: Power }],
      storageType: String,
  ): Power =
    storage
      .getOrElse(
        throw new InvalidParameterException(
          s"Could not find $storageType but expected one."
        )
      )
      .pThermalMax

  protected def distribute(qDot: Power, maxCapacity: Power): (Power, Power) =
    // Check if storage can handle qDot
    if qDot > maxCapacity then (qDot - maxCapacity, maxCapacity)
    else (zeroKW, qDot)
}

private object SplitHouseWaterStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) = {
    val maxWater = getMaxPower(waterStorage, "waterStorage")
    val halfQDot = qDot / 2

    // Check if water storage can handle half of qDot
    if halfQDot > maxWater then {
      val remaining = qDot - maxWater
      (remaining, zeroKW, maxWater)
    } else (halfQDot, zeroKW, halfQDot)
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
    val maxWater = getMaxPower(waterStorage, "waterStorage")
    val (remaining, toWater) = distribute(qDot, maxWater)
    (remaining, zeroKW, toWater)
  }
}

private object HeatStorageFirstStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
      waterStorage: Option[DomesticHotWaterStorage],
  ): (Power, Power, Power) = {
    val maxHeat = getMaxPower(heatStorage, "heatStorage")
    val (remaining, toHeat) = distribute(qDot, maxHeat)
    (remaining, toHeat, zeroKW)
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
