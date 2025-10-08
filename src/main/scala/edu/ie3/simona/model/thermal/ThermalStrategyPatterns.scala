/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import squants.Power
import scala.reflect.Selectable.reflectiveSelectable

import java.security.InvalidParameterException

/** Trait to provide a feed-in strategy for handling thermal infeed (qDot).
  */
private sealed trait FeedInStrategy {
  def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
  ): (Power, Power) // (house, heatStorage)

  /** Get maximum thermal power of a storage.
    *
    * @param storage
    *   The storage to determine.
    * @return
    */
  protected def getMaxPower(
      storage: Option[? <: { def pThermalMax: Power }]
  ): Power =
    storage
      .getOrElse(
        throw new InvalidParameterException(
          s"Could not find $storage but expected one."
        )
      )
      .pThermalMax

  /** Distributes the thermal infeed qDot in case the maximum capacity exceeds
    * the infeed.
    *
    * @param qDot
    *   The thermal infeed.
    * @param maxCapacity
    *   The maximum capacity of the thermal sink.
    * @return
    *   Distributed qDot, zero and qDot in case the thermal sink can handle the
    *   infeed, else remaining qDot and the maximum capacity of the thermal
    *   sink.
    */
  protected def distribute(qDot: Power, maxCapacity: Power): (Power, Power) =
    // Check if storage can handle qDot
    if qDot > maxCapacity then (qDot - maxCapacity, maxCapacity)
    else (zeroKW, qDot)
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
    val maxHeat = getMaxPower(heatStorage)
    val (remaining, toHeat) = distribute(qDot, maxHeat)
    (remaining, toHeat)
  }
}

private object NoOperationStrategy extends FeedInStrategy {
  override def apply(
      qDot: Power,
      heatStorage: Option[CylindricalThermalStorage],
  ): (Power, Power) =
    (zeroKW, zeroKW)
}
