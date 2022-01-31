/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.market

import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.quantities.interfaces.EnergyPrice
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime

trait MarketSource {
  def price(time: ZonedDateTime): ComparableQuantity[EnergyPrice]

  def prices(
      interval: ClosedInterval[ZonedDateTime]
  ): Map[ZonedDateTime, ComparableQuantity[EnergyPrice]]
}
