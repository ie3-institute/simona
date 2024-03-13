/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import java.util.UUID

object ExtTimeSeriesSubscribersSource {
  private val subscribers: Iterable[UUID] = Iterable(
    UUID.fromString("fd1a8de9-722a-4304-8799-e1e976d9979c"),
    UUID.fromString("ff0b995a-86ff-4f4d-987e-e475a64f2180")
  )

  def getSubscribers: Iterable[UUID] = subscribers
}
