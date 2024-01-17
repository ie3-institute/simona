package edu.ie3.simona.service.primary

import java.util.UUID

object ExtTimeSeriesSubscribersSource {
  private var subscribers: Iterable[UUID] = Iterable.empty[UUID]

  def getSubscribers(): Iterable[UUID] = subscribers
}
