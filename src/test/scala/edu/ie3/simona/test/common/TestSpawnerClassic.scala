/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.scheduler.ScheduleLock.Spawner
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorSystemOps
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.apache.pekko.testkit.TestKit

trait TestSpawnerClassic {
  this: TestKit =>

  object TSpawner extends Spawner {
    override def spawn[T](behavior: Behavior[T]): ActorRef[T] =
      system.spawnAnonymous(behavior)
  }
}
