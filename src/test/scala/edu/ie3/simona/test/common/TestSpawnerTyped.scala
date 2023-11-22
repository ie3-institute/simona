/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import akka.actor.testkit.typed.scaladsl.ActorTestKitBase
import akka.actor.typed.{ActorRef, Behavior}
import edu.ie3.simona.scheduler.ScheduleLock.Spawner

trait TestSpawnerTyped {
  this: ActorTestKitBase =>

  object TSpawner extends Spawner {
    override def spawn[T](behavior: Behavior[T]): ActorRef[T] =
      TestSpawnerTyped.this.spawn(behavior)
  }
}
