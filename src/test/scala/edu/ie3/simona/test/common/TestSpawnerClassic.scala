/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import akka.actor.typed.scaladsl.adapter.ClassicActorSystemOps
import akka.actor.typed.{ActorRef, Behavior}
import akka.testkit.TestKit
import edu.ie3.simona.scheduler.ScheduleLock.Spawner

trait TestSpawnerClassic {
  this: TestKit =>

  object TSpawner extends Spawner {
    override def spawn[T](behavior: Behavior[T]): ActorRef[T] =
      system.spawnAnonymous(behavior)
  }
}
