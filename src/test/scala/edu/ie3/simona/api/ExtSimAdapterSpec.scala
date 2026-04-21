/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.simona.api.ExtSimAdapter.{ExtSimAdapterStateData, Stop}
import edu.ie3.simona.api.ontology.{
  DataMessageFromExt,
  ScheduleDataServiceMessage,
}
import edu.ie3.simona.api.data.connection.ExtSimDataConnection
import edu.ie3.simona.api.ontology.simulation.{
  ActivationMessage,
  ControlResponseMessageFromExt,
  TerminationCompleted,
  TerminationMessage,
  CompletionMessage as ExtCompletionMessage,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.{OptionalLong, UUID}
import scala.jdk.OptionConverters.RichOption
import scala.language.{existentials, implicitConversions}

class ExtSimAdapterSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with TestSpawnerTyped {

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")

  "An uninitialized ExtSimScheduler" must {
    "send correct completion message after initialisation" in {
      val lock = TestProbe[ScheduleLock.Message]("lock")

      val extSimAdapter = testKit.spawn(ExtSimAdapter(scheduler.ref))
      val extData = new ExtSimDataConnection(extSimAdapter)

      val key1 = ScheduleKey(lock.ref, UUID.randomUUID())
      extSimAdapter ! ExtSimAdapter.Create(extData, key1)

      scheduler.expectMessage(
        ScheduleActivation(extSimAdapter, INIT_SIM_TICK, Some(key1))
      )
    }
  }

  "An initialized ExtSimScheduler" must {
    "forward an activation trigger and a corresponding completion message properly" in {
      val lock = TestProbe[ScheduleLock.Message]("lock")
      val key1 = ScheduleKey(lock.ref, UUID.randomUUID())

      val extSimAdapter = testKit.spawn(ExtSimAdapter(scheduler.ref))
      val extData = new ExtSimDataConnection(extSimAdapter)

      extSimAdapter ! ExtSimAdapter.Create(extData, key1)

      scheduler.expectMessage(
        ScheduleActivation(extSimAdapter, INIT_SIM_TICK, Some(key1))
      )

      extSimAdapter ! Activation(INIT_SIM_TICK)

      extData.receive() shouldBe new ActivationMessage(
        INIT_SIM_TICK
      )
      scheduler.expectNoMessage()

      // external simulation sends completion
      val nextTick = 900L
      extData.send(new ExtCompletionMessage(OptionalLong.of(nextTick)))

      scheduler.expectMessage(Completion(extSimAdapter, Some(nextTick)))
    }

    "schedule the data service when it is told to" in {
      val lock = TestProbe[ScheduleLock.Message]("lock")
      val key1 = ScheduleKey(lock.ref, UUID.randomUUID())

      val extSimAdapter = testKit.spawn(ExtSimAdapter(scheduler.ref))
      val extData = new ExtSimDataConnection(extSimAdapter)
      val dataService = TestProbe[DataMessageFromExt]("dataService")

      extSimAdapter ! ExtSimAdapter.Create(extData, key1)

      scheduler.expectMessage(
        ScheduleActivation(extSimAdapter, INIT_SIM_TICK, Some(key1))
      )

      extSimAdapter ! Activation(INIT_SIM_TICK)

      extData.receive()

      extSimAdapter ! new ScheduleDataServiceMessage(dataService.ref)

      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      dataService
        .expectMessageType[ScheduleServiceActivation]
        .tick shouldBe INIT_SIM_TICK
      scheduler.expectNoMessage()
    }

    "terminate the external simulation and itself when told to" in {
      forAll(Table("simSuccessful", true, false)) { (simSuccessful: Boolean) =>
        val probe = TestProbe[ControlResponseMessageFromExt]("probe")
        val extData = new ExtSimDataConnection(probe.ref)

        val extSimAdapter = BehaviorTestKit(
          ExtSimAdapter.receiveIdle(
            ExtSimAdapterStateData(
              extData,
              None,
            )
          )(using scheduler.ref)
        )

        extSimAdapter.isAlive shouldBe true

        extSimAdapter.run(Stop(simSuccessful))

        extData.receive() shouldBe new TerminationMessage(
          simSuccessful
        )

        // up until now, extSimAdapter should still be running
        extSimAdapter.run(new TerminationCompleted())

        // extSimAdapter should have terminated now
        extSimAdapter.isAlive shouldBe false

        // scheduler is not involved in this
        scheduler.expectNoMessage()
      }
    }
  }

}
