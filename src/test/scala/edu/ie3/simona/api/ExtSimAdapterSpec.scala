/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.api.ExtSimAdapter.Stop
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.api.simulation.ontology.{
  ActivationMessage,
  TerminationCompleted,
  TerminationMessage,
  CompletionMessage => ExtCompletionMessage,
}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.test.common.{TestKitWithShutdown, TestSpawnerClassic}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorSystem, Terminated}
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.OptionConverters.RichOption

class ExtSimAdapterSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ExtSimAdapterSpec",
        ConfigFactory
          .parseString("""
            |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
            |pekko.loglevel = "INFO"
            |""".stripMargin),
      )
    )
    with AnyWordSpecLike
    with TestSpawnerClassic {

  private val scheduler = TestProbe("scheduler")
  private val mainArgs = Array.empty[String]

  "An uninitialized ExtSimScheduler" must {
    "send correct completion message after initialisation" in {
      val lock = TestProbe("lock")

      val extSimAdapter = TestActorRef(
        new ExtSimAdapter(scheduler.ref)
      )

      val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)

      val key1 = ScheduleKey(lock.ref.toTyped, UUID.randomUUID())
      scheduler.send(extSimAdapter, ExtSimAdapter.Create(extData, key1))
      scheduler.expectMsg(
        ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK, Some(key1))
      )
    }
  }

  "An initialized ExtSimScheduler" must {
    "forward an activation trigger and a corresponding completion message properly" in {
      val lock = TestProbe("lock")

      val extSimAdapter = TestActorRef(
        new ExtSimAdapter(scheduler.ref)
      )

      val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)

      val key1 = ScheduleKey(lock.ref.toTyped, UUID.randomUUID())
      scheduler.send(extSimAdapter, ExtSimAdapter.Create(extData, key1))
      scheduler.expectMsg(
        ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK, Some(key1))
      )

      scheduler.send(extSimAdapter, Activation(INIT_SIM_TICK))

      awaitCond(
        !extData.receiveMessageQueue.isEmpty,
        max = 3.seconds,
        message = "No message received",
      )
      extData.receiveMessageQueue.size() shouldBe 1
      extData.receiveMessageQueue.take() shouldBe new ActivationMessage(
        INIT_SIM_TICK
      )
      scheduler.expectNoMessage()

      // external simulation sends completion
      val nextTick = 900L
      extData.send(
        new ExtCompletionMessage(
          Option[java.lang.Long](nextTick).toJava
        )
      )

      scheduler.expectMsg(Completion(extSimAdapter.toTyped, Some(nextTick)))
    }

    "schedule the data service when it is told to" in {
      val lock = TestProbe("lock")

      val extSimAdapter = TestActorRef(
        new ExtSimAdapter(scheduler.ref)
      )

      val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)
      val dataService = TestProbe("dataService")

      val key1 = ScheduleKey(lock.ref.toTyped, UUID.randomUUID())
      scheduler.send(extSimAdapter, ExtSimAdapter.Create(extData, key1))
      scheduler.expectMsg(
        ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK, Some(key1))
      )

      scheduler.send(extSimAdapter, Activation(INIT_SIM_TICK))

      awaitCond(
        !extData.receiveMessageQueue.isEmpty,
        max = 3.seconds,
        message = "No message received",
      )
      extData.receiveMessageQueue.size() shouldBe 1
      extData.receiveMessageQueue.take()

      extSimAdapter ! new ScheduleDataServiceMessage(
        dataService.ref
      )
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      dataService
        .expectMsgType[ScheduleServiceActivation]
        .tick shouldBe INIT_SIM_TICK
      scheduler.expectNoMessage()
    }

    "terminate the external simulation and itself when told to" in {
      forAll(Table("simSuccessful", true, false)) { (simSuccessful: Boolean) =>
        val lock = TestProbe("lock")

        val extSimAdapter = TestActorRef(
          new ExtSimAdapter(scheduler.ref)
        )

        val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)

        val key1 = ScheduleKey(lock.ref.toTyped, UUID.randomUUID())
        scheduler.send(extSimAdapter, ExtSimAdapter.Create(extData, key1))
        scheduler.expectMsg(
          ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK, Some(key1))
        )

        val stopWatcher = TestProbe()
        stopWatcher.watch(extSimAdapter)

        extSimAdapter ! Stop(simSuccessful)

        awaitCond(
          !extData.receiveMessageQueue.isEmpty,
          max = 3.seconds,
          message = "No message received",
        )
        extData.receiveMessageQueue.size() shouldBe 1
        extData.receiveMessageQueue.take() shouldBe new TerminationMessage(
          simSuccessful
        )

        // up until now, extSimAdapter should still be running
        stopWatcher.expectNoMessage()

        extSimAdapter ! new TerminationCompleted()

        // extSimAdapter should have terminated now
        stopWatcher.expectMsgType[Terminated].actor shouldBe extSimAdapter

        // scheduler is not involved in this
        scheduler.expectNoMessage()
      }
    }
  }

}
