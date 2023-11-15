/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import akka.actor.typed.scaladsl.adapter.ClassicActorRefOps
import akka.actor.{ActorSystem, Terminated}
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.api.simulation.ontology.{
  Terminate,
  TerminationCompleted,
  ActivityStartTrigger => ExtActivityStartTrigger,
  CompletionMessage => ExtCompletionMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessageTyped.{
  Completion,
  ScheduleActivation
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, StopMessage}
import edu.ie3.simona.test.common.TestKitWithShutdown
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters.SeqHasAsJava

class ExtSimAdapterSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ExtSimAdapterSpec",
        ConfigFactory
          .parseString("""
                     |akka.loggers = ["akka.testkit.TestEventListener"]
                     |akka.loglevel = "INFO"
                     |""".stripMargin)
      )
    )
    with AnyWordSpecLike {

  private val scheduler = TestProbe("scheduler")
  private val mainArgs = Array.empty[String]

  "An uninitialized ExtSimScheduler" must {
    "send correct completion message after initialisation" in {
      val extSimAdapter = TestActorRef(
        new ExtSimAdapter(scheduler.ref)
      )

      val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)

      scheduler.send(extSimAdapter, ExtSimAdapter.Init(extData))
      scheduler.expectMsg(
        ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK)
      )
    }
  }

  "An initialized ExtSimScheduler" must {
    "forward an activation trigger and a corresponding completion message properly" in {
      val extSimAdapter = TestActorRef(
        new ExtSimAdapter(scheduler.ref)
      )

      val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)

      scheduler.send(extSimAdapter, ExtSimAdapter.Init(extData))
      scheduler.expectMsg(
        ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK)
      )

      scheduler.send(extSimAdapter, Activation(INIT_SIM_TICK))

      awaitCond(
        !extData.receiveMessageQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveMessageQueue.size() shouldBe 1
      extData.receiveMessageQueue.take() shouldBe new ExtActivityStartTrigger(
        INIT_SIM_TICK
      )
      scheduler.expectNoMessage()

      // external simulation sends completion
      val nextTick = 900L
      extData.send(
        new ExtCompletionMessage(
          List[java.lang.Long](nextTick).asJava
        )
      )

      scheduler.expectMsg(Completion(extSimAdapter.toTyped, Some(nextTick)))
    }

    "schedule the data service when it is told to" in {
      val extSimAdapter = TestActorRef(
        new ExtSimAdapter(scheduler.ref)
      )

      val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)
      val dataService = TestProbe("dataService")

      scheduler.send(extSimAdapter, ExtSimAdapter.Init(extData))
      scheduler.expectMsg(
        ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK)
      )

      scheduler.send(extSimAdapter, Activation(INIT_SIM_TICK))

      awaitCond(
        !extData.receiveMessageQueue.isEmpty,
        max = 3.seconds,
        message = "No message received"
      )
      extData.receiveMessageQueue.size() shouldBe 1
      extData.receiveMessageQueue.take()

      extSimAdapter ! new ScheduleDataServiceMessage(
        dataService.ref
      )

      scheduler.expectNoMessage()
      dataService
        .expectMsgType[ScheduleServiceActivation]
        .tick shouldBe INIT_SIM_TICK
    }

    "terminate the external simulation and itself when told to" in {
      forAll(Table("simSuccessful", true, false)) { (simSuccessful: Boolean) =>
        val extSimAdapter = TestActorRef(
          new ExtSimAdapter(scheduler.ref)
        )

        val extData = new ExtSimAdapterData(extSimAdapter, mainArgs)

        scheduler.send(extSimAdapter, ExtSimAdapter.Init(extData))
        scheduler.expectMsg(
          ScheduleActivation(extSimAdapter.toTyped, INIT_SIM_TICK)
        )

        val stopWatcher = TestProbe()
        stopWatcher.watch(extSimAdapter)

        extSimAdapter ! StopMessage(simSuccessful)

        awaitCond(
          !extData.receiveMessageQueue.isEmpty,
          max = 3.seconds,
          message = "No message received"
        )
        extData.receiveMessageQueue.size() shouldBe 1
        extData.receiveMessageQueue.take() shouldBe new Terminate(simSuccessful)

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
