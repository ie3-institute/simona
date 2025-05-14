/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api

import edu.ie3.simona.api.ExtSimAdapter.{
  ExtSimAdapterStateData,
  Stop,
  WrappedActivation,
  WrappedControlMessage,
}
import edu.ie3.simona.api.data.ontology.{
  DataMessageFromExt,
  ScheduleDataServiceMessage,
}
import edu.ie3.simona.api.simulation.ExtSimAdapterData
import edu.ie3.simona.api.simulation.ontology.{
  ActivationMessage,
  ControlResponseMessageFromExt,
  TerminationCompleted,
  TerminationMessage,
  CompletionMessage => ExtCompletionMessage,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.ScheduleServiceActivation
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.{LockMsg, ScheduleKey}
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.testkit.TestKit.awaitCond
import org.scalatest.prop.TableDrivenPropertyChecks.forAll
import org.scalatest.prop.Tables.Table
import org.scalatest.wordspec.AnyWordSpecLike

import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.OptionConverters.RichOption
import scala.language.{existentials, implicitConversions}

class ExtSimAdapterSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with TestSpawnerTyped {

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val mainArgs = Array.empty[String]

  implicit def wrap(activation: Activation): WrappedActivation =
    WrappedActivation(activation)

  "An uninitialized ExtSimScheduler" must {
    "send correct completion message after initialisation" in {
      val lock = TestProbe[LockMsg]("lock")

      val extSimAdapter = testKit.spawn(ExtSimAdapter(scheduler.ref))
      val controlMsgAdapter =
        testKit.spawn(ExtSimAdapter.controlMessageAdapter(extSimAdapter))
      val extData = new ExtSimAdapterData(controlMsgAdapter, mainArgs)

      val key1 = ScheduleKey(lock.ref, UUID.randomUUID())
      extSimAdapter ! ExtSimAdapter.Create(extData, key1)

      val activationMessage = scheduler.expectMessageType[ScheduleActivation]
      activationMessage.tick shouldBe INIT_SIM_TICK
      activationMessage.unlockKey shouldBe Some(key1)
    }
  }

  "An initialized ExtSimScheduler" must {
    "forward an activation trigger and a corresponding completion message properly" in {
      val lock = TestProbe[LockMsg]("lock")
      val key1 = ScheduleKey(lock.ref, UUID.randomUUID())

      val extSimAdapter = testKit.spawn(ExtSimAdapter(scheduler.ref))
      val controlMsgAdapter =
        testKit.spawn(ExtSimAdapter.controlMessageAdapter(extSimAdapter))
      val extData = new ExtSimAdapterData(controlMsgAdapter, mainArgs)

      extSimAdapter ! ExtSimAdapter.Create(extData, key1)

      val activationMessage = scheduler.expectMessageType[ScheduleActivation]
      activationMessage.tick shouldBe INIT_SIM_TICK
      activationMessage.unlockKey shouldBe Some(key1)
      val activationAdapter = activationMessage.actor

      extSimAdapter ! Activation(INIT_SIM_TICK)

      awaitCond(
        !extData.receiveMessageQueue.isEmpty,
        max = 3.seconds,
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

      scheduler.expectMessage(Completion(activationAdapter, Some(nextTick)))
    }

    "schedule the data service when it is told to" in {
      val lock = TestProbe[LockMsg]("lock")
      val key1 = ScheduleKey(lock.ref, UUID.randomUUID())

      val extSimAdapter = testKit.spawn(ExtSimAdapter(scheduler.ref))

      val controlMsgAdapter =
        testKit.spawn(ExtSimAdapter.controlMessageAdapter(extSimAdapter))
      val extData = new ExtSimAdapterData(controlMsgAdapter, mainArgs)
      val dataService = TestProbe[DataMessageFromExt]("dataService")

      extSimAdapter ! ExtSimAdapter.Create(extData, key1)

      val activationMessage = scheduler.expectMessageType[ScheduleActivation]
      activationMessage.tick shouldBe INIT_SIM_TICK
      activationMessage.unlockKey shouldBe Some(key1)

      extSimAdapter ! Activation(INIT_SIM_TICK)

      awaitCond(
        !extData.receiveMessageQueue.isEmpty,
        max = 3.seconds,
      )
      extData.receiveMessageQueue.size() shouldBe 1
      extData.receiveMessageQueue.take()

      extSimAdapter ! WrappedControlMessage(
        new ScheduleDataServiceMessage(dataService.ref)
      )

      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      dataService
        .expectMessageType[ScheduleServiceActivation]
        .tick shouldBe INIT_SIM_TICK
      scheduler.expectNoMessage()
    }

    "terminate the external simulation and itself when told to" in {
      forAll(Table("simSuccessful", true, false)) { (simSuccessful: Boolean) =>
        val activationAdapter = TestProbe[Activation]("activationAdapter")

        val probe = TestProbe[ControlResponseMessageFromExt]("probe")
        val extData = new ExtSimAdapterData(probe.ref, mainArgs)

        val extSimAdapter = BehaviorTestKit(
          ExtSimAdapter.receiveIdle(
            ExtSimAdapterStateData(
              extData,
              None,
            )
          )(using scheduler.ref, activationAdapter.ref)
        )

        extSimAdapter.isAlive shouldBe true

        extSimAdapter.run(Stop(simSuccessful))

        awaitCond(
          !extData.receiveMessageQueue.isEmpty,
          max = 3.seconds,
        )
        extData.receiveMessageQueue.size() shouldBe 1
        extData.receiveMessageQueue.take() shouldBe new TerminationMessage(
          simSuccessful
        )

        // up until now, extSimAdapter should still be running
        extSimAdapter.run(WrappedControlMessage(new TerminationCompleted()))

        // extSimAdapter should have terminated now
        extSimAdapter.isAlive shouldBe false

        // scheduler is not involved in this
        scheduler.expectNoMessage()
      }
    }
  }

}
