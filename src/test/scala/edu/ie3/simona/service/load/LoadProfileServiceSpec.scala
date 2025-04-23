/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.profile.{BdewStandardLoadProfile, LoadProfile}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  DataProvision,
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.model.participant.load.LoadModel.ProfileLoadFactoryData
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage.{
  LoadData,
  RegisterForLoadProfileService,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.load.LoadProfileService.InitLoadProfileServiceStateData
import edu.ie3.simona.test.common.{ConfigTestData, TestSpawnerTyped}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.scalatest.PrivateMethodTester
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.{KilowattHours, Kilowatts, Watts}

import scala.language.implicitConversions

class LoadProfileServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with ConfigTestData
    with TestSpawnerTyped {

  private implicit def wrap(msg: Activation): ServiceMessage =
    WrappedActivation(msg)

  private val sourceDefinition: Datasource = Datasource()

  private val invalidLoadProfile = new LoadProfile {
    override def getKey: String = "invalid"
  }

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val agent = TestProbe[ParticipantAgent.Request]("agent")

  // build the load profile service
  private val loadProfileService = testKit.spawn(
    LoadProfileService(scheduler.ref)
  )

  "A load profile service" should {
    "receive correct completion message after initialisation" in {
      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      loadProfileService ! Create(
        InitLoadProfileServiceStateData(
          sourceDefinition,
          TimeUtil.withDefaults.toZonedDateTime(
            simonaConfig.simona.time.startDateTime
          ),
          TimeUtil.withDefaults.toZonedDateTime(
            simonaConfig.simona.time.endDateTime
          ),
        ),
        key,
      )

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      loadProfileService ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor, Some(0)))
    }

    "announce failed load profile registration on invalid load profile" in {
      loadProfileService ! RegisterForLoadProfileService(
        agent.ref,
        invalidLoadProfile,
      )

      agent.expectMessage(RegistrationFailedMessage(loadProfileService))
    }

    "announce, that a load profile is registered" in {
      /* The successful registration stems from the test above */
      loadProfileService ! RegisterForLoadProfileService(
        agent.ref,
        BdewStandardLoadProfile.G0,
      )

      agent.expectMessage(
        RegistrationSuccessfulMessage(
          loadProfileService,
          0L,
          Some(
            ProfileLoadFactoryData(
              Some(Watts(240.4)),
              Some(KilowattHours(1000)),
            )
          ),
        )
      )
    }

    "recognize, that a valid coordinate yet is registered" in {
      /* The successful registration stems from the test above */
      loadProfileService ! RegisterForLoadProfileService(
        agent.ref,
        BdewStandardLoadProfile.G0,
      )

      agent.expectNoMessage()
    }

    "send out correct load profile information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      loadProfileService ! Activation(0)

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe Some(900)

      agent.expectMessage(
        DataProvision(
          0,
          loadProfileService,
          LoadData(Kilowatts(0.0683)),
          Some(900L),
        )
      )

    }

    "sends out correct load profile information when triggered again and does not as for triggering, if the end is reached" in {
      /* Send out an activity start trigger as the scheduler */
      loadProfileService ! Activation(900)

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe Some(1800)

      agent.expectMessage(
        DataProvision(
          900,
          loadProfileService,
          LoadData(Kilowatts(0.0665)),
          Some(1800L),
        )
      )
    }
  }
}
