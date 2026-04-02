/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.profile.{BdewStandardLoadProfile, LoadProfile}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.config.InputConfig.LoadProfile.Datasource
import edu.ie3.simona.model.participant.load.ProfileLoadModel.ProfileLoadFactoryData
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.SecondaryData.{LoadData, SecondarySeriesData}
import edu.ie3.simona.service.DataTimeType
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
import squants.time.Hours

import scala.language.implicitConversions

class LoadProfileServiceSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with ConfigTestData
    with TestSpawnerTyped {

  private val sourceDefinition: Datasource = Datasource()

  private val invalidLoadProfile: LoadProfile = new LoadProfile {
    override def getKey: String = "invalid"
  }

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")

  private val agent1 = TestProbe[ParticipantAgent.Message]("agent")
  private val agent2 = TestProbe[ParticipantAgent.Message]("agent2")

  "A load profile service" should {

    val serviceKey =
      ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
    // lock activation scheduled
    scheduler.expectMessageType[ScheduleActivation]
    val loadProfileService = testKit.spawn(
      LoadProfileService(
        scheduler.ref,
        InitLoadProfileServiceStateData(
          sourceDefinition,
          simonaConfig.time.simStartTime,
        ),
        serviceKey,
      )
    )

    "send correct schedule message after initialisation" in {
      scheduler.expectMessage(
        ScheduleActivation(loadProfileService, 0L, Some(serviceKey))
      )
    }

    "announce failed load profile registration on invalid load profile" in {
      loadProfileService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        invalidLoadProfile,
      )

      agent1.expectMessage(RegistrationFailedMessage(loadProfileService))
    }

    "announce, that a load profile is registered" in {
      /* The successful registration stems from the test above */
      loadProfileService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        BdewStandardLoadProfile.G0,
      )

      agent1.expectMessage(
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

    "announce, that a valid coordinate is registered for forecast data" in {
      loadProfileService ! SecondaryServiceRegistrationMessage(
        agent2.ref,
        DataTimeType.CurrentAndForecast(
          forecastLength = Hours(6),
          forecastResolution = Hours(1),
        ),
        BdewStandardLoadProfile.H0,
      )

      agent2.expectMessage(
        RegistrationSuccessfulMessage(
          loadProfileService,
          0L,
          Some(
            ProfileLoadFactoryData(
              Some(Watts(268.6)),
              Some(KilowattHours(1000)),
            )
          ),
        )
      )

      agent1.expectNoMessage()
    }

    "recognize that agent is already registered" in {
      loadProfileService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        BdewStandardLoadProfile.G0,
      )

      agent1.expectNoMessage()
      agent2.expectNoMessage()
    }

    "send out correct load profile information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      loadProfileService ! Activation(0)

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe Some(900)

      agent1.expectMessage(
        DataProvision(
          0,
          loadProfileService,
          LoadData(Kilowatts(0.0683)),
          Some(900L),
        )
      )

      agent2.expectMessageType[DataProvision] match {
        case DataProvision(tick, serviceRef, data, nextTick) =>
          tick shouldBe 0L
          serviceRef shouldBe loadProfileService
          data match {
            case SecondarySeriesData(series) =>
              series.size shouldBe 7
            case unexpected =>
              fail(s"Received unexpected data $unexpected")
          }
          nextTick shouldBe Some(900L)
      }

    }

    "sends out correct load profile information when triggered again and does not as for triggering, if the end is reached" in {
      /* Send out an activity start trigger as the scheduler */
      loadProfileService ! Activation(900)

      val activationMsg = scheduler.expectMessageType[Completion]
      activationMsg.newTick shouldBe Some(1800)

      agent1.expectMessage(
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
