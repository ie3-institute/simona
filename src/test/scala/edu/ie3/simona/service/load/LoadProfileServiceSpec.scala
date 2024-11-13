/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.load

import com.typesafe.config.ConfigFactory
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.profile.{BdewStandardLoadProfile, LoadProfile}
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage.{
  LoadProfileData,
  ProvideLoadProfileValue,
  RegisterForLoadProfileService,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage,
}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.load.LoadProfileService.InitLoadProfileServiceStateData
import edu.ie3.simona.test.common.{
  ConfigTestData,
  TestKitWithShutdown,
  TestSpawnerClassic,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{
  EventFilter,
  ImplicitSender,
  TestActorRef,
  TestProbe,
}
import org.scalatest.PrivateMethodTester
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.Watts

class LoadProfileServiceSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "LoadProfileServiceSpec",
        ConfigFactory
          .parseString("""
                     |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
                     |pekko.loglevel = "INFO"
          """.stripMargin),
      )
    )
    with ImplicitSender
    with AnyWordSpecLike
    with PrivateMethodTester
    with LazyLogging
    with ConfigTestData
    with TestSpawnerClassic {

  // setup config for scheduler
  private val config = ConfigFactory
    .parseString(s"""
            simona.time.startDateTime = "2011-01-01T00:00:00Z"
            simona.time.endDateTime = "2011-01-01T01:00:00Z"
            simona.time.schedulerReadyCheckWindow = 900
            simona.input.grid.datasource.id = "csv"
            simona.input.grid.datasource.csvParams.folderPath = "netdata"
            simona.input.grid.datasource.csvParams.csvSep =","
            simona.input.grid.datatarget.id = "csv"
            simona.powerflow.maxSweepPowerDeviation = 1E-5 // the maximum allowed deviation in power between two sweeps, before overall convergence is assumed
            simona.powerflow.newtonraphson.epsilon = [1E-12]
            simona.powerflow.newtonraphson.iterations = 50
            simona.simulationName = "ConfigTestDataSimulation"
            simona.gridConfig.refSystems = []
          """)
    .resolve()
    .withFallback(typesafeConfig)
  override protected val simonaConfig: SimonaConfig = SimonaConfig(config)
  // setup values
  private val invalidLoadProfile: LoadProfile = new LoadProfile {
    override def getKey: String = "invalidLoadProfile"
  }
  private val validLoadProfile: LoadProfile = BdewStandardLoadProfile.G0

  private val scheduler = TestProbe("scheduler")

  // build the load profile service
  private val loadProfileActor: TestActorRef[LoadProfileService] = TestActorRef(
    new LoadProfileService(
      scheduler.ref,
      TimeUtil.withDefaults.toZonedDateTime(
        simonaConfig.simona.time.startDateTime
      ),
      TimeUtil.withDefaults.toZonedDateTime(
        simonaConfig.simona.time.endDateTime
      ),
    )
  )

  "A load profile service" must {

    "receive correct completion message after initialisation" in {
      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        loadProfileActor,
        SimonaService.Create(
          InitLoadProfileServiceStateData(
            simonaConfig.simona.input.loadprofile.datasource
          ),
          key,
        ),
      )
      scheduler.expectMsg(
        ScheduleActivation(loadProfileActor.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(loadProfileActor, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(loadProfileActor.toTyped, Some(0)))
    }

    "announce failed load profile registration on invalid coordinate" in {
      EventFilter
        .error(
          pattern =
            "\\[.*] Unable to obtain necessary information to register for load profile 'invalidLoadProfile'.",
          occurrences = 1,
        )
        .intercept {
          loadProfileActor ! RegisterForLoadProfileService(invalidLoadProfile)
        }

      expectMsg(RegistrationFailedMessage(loadProfileActor))
    }

    "announce, that a valid load profile is registered" in {
      /* The successful registration stems from the test above */
      loadProfileActor ! RegisterForLoadProfileService(validLoadProfile)

      expectMsg(RegistrationSuccessfulMessage(loadProfileActor.ref, Some(0L)))
    }

    "recognize, that a valid coordinate yet is registered" in {
      /* The successful registration stems from the test above */
      EventFilter
        .warning(
          pattern = "Sending actor Actor\\[.*] is already registered",
          occurrences = 1,
        )
        .intercept {
          loadProfileActor ! RegisterForLoadProfileService(validLoadProfile)
        }
      expectNoMessage()
    }

    "send out correct load profile information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      scheduler.send(loadProfileActor, Activation(0))

      scheduler.expectMsg(Completion(loadProfileActor.toTyped, Some(900)))

      expectMsg(
        ProvideLoadProfileValue(
          0,
          loadProfileActor,
          LoadProfileData(
            Watts(70d),
            Watts(240.4),
          ),
          Some(900L),
        )
      )

    }

    "sends out correct load profile information when triggered again and does not as for triggering, if the end is reached" in {
      /* Send out an activity start trigger as the scheduler */
      scheduler.send(loadProfileActor, Activation(900L))

      scheduler.expectMsg(Completion(loadProfileActor.toTyped, Some(1800L)))

      expectMsg(
        ProvideLoadProfileValue(
          900L,
          loadProfileActor,
          LoadProfileData(
            Watts(73d),
            Watts(240.4),
          ),
          Some(1800L),
        )
      )
    }
  }
}
