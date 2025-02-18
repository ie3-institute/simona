/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ActivePowerExtra
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  PrimaryRegistrationSuccessfulMessage,
  RegistrationFailedMessage,
}
import edu.ie3.simona.config.ConfigParams.TimeStampedSqlParams
import edu.ie3.simona.config.InputConfig.Primary
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.test.common.{AgentSpec, TestSpawnerClassic}
import edu.ie3.simona.test.helper.TestContainerHelper
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.{
  ClassicActorRefOps,
  TypedActorRefOps,
}
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.testcontainers.utility.DockerImageName

import java.util.UUID

class PrimaryServiceProxySqlIT
    extends AgentSpec(
      ActorSystem(
        "PrimaryServiceWorkerSqlIT",
        ConfigFactory
          .parseString("""
                     |pekko.loglevel="OFF"
          """.stripMargin),
      )
    )
    with ForAllTestContainer
    with BeforeAndAfterAll
    with TestContainerHelper
    with TestSpawnerClassic {

  override val container: PostgreSQLContainer = PostgreSQLContainer(
    DockerImageName.parse("postgres:14.2")
  )

  private val simulationStart =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

  private val schemaName = "public"

  override protected def beforeAll(): Unit = {
    // Copy sql import scripts into docker
    val sqlImportFile = getMountableFile("_timeseries/")
    container.copyFileToContainer(sqlImportFile, "/home/")

    Iterable(
      "time_series_p.sql",
      "time_series_pqh.sql",
      "time_series_mapping.sql",
    ).foreach { file =>
      val res = container.execInContainer("psql", "-Utest", "-f/home/" + file)
      res.getStderr shouldBe empty
    }
  }

  override protected def afterAll(): Unit = {
    container.stop()
    container.close()
  }

  private val scheduler = TestProbe("Scheduler")

  // function definition because postgres parameters are only available after initialization
  private def sqlParams: TimeStampedSqlParams = TimeStampedSqlParams(
    jdbcUrl = container.jdbcUrl,
    userName = container.username,
    password = container.password,
    schemaName = schemaName,
    timePattern = "yyyy-MM-dd'T'HH:mm:ssX",
  )

  private def createProxy(): TestActorRef[PrimaryServiceProxy] = {
    val initData = InitPrimaryServiceProxyStateData(
      Primary(
        None,
        None,
        None,
        sqlParams = Some(sqlParams),
      ),
      simulationStart,
    )

    TestActorRef(
      PrimaryServiceProxy.props(
        scheduler.ref,
        initData,
        simulationStart,
      )
    )
  }

  "A primary service proxy with SQL source" should {

    "initialize when given proper SQL input configs" in {
      val proxyRef = createProxy()

      scheduler.send(proxyRef, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(proxyRef.toTyped))
    }

    "handle participant request correctly if participant has primary data" in {
      val systemParticipantProbe = TestProbe("SystemParticipant")

      val proxyRef = createProxy()

      scheduler.send(proxyRef, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(proxyRef.toTyped))

      systemParticipantProbe.send(
        proxyRef,
        PrimaryServiceRegistrationMessage(
          systemParticipantProbe.ref,
          UUID.fromString("b86e95b0-e579-4a80-a534-37c7a470a409"),
        ),
      )
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      val initActivation = scheduler.expectMsgType[ScheduleActivation]
      initActivation.tick shouldBe INIT_SIM_TICK
      initActivation.unlockKey should not be empty

      // extract ref to the worker that the proxy created
      val workerRef = initActivation.actor
      scheduler.send(
        workerRef.toClassic,
        Activation(INIT_SIM_TICK),
      )

      scheduler.expectMsg(Completion(workerRef, Some(0)))

      systemParticipantProbe.expectMsg(
        PrimaryRegistrationSuccessfulMessage(
          workerRef.toClassic,
          0L,
          ActivePowerExtra,
        )
      )
    }

    "handle participant request correctly if participant does not have primary data" in {
      val systemParticipantProbe = TestProbe("SystemParticipant")

      val proxyRef = createProxy()

      scheduler.send(proxyRef, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(proxyRef.toTyped))

      systemParticipantProbe.send(
        proxyRef,
        PrimaryServiceRegistrationMessage(
          systemParticipantProbe.ref,
          UUID.fromString("db958617-e49d-44d3-b546-5f7b62776afd"),
        ),
      )

      scheduler.expectNoMessage()

      systemParticipantProbe.expectMsg(RegistrationFailedMessage(proxyRef))
    }
  }
}
