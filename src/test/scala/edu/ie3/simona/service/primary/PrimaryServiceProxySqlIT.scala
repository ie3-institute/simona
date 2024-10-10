/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.InputConfig.PrimaryConfig
import edu.ie3.simona.config.IoConfigUtils.TimeStampedSqlParams
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.PrimaryServiceRegistrationMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.{
  RegistrationFailedMessage,
  RegistrationSuccessfulMessage
}
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.primary.PrimaryServiceWorker.SqlInitPrimaryServiceStateData
import edu.ie3.simona.test.common.AgentSpec
import edu.ie3.simona.test.helper.TestContainerHelper
import edu.ie3.util.TimeUtil
import org.scalatest.BeforeAndAfterAll

import java.util.UUID

class PrimaryServiceProxySqlIT
    extends AgentSpec(
      ActorSystem(
        "PrimaryServiceWorkerSqlIT",
        ConfigFactory
          .parseString("""
                     |akka.loglevel="OFF"
          """.stripMargin)
      )
    )
    with ForAllTestContainer
    with BeforeAndAfterAll
    with TestContainerHelper {

  override val container: PostgreSQLContainer = PostgreSQLContainer(
    "postgres:14.2"
  )

  private val simulationStart =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")

  private val schemaName = "public"

  override protected def beforeAll(): Unit = {
    // Copy sql import scripts into docker
    val sqlImportFile = getMountableFile("_timeseries/")
    container.copyFileToContainer(sqlImportFile, "/home/")

    Iterable(
      "time_series_p.sql",
      "time_series_pqh.sql",
      "time_series_mapping.sql"
    ).foreach { file =>
      val res = container.execInContainer("psql", "-Utest", "-f/home/" + file)
      res.getStderr shouldBe empty
    }
  }

  override protected def afterAll(): Unit = {
    container.stop()
    container.close()
  }

  // function definition because postgres parameters are only available after initialization
  private def sqlParams: TimeStampedSqlParams = TimeStampedSqlParams(
    jdbcUrl = container.jdbcUrl,
    userName = container.username,
    password = container.password,
    schemaName = schemaName,
    tableName = "is_ignored",
    timePattern = "yyyy-MM-dd HH:mm:ss"
  )

  "A primary service proxy with SQL source" should {
    val scheduler = TestProbe("Scheduler")

    val proxyRef = TestActorRef(
      PrimaryServiceProxy.props(
        scheduler.ref,
        simulationStart
      )
    )

    "initialize when given proper SQL input configs" in {
      val initData = InitPrimaryServiceProxyStateData(
        PrimaryConfig(
          None,
          None,
          sqlParams = Some(sqlParams),
          None
        ),
        simulationStart
      )

      val triggerIdInit1 = 1L

      scheduler.send(
        proxyRef,
        TriggerWithIdMessage(
          InitializeServiceTrigger(initData),
          triggerIdInit1,
          proxyRef
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerIdInit1,
          None
        )
      )
    }

    "handle participant request correctly if participant has primary data" in {
      val systemParticipantProbe = TestProbe("SystemParticipant")

      systemParticipantProbe.send(
        proxyRef,
        PrimaryServiceRegistrationMessage(
          UUID.fromString("b86e95b0-e579-4a80-a534-37c7a470a409")
        )
      )

      val initTriggerMsg = scheduler.expectMsgType[ScheduleTriggerMessage]

      initTriggerMsg.trigger match {
        case InitializeServiceTrigger(
              sqlInit: SqlInitPrimaryServiceStateData
            ) =>
          sqlInit.sqlParams shouldBe sqlParams
          sqlInit.simulationStart shouldBe simulationStart
          sqlInit.timeSeriesUuid shouldBe UUID.fromString(
            "9185b8c1-86ba-4a16-8dea-5ac898e8caa5"
          )
        case unexpected => fail(s"Received unexpected trigger $unexpected")
      }

      val triggerIdInit2 = 2L

      // extract ref to the worker that the proxy created
      val workerRef = initTriggerMsg.actorToBeScheduled
      scheduler.send(
        workerRef,
        TriggerWithIdMessage(
          initTriggerMsg.trigger,
          triggerIdInit2,
          workerRef
        )
      )

      scheduler.expectMsg(
        CompletionMessage(
          triggerIdInit2,
          Some(
            Seq(
              ScheduleTriggerMessage(
                ActivityStartTrigger(0L),
                workerRef
              )
            )
          )
        )
      )

      systemParticipantProbe.expectMsg(RegistrationSuccessfulMessage(Some(0L)))
    }

    "handle participant request correctly if participant does not have primary data" in {
      val systemParticipantProbe = TestProbe("SystemParticipant")

      systemParticipantProbe.send(
        proxyRef,
        PrimaryServiceRegistrationMessage(
          UUID.fromString("db958617-e49d-44d3-b546-5f7b62776afd")
        )
      )

      scheduler.expectNoMessage()

      systemParticipantProbe.expectMsg(RegistrationFailedMessage)
    }
  }
}
