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
import edu.ie3.datamodel.io.naming.DatabaseNamingStrategy
import edu.ie3.datamodel.models.value.{HeatAndSValue, PValue}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ApparentPowerAndHeat
}
import edu.ie3.simona.config.IoConfigUtils.TimeStampedSqlParams
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.WorkerRegistrationMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  ProvidePrimaryDataMessage,
  SqlInitPrimaryServiceStateData
}
import edu.ie3.simona.test.common.AgentSpec
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.test.helper.TestContainerHelper
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.Kilovars
import org.scalatest.BeforeAndAfterAll
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.Kilowatts

class PrimaryServiceWorkerSqlIT
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
    with TableDrivenPropertyChecks
    with TimeSeriesTestData
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

    Iterable("time_series_p.sql", "time_series_pqh.sql")
      .foreach { file =>
        val res = container.execInContainer("psql", "-Utest", "-f/home/" + file)
        res.getStderr shouldBe empty
      }
  }

  override protected def afterAll(): Unit = {
    container.stop()
    container.close()
  }

  "A primary service actor with SQL source" should {
    "initialize and send out data when activated" in {
      val scheduler = TestProbe("Scheduler")

      val cases = Table(
        (
          "service",
          "uuid",
          "firstTick",
          "firstData",
          "maybeNextTick"
        ),
        (
          PrimaryServiceWorker.props(
            scheduler.ref,
            classOf[HeatAndSValue]
          ),
          uuidPqh,
          0L,
          ApparentPowerAndHeat(
            Kilowatts(1000.0),
            Kilovars(329.0),
            Kilowatts(8000.0)
          ),
          Some(900L)
        ),
        (
          PrimaryServiceWorker.props(
            scheduler.ref,
            classOf[PValue]
          ),
          uuidP,
          0L,
          ActivePower(
            Kilowatts(1000.0)
          ),
          Some(900L)
        )
      )

      forAll(cases) {
        (
            service,
            uuid,
            firstTick,
            firstData,
            maybeNextTick
        ) =>
          val serviceRef = TestActorRef(service)

          val initData = SqlInitPrimaryServiceStateData(
            uuid,
            simulationStart,
            TimeStampedSqlParams(
              jdbcUrl = container.jdbcUrl,
              userName = container.username,
              password = container.password,
              schemaName = schemaName,
              timePattern = "yyyy-MM-dd HH:mm:ss",
              tableName = "is_ignored"
            ),
            new DatabaseNamingStrategy()
          )

          val triggerId1 = 1L

          scheduler.send(
            serviceRef,
            TriggerWithIdMessage(
              InitializeServiceTrigger(initData),
              triggerId1,
              serviceRef
            )
          )

          scheduler.expectMsg(
            CompletionMessage(
              triggerId1,
              Some(
                Seq(
                  ScheduleTriggerMessage(
                    ActivityStartTrigger(firstTick),
                    serviceRef
                  )
                )
              )
            )
          )

          val participant = TestProbe()

          participant.send(
            serviceRef,
            WorkerRegistrationMessage(participant.ref)
          )
          participant.expectMsg(RegistrationSuccessfulMessage(Some(firstTick)))

          val triggerId2 = 2L

          scheduler.send(
            serviceRef,
            TriggerWithIdMessage(
              ActivityStartTrigger(firstTick),
              triggerId2,
              serviceRef
            )
          )

          scheduler.expectMsgType[CompletionMessage]

          val dataMsg = participant.expectMsgType[ProvidePrimaryDataMessage]
          dataMsg.tick shouldBe firstTick
          dataMsg.data shouldBe firstData
          dataMsg.nextDataTick shouldBe maybeNextTick

          scheduler.expectNoMessage()
      }
    }
  }
}
