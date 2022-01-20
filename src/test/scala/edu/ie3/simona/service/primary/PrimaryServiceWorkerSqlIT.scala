/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestActorRef, TestProbe}
import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.models.value.{HeatAndSValue, PValue, Value}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ApparentPowerAndHeat
}
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary.SqlParams
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
import edu.ie3.util.TimeUtil
import org.scalatest.BeforeAndAfterAll
import org.scalatest.prop.TableDrivenPropertyChecks
import org.testcontainers.utility.MountableFile

import java.nio.file.Paths
import java.util.UUID
import scala.language.postfixOps
import scala.reflect.ClassTag

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
    with TableDrivenPropertyChecks {

  override val container: PostgreSQLContainer = PostgreSQLContainer(
    "postgres:11.14"
  )

  private val simulationStart =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")

  private val schemaName = "public"

  private val uuidP = UUID.fromString("9185b8c1-86ba-4a16-8dea-5ac898e8caa5")
  private val uuidPhq = UUID.fromString("46be1e57-e4ed-4ef7-95f1-b2b321cb2047")

  private val tableNameP = s"its_p_$uuidP"
  private val tableNamePhq = s"its_pqh_$uuidPhq"

  override protected def beforeAll(): Unit = {
    val url = getClass.getResource("timeseries/")
    url shouldNot be(null)
    val path = Paths.get(url.toURI)

    // Copy sql import scripts into docker
    val sqlImportFile = MountableFile.forHostPath(path)
    container.copyFileToContainer(sqlImportFile, "/home/")

    Iterable(s"$tableNameP.sql", s"$tableNamePhq.sql")
      .foreach { file =>
        val res = container.execInContainer("psql", "-Utest", "-f/home/" + file)
        res.getStderr shouldBe empty
      }
  }

  override protected def afterAll(): Unit = {
    container.stop()
    container.close()
  }

  private def getServiceActor[T <: Value](
      scheduler: ActorRef
  )(implicit tag: ClassTag[T]): PrimaryServiceWorker[T] = {
    new PrimaryServiceWorker[T](
      scheduler,
      tag.runtimeClass.asInstanceOf[Class[T]],
      simulationStart
    )
  }

  "A primary service actor with SQL source" should {
    "initialize and send out data when activated" in {

      val cases = Table(
        (
          "getService",
          "uuid",
          "tableName",
          "firstTick",
          "dataValueClass",
          "maybeNextTick"
        ),
        (
          getServiceActor[HeatAndSValue](_),
          uuidPhq,
          tableNamePhq,
          0L,
          classOf[ApparentPowerAndHeat],
          Some(900L)
        ),
        (
          getServiceActor[PValue](_),
          uuidP,
          tableNameP,
          0L,
          classOf[ActivePower],
          Some(900L)
        )
      )

      forAll(cases) {
        (
            getService,
            uuid,
            tableName,
            firstTick,
            dataValueClass,
            maybeNextTick
        ) =>
          val scheduler = TestProbe("scheduler")

          val serviceRef =
            TestActorRef(
              getService(scheduler.ref)
            )

          val initData = SqlInitPrimaryServiceStateData(
            SqlParams(
              jdbcUrl = container.jdbcUrl,
              userName = container.username,
              password = container.password,
              schemaName = schemaName,
              tableName = tableName,
              timePattern = "yyyy-MM-dd HH:mm:ss"
            ),
            uuid,
            simulationStart
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
                List(
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

          val dataMsg = participant.expectMsgType[ProvidePrimaryDataMessage]
          dataMsg.tick shouldBe firstTick
          dataMsg.data.getClass shouldBe dataValueClass
          dataMsg.nextDataTick shouldBe maybeNextTick
      }
    }
  }
}
