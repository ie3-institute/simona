/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.io.naming.DatabaseNamingStrategy
import edu.ie3.datamodel.models.value.{HeatAndSValue, PValue}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ComplexPowerAndHeat,
}
import edu.ie3.simona.config.IoConfigUtils.TimeStampedSqlParams
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.WorkerRegistrationMessage
import edu.ie3.simona.scheduler.ScheduleLock.ScheduleKey
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  ProvidePrimaryDataMessage,
  SqlInitPrimaryServiceStateData,
}
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.test.common.{AgentSpec, TestSpawnerClassic}
import edu.ie3.simona.test.helper.TestContainerHelper
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.Kilovars
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.prop.TableDrivenPropertyChecks
import org.testcontainers.utility.DockerImageName
import squants.energy.Kilowatts

import java.util.UUID

class PrimaryServiceWorkerSqlIT
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
    with TableDrivenPropertyChecks
    with TimeSeriesTestData
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
      val lock = TestProbe("lock")

      val cases = Table(
        (
          "service",
          "uuid",
          "firstTick",
          "firstData",
          "maybeNextTick",
        ),
        (
          PrimaryServiceWorker.props(
            scheduler.ref,
            classOf[HeatAndSValue],
          ),
          uuidPqh,
          0L,
          ComplexPowerAndHeat(
            Kilowatts(1000.0),
            Kilovars(329.0),
            Kilowatts(8000.0),
          ),
          Some(900L),
        ),
        (
          PrimaryServiceWorker.props(
            scheduler.ref,
            classOf[PValue],
          ),
          uuidP,
          0L,
          ActivePower(
            Kilowatts(1000.0)
          ),
          Some(900L),
        ),
      )

      forAll(cases) {
        (
            service,
            uuid,
            firstTick,
            firstData,
            maybeNextTick,
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
              timePattern = "yyyy-MM-dd'T'HH:mm:ssX",
              tableName = "is_ignored",
            ),
            new DatabaseNamingStrategy(),
          )

          val key1 = ScheduleKey(lock.ref.toTyped, UUID.randomUUID())
          scheduler.send(
            serviceRef,
            SimonaService.Create(initData, key1),
          )
          scheduler.expectMsg(
            ScheduleActivation(serviceRef.toTyped, INIT_SIM_TICK, Some(key1))
          )

          scheduler.send(serviceRef, Activation(INIT_SIM_TICK))
          scheduler.expectMsg(Completion(serviceRef.toTyped, Some(firstTick)))

          val participant = TestProbe()

          participant.send(
            serviceRef,
            WorkerRegistrationMessage(participant.ref),
          )
          participant.expectMsg(
            RegistrationSuccessfulMessage(serviceRef, Some(firstTick))
          )

          scheduler.send(serviceRef, Activation(firstTick))
          scheduler.expectMsg(Completion(serviceRef.toTyped, maybeNextTick))

          val dataMsg = participant.expectMsgType[ProvidePrimaryDataMessage]
          dataMsg.tick shouldBe firstTick
          dataMsg.data shouldBe firstData
          dataMsg.nextDataTick shouldBe maybeNextTick

          scheduler.expectNoMessage()
      }
    }
  }
}
