/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.dimafeng.testcontainers.{ForAllTestContainer, PostgreSQLContainer}
import edu.ie3.datamodel.io.naming.DatabaseNamingStrategy
import edu.ie3.datamodel.models.value.{HeatAndSValue, PValue}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ActivePowerExtra,
  ComplexPowerAndHeat,
  ComplexPowerAndHeatExtra,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  PrimaryRegistrationSuccessfulMessage,
}
import edu.ie3.simona.config.ConfigParams.TimeStampedSqlParams
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  Create,
  WorkerRegistrationMessage,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.{LockMsg, ScheduleKey}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.SqlInitPrimaryServiceStateData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.test.helper.TestContainerHelper
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.Kilovars
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.scalatest.BeforeAndAfterAll
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpecLike
import org.testcontainers.utility.DockerImageName
import squants.energy.Kilowatts

import java.util.UUID
import scala.language.implicitConversions

class PrimaryServiceWorkerSqlIT
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with ForAllTestContainer
    with BeforeAndAfterAll
    with TableDrivenPropertyChecks
    with TimeSeriesTestData
    with TestContainerHelper
    with TestSpawnerTyped {

  implicit def wrap(msg: Activation): ServiceMessage =
    WrappedActivation(msg)

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
      val scheduler = TestProbe[SchedulerMessage]("Scheduler")
      val lock = TestProbe[LockMsg]("lock")

      val cases = Table(
        (
          "valueClass",
          "uuid",
          "firstTick",
          "firstData",
          "primaryDataExtra",
          "maybeNextTick",
        ),
        (
          classOf[HeatAndSValue],
          uuidPqh,
          0L,
          ComplexPowerAndHeat(
            Kilowatts(1000.0),
            Kilovars(329.0),
            Kilowatts(8000.0),
          ),
          ComplexPowerAndHeatExtra,
          Some(900L),
        ),
        (
          classOf[PValue],
          uuidP,
          0L,
          ActivePower(
            Kilowatts(1000.0)
          ),
          ActivePowerExtra,
          Some(900L),
        ),
      )

      forAll(cases) {
        (
            valueClass,
            uuid,
            firstTick,
            firstData,
            primaryDataExtra,
            maybeNextTick,
        ) =>
          val serviceRef = testKit.spawn(PrimaryServiceWorker(scheduler.ref))

          val initData = SqlInitPrimaryServiceStateData(
            uuid,
            simulationStart,
            valueClass,
            TimeStampedSqlParams(
              jdbcUrl = container.jdbcUrl,
              userName = container.username,
              password = container.password,
              schemaName = schemaName,
              timePattern = "yyyy-MM-dd'T'HH:mm:ssX",
            ),
            new DatabaseNamingStrategy(),
          )

          val key1 = ScheduleKey(lock.ref, UUID.randomUUID())
          serviceRef ! Create(initData, key1)

          val scheduleActivationMsg =
            scheduler.expectMessageType[ScheduleActivation]
          scheduleActivationMsg.tick shouldBe INIT_SIM_TICK
          scheduleActivationMsg.unlockKey shouldBe Some(key1)

          serviceRef ! Activation(INIT_SIM_TICK)
          scheduler.expectMessage(
            Completion(scheduleActivationMsg.actor, Some(firstTick))
          )

          val participant = TestProbe[Any]()

          serviceRef ! WorkerRegistrationMessage(participant.ref)

          participant.expectMessage(
            PrimaryRegistrationSuccessfulMessage(
              serviceRef.toClassic,
              firstTick,
              primaryDataExtra,
            )
          )

          serviceRef ! Activation(firstTick)
          scheduler.expectMessage(
            Completion(scheduleActivationMsg.actor, maybeNextTick)
          )

          val dataMsg =
            participant.expectMessageType[DataProvision[PrimaryData]]
          dataMsg.tick shouldBe firstTick
          dataMsg.data shouldBe firstData
          dataMsg.nextDataTick shouldBe maybeNextTick

          scheduler.expectNoMessage()
      }
    }
  }
}
