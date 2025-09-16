/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.datamodel.models.value.{PValue, Value}
import edu.ie3.simona.api.data.connection.{
  ExtEvDataConnection,
  ExtPrimaryDataConnection,
}
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.ev.RequestEvcsFreeLots
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.ontology.messages.{SchedulerMessage, ServiceMessage}
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.scalatestplus.mockito.MockitoSugar.mock

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsJava
import scala.util.Try

class ExtSimSetupSpec extends ScalaTestWithActorTestKit with UnitSpec {

  "An ExtSimSetup" should {
    val uuid1 = UUID.fromString("726c40e1-b1cd-4f16-a5b6-3972e852f60b")
    val uuid2 = UUID.fromString("614fa950-53fa-4f5e-8ea1-b51234c4866c")
    val uuid3 = UUID.fromString("7a9cd186-ad23-47b2-912e-1a2c777f46b0")
    val uuid4 = UUID.fromString("044f9398-58f6-44fa-94de-039e0a6856fb")
    val uuid5 = UUID.fromString("ebcefed4-a3e6-4a2a-b4a5-74226d548546")
    val uuid6 = UUID.fromString("4a9c8e14-c0ee-425b-af40-9552b9075414")

    def toMap(uuids: Set[UUID]): java.util.Map[UUID, Class[? <: Value]] = uuids
      .map(uuid => uuid -> classOf[Value])
      .toMap
      .asJava

    "validate primary data connections without duplicates correctly" in {
      val extPrimaryDataConnection: Seq[ExtPrimaryDataConnection] = Seq(
        new ExtPrimaryDataConnection(toMap(Set(uuid1, uuid2))),
        new ExtPrimaryDataConnection(toMap(Set(uuid3, uuid4))),
        new ExtPrimaryDataConnection(toMap(Set(uuid5, uuid6))),
      )

      Try(
        ExtSimSetup.validatePrimaryData(extPrimaryDataConnection)
      ).isSuccess shouldBe true
    }

    "throw exception while validate primary data connections if duplicates are found" in {
      val extPrimaryDataConnection: Seq[ExtPrimaryDataConnection] = Seq(
        new ExtPrimaryDataConnection(toMap(Set(uuid1, uuid2))),
        new ExtPrimaryDataConnection(toMap(Set(uuid3, uuid4))),
        new ExtPrimaryDataConnection(toMap(Set(uuid4, uuid5, uuid6))),
        new ExtPrimaryDataConnection(toMap(Set(uuid6))),
      )

      intercept[ServiceException](
        ExtSimSetup.validatePrimaryData(extPrimaryDataConnection)
      ).getMessage shouldBe s"Multiple data connections provide primary data for assets: $uuid6,$uuid4"
    }

    "set up a service correctly" in {
      val connection = new ExtEvDataConnection()
      val evService = TestProbe[ExtEvDataService.Message]("evService")
      val extSimAdapter =
        TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
      val scheduler = TestProbe[SchedulerMessage]("scheduler")

      val ctx = mock[ActorContext[?]]

      ExtSimSetup.setupService(connection, evService.ref, InitExtEvData.apply)(
        using
        ctx,
        scheduler.ref,
        extSimAdapter.ref,
      )

      scheduler.expectMessageType[ScheduleActivation]

      evService
        .expectMessageType[ServiceMessage.Create]
        .initializeStateData shouldBe InitExtEvData(connection)

      // request to check if the actor references are set correctly
      connection.sendExtMsg(new RequestEvcsFreeLots())

      extSimAdapter
        .expectMessageType[ScheduleDataServiceMessage]
        .dataService shouldBe evService.ref
    }

  }
}
