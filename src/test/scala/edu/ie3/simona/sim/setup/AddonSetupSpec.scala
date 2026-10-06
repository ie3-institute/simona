/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.datamodel.models.value.Value
import edu.ie3.simona.api.data.connection.ExtEmDataConnection.EmMode
import edu.ie3.simona.api.data.connection.{
  ExtEmDataConnection,
  ExtEvDataConnection,
  ExtPrimaryDataConnection,
  ExtResultDataConnection,
  ExtResultListener,
}
import edu.ie3.simona.api.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.ontology.em.EmSimulationInternal
import edu.ie3.simona.api.ontology.ev.RequestCurrentPrices
import edu.ie3.simona.api.ontology.primary.ProvidePrimaryData
import edu.ie3.simona.api.ontology.results.{
  RequestResultEntities,
  ResultDataResponseMessageToExt,
}
import edu.ie3.simona.api.ontology.simulation.ControlResponseMessageFromExt
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.service.results.ResultServiceProxy.AddListener
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Props}
import org.mockito.ArgumentMatchers.{any, anyString}
import org.mockito.Mockito.doAnswer
import org.scalatestplus.mockito.MockitoSugar.mock

import java.time.ZonedDateTime
import java.util
import java.util.{OptionalLong, UUID}
import scala.jdk.CollectionConverters.MapHasAsJava
import scala.util.Try

class AddonSetupSpec extends ScalaTestWithActorTestKit with UnitSpec {

  private given ActorContext[?] = {
    val ctx = mock[ActorContext[Any]]

    doAnswer { inv =>
      val behavior: Behavior[Any] = inv.getArgument(0)
      val name: String = inv.getArgument(1)
      val props: Props = inv.getArgument(2)
      testKit.spawn(behavior, name, props)
    }.when(ctx).spawn(any[Behavior[Any]], anyString(), any[Props])

    ctx
  }
  private given ActorRef[SchedulerMessage] = TestProbe("scheduler").ref
  private val adapter =
    TestProbe[ControlResponseMessageFromExt]("extSimAdapter")
  private given ActorRef[ControlResponseMessageFromExt] = adapter.ref
  private val resultProxy = TestProbe[ResultServiceProxy.Message]("resultProxy")
  private given ActorRef[ResultServiceProxy.Message] = resultProxy.ref
  private given ZonedDateTime = ZonedDateTime.now()

  "An AddonSetup" should {
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

    "connect an external primary data connection correctly" in {
      val extDataConnection =
        new ExtPrimaryDataConnection(toMap(Set(uuid1, uuid2, uuid3)))
      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 1
      updatedData.primaryDataServices(0)._1 shouldBe extDataConnection
      updatedData.primaryDataConnections shouldBe Seq(extDataConnection)

      // test if the actor refs are set up correctly
      val serviceRef = updatedData.primaryDataServices(0)._2

      extDataConnection.sendExtMsg(
        new ProvidePrimaryData(
          0L,
          new util.HashMap[UUID, Value](),
          OptionalLong.empty,
        )
      )

      adapter.expectMessage(new ScheduleDataServiceMessage(serviceRef))
    }

    "connect no external em data connection, if there are no controlled ems provided" in {
      val controlled = new util.ArrayList[UUID]

      val extDataConnection = new ExtEmDataConnection(controlled, EmMode.BASE)
      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 0
      updatedData.emDataService.isDefined shouldBe false
    }

    "connect an external em data connection correctly" in {
      val controlled = new util.ArrayList[UUID]
      controlled.add(uuid1)

      val extDataConnection = new ExtEmDataConnection(controlled, EmMode.BASE)
      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 0
      updatedData.emDataService.isDefined shouldBe true

      // test if the actor refs are set up correctly
      val serviceRef = updatedData.emDataService.value

      extDataConnection.sendExtMsg(new EmSimulationInternal(0L))

      adapter.expectMessage(new ScheduleDataServiceMessage(serviceRef))
    }

    "throw an exception when trying to connect a second external em data connection correctly" in {
      val controlled = new util.ArrayList[UUID]
      controlled.add(uuid1)

      val extDataConnection = new ExtEmDataConnection(controlled, EmMode.BASE)

      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 0
      updatedData.emDataService.isDefined shouldBe true

      val secondExtDataConnection =
        new ExtEmDataConnection(new util.ArrayList[UUID](), EmMode.BASE)

      intercept[ServiceException](
        AddonSetup.connect(secondExtDataConnection, updatedData, 1)
      ).getMessage shouldBe s"Trying to connect another EmDataConnection. Currently only one is allowed."
    }

    "connect an external ev data connection correctly" in {
      val extDataConnection = new ExtEvDataConnection()
      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 0
      updatedData.evDataService.isDefined shouldBe true

      // test if the actor refs are set up correctly
      val serviceRef = updatedData.evDataService.value

      extDataConnection.sendExtMsg(new RequestCurrentPrices())

      adapter.expectMessage(new ScheduleDataServiceMessage(serviceRef))
    }

    "throw an exception when trying to connect a second external ev data connection correctly" in {
      val extDataConnection = new ExtEvDataConnection()

      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 0
      updatedData.evDataService.isDefined shouldBe true

      val secondExtDataConnection = new ExtEvDataConnection()

      intercept[ServiceException](
        AddonSetup.connect(secondExtDataConnection, updatedData, 1)
      ).getMessage shouldBe s"Trying to connect another EvDataConnection. Currently only one is allowed."
    }

    "connect an external result data connection correctly" in {
      val extDataConnection =
        new ExtResultDataConnection(new util.ArrayList[UUID]())
      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 0
      updatedData.resultProviders.size shouldBe 1
      updatedData.resultListeners.size shouldBe 0

      // test if the actor refs are set up correctly
      val serviceRef = updatedData.resultProviders(0)

      extDataConnection.sendExtMsg(
        new RequestResultEntities(0L, new util.ArrayList[UUID](), false)
      )

      adapter.expectMessage(new ScheduleDataServiceMessage(serviceRef))
    }

    "connect an external result listener correctly" in {
      val extDataConnection = new ExtResultListener {
        override def processResponse(
            msg: ResultDataResponseMessageToExt
        ): Unit = {}

        override def close(): Unit = {}
      }

      val updatedData =
        AddonSetup.connect(extDataConnection, AddonSetupData.apply, 0)

      updatedData.primaryDataServices.size shouldBe 0
      updatedData.resultProviders.size shouldBe 0
      updatedData.resultListeners.size shouldBe 1

      val listenerRef = updatedData.resultListeners(0)
      resultProxy.expectMessage(AddListener(listenerRef))
    }

    "validate primary data connections without duplicates correctly" in {
      val extPrimaryDataConnection: Seq[ExtPrimaryDataConnection] = Seq(
        new ExtPrimaryDataConnection(toMap(Set(uuid1, uuid2))),
        new ExtPrimaryDataConnection(toMap(Set(uuid3, uuid4))),
        new ExtPrimaryDataConnection(toMap(Set(uuid5, uuid6))),
      )

      Try(
        AddonSetup.validatePrimaryData(extPrimaryDataConnection)
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
        AddonSetup.validatePrimaryData(extPrimaryDataConnection)
      ).getMessage shouldBe s"Multiple data connections provide primary data for assets: $uuid6,$uuid4"
    }

  }
}
