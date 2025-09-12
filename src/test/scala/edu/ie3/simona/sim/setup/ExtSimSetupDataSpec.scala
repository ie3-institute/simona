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
}
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.service.ev.ExtEvDataService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}

import java.util.UUID
import scala.jdk.CollectionConverters.{MapHasAsJava, SeqHasAsJava}

class ExtSimSetupDataSpec extends ScalaTestWithActorTestKit with UnitSpec {

  "An ExtSimSetupData" should {

    val emptyMapInput = Map.empty[UUID, Class[? <: Value]].asJava
    val emptyUuidList = List.empty[UUID].asJava

    "be updated with an ExtPrimaryDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData.apply

      val connection = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef =
        TestProbe[PrimaryServiceProxy.Message]("primary_service").ref

      val updated = extSimSetupData.update(
        connection,
        primaryRef,
      )

      updated.extSimAdapters shouldBe empty
      updated.primaryDataServices shouldBe Seq((connection, primaryRef))
      updated.evDataService shouldBe None
      updated.resultListeners shouldBe empty
      updated.resultProviders shouldBe empty
    }

    "be updated with multiple ExtPrimaryDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData.apply

      val connection1 = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef1 =
        TestProbe[PrimaryServiceProxy.Message]("primary_service1").ref

      val connection2 = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef2 =
        TestProbe[PrimaryServiceProxy.Message]("primary_service2").ref

      val updated = extSimSetupData
        .update(connection1, primaryRef1)
        .update(connection2, primaryRef2)

      updated.extSimAdapters shouldBe empty
      updated.primaryDataServices shouldBe Seq(
        (connection1, primaryRef1),
        (connection2, primaryRef2),
      )
      updated.evDataService shouldBe None
      updated.resultListeners shouldBe empty
      updated.resultProviders shouldBe empty
    }

    "be updated with an ExtInputDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData.apply

      val primaryConnection = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef =
        TestProbe[PrimaryServiceProxy.Message]("primary_service").ref

      val evConnection = new ExtEvDataConnection()
      val evRef = TestProbe[ExtEvDataService.Message]("ev_service").ref

      val cases = Table(
        ("connection", "serviceRef", "expected"),
        (
          primaryConnection,
          primaryRef,
          extSimSetupData.copy(primaryDataServices =
            Seq((primaryConnection, primaryRef))
          ),
        ),
        (
          evConnection,
          evRef,
          extSimSetupData.copy(evDataService = Some(evRef)),
        ),
      )

      forAll(cases) { (connection, serviceRef, expected) =>
        val updated = extSimSetupData.update(connection, serviceRef)

        updated.extSimAdapters shouldBe expected.extSimAdapters
        updated.primaryDataServices shouldBe expected.primaryDataServices
        updated.evDataService shouldBe expected.evDataService
        updated.resultListeners shouldBe expected.resultListeners
        updated.resultProviders shouldBe expected.resultProviders
      }
    }

    "be updated with an ExtResultDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData.apply

      val resultConnection = new ExtResultDataConnection(emptyUuidList)
      val resultServiceProxyRef =
        TestProbe[ServiceMessage]("resultServiceProxy").ref

      val updated =
        extSimSetupData.update(resultConnection, resultServiceProxyRef)

      updated.extSimAdapters shouldBe empty
      updated.primaryDataServices shouldBe empty
      updated.evDataService shouldBe None
      updated.resultListeners shouldBe empty
      updated.resultProviders shouldBe Seq(resultServiceProxyRef)
    }

    "be updated with multiple different connections" in {
      val extSimSetupData = ExtSimSetupData.apply

      val primaryConnection = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef =
        TestProbe[PrimaryServiceProxy.Message]("primary_service").ref

      val evConnection = new ExtEvDataConnection()
      val evRef = TestProbe[ExtEvDataService.Message]("ev_service").ref

      val resultConnection = new ExtResultDataConnection(emptyUuidList)
      val resultServiceProxyRef =
        TestProbe[ServiceMessage]("resultServiceProxy").ref

      val updated = extSimSetupData
        .update(primaryConnection, primaryRef)
        .update(evConnection, evRef)
        .update(resultConnection, resultServiceProxyRef)

      updated.extSimAdapters shouldBe empty
      updated.primaryDataServices shouldBe Seq((primaryConnection, primaryRef))
      updated.evDataService shouldBe Some(evRef)
      updated.resultListeners shouldBe empty
      updated.resultProviders shouldBe Seq(resultServiceProxyRef)
    }

    "return evDataService correctly" in {
      val evConnection = new ExtEvDataConnection()
      val evRef = TestProbe[ExtEvDataService.Message]("ev_service").ref

      val cases = Table(
        ("extSimSetupData", "expectedService"),
        (ExtSimSetupData.apply.update(evConnection, evRef), Some(evRef)),
        (ExtSimSetupData.apply, None),
      )

      forAll(cases) { (extSimSetupData, expectedService) =>
        extSimSetupData.evDataService shouldBe expectedService
      }
    }

  }

}
