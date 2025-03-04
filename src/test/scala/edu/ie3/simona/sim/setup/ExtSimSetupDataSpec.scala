/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.simona.api.data.em.ExtEmDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection
import edu.ie3.simona.api.data.results.ExtResultDataConnection
import edu.ie3.simona.test.common.UnitSpec
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps

import java.util.UUID
import scala.jdk.CollectionConverters.MapHasAsJava

class ExtSimSetupDataSpec extends ScalaTestWithActorTestKit with UnitSpec {

  "An ExtSimSetupData" should {

    val emptyMapInput = Map.empty[String, UUID].asJava
    val emptyMapResult = Map.empty[UUID, String].asJava

    "be updated with an ExtPrimaryDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData()

      val connection = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef = TestProbe("primary_service").ref.toClassic

      val updated = extSimSetupData.update(
        connection,
        primaryRef,
      )

      updated.extSimAdapters shouldBe empty
      updated.extPrimaryDataServices shouldBe Seq((connection, primaryRef))
      updated.extDataServices shouldBe empty
      updated.extResultListeners shouldBe empty
    }

    "be updated with multiple ExtPrimaryDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData()

      val connection1 = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef1 = TestProbe("primary_service1").ref.toClassic

      val connection2 = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef2 = TestProbe("primary_service2").ref.toClassic

      val updated = extSimSetupData
        .update(connection1, primaryRef1)
        .update(connection2, primaryRef2)

      updated.extSimAdapters shouldBe empty
      updated.extPrimaryDataServices shouldBe Seq(
        (connection1, primaryRef1),
        (connection2, primaryRef2),
      )
      updated.extDataServices shouldBe empty
      updated.extResultListeners shouldBe empty
    }

    "be updated with an ExtInputDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData()

      val primaryConnection = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef = TestProbe("primary_service").ref.toClassic

      val evConnection = new ExtEvDataConnection()
      val evRef = TestProbe("ev_service").ref.toClassic

      val emConnection = new ExtEmDataConnection(emptyMapInput)
      val emRef = TestProbe("em_service").ref.toClassic

      val cases = Table(
        ("connection", "serviceRef", "expected"),
        (
          primaryConnection,
          primaryRef,
          extSimSetupData.copy(extPrimaryDataServices =
            Seq((primaryConnection, primaryRef))
          ),
        ),
        (
          evConnection,
          evRef,
          extSimSetupData.copy(extDataServices = Seq((evConnection, evRef))),
        ),
        (
          emConnection,
          emRef,
          extSimSetupData.copy(extDataServices = Seq((emConnection, emRef))),
        ),
      )

      forAll(cases) { (connection, serviceRef, expected) =>
        val updated = extSimSetupData.update(connection, serviceRef)

        updated.extSimAdapters shouldBe expected.extSimAdapters
        updated.extPrimaryDataServices shouldBe expected.extPrimaryDataServices
        updated.extDataServices shouldBe expected.extDataServices
        updated.extResultListeners shouldBe expected.extResultListeners
      }
    }

    "be updated with an ExtResultDataConnection correctly" in {
      val extSimSetupData = ExtSimSetupData()

      val resultConnection =
        new ExtResultDataConnection(emptyMapResult, emptyMapResult, emptyMapResult)
      val resultRef = TestProbe("result_service").ref

      val updated = extSimSetupData.update(resultConnection, resultRef)

      updated.extSimAdapters shouldBe empty
      updated.extPrimaryDataServices shouldBe empty
      updated.extDataServices shouldBe empty
      updated.extResultListeners shouldBe Seq((resultConnection, resultRef))
    }

    "be updated with multiple different connections" in {
      val extSimSetupData = ExtSimSetupData()

      val primaryConnection = new ExtPrimaryDataConnection(emptyMapInput)
      val primaryRef = TestProbe("primary_service").ref.toClassic

      val evConnection = new ExtEvDataConnection()
      val evRef = TestProbe("ev_service").ref.toClassic

      val emConnection = new ExtEmDataConnection(emptyMapInput)
      val emRef = TestProbe("em_service").ref.toClassic

      val resultConnection =
        new ExtResultDataConnection(emptyMapResult, emptyMapResult, emptyMapResult)
      val resultRef = TestProbe("result_service").ref

      val updated = extSimSetupData
        .update(primaryConnection, primaryRef)
        .update(evConnection, evRef)
        .update(emConnection, emRef)
        .update(resultConnection, resultRef)

      updated.extSimAdapters shouldBe empty
      updated.extPrimaryDataServices shouldBe Seq(
        (
          primaryConnection,
          primaryRef,
        )
      )
      updated.extDataServices shouldBe Seq(
        (evConnection, evRef),
        (emConnection, emRef),
      )
      updated.extResultListeners shouldBe Seq((resultConnection, resultRef))
    }

    "return evDataService correctly" in {
      val evConnection = new ExtEvDataConnection()
      val evRef = TestProbe("ev_service").ref.toClassic

      val cases = Table(
        ("extSimSetupData", "expectedConnection", "expectedService"),
        (
          ExtSimSetupData().update(evConnection, evRef),
          Some(evConnection),
          Some(evRef),
        ),
        (ExtSimSetupData(), None, None),
      )

      forAll(cases) { (extSimSetupData, expectedConnection, expectedService) =>
        extSimSetupData.evDataConnection shouldBe expectedConnection
        extSimSetupData.evDataService shouldBe expectedService
      }
    }

    "return emDataService correctly" in {
      val emConnection = new ExtEmDataConnection(emptyMapInput)
      val emRef = TestProbe("em_service").ref.toClassic

      val cases = Table(
        ("extSimSetupData", "expectedConnection", "expectedService"),
        (
          ExtSimSetupData().update(emConnection, emRef),
          Some(emConnection),
          Some(emRef),
        ),
        (ExtSimSetupData(), None, None),
      )

      forAll(cases) { (extSimSetupData, expectedConnection, expectedService) =>
        extSimSetupData.emDataConnection shouldBe expectedConnection
        extSimSetupData.emDataService shouldBe expectedService
      }
    }

  }

}
