/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.util.UUID

import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.{TestKit, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.test.common.{TestKitWithShutdown, UnitSpec}
import edu.ie3.simona.test.common.model.grid.SubGridGateMokka

class ReceivedValuesStoreSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ReceivedValuesStoreSpec",
        ConfigFactory
          .parseString("""
            |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
            |akka.loglevel="OFF"
        """.stripMargin)
      )
    )
    with UnitSpec
    with SubGridGateMokka {

  // test actorRefs
  val actorProbe1: TestProbe = TestProbe()
  val actorProbe2: TestProbe = TestProbe()
  val actorProbe3: TestProbe = TestProbe()

  // test data used by almost all tests
  // / node to asset agents mapping
  val nodeToAssetAgentsMap: Map[UUID, Set[ActorRef]] = Map(
    UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546") -> Set(
      actorProbe1.ref
    ),
    UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97") -> Set(
      actorProbe2.ref
    )
  )

  // / subnet gate mapping for inferior grids
  val inferiorSubGridGateToActorRefMap: Map[SubGridGate, ActorRef] = Map(
    build2wSubGridGate(
      UUID.fromString("5cd55ab5-a7d2-499f-a25f-6dbc3845c5e8"),
      1,
      UUID.fromString("1676360a-c7c4-43a9-a667-90ddfe8a18e6"),
      2
    ) -> actorProbe3.ref
  )

  // / superior grid nodeUuid vector
  val superiorGridNodeUuids: Vector[UUID] = Vector(
    UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843")
  )

  "A ReceivedValueStore" must {

    "initialize an empty store correctly when everything is empty" in {

      val nodeToAssetAgentsMap = Map.empty[UUID, Set[ActorRef]]
      val inferiorSubGridGateToActorRefMap = Map.empty[SubGridGate, ActorRef]
      val superiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0
      receivedValuesStore.nodeToReceivedPower.size shouldBe 0

    }

    "initialize an empty store correctly when every data needed is provided correctly" in {

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids
        )

      receivedValuesStore.nodeToReceivedPower.size shouldBe 3
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(actorProbe1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(actorProbe2.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("5cd55ab5-a7d2-499f-a25f-6dbc3845c5e8")
      ) shouldBe Map(actorProbe3.ref -> None)

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 1
      receivedValuesStore.nodeToReceivedSlackVoltage(
        UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843")
      ) shouldBe None

    }

    "initialize an empty store correctly when only a valid mapping for asset agents is provided" in {

      val nodeToAssetAgentsMap =
        Map(
          UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546") -> Set(
            actorProbe1.ref
          ),
          UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97") -> Set(
            actorProbe2.ref,
            actorProbe3.ref
          )
        )

      val inferiorSubGridGateToActorRefMap = Map.empty[SubGridGate, ActorRef]
      val superiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 2
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(actorProbe1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(
        actorProbe2.ref -> None,
        actorProbe3.ref -> None
      )

    }

    "initialize an empty store correctly when only a valid mapping for asset agents and inferior grid agents is provided" in {

      val superiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 3
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(actorProbe1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(actorProbe2.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("5cd55ab5-a7d2-499f-a25f-6dbc3845c5e8")
      ) shouldBe Map(actorProbe3.ref -> None)

    }

    "initialize an empty store correctly when only information on the superior grid slack nodes are provided" in {

      val nodeToAssetAgentsMap = Map.empty[UUID, Set[ActorRef]]
      val inferiorSubGridGateToActorRefMap = Map.empty[SubGridGate, ActorRef]

      val superiorGridNodeUuids = Vector(
        UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843"),
        UUID.fromString("d5040bf7-56c1-4d6a-908a-47c05b0c5c54")
      )

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids
        )

      receivedValuesStore.nodeToReceivedPower.size shouldBe 0

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 2
      receivedValuesStore.nodeToReceivedSlackVoltage(
        UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843")
      ) shouldBe None
      receivedValuesStore.nodeToReceivedSlackVoltage(
        UUID.fromString("d5040bf7-56c1-4d6a-908a-47c05b0c5c54")
      ) shouldBe None

    }

    "initialize an empty store correctly when only an invalid mapping for asset agents with duplicates is provided" in {

      val inferiorSubGridGateToActorRefMap = Map.empty[SubGridGate, ActorRef]
      val superiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 2
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(actorProbe1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(actorProbe2.ref -> None)

    }

  }

}
