/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.SubGridGateMokka
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef

import java.util.UUID

class ReceivedValuesStoreSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with SubGridGateMokka {

  // test actorRefs
  val participant1: TestProbe[ParticipantAgent.Request] =
    TestProbe[ParticipantAgent.Request]()
  val participant2: TestProbe[ParticipantAgent.Request] =
    TestProbe[ParticipantAgent.Request]()
  val participant3: TestProbe[ParticipantAgent.Request] =
    TestProbe[ParticipantAgent.Request]()
  val gridAgent: TestProbe[GridAgent.Request] = TestProbe[GridAgent.Request]()

  // test data used by almost all tests
  // / node to asset agents mapping
  val defaultNodeToAssetAgentsMap
      : Map[UUID, Set[ActorRef[ParticipantAgent.Request]]] =
    Map(
      UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546") -> Set(
        participant1.ref
      ),
      UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97") -> Set(
        participant2.ref
      ),
    )

  // / subnet gate mapping for inferior grids
  val defaultInferiorSubGridGateToActorRefMap
      : Map[SubGridGate, ActorRef[GridAgent.Request]] = Map(
    build2wSubGridGate(
      UUID.fromString("5cd55ab5-a7d2-499f-a25f-6dbc3845c5e8"),
      1,
      UUID.fromString("1676360a-c7c4-43a9-a667-90ddfe8a18e6"),
      2,
    ) -> gridAgent.ref
  )

  // / superior grid nodeUuid vector
  val defaultSuperiorGridNodeUuids: Vector[UUID] = Vector(
    UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843")
  )

  "A ReceivedValueStore" must {

    "initialize an empty store correctly when everything is empty" in {

      val emptyNodeToAssetAgentsMap =
        Map.empty[UUID, Set[ActorRef[ParticipantAgent.Request]]]
      val emptyInferiorSubGridGateToActorRefMap =
        Map.empty[SubGridGate, ActorRef[GridAgent.Request]]
      val emptySuperiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          emptyNodeToAssetAgentsMap,
          emptyInferiorSubGridGateToActorRefMap,
          emptySuperiorGridNodeUuids,
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0
      receivedValuesStore.nodeToReceivedPower.size shouldBe 0

    }

    "initialize an empty store correctly when every data needed is provided correctly" in {

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          defaultNodeToAssetAgentsMap,
          defaultInferiorSubGridGateToActorRefMap,
          defaultSuperiorGridNodeUuids,
        )

      receivedValuesStore.nodeToReceivedPower.size shouldBe 3
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(participant1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(participant2.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("5cd55ab5-a7d2-499f-a25f-6dbc3845c5e8")
      ) shouldBe Map(gridAgent.ref -> None)

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 1
      receivedValuesStore.nodeToReceivedSlackVoltage(
        UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843")
      ) shouldBe None

    }

    "initialize an empty store correctly when only a valid mapping for asset agents is provided" in {

      val nodeToAssetAgentsMap =
        Map(
          UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546") -> Set(
            participant1.ref
          ),
          UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97") -> Set(
            participant2.ref,
            participant3.ref,
          ),
        )

      val inferiorSubGridGateToActorRefMap =
        Map.empty[SubGridGate, ActorRef[GridAgent.Request]]
      val superiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids,
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 2
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(participant1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(
        participant2.ref -> None,
        participant3.ref -> None,
      )

    }

    "initialize an empty store correctly when only a valid mapping for asset agents and inferior grid agents is provided" in {

      val superiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          defaultNodeToAssetAgentsMap,
          defaultInferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids,
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 3
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(participant1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(participant2.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("5cd55ab5-a7d2-499f-a25f-6dbc3845c5e8")
      ) shouldBe Map(gridAgent.ref -> None)

    }

    "initialize an empty store correctly when only information on the superior grid slack nodes are provided" in {

      val nodeToAssetAgentsMap =
        Map.empty[UUID, Set[ActorRef[ParticipantAgent.Request]]]
      val inferiorSubGridGateToActorRefMap =
        Map.empty[SubGridGate, ActorRef[GridAgent.Request]]

      val superiorGridNodeUuids = Vector(
        UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843"),
        UUID.fromString("d5040bf7-56c1-4d6a-908a-47c05b0c5c54"),
      )

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids,
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

      val inferiorSubGridGateToActorRefMap =
        Map.empty[SubGridGate, ActorRef[GridAgent.Request]]
      val superiorGridNodeUuids = Vector.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          defaultNodeToAssetAgentsMap,
          inferiorSubGridGateToActorRefMap,
          superiorGridNodeUuids,
        )

      receivedValuesStore.nodeToReceivedSlackVoltage.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 2
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("dd9a5b54-94bb-4201-9108-2b1b7d689546")
      ) shouldBe Map(participant1.ref -> None)
      receivedValuesStore.nodeToReceivedPower(
        UUID.fromString("34e807f1-c62b-4968-b0f6-980ce500ff97")
      ) shouldBe Map(participant2.ref -> None)

    }

  }

}
