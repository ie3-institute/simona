/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.powerflow

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.data.GridAgentData.GridAgentRef
import edu.ie3.simona.agent.grid.powerflow.ReceivedValuesStore
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.SubGridGateMokka
import edu.ie3.util.scala.collection.immutable.RichMultiMap.MultiMap
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
  val gridAgent: TestProbe[GridAgent.Message] = TestProbe[GridAgent.Message]()

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
  val defaultInferiorConnections: MultiMap[GridAgentRef, UUID] = Map(
    gridAgent.ref -> Set(
      UUID.fromString("5cd55ab5-a7d2-499f-a25f-6dbc3845c5e8"),
      UUID.fromString("1676360a-c7c4-43a9-a667-90ddfe8a18e6"),
    )
  )

  // / superior grid nodeUuid set
  val defaultSuperiorGridNodeUuids: Set[UUID] = Set(
    UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843")
  )

  "A ReceivedValueStore" must {

    "initialize an empty store correctly when everything is empty" in {

      val emptyNodeToAssetAgentsMap =
        Map.empty[UUID, Set[ActorRef[ParticipantAgent.Request]]]
      val emptyInferiorConnections: MultiMap[GridAgentRef, UUID] = Map.empty
      val emptySuperiorGridNodeUuids = Set.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          emptyNodeToAssetAgentsMap,
          emptyInferiorConnections,
          emptySuperiorGridNodeUuids,
        )

      receivedValuesStore.slackVoltages.size shouldBe 0
      receivedValuesStore.nodeToReceivedPower.size shouldBe 0

    }

    "initialize an empty store correctly when every data needed is provided correctly" in {

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          defaultNodeToAssetAgentsMap,
          defaultInferiorConnections,
          defaultSuperiorGridNodeUuids,
        )

      receivedValuesStore.nodeToReceivedPower.size shouldBe 0
      receivedValuesStore.getExpectedPowerResponses shouldBe Set(
        participant1.ref,
        participant2.ref,
        gridAgent.ref,
      )
      receivedValuesStore.nodeToSlackVoltage.size shouldBe 1
      receivedValuesStore.getSlackVoltage(
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

      val inferiorConnections: MultiMap[GridAgentRef, UUID] = Map.empty
      val superiorGridNodeUuids = Set.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorConnections,
          superiorGridNodeUuids,
        )

      receivedValuesStore.slackVoltages.size shouldBe 0
      receivedValuesStore.nodeToReceivedPower.size shouldBe 0
      receivedValuesStore.getExpectedPowerResponses shouldBe Set(
        participant1.ref,
        participant2.ref,
        participant3.ref,
      )
    }

    "initialize an empty store correctly when only a valid mapping for asset agents and inferior grid agents is provided" in {

      val superiorGridNodeUuids = Set.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          defaultNodeToAssetAgentsMap,
          defaultInferiorConnections,
          superiorGridNodeUuids,
        )

      receivedValuesStore.slackVoltages.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 0
      receivedValuesStore.getExpectedPowerResponses shouldBe Set(
        participant1.ref,
        participant2.ref,
        gridAgent.ref,
      )

    }

    "initialize an empty store correctly when only information on the superior grid slack nodes are provided" in {

      val nodeToAssetAgentsMap =
        Map.empty[UUID, Set[ActorRef[ParticipantAgent.Request]]]
      val inferiorConnections: MultiMap[GridAgentRef, UUID] = Map.empty

      val superiorGridNodeUuids = Set(
        UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843"),
        UUID.fromString("d5040bf7-56c1-4d6a-908a-47c05b0c5c54"),
      )

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          nodeToAssetAgentsMap,
          inferiorConnections,
          superiorGridNodeUuids,
        )

      receivedValuesStore.nodeToReceivedPower.size shouldBe 0

      receivedValuesStore.nodeToSlackVoltage.size shouldBe 2
      receivedValuesStore.getSlackVoltage(
        UUID.fromString("baded8c4-b703-4316-b62f-75ffe09c9843")
      ) shouldBe None
      receivedValuesStore.getSlackVoltage(
        UUID.fromString("d5040bf7-56c1-4d6a-908a-47c05b0c5c54")
      ) shouldBe None

    }

    "initialize an empty store correctly when only an invalid mapping for asset agents with duplicates is provided" in {

      val inferiorConnections: MultiMap[GridAgentRef, UUID] = Map.empty
      val superiorGridNodeUuids = Set.empty[UUID]

      val receivedValuesStore =
        ReceivedValuesStore.empty(
          defaultNodeToAssetAgentsMap,
          inferiorConnections,
          superiorGridNodeUuids,
        )

      receivedValuesStore.slackVoltages.size shouldBe 0

      receivedValuesStore.nodeToReceivedPower.size shouldBe 0
      receivedValuesStore.getExpectedPowerResponses shouldBe Set(
        participant1.ref,
        participant2.ref,
      )
    }

  }

}
