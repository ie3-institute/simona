/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import org.apache.pekko.actor.{
  ActorContext,
  ActorRef => classicRef,
  ActorSystem,
}
import org.apache.pekko.actor.typed.ActorRef
import edu.ie3.datamodel.exceptions.NotImplementedException
import edu.ie3.datamodel.models.input.connector.{
  ConnectorPort,
  Transformer3WInput,
}
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessage
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.SubGridGateMokka

import java.util.UUID

class SimonaSetupSpec extends UnitSpec with SimonaSetup with SubGridGateMokka {

  override val args: Array[String] = Array.empty[String]

  override val buildActorSystem: () => ActorSystem = () =>
    throw new NotImplementedException("This is a dummy setup")

  override def runtimeEventListener(
      context: ActorContext
  ): ActorRef[RuntimeEvent] =
    throw new NotImplementedException("This is a dummy setup")

  override def systemParticipantsListener(
      context: ActorContext
  ): Seq[classicRef] = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def primaryServiceProxy(
      context: ActorContext,
      scheduler: classicRef,
  ): classicRef =
    throw new NotImplementedException("This is a dummy setup")

  override def weatherService(
      context: ActorContext,
      scheduler: classicRef,
  ): classicRef =
    throw new NotImplementedException("This is a dummy setup")

  override def extSimulations(
      context: ActorContext,
      scheduler: classicRef,
  ): ExtSimSetupData =
    throw new NotImplementedException("This is a dummy setup")

  override def timeAdvancer(
      context: ActorContext,
      simulation: classicRef,
      runtimeEventListener: ActorRef[RuntimeEvent],
  ): ActorRef[TimeAdvancer.Incoming] =
    throw new NotImplementedException("This is a dummy setup")

  override def scheduler(
      context: ActorContext,
      timeAdvancer: ActorRef[TimeAdvancer.Incoming],
  ): classicRef = throw new NotImplementedException("This is a dummy setup")

  override def gridAgents(
      context: ActorContext,
      environmentRefs: EnvironmentRefs,
      systemParticipantListener: Seq[classicRef],
  ): Iterable[ActorRef[GridAgentMessage]] =
    throw new NotImplementedException("This is a dummy setup")

  "Attempting to modify a sub grid gate" should {
    val nodeAUuid = UUID.randomUUID()
    val nodeBUuid = UUID.randomUUID()
    val nodeCUuid = UUID.randomUUID()

    "not touch a gate, that contains a two winding transformer" in {
      val subGridGate = build2wSubGridGate(nodeAUuid, 1, nodeBUuid, 2)

      modifySubGridGateForThreeWindingSupport.apply(
        subGridGate
      ) shouldBe subGridGate
    }

    "modifies a gate, that contains a three winding transformer" in {
      val subGridGate = build3wSubGridGate(
        nodeAUuid,
        1,
        nodeBUuid,
        2,
        nodeCUuid,
        3,
        ConnectorPort.C,
      )
      val internalNode = subGridGate.link match {
        case input: Transformer3WInput => input.getNodeInternal
        case _                         => fail("Got wrong link")
      }

      val alteredGate =
        modifySubGridGateForThreeWindingSupport.apply(subGridGate)
      alteredGate.superiorNode shouldBe internalNode
      alteredGate.getSuperiorSubGrid shouldBe subGridGate.getSuperiorSubGrid
      alteredGate.inferiorNode shouldBe subGridGate.inferiorNode
      alteredGate.getInferiorSubGrid shouldBe subGridGate.getInferiorSubGrid
    }
  }
}
