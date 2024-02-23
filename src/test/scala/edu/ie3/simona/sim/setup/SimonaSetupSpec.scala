/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import org.apache.pekko.actor.{
  ActorSystem,
  ActorContext,
  ActorRef => ClassicRef,
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
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.SubGridGateMokka
import org.apache.pekko.actor.typed.scaladsl
import org.apache.pekko.actor.{ActorRef, typed}

import java.util.UUID

class SimonaSetupSpec extends UnitSpec with SimonaSetup with SubGridGateMokka {

  override val args: Array[String] = Array.empty[String]

  override def runtimeEventListener(
      context: scaladsl.ActorContext[_]
  ): typed.ActorRef[RuntimeEventListener.Request] = // todo typed
    throw new NotImplementedException(
      "This is a dummy setup"
    )

  override def resultEventListener(
      context: scaladsl.ActorContext[_]
  ): Seq[typed.ActorRef[ResultEventListener.Request]] =
    throw new NotImplementedException("This is a dummy setup")

  override def primaryServiceProxy(
      context: scaladsl.ActorContext[_],
      scheduler: typed.ActorRef[SchedulerMessage],
  ): ActorRef = throw new NotImplementedException("This is a dummy setup")

  override def weatherService(
      context: scaladsl.ActorContext[_],
      scheduler: typed.ActorRef[SchedulerMessage],
  ): ActorRef = throw new NotImplementedException("This is a dummy setup")

  override def extSimulations(
      context: scaladsl.ActorContext[_],
      scheduler: typed.ActorRef[SchedulerMessage],
  ): ExtSimSetupData = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def timeAdvancer(
      context: scaladsl.ActorContext[_],
      simulation: typed.ActorRef[SimonaSim.SimulationEnded.type],
      runtimeEventListener: typed.ActorRef[RuntimeEvent],
  ): typed.ActorRef[TimeAdvancer.Request] = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def scheduler(
      context: scaladsl.ActorContext[_],
      timeAdvancer: typed.ActorRef[TimeAdvancer.Request],
  ): typed.ActorRef[SchedulerMessage] = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def gridAgents(
      context: scaladsl.ActorContext[_],
      environmentRefs: EnvironmentRefs,
      systemParticipantListener: Seq[ClassicRef],
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
