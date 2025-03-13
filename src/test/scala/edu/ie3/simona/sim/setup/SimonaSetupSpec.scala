/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.datamodel.exceptions.NotImplementedException
import edu.ie3.datamodel.models.input.connector.{
  ConnectorPort,
  Transformer3WInput,
}
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.simona.test.common.model.grid.SubGridGateMokka
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.nio.file.Path
import java.util.UUID

class SimonaSetupSpec
    extends UnitSpec
    with SimonaSetup
    with SubGridGateMokka
    with ConfigTestData {

  override val args: Array[String] = Array.empty[String]
  override val simonaConfig: SimonaConfig = SimonaConfig(typesafeConfig)

  override def logOutputDir: Path = throw new NotImplementedError()

  override def runtimeEventListener(
      context: ActorContext[_]
  ): ActorRef[RuntimeEventListener.Request] =
    throw new NotImplementedException(
      "This is a dummy setup"
    )

  override def resultEventListener(
      context: ActorContext[_]
  ): Seq[ActorRef[ResultEventListener.Request]] =
    throw new NotImplementedException("This is a dummy setup")

  override def primaryServiceProxy(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimSetupData: ExtSimSetupData,
  ): ClassicRef = throw new NotImplementedException("This is a dummy setup")

  override def weatherService(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[WeatherMessage] = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def extSimulations(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimPath: Option[Path],
  ): ExtSimSetupData = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def timeAdvancer(
      context: ActorContext[_],
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      runtimeEventListener: ActorRef[RuntimeEvent],
  ): ActorRef[TimeAdvancer.Request] = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def scheduler(
      context: ActorContext[_],
      timeAdvancer: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): ActorRef[SchedulerMessage] = throw new NotImplementedException(
    "This is a dummy setup"
  )

  override def gridAgents(
      context: ActorContext[_],
      environmentRefs: EnvironmentRefs,
      resultEventListeners: Seq[ActorRef[ResultEvent]],
  ): Iterable[ActorRef[GridAgent.Request]] =
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
