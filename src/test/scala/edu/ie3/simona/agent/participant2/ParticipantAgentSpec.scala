/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant2

import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.model.participant2.ParticipantModel.FixedState
import edu.ie3.simona.model.participant2.ParticipantModelShell
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.OperationInterval
import org.apache.pekko.actor.testkit.typed.scaladsl.ScalaTestWithActorTestKit
import org.apache.pekko.actor.typed.ActorRef

import java.time.ZonedDateTime

class ParticipantAgentSpec extends ScalaTestWithActorTestKit with UnitSpec {

  private val simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z")

  "A ParticipantAgent without services" in {

    val scheduler = createTestProbe[SchedulerMessage]()
    val gridAgent = createTestProbe[GridAgent.Request]()
    val resultListener = createTestProbe[ResultEvent]()

    val receiveAdapter = createTestProbe[ActorRef[Activation]]()

    val participantAgent = spawn(
      ParticipantAgentMockFactory.create(
        ParticipantModelShell(
          new MockParticipantModel(),
          OperationInterval(0, 24 * 60 * 60),
          simulationStartDate,
          FixedState(-1),
        ),
        ParticipantInputHandler(
          Map.empty
        ),
        ParticipantGridAdapter(
          gridAgent.ref,
          3600,
        ),
        Iterable(resultListener.ref),
        Left(scheduler.ref, receiveAdapter.ref),
      )
    )
    val activationRef = receiveAdapter.expectMessageType[ActorRef[Activation]]

    activationRef ! Activation(0)

    // fixme
    // scheduler.expectMessage(Completion(activationRef))

  }

}
