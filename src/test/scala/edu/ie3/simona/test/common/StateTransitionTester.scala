/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import akka.actor.FSM.{
  CurrentState,
  SubscribeTransitionCallBack,
  Transition,
  UnsubscribeTransitionCallBack
}
import akka.actor.{ActorRef, ActorSystem}
import akka.testkit.TestProbe
import org.scalatest.Assertion
import org.scalatest.matchers.should

import scala.concurrent.duration.Duration

/** Provides capabilities to assert state changes of [[akka.actor.FSM]]. Before
  * the expected state change happens, [[startListening()]] has to be called.
  * Afterwards, [[stopListening()]] should be called.
  *
  * @param fsmActorRef
  *   FSM ref of which state changes are anticipated
  * @param ignoreSameState
  *   Whether to ignore state changes S -> S. These kind of changes are then
  *   skipped, while waiting for an actual state change.
  * @param probe
  *   The test probe that is listening to changes
  */
class StateTransitionTester private (
    val fsmActorRef: ActorRef,
    val ignoreSameState: Boolean,
    val probe: TestProbe
) extends should.Matchers {

  /** Starts listening to state changes. Given expected state is compared to the
    * actual current state.
    * @param expectedCurrentState
    *   The expected current state
    */
  def startListening(expectedCurrentState: Any): Unit = {
    fsmActorRef ! SubscribeTransitionCallBack(probe.ref)
    probe.expectMsgPF() { case CurrentState(_, stateName) =>
      stateName shouldBe expectedCurrentState
    }
  }

  /** Starts listening to state changes.
    */
  def startListening(): Unit = {
    fsmActorRef ! SubscribeTransitionCallBack(probe.ref)
    probe.expectMsgType[CurrentState[_]]
  }

  /** Waits at maximum the given time duration for a state change to the given
    * new state.
    * @param expectedNewState
    *   New state that is expected.
    * @param max
    *   The maximum duration to wait for a state change.
    * @return
    *   The assertion comparing the expected to the actual state
    */
  def expectStateChange(
      expectedNewState: Any,
      max: Duration = Duration.Undefined
  ): Assertion =
    probe.fishForSpecificMessage(
      max,
      s"Waiting for state change of $fsmActorRef to $expectedNewState timed out."
    ) {
      case Transition(_, oldState, newState)
          if !ignoreSameState || oldState != newState =>
        newState shouldBe expectedNewState
    }

  /** Stops listening to state changes.
    */
  def stopListening(): Unit =
    fsmActorRef ! UnsubscribeTransitionCallBack(probe.ref)

}

object StateTransitionTester extends should.Matchers {

  /** Provides capabilities to assert state changes of [[akka.actor.FSM]].
    * Before the expected state change happens,
    * [[StateTransitionTester.startListening()]] has to be called. Afterwards,
    * [[StateTransitionTester.stopListening()]] should be called.
    *
    * @param fsmActorRef
    *   FSM ref of which state changes are anticipated
    * @param ignoreSameState
    *   Whether to ignore state changes S -> S. These kind of changes are then
    *   skipped, while waiting for an actual state change.
    */
  def apply(fsmActorRef: ActorRef, ignoreSameState: Boolean)(implicit
      system: ActorSystem
  ): StateTransitionTester = {
    new StateTransitionTester(fsmActorRef, ignoreSameState, TestProbe())
  }
}
