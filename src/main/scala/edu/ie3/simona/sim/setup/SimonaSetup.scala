/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import org.apache.pekko.actor.{ActorContext, ActorRef, ActorSystem}
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.connector.Transformer3WInput
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.scheduler.TimeAdvancer

/** Trait that can be used to setup a customized simona simulation by providing
  * implementations for all setup information required by a
  * [[edu.ie3.simona.sim.SimonaSim]]. Most of the time, using or extending
  * [[SimonaStandaloneSetup]] might be considered instead of providing a your
  * own implementation for all methods.
  *
  * @version 0.1
  * @since 01.07.20
  */
trait SimonaSetup {

  /** Main arguments of the executable. May be used to pass additional
    * configuration parameters to the setup e.g. for external simulation
    * configuration
    */
  val args: Array[String]

  /** A function, that constructs the [[ActorSystem]], the simulation shall live
    * in
    */
  val buildActorSystem: () => ActorSystem

  /** Creates the runtime event listener
    *
    * @param context
    *   Actor context to use
    * @return
    *   An actor reference to the runtime event listener
    */
  def runtimeEventListener(
      context: ActorContext
  ): org.apache.pekko.actor.typed.ActorRef[RuntimeEvent]

  /** Creates a sequence of system participant event listeners
    *
    * @param context
    *   Actor context to use
    * @return
    *   A sequence of actor references to runtime event listeners
    */
  def systemParticipantsListener(
      context: ActorContext,
      extSimulationData: ExtSimSetupData,
  ): Seq[ActorRef]

  /** Creates a primary service proxy. The proxy is the first instance to ask
    * for primary data. If necessary, it delegates the registration request to
    * it's subordinate workers.
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @return
    *   An actor reference to the service
    */
  def primaryServiceProxy(
      context: ActorContext,
      scheduler: ActorRef,
      extSimulationData: ExtSimSetupData,
  ): ActorRef

  /** Creates a weather service
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @return
    *   An actor reference to the service as well as matching data to initialize
    *   the service
    */
  def weatherService(
      context: ActorContext,
      scheduler: ActorRef,
  ): ActorRef

  /** Loads external simulations and provides corresponding actors and init data
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @return
    *   External simulations and their init data
    */
  def extSimulations(
      context: ActorContext,
      scheduler: ActorRef,
  ): ExtSimSetupData

  /** Creates the time advancer
    *
    * @param context
    *   Actor context to use
    * @param simulation
    *   The simulation root actor ([[edu.ie3.simona.sim.SimonaSim]])
    * @param runtimeEventListener
    *   Runtime event listener
    * @return
    *   An actor reference to the time advancer
    */
  def timeAdvancer(
      context: ActorContext,
      simulation: ActorRef,
      runtimeEventListener: org.apache.pekko.actor.typed.ActorRef[RuntimeEvent],
  ): org.apache.pekko.actor.typed.ActorRef[TimeAdvancer.Incoming]

  /** Creates a scheduler service
    *
    * @param context
    *   Actor context to use
    * @param timeAdvancer
    *   The time advancer, sitting at the root of the scheduler hierarchy
    * @return
    *   An actor reference to the scheduler
    */
  def scheduler(
      context: ActorContext,
      timeAdvancer: org.apache.pekko.actor.typed.ActorRef[TimeAdvancer.Incoming],
  ): ActorRef

  /** Creates all the needed grid agents
    *
    * @param context
    *   Actor context to use
    * @param environmentRefs
    *   EnvironmentRefs to use
    * @param systemParticipantListener
    *   Listeners that await events from system participants
    * @return
    *   A mapping from actor reference to it's according initialization data to
    *   be used when setting up the agents
    */
  def gridAgents(
      context: ActorContext,
      environmentRefs: EnvironmentRefs,
      systemParticipantListener: Seq[ActorRef],
  ): Iterable[ActorRef]

  /** SIMONA links sub grids connected by a three winding transformer a bit
    * different. Therefore, the internal node has to be set as superior node.
    * All other gates are left unchanged
    */
  protected val modifySubGridGateForThreeWindingSupport
      : SubGridGate => SubGridGate =
    (gate: SubGridGate) =>
      gate.link match {
        case transformer: Transformer3WInput =>
          new SubGridGate(
            transformer,
            transformer.getNodeInternal,
            gate.inferiorNode,
          )
        case _ => gate
      }
}
