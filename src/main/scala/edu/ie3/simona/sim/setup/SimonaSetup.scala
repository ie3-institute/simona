/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import akka.actor.typed.ActorRef
import akka.actor.{ActorContext, ActorRef => ClassicActorRef}
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.connector.Transformer3WInput
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim

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

  /** Creates a sequence of runtime event listeners
    *
    * @param context
    *   Actor context to use
    * @return
    *   A sequence of actor references to runtime event listeners
    */
  def runtimeEventListener(
      context: ActorContext
  ): Seq[ClassicActorRef]

  /** Creates a sequence of system participant event listeners
    *
    * @param context
    *   Actor context to use
    * @return
    *   A sequence of actor references to runtime event listeners
    */
  def systemParticipantsListener(
      context: ActorContext,
      simonaSim: ActorRef[SimonaSim.Request]
  ): Seq[ClassicActorRef]

  /** Creates a primary service proxy. The proxy is the first instance to ask
    * for primary data. If necessary, it delegates the registration request to
    * it's subordinate workers.
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @return
    *   An actor reference to the service as well as matching data to initialize
    *   the service
    */
  def primaryServiceProxy(
      context: ActorContext,
      scheduler: ClassicActorRef
  ): (ClassicActorRef, InitPrimaryServiceProxyStateData)

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
      scheduler: ClassicActorRef
  ): (ClassicActorRef, InitWeatherServiceStateData)

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
      scheduler: ClassicActorRef
  ): ExtSimSetupData

  /** Creates a scheduler service
    *
    * @param context
    *   Actor context to use
    * @return
    *   An actor reference to the scheduler
    */
  def scheduler(
      context: ActorContext,
      runtimeEventListener: Seq[ClassicActorRef]
  ): ClassicActorRef

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
      systemParticipantListener: Seq[ClassicActorRef]
  ): Map[ClassicActorRef, GridAgentInitData]

  /** SIMONA links sub grids connected by a three winding transformer a bit
    * different. Therefore, the internal node has to be set as superior node.
    * All other gates are left unchanged
    */
  protected val modifySubGridGateForThreeWindingSupport
      : SubGridGate => SubGridGate =
    (gate: SubGridGate) =>
      gate.getLink match {
        case transformer: Transformer3WInput =>
          new SubGridGate(
            transformer,
            transformer.getNodeInternal,
            gate.getInferiorNode
          )
        case _ => gate
      }
}
