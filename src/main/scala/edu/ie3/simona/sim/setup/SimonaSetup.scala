/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.connector.Transformer3WInput
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.services.LoadProfileMessage
import edu.ie3.simona.ontology.messages.services.{
  ServiceMessage,
  WeatherMessage,
}
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.sim.SimonaSim
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.nio.file.Path

/** Trait that can be used to set up a customized simona simulation by providing
  * implementations for all setup information required by a
  * [[edu.ie3.simona.sim.SimonaSim]]. Most of the time, using or extending
  * [[SimonaStandaloneSetup]] might be considered instead of providing your own
  * implementation for all methods.
  *
  * @version 0.1
  * @since 01.07.20
  */
trait SimonaSetup {

  val simonaConfig: SimonaConfig

  /** Main arguments of the executable. May be used to pass additional
    * configuration parameters to the setup e.g. for external simulation
    * configuration
    */
  val args: Array[String]

  /** Directory of the log output.
    */
  def logOutputDir: Path

  /** Creates the runtime event listener
    *
    * @param context
    *   Actor context to use
    * @return
    *   An actor reference to the runtime event listener
    */
  def runtimeEventListener(
      context: ActorContext[_]
  ): ActorRef[RuntimeEventListener.Request]

  /** Creates a sequence of result event listeners
    *
    * @param context
    *   Actor context to use
    * @return
    *   A sequence of actor references to result event listeners
    */
  def resultEventListener(
      context: ActorContext[_],
      extSimSetupData: ExtSimSetupData,
  ): Seq[ActorRef[ResultEventListener.Request]]

  /** Creates a primary service proxy. The proxy is the first instance to ask
    * for primary data. If necessary, it delegates the registration request to
    * it's subordinate workers.
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @param extSimSetupData
    *   that can contain external
    *   [[edu.ie3.simona.api.data.primarydata.ExtPrimaryDataConnection]]
    * @return
    *   An actor reference to the service
    */
  def primaryServiceProxy(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimSetupData: ExtSimSetupData,
  ): ActorRef[ServiceMessage]

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
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[WeatherMessage]

  /** Creates a load profile service
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to it's according scheduler to use
    * @return
    *   An actor reference to the service as well as matching data to initialize
    *   the service
    */
  def loadProfileService(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[LoadProfileMessage]

  /** Loads external simulations and provides corresponding actors and init data
    *
    * @param context
    *   Actor context to use
    * @param scheduler
    *   Actor reference to the scheduler to use
    * @param extSimPath
    *   option for a directory with external simulations
    * @return
    *   External simulations and their init data
    */
  def extSimulations(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
      extSimPath: Option[Path],
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
      context: ActorContext[_],
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      runtimeEventListener: ActorRef[RuntimeEvent],
  ): ActorRef[TimeAdvancer.Request]

  /** Creates a scheduler service
    *
    * @param context
    *   Actor context to use
    * @param parent
    *   The parent scheduler, which could be a time advancer
    * @param coreFactory
    *   The factory creating a scheduler core that determines the scheduler's
    *   behavior, defaulting to a regular scheduler
    * @return
    *   An actor reference to the scheduler
    */
  def scheduler(
      context: ActorContext[_],
      parent: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): ActorRef[SchedulerMessage]

  /** Creates all the needed grid agents
    *
    * @param context
    *   Actor context to use
    * @param environmentRefs
    *   EnvironmentRefs to use
    * @param resultEventListeners
    *   Listeners that await events from system participants
    * @return
    *   A mapping from actor reference to it's according initialization data to
    *   be used when setting up the agents
    */
  def gridAgents(
      context: ActorContext[_],
      environmentRefs: EnvironmentRefs,
      resultEventListeners: Seq[ActorRef[ResultEvent]],
  ): Iterable[ActorRef[GridAgent.Request]]

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
