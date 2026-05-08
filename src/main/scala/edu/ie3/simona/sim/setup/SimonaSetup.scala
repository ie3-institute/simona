/*
 * © 2020-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  ThermalGrid,
}
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentCoordinator
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.listener.{ResultListener, RuntimeEventListener}
import edu.ie3.simona.io.grid.GridProvider
import edu.ie3.simona.ontology.messages.ResultMessage.ResultResponse
import edu.ie3.simona.ontology.messages.{SchedulerMessage, ServiceMessage}
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.service.results.ResultServiceProxy
import edu.ie3.simona.sim.SimonaSim
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.nio.file.Path
import java.time.ZonedDateTime

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
    * configuration.
    */
  val args: Array[String]

  /** The electrical grid.
    */
  lazy val grid: JointGridContainer = GridProvider.gridFromConfig(
    simonaConfig.simulationName,
    simonaConfig.input.grid.datasource,
  )

  /** Map: thermal bus to thermal grid.
    */
  lazy val thermalGridsByThermalBus: Map[ThermalBusInput, ThermalGrid] =
    GridProvider.getThermalGridsFromConfig(simonaConfig.input.grid.datasource)

  lazy val baseInputPath: Path = Path.of(simonaConfig.input.baseInputDir)

  /** Directory of the log output.
    */
  def logOutputDir: Path

  /** Creates the runtime event listener.
    *
    * @param context
    *   Actor context to use.
    * @return
    *   An actor reference to the runtime event listener.
    */
  def runtimeEventListener(
      context: ActorContext[?]
  ): ActorRef[RuntimeEventListener.Request]

  /** Creates a sequence of result event listeners.
    *
    * @param context
    *   Actor context to use.
    * @return
    *   A sequence of actor references to result event listeners.
    */
  def resultEventListener(
      context: ActorContext[?]
  ): Seq[ActorRef[ResultListener.Message]]

  /** Creates a primary service proxy. The proxy is the first instance to ask
    * for primary data. If necessary, it delegates the registration request to
    * it's subordinate workers.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @param extSimSetupData
    *   that can contain external
    *   [[edu.ie3.simona.api.data.connection.ExtPrimaryDataConnection]].
    * @return
    *   An actor reference to the service.
    */
  def primaryServiceProxy(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      extSimSetupData: ExtSimSetupData,
  ): ActorRef[ServiceMessage]

  /** Creates a result service proxy. The proxy will receive information about
    * the result that should be expected for the current tick and all result
    * events that are send by the agents. The proxy is responsible for
    * processing the result events and passing the processed data to the
    * different result listeners and providers.
    *
    * @param context
    *   Actor context to use.
    * @param listeners
    *   The internal result event listeners.
    * @param simStartTime
    *   The start time of the simulation.
    * @return
    *   An actor reference to the service.
    */
  def resultServiceProxy(
      context: ActorContext[?],
      listeners: Seq[ActorRef[ResultResponse]],
      simStartTime: ZonedDateTime,
  ): ActorRef[ResultServiceProxy.Message]

  /** Creates a weather service.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @return
    *   An actor reference to the service.
    */
  def weatherService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[ServiceMessage]

  /** Creates an energy price service, if such service is configured.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @return
    *   An actor reference to the service.
    */
  def priceService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): Option[ActorRef[ServiceMessage]]

  /** Creates a load profile service.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to it's according scheduler to use.
    * @return
    *   An actor reference to the service as well as matching data to initialize
    *   the service.
    */
  def loadProfileService(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
  ): ActorRef[ServiceMessage]

  /** Loads external simulations and provides corresponding actors and init
    * data.
    *
    * @param context
    *   Actor context to use.
    * @param scheduler
    *   Actor reference to the scheduler to use.
    * @param resultProxy
    *   Actor reference to the result provider.
    * @param extSimPath
    *   Option for a directory with external simulations.
    * @return
    *   External simulations and their init data.
    */
  def extSimulations(
      context: ActorContext[?],
      scheduler: ActorRef[SchedulerMessage],
      resultProxy: ActorRef[ResultServiceProxy.Message],
      extSimPath: Option[Path],
  ): ExtSimSetupData

  /** Creates the time advancer.
    *
    * @param context
    *   Actor context to use.
    * @param simulation
    *   The simulation root actor ([[edu.ie3.simona.sim.SimonaSim]]).
    * @param runtimeEventListener
    *   Runtime event listener.
    * @return
    *   An actor reference to the time advancer.
    */
  def timeAdvancer(
      context: ActorContext[?],
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      runtimeEventListener: ActorRef[RuntimeEvent],
  ): ActorRef[TimeAdvancer.Request]

  /** Creates a scheduler service.
    *
    * @param context
    *   Actor context to use.
    * @param parent
    *   The parent scheduler, which could be a time advancer.
    * @param coreFactory
    *   The factory creating a scheduler core that determines the scheduler's
    *   behavior, defaulting to a regular scheduler.
    * @return
    *   An actor reference to the scheduler.
    */
  def scheduler(
      context: ActorContext[?],
      parent: ActorRef[SchedulerMessage],
      coreFactory: CoreFactory = RegularSchedulerCore,
  ): ActorRef[SchedulerMessage]

  /** Creates the grid agent coordinator which will create and coordinate all
    * grid agents.
    *
    * @param context
    *   Actor context to use.
    * @param environmentRefs
    *   EnvironmentRefs to use.
    * @return
    *   The reference to the [[GridAgentCoordinator]].
    */
  def gridAgentCoordinator(using
      context: ActorContext[?],
      environmentRefs: EnvironmentRefs,
  ): ActorRef[GridAgentCoordinator.Message]
}
