/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.graph.SubGridTopologyGraph
import edu.ie3.datamodel.models.input.container.{GridContainer, ThermalGrid}
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.actor.SimonaActorNaming.RichActorRefFactory
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.GridAgentMessages.CreateGridAgent
import edu.ie3.simona.config.{ArgsParser, RefSystemParser, SimonaConfig}
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.event.{ResultEvent, RuntimeEvent}
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.io.grid.GridProvider
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.ScheduleActivation
import edu.ie3.simona.scheduler.core.Core.CoreFactory
import edu.ie3.simona.scheduler.core.RegularSchedulerCore
import edu.ie3.simona.scheduler.{ScheduleLock, Scheduler, TimeAdvancer}
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.primary.PrimaryServiceProxy
import edu.ie3.simona.service.primary.PrimaryServiceProxy.InitPrimaryServiceProxyStateData
import edu.ie3.simona.service.results.ExtResultDataProvider
import edu.ie3.simona.service.weather.WeatherService
import edu.ie3.simona.service.weather.WeatherService.InitWeatherServiceStateData
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.simona.util.TickUtil.RichZonedDateTime
import edu.ie3.simpleextsim.SimpleExtSimulationWithEm
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.adapter.{ClassicActorRefOps, TypedActorContextOps, TypedActorRefOps}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.jdk.CollectionConverters._

/** Sample implementation to run a standalone simulation of simona configured
  * with the provided [[SimonaConfig]] and [[ResultFileHierarchy]]
  *
  * @version 0.1
  * @since 01.07.20
  */
class SimonaSimpleExtSimulationSetup(
                                      override val typeSafeConfig: Config,
                                      override val simonaConfig: SimonaConfig,
                                      override val resultFileHierarchy: ResultFileHierarchy,
                                      override val runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
                                      override val args: Array[String],
) extends SimonaExtSimSetup(
  typeSafeConfig,
  simonaConfig,
  resultFileHierarchy,
  runtimeEventQueue,
  args
) {

  override def extSimulations(
                               context: ActorContext[_],
                               scheduler: ActorRef[SchedulerMessage],
                             ): ExtSimSetupData = {
    val simpleExtSim = new SimpleExtSimulationWithEm()
    //val simpleExtSim = new SimpleExtSimulationWithPrimaryData()

    extSimulationSetup(
      context,
      scheduler,
      simpleExtSim
    )
  }
}

/** Companion object to provide [[SetupHelper]] methods for
  * [[SimonaStandaloneSetup]]
  */
object SimonaSimpleExtSimulationSetup extends LazyLogging with SetupHelper {

  def apply(
      typeSafeConfig: Config,
      resultFileHierarchy: ResultFileHierarchy,
      runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
      mainArgs: Array[String] = Array.empty[String],
  ): SimonaSimpleExtSimulationSetup =
    new SimonaSimpleExtSimulationSetup(
      typeSafeConfig,
      SimonaConfig(typeSafeConfig),
      resultFileHierarchy,
      runtimeEventQueue,
      mainArgs,
    )
}
