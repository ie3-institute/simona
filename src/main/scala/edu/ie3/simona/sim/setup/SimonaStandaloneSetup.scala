/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.sim.setup.ExtSimSetup.setupExtSim
import edu.ie3.simona.util.ResultFileHierarchy
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext

import java.util.concurrent.LinkedBlockingQueue

/** Sample implementation to run a standalone simulation of simona configured
  * with the provided [[SimonaConfig]] and [[ResultFileHierarchy]]
  *
  * @version 0.1
  * @since 01.07.20
  */
class SimonaStandaloneSetup(
    val typeSafeConfig: Config,
    override val simonaConfig: SimonaConfig,
    override val resultFileHierarchy: ResultFileHierarchy,
    override val runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] =
      None,
    override val args: Array[String],
) extends SimonaSetup {

  override def extSimulations(
      context: ActorContext[_],
      scheduler: ActorRef[SchedulerMessage],
  ): ExtSimSetupData = {
    val jars = ExtSimLoader.scanInputFolder()

    val extLinks = jars.flatMap(ExtSimLoader.loadExtLink).toList

    setupExtSim(extLinks.map(_.getExtSimulation), args)(
      context,
      scheduler,
      simonaConfig,
    )
  }
}

/** Companion object to provide [[SetupHelper]] methods for
  * [[SimonaStandaloneSetup]]
  */
object SimonaStandaloneSetup extends LazyLogging with SetupHelper {

  def apply(
      typeSafeConfig: Config,
      resultFileHierarchy: ResultFileHierarchy,
      runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]] = None,
      mainArgs: Array[String] = Array.empty[String],
  ): SimonaStandaloneSetup =
    new SimonaStandaloneSetup(
      typeSafeConfig,
      SimonaConfig(typeSafeConfig),
      resultFileHierarchy,
      runtimeEventQueue,
      mainArgs,
    )
}
