/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.integration

import akka.actor.ActorSystem
import akka.util.Timeout
import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import edu.ie3.simona.config.{ConfigFailFast, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.integration.common.IntegrationSpecCommon
import edu.ie3.simona.main.RunSimonaStandalone
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.{IOTestCommons, UnitSpec}
import edu.ie3.util.io.FileIOUtils
import org.scalatest.BeforeAndAfterAll

import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import scala.concurrent.duration.DurationInt

class RunSimonaLocalIT
    extends IntegrationSpecCommon
    with UnitSpec
    with BeforeAndAfterAll
    with IOTestCommons {

  // simulation test runs in ~30 seconds on a fast laptop
  // this is not passed as implicit in order to enforce the argument in 'run'
  private val timeout: Timeout = 10.minutes

  override def afterAll(): Unit = {
    FileIOUtils.deleteRecursively(testTmpDir)
  }

  "A local simona simulation" must {

    "run und produce results based on a valid config correctly" in {

      /* setup config */
      val parsedConfig = getConfig

      runLocalSimulation(parsedConfig)
    }

    def getConfig =
      ConfigFactory
        .empty()
        .withValue(
          "akka.actor.provider",
          ConfigValueFactory.fromAnyRef("local")
        )
        .withValue(
          "simona.output.base.dir",
          ConfigValueFactory.fromAnyRef(testTmpDir)
        )
        .withValue(
          "simona.time.startDateTime",
          ConfigValueFactory.fromAnyRef("2011-01-01 00:00:00")
        )
        .withValue(
          "simona.time.endDateTime",
          ConfigValueFactory.fromAnyRef("2011-01-01 02:00:00")
        )
        .withValue(
          "simona.execution.computationMode",
          ConfigValueFactory.fromAnyRef("Local")
        )
        .withFallback(ConfigFactory.parseFile(new File(configFile)))
        .withFallback(ConfigFactory.parseString(s"config=$configFile"))
        .withFallback(
          ConfigFactory
            .parseString("""
                           |akka.loggers =["akka.event.slf4j.Slf4jLogger"]
                           |akka.loglevel="OFF"
                           |""".stripMargin)
        )
        .resolve()
  }

  private def runLocalSimulation(config: Config) = {
    /* validate config */
    val simonaConfig = SimonaConfig(config)
    ConfigFailFast.check(simonaConfig)

    val resultFileHierarchy =
      SimonaStandaloneSetup.buildResultFileHierarchy(config)

    val runtimeEventQueue = new LinkedBlockingQueue[RuntimeEvent]()

    val simonaSetup = SimonaStandaloneSetup(
      simonaConfig,
      resultFileHierarchy,
      Some(runtimeEventQueue),
      Array.empty[String]
    )

    /* run simulation */
    RunSimonaStandalone.run(simonaSetup, ActorSystem("simona", config))(timeout)

    checkResults(resultFileHierarchy, runtimeEventQueue)
  }
}
