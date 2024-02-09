/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.integration

import java.io.File
import java.util.concurrent.LinkedBlockingQueue
import com.typesafe.config.{ConfigFactory, ConfigValueFactory}
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.PvResult
import edu.ie3.simona.config.{ConfigFailFast, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent._
import edu.ie3.simona.integration.common.IntegrationSpecCommon
import edu.ie3.simona.main.RunSimonaStandalone
import edu.ie3.simona.sim.setup.SimonaStandaloneSetup
import edu.ie3.simona.test.common.{IOTestCommons, UnitSpec}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.util.io.FileIOUtils
import org.scalatest.BeforeAndAfterAll

import scala.io.{BufferedSource, Source}
import scala.jdk.CollectionConverters._

class RunSimonaStandaloneIT
    extends IntegrationSpecCommon
    with UnitSpec
    with BeforeAndAfterAll
    with IOTestCommons {

  override def afterAll(): Unit = {
    FileIOUtils.deleteRecursively(testTmpDir)
  }

  "A simona standalone simulation" must {

    "run und produce results based on a valid config correctly" in {

      /* setup config */
      val parsedConfig =
        ConfigFactory
          .empty()
          .withValue(
            "simona.output.base.dir",
            ConfigValueFactory.fromAnyRef(testTmpDir),
          )
          .withValue(
            "simona.time.startDateTime",
            ConfigValueFactory.fromAnyRef("2011-01-01 00:00:00"),
          )
          .withValue(
            "simona.time.endDateTime",
            ConfigValueFactory.fromAnyRef("2011-01-01 02:00:00"),
          )
          .withFallback(
            ConfigFactory
              .parseString("""
                           |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
                           |pekko.loglevel="OFF"
                           |""".stripMargin)
          )
          .withFallback(ConfigFactory.parseFile(new File(configFile)))
          .withFallback(ConfigFactory.parseString(s"config=$configFile"))
          .resolve()

      /* validate config */
      val simonaConfig = SimonaConfig(parsedConfig)
      ConfigFailFast.check(simonaConfig)

      val resultFileHierarchy =
        SimonaStandaloneSetup.buildResultFileHierarchy(parsedConfig)

      val runtimeEventQueue = new LinkedBlockingQueue[RuntimeEvent]()

      val simonaStandaloneSetup = SimonaStandaloneSetup(
        parsedConfig,
        resultFileHierarchy,
        Some(runtimeEventQueue),
      )

      /* run simulation */
      RunSimonaStandalone.run(
        simonaStandaloneSetup
      )

      /* check the results */
      // check configs
      val configOutputDir = new File(resultFileHierarchy.configOutputDir)

      configOutputDir.isDirectory shouldBe true
      configOutputDir.listFiles.toVector.size shouldBe 1

      // check runtime event queue for the expected runtime events
      checkRuntimeEvents(runtimeEventQueue.asScala)

      // check result data
      // todo implement if valid result handling is implemented
      val pvResultFileContent = getFileSource(
        resultFileHierarchy,
        classOf[PvResult],
      ).getLines().toVector
      pvResultFileContent.size shouldBe 190
      pvResultFileContent.headOption.map(
        _.equals("uuid,inputModel,p,q,timestamp")
      )

    }

  }

  private def getFileSource(
      resultFileHierarchy: ResultFileHierarchy,
      entityClass: Class[_ <: ResultEntity],
  ): BufferedSource = {
    Source.fromFile(
      resultFileHierarchy.rawOutputDataFilePaths.getOrElse(
        entityClass,
        fail(s"Unable to get output path for result entity: $entityClass"),
      )
    )
  }

  private def checkRuntimeEvents(
      runtimeEvents: Iterable[RuntimeEvent]
  ): Unit = {
    runtimeEvents.toVector.size shouldBe 11
    val groupedRuntimeEvents = runtimeEvents.toVector.groupBy {
      case Initializing            => Initializing
      case InitComplete(_)         => InitComplete
      case Simulating(_, _)        => Simulating
      case CheckWindowPassed(_, _) => CheckWindowPassed
      case Done(_, _, _)           => Done
      case other                   => fail(s"Unexpected runtime event: $other")
    }

    groupedRuntimeEvents.size shouldBe 5
    groupedRuntimeEvents.keySet should contain allOf (Simulating, CheckWindowPassed, InitComplete, Initializing, Done)

    groupedRuntimeEvents
      .get(Simulating)
      .foreach(simulatingEvents => {
        simulatingEvents.size shouldBe 1
        simulatingEvents.headOption.foreach(_ shouldBe Simulating(0, 7200))
      })

    groupedRuntimeEvents
      .get(CheckWindowPassed)
      .foreach(checkWindowsPassed => {
        checkWindowsPassed.size shouldBe 7
        checkWindowsPassed.foreach {
          case CheckWindowPassed(tick, _) =>
            tick % 900L shouldBe 0 // config has 900 sec as check window value
          case invalidEvent =>
            fail(
              s"Invalid event when expecting CheckWindowPassed: $invalidEvent"
            )
        }
      })

    groupedRuntimeEvents
      .get(InitComplete)
      .foreach(initComplets => {
        initComplets.size shouldBe 1
      })

    groupedRuntimeEvents
      .get(Initializing)
      .foreach(initializings => {
        initializings.size shouldBe 1
      })

    groupedRuntimeEvents
      .get(Done)
      .foreach(dones => {
        dones.size shouldBe 1
        dones.headOption.foreach {
          case Done(tick, _, errorInSim) =>
            tick shouldBe 7200
            errorInSim shouldBe false
          case invalidEvent =>
            fail(s"Invalid event when expecting Done: $invalidEvent")
        }
      })
  }

}
