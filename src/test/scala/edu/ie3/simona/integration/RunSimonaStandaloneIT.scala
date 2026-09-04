/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.integration

import com.typesafe.config.{Config, ConfigFactory, ConfigValueFactory}
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.ResultEntitySource
import edu.ie3.datamodel.io.source.csv.CsvDataSource
import edu.ie3.datamodel.models.result.connector.{
  ConnectorResult,
  SwitchResult,
  Transformer3WResult,
  TransformerResult,
}
import edu.ie3.datamodel.models.result.system.*
import edu.ie3.datamodel.models.result.{
  CongestionResult,
  NodeResult,
  ResultEntity,
}
import edu.ie3.simona.config.{ConfigFailFast, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.*
import edu.ie3.simona.integration.common.IntegrationSpecCommon
import edu.ie3.simona.main.RunSimonaStandalone
import edu.ie3.simona.ontology.messages.ResultMessage
import edu.ie3.simona.ontology.messages.ResultMessage.ResultResponse
import edu.ie3.simona.service.results.ResultServiceProxy.Message
import edu.ie3.simona.sim.setup.SimonaSetup
import edu.ie3.simona.test.common.{IOTestCommons, UnitSpec}
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.util.io.FileIOUtils
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.scalatest.BeforeAndAfterAll

import java.io.File
import java.nio.file.Path
import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.mutable
import scala.io.{BufferedSource, Source}
import scala.jdk.CollectionConverters.*

class RunSimonaStandaloneIT
    extends IntegrationSpecCommon
    with UnitSpec
    with BeforeAndAfterAll
    with IOTestCommons {

  override def afterAll(): Unit = {
    FileIOUtils.deleteRecursively(testTmpDir)
  }

  private given tolerance: Double = 1e-7

  // small test setup that adds a new listener
  final class Setup(
      override val typeSafeConfig: Config,
      override val simonaConfig: SimonaConfig,
      override val args: Array[String] = Array.empty[String],
      override val runtimeEventQueue: Option[LinkedBlockingQueue[RuntimeEvent]],
      val actualResults: mutable.Map[
        (UUID, ZonedDateTime, Class[? <: ResultEntity]),
        ResultEntity,
      ] = mutable.Map.empty,
  ) extends SimonaSetup(typeSafeConfig, simonaConfig, args, runtimeEventQueue) {

    override def resultServiceProxy(
        context: ActorContext[?],
        listeners: Seq[ActorRef[ResultMessage.ResultResponse]],
        simStartTime: ZonedDateTime,
    ): ActorRef[Message] = {
      val behavior = Behaviors.receiveMessage[ResultResponse] { msg =>
        msg.results.values.flatten.foreach { res =>
          actualResults.put((res.getInputModel, res.getTime, res.getClass), res)
        }

        Behaviors.same
      }

      val ref = context.spawn(behavior, "listener")

      super.resultServiceProxy(context, listeners :+ ref, simStartTime)
    }
  }

  "A simona standalone simulation" must {

    val expectedResultSource = new CsvDataSource(
      ",",
      Path.of(this.getClass.getResource("").getPath),
      new FileNamingStrategy(),
    )
    val expectedResults
        : Map[(UUID, ZonedDateTime, Class[? <: ResultEntity]), ResultEntity] = {
      val sourse = new ResultEntitySource(expectedResultSource)
      val tmp = mutable.Map
        .empty[(UUID, ZonedDateTime, Class[? <: ResultEntity]), ResultEntity]

      def add(res: ResultEntity): Unit =
        tmp.put((res.getInputModel, res.getTime, res.getClass), res)

      sourse.getNodeResults.forEach(add)
      sourse.getNodeResults.forEach(add)
      sourse.getSwitchResults.forEach(add)
      sourse.getLineResults.forEach(add)
      sourse.getTransformer2WResultResults.forEach(add)
      sourse.getTransformer3WResultResults.forEach(add)
      sourse.getPowerLimitFlexOptionsResults.forEach(add)
      sourse.getEnergyBoundariesFlexOptionsResults.forEach(add)
      sourse.getLoadResults.forEach(add)
      sourse.getPvResults.forEach(add)
      sourse.getFixedFeedInResults.forEach(add)
      sourse.getBmResults.forEach(add)
      sourse.getChpResults.forEach(add)
      sourse.getWecResults.forEach(add)
      sourse.getStorageResults.forEach(add)
      sourse.getEvcsResults.forEach(add)
      sourse.getEvResults.forEach(add)
      sourse.getAcResults.forEach(add)
      sourse.getHpResults.forEach(add)
      sourse.getCylindricalStorageResult.forEach(add)
      sourse.getDomesticHotWaterStorageResult.forEach(add)
      sourse.getThermalHouseResults.forEach(add)
      sourse.getEmResults.forEach(add)
      sourse.getCongestionResults.forEach(add)

      tmp.toMap
    }

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
            ConfigValueFactory.fromAnyRef("2011-01-01T00:00:00Z"),
          )
          .withValue(
            "simona.time.endDateTime",
            ConfigValueFactory.fromAnyRef("2011-01-01T02:00:00Z"),
          )
          .withFallback(
            ConfigFactory.parseString("""
                |simona.output.log.level = "INFO"
                |simona.output.log.consoleLevel = "ERROR"
                |simona.output.grid = {
                |  nodes = true
                |  lines = true
                |  switches = true
                |  transformers2w = true
                |  transformers3w = true
                |  congestions = true
                |}
                |""".stripMargin)
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

      val resultFileHierarchy = ResultFileHierarchy(parsedConfig, simonaConfig)

      val runtimeEventQueue = new LinkedBlockingQueue[RuntimeEvent]()

      val simonaSetup = Setup(
        parsedConfig,
        simonaConfig,
        runtimeEventQueue = Some(runtimeEventQueue),
      )

      /* run simulation */
      val successful = RunSimonaStandalone.run(
        simonaSetup
      )

      successful shouldBe true

      /* check the results */
      // check configs
      val configOutputDir = resultFileHierarchy.configOutputDir.toFile

      configOutputDir.isDirectory shouldBe true
      configOutputDir.listFiles.toVector.size shouldBe 1

      // check runtime event queue for the expected runtime events
      checkRuntimeEvents(runtimeEventQueue.asScala)

      // check result data
      simonaSetup.actualResults.foreach { case (key, result) =>
        checkResult(result, expectedResults(key))
      }

      // todo implement if valid result handling is implemented (see issue #1491)
      val pvResultFileContent = getFileSource(
        resultFileHierarchy,
        classOf[PvResult],
      ).getLines().toVector
      pvResultFileContent.size shouldBe 190
      pvResultFileContent.headOption.map(
        _.equals("uuid,inputModel,p,q,timestamp")
      )

    }

    "run und produce results based on a valid minimal config correctly" in {

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
            ConfigValueFactory.fromAnyRef("2011-01-01T00:00:00Z"),
          )
          .withValue(
            "simona.time.endDateTime",
            ConfigValueFactory.fromAnyRef("2011-01-01T02:00:00Z"),
          )
          .withFallback(
            ConfigFactory.parseString("""
                |simona.output.log.level = "INFO"
                |simona.output.log.consoleLevel = "ERROR"
                |simona.output.grid = {
                |  nodes = true
                |  lines = true
                |  switches = true
                |  transformers2w = true
                |  transformers3w = true
                |  congestions = true
                |}
                |""".stripMargin)
          )
          .withFallback(
            ConfigFactory
              .parseString("""
                  |pekko.loggers =["org.apache.pekko.event.slf4j.Slf4jLogger"]
                  |pekko.loglevel="OFF"
                  |""".stripMargin)
          )
          .withFallback(ConfigFactory.parseFile(new File(minimalConfigFile)))
          .withFallback(ConfigFactory.parseString(s"config=$minimalConfigFile"))
          .resolve()

      /* validate config */
      val simonaConfig = SimonaConfig(parsedConfig)
      ConfigFailFast.check(simonaConfig)

      val resultFileHierarchy = ResultFileHierarchy(parsedConfig, simonaConfig)

      val runtimeEventQueue = new LinkedBlockingQueue[RuntimeEvent]()

      val simonaSetup = Setup(
        parsedConfig,
        simonaConfig,
        runtimeEventQueue = Some(runtimeEventQueue),
      )

      /* run simulation */
      val successful = RunSimonaStandalone.run(
        simonaSetup
      )

      successful shouldBe true

      /* check the results */
      // check configs
      val configOutputDir = resultFileHierarchy.configOutputDir.toFile

      configOutputDir.isDirectory shouldBe true
      configOutputDir.listFiles.toVector.size shouldBe 1

      // check runtime event queue for the expected runtime events
      checkRuntimeEvents(runtimeEventQueue.asScala)

      // check result data
      simonaSetup.actualResults.foreach { case (key, result) =>
        checkResult(result, expectedResults(key))
      }

      // check result data
      // todo implement if valid result handling is implemented (see issue #1491)
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
      entityClass: Class[? <: ResultEntity],
  ): BufferedSource = {
    Source.fromFile(
      resultFileHierarchy.rawOutputDataFilePaths
        .getOrElse(
          entityClass,
          fail(s"Unable to get output path for result entity: $entityClass"),
        )
        .toFile
    )
  }

  private def checkRuntimeEvents(
      runtimeEvents: Iterable[RuntimeEvent]
  ): Unit = {
    val groupedRuntimeEvents = runtimeEvents.groupBy(event => event.getClass)

    groupedRuntimeEvents.keySet should contain only (
      classOf[Simulating],
      classOf[CheckWindowPassed],
      classOf[InitComplete],
      classOf[Initializing.type],
      classOf[Done]
    )

    groupedRuntimeEvents
      .get(classOf[Simulating])
      .foreach(simulatingEvents => {
        simulatingEvents.size shouldBe 1
        simulatingEvents.headOption.foreach(_ shouldBe Simulating(0, 7200))
      })

    groupedRuntimeEvents
      .get(classOf[CheckWindowPassed])
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
      .get(classOf[InitComplete])
      .foreach(initComplets => {
        initComplets.size shouldBe 1
      })

    groupedRuntimeEvents
      .get(classOf[Initializing.type])
      .foreach(initializings => {
        initializings.size shouldBe 1
      })

    groupedRuntimeEvents
      .get(classOf[Done])
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

  private def checkResult[A <: ResultEntity](actual: A, expected: A): Unit = {
    // check the common fields
    actual.getInputModel shouldBe expected.getInputModel
    actual.getTime shouldBe expected.getTime

    (actual, expected) match {
      case (a: NodeResult, e: NodeResult) =>
        a.getvAng should equalWithTolerance(e.getvAng)
        a.getvMag should equalWithTolerance(e.getvMag)

      case (a: CongestionResult, e: CongestionResult) =>
        a.getSubgrid shouldBe e.getSubgrid
        a.getType shouldBe e.getType
        a.getValue should equalWithTolerance(a.getValue)
        a.getMin should equalWithTolerance(a.getMin)
        a.getMax should equalWithTolerance(a.getMax)

      case (a: Transformer3WResult, e: Transformer3WResult) =>
        a.getiAAng should equalWithTolerance(e.getiAAng)
        a.getiAMag should equalWithTolerance(e.getiAMag)
        a.getiBAng should equalWithTolerance(e.getiBAng)
        a.getiBMag should equalWithTolerance(e.getiBMag)
        a.getiCAng should equalWithTolerance(e.getiCAng)
        a.getiCMag should equalWithTolerance(e.getiCMag)
        a.getTapPos shouldBe e.getTapPos

      case (a: TransformerResult, e: TransformerResult) =>
        a.getiAAng should equalWithTolerance(e.getiAAng)
        a.getiAMag should equalWithTolerance(e.getiAMag)
        a.getiBAng should equalWithTolerance(e.getiBAng)
        a.getiBMag should equalWithTolerance(e.getiBMag)
        a.getTapPos shouldBe e.getTapPos

      case (a: ConnectorResult, e: ConnectorResult) =>
        a.getiAAng should equalWithTolerance(e.getiAAng)
        a.getiAMag should equalWithTolerance(e.getiAMag)
        a.getiBAng should equalWithTolerance(e.getiBAng)
        a.getiBMag should equalWithTolerance(e.getiBMag)

      case (a: SwitchResult, e: SwitchResult) =>
        a.getClosed shouldBe e.getClosed

      case (
            a: SystemParticipantWithHeatResult,
            e: SystemParticipantWithHeatResult,
          ) =>
        a.getP should equalWithTolerance(e.getP)
        a.getQ should equalWithTolerance(e.getQ)
        a.getqDot should equalWithTolerance(e.getqDot)

      case (
            a: ElectricalEnergyStorageResult,
            e: ElectricalEnergyStorageResult,
          ) =>
        a.getP should equalWithTolerance(e.getP)
        a.getQ should equalWithTolerance(e.getQ)
        a.getSoc should equalWithTolerance(e.getSoc)

      case (a: FlexOptionsResult, e: FlexOptionsResult) =>
        a.getpMin should equalWithTolerance(e.getpMin)
        a.getpMax should equalWithTolerance(e.getpMax)

      case (a: SystemParticipantResult, e: SystemParticipantResult) =>
        a.getP should equalWithTolerance(e.getP)
        a.getQ should equalWithTolerance(e.getQ)

    }
  }

}
