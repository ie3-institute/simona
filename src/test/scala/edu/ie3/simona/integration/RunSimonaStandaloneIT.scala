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
import edu.ie3.datamodel.models.result.thermal.{
  AbstractThermalStorageResult,
  ThermalHouseResult,
}
import edu.ie3.datamodel.models.result.{
  CongestionResult,
  NodeResult,
  ResultEntity,
}
import edu.ie3.simona.config.{ConfigFailFast, SimonaConfig}
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.RuntimeEvent.*
import edu.ie3.simona.event.listener.{DelayedStopHelper, ResultListener}
import edu.ie3.simona.integration.common.IntegrationSpecCommon
import edu.ie3.simona.main.RunSimonaStandalone
import edu.ie3.simona.ontology.messages.ResultMessage
import edu.ie3.simona.ontology.messages.ResultMessage.ResultResponse
import edu.ie3.simona.service.results.ResultServiceProxy.Message
import edu.ie3.simona.sim.setup.SimonaSetup
import edu.ie3.simona.test.common.{IOTestCommons, UnitSpec}
import edu.ie3.simona.test.helper.TestResourceHelper
import edu.ie3.simona.util.ResultFileHierarchy
import edu.ie3.util.io.FileIOUtils
import edu.ie3.util.quantities.PowerSystemUnits
import org.apache.pekko.actor.typed.{ActorRef, PostStop}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.scalatest.BeforeAndAfterAll

import java.io.File
import java.time.ZonedDateTime
import java.util.UUID
import java.util.concurrent.LinkedBlockingQueue
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.Await
import scala.jdk.CollectionConverters.*

class RunSimonaStandaloneIT
    extends IntegrationSpecCommon
    with UnitSpec
    with BeforeAndAfterAll
    with IOTestCommons
    with TestResourceHelper {

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
      val actualResults: TrieMap[
        (UUID, ZonedDateTime, Class[? <: ResultEntity]),
        ResultEntity,
      ] = TrieMap.empty,
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
      getResourcePath("vn_simona"),
      new FileNamingStrategy(),
    )
    val expectedResults
        : Map[(UUID, ZonedDateTime, Class[? <: ResultEntity]), ResultEntity] = {
      val source = new ResultEntitySource(expectedResultSource)
      val tmp = mutable.Map
        .empty[(UUID, ZonedDateTime, Class[? <: ResultEntity]), ResultEntity]

      def add(res: ResultEntity): Unit =
        tmp.put((res.getInputModel, res.getTime, res.getClass), res)

      source.getNodeResults.forEach(add)
      source.getSwitchResults.forEach(add)
      source.getLineResults.forEach(add)
      source.getTransformer2WResultResults.forEach(add)
      source.getTransformer3WResultResults.forEach(add)
      source.getPowerLimitFlexOptionsResults.forEach(add)
      source.getEnergyBoundariesFlexOptionsResults.forEach(add)
      source.getLoadResults.forEach(add)
      source.getPvResults.forEach(add)
      source.getFixedFeedInResults.forEach(add)
      source.getBmResults.forEach(add)
      source.getChpResults.forEach(add)
      source.getWecResults.forEach(add)
      source.getStorageResults.forEach(add)
      source.getEvcsResults.forEach(add)
      source.getEvResults.forEach(add)
      source.getAcResults.forEach(add)
      source.getHpResults.forEach(add)
      source.getCylindricalStorageResult.forEach(add)
      source.getDomesticHotWaterStorageResult.forEach(add)
      source.getThermalHouseResults.forEach(add)
      source.getEmResults.forEach(add)
      source.getCongestionResults.forEach(add)

      tmp.toMap
    }

    "check the loaded results" in {
      expectedResults.size shouldBe 5839
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
      simonaSetup.actualResults.size shouldBe expectedResults.size
      simonaSetup.actualResults.foreach { case (key, result) =>
        checkResult(result, expectedResults(key)).withClue(
          s"$result\n${expectedResults(key)}"
        )
      }

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
      simonaSetup.actualResults.size shouldBe expectedResults.size
      simonaSetup.actualResults.foreach { case (key, result) =>
        checkResult(result, expectedResults(key)).withClue(
          s"$result\n${expectedResults(key)}"
        )
      }

    }
  }

  "A simona standalone simulation with Markov load models" must {

    def runSimulation(
        simulationName: String
    ): Map[(UUID, ZonedDateTime), Double] = {
      val markovDirectory =
        getResourcePath("markov").toString.replace("\\", "/")

      /* setup config */
      val parsedConfig =
        ConfigFactory
          .empty()
          .withValue(
            "simona.simulationName",
            ConfigValueFactory.fromAnyRef(simulationName),
          )
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
          .withValue(
            "simona.runtime.participant.load.modelBehaviour",
            ConfigValueFactory.fromAnyRef("markov"),
          )
          .withValue(
            "simona.runtime.participant.load.reference",
            ConfigValueFactory.fromAnyRef("power"),
          )
          .withFallback(
            ConfigFactory.parseString(s"""
                |simona.input.loadProfile.datasource.csvParams = {
                |  csvSep = ","
                |  directoryPath = "$markovDirectory"
                |  isHierarchic = false
                |}
                |simona.output.log.level = "INFO"
                |simona.output.log.consoleLevel = "ERROR"
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

      val runtimeEventQueue = new LinkedBlockingQueue[RuntimeEvent]()

      val simonaSetup = Setup(
        parsedConfig,
        simonaConfig,
        runtimeEventQueue = Some(runtimeEventQueue),
      )

      /* run simulation */
      RunSimonaStandalone.run(simonaSetup) shouldBe true

      checkRuntimeEvents(runtimeEventQueue.asScala)

      simonaSetup.actualResults.collect {
        case ((uuid, time, _), result: LoadResult) =>
          (uuid, time) -> result.getP
            .to(PowerSystemUnits.KILOWATT)
            .getValue
            .doubleValue
      }.toMap
    }

    "run and produce reproducible load results" in {
      val loadResults = runSimulation("vn_simona_markov_1")

      // all loads of the grid provide results
      val expectedLoads = expectedLoadResults.keySet.map(_._1)
      loadResults.keySet.map(_._1) shouldBe expectedLoads

      // loads with primary data (see time_series_mapping.csv) or an individual
      // model behaviour (see load_input.csv) keep their previous results
      val loadsWithoutMarkov = loadResults
        .groupBy { case ((uuid, _), _) => uuid }
        .collect {
          case (uuid, results) if results.forall { case (key, power) =>
                expectedLoadResults
                  .get(key)
                  .exists(expected => math.abs(expected - power) < tolerance)
              } =>
            uuid
        }
        .toSet

      loadsWithoutMarkov shouldBe Set(
        "4642d648-b0dd-4597-a3bd-2cc1fce74f27",
        "50c89980-8da2-4e98-8602-e2f0b560e7c4",
        "ff5bf65c-e880-4a49-a286-fd51ce1f48b7",
      ).map(UUID.fromString)

      // the Markov model is normalized between 0 kW and 1.5 kW
      loadResults.foreach {
        case ((uuid, _), power) if !loadsWithoutMarkov.contains(uuid) =>
          power should (be >= -tolerance and be <= 1.5 + tolerance)
        case _ =>
      }

      // the same seeds lead to the same results
      runSimulation("vn_simona_markov_2") shouldBe loadResults
    }
  }

  /** Load results of the simulation with standard load profiles.
    */
  private lazy val expectedLoadResults: Map[(UUID, ZonedDateTime), Double] = {
    val source = new ResultEntitySource(
      new CsvDataSource(
        ",",
        getResourcePath("vn_simona"),
        new FileNamingStrategy(),
      )
    )

    source.getLoadResults.asScala.map { result =>
      (result.getInputModel, result.getTime) -> result.getP
        .to(PowerSystemUnits.KILOWATT)
        .getValue
        .doubleValue
    }.toMap
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
      case (actualResult: NodeResult, expectedResult: NodeResult) =>
        actualResult.getvAng should equalWithTolerance(expectedResult.getvAng)
        actualResult.getvMag should equalWithTolerance(expectedResult.getvMag)

      case (actualResult: CongestionResult, expectedResult: CongestionResult) =>
        actualResult.getSubgrid shouldBe expectedResult.getSubgrid
        actualResult.getType shouldBe expectedResult.getType
        actualResult.getValue should equalWithTolerance(expectedResult.getValue)
        actualResult.getMin should equalWithTolerance(expectedResult.getMin)
        actualResult.getMax should equalWithTolerance(expectedResult.getMax)

      case (
            actualResult: Transformer3WResult,
            expectedResult: Transformer3WResult,
          ) =>
        actualResult.getiAAng should equalWithTolerance(expectedResult.getiAAng)
        actualResult.getiAMag should equalWithTolerance(expectedResult.getiAMag)
        actualResult.getiBAng should equalWithTolerance(expectedResult.getiBAng)
        actualResult.getiBMag should equalWithTolerance(expectedResult.getiBMag)
        actualResult.getiCAng should equalWithTolerance(expectedResult.getiCAng)
        actualResult.getiCMag should equalWithTolerance(expectedResult.getiCMag)
        actualResult.getTapPos shouldBe expectedResult.getTapPos

      case (
            actualResult: TransformerResult,
            expectedResult: TransformerResult,
          ) =>
        actualResult.getiAAng should equalWithTolerance(expectedResult.getiAAng)
        actualResult.getiAMag should equalWithTolerance(expectedResult.getiAMag)
        actualResult.getiBAng should equalWithTolerance(expectedResult.getiBAng)
        actualResult.getiBMag should equalWithTolerance(expectedResult.getiBMag)
        actualResult.getTapPos shouldBe expectedResult.getTapPos

      case (actualResult: ConnectorResult, expectedResult: ConnectorResult) =>
        actualResult.getiAAng should equalWithTolerance(expectedResult.getiAAng)
        actualResult.getiAMag should equalWithTolerance(expectedResult.getiAMag)
        actualResult.getiBAng should equalWithTolerance(expectedResult.getiBAng)
        actualResult.getiBMag should equalWithTolerance(expectedResult.getiBMag)

      case (actualResult: SwitchResult, expectedResult: SwitchResult) =>
        actualResult.getClosed shouldBe expectedResult.getClosed

      case (
            actualResult: SystemParticipantWithHeatResult,
            expectedResult: SystemParticipantWithHeatResult,
          ) =>
        actualResult.getP should equalWithTolerance(expectedResult.getP)
        actualResult.getQ should equalWithTolerance(expectedResult.getQ)
        actualResult.getqDot should equalWithTolerance(expectedResult.getqDot)

      case (
            actualResult: ElectricalEnergyStorageResult,
            expectedResult: ElectricalEnergyStorageResult,
          ) =>
        actualResult.getP should equalWithTolerance(expectedResult.getP)
        actualResult.getQ should equalWithTolerance(expectedResult.getQ)
        actualResult.getSoc should equalWithTolerance(expectedResult.getSoc)

      case (
            actualResult: PowerLimitFlexOptionsResult,
            expectedResult: PowerLimitFlexOptionsResult,
          ) =>
        actualResult.getpMin should equalWithTolerance(expectedResult.getpMin)
        actualResult.getpRef should equalWithTolerance(expectedResult.getpRef)
        actualResult.getpMax should equalWithTolerance(expectedResult.getpMax)

      case (
            actualResult: EnergyBoundariesFlexOptionsResult,
            expectedResult: EnergyBoundariesFlexOptionsResult,
          ) =>
        actualResult.getpMin should equalWithTolerance(expectedResult.getpMin)
        actualResult.getpMax should equalWithTolerance(expectedResult.getpMax)
        actualResult.geteState should equalWithTolerance(
          expectedResult.geteState
        )
        actualResult.geteMin should equalWithTolerance(expectedResult.geteMin)
        actualResult.geteMax should equalWithTolerance(expectedResult.geteMax)

      case (
            actualResult: SystemParticipantResult,
            expectedResult: SystemParticipantResult,
          ) =>
        actualResult.getP should equalWithTolerance(expectedResult.getP)
        actualResult.getQ should equalWithTolerance(expectedResult.getQ)

      case (
            actualResult: AbstractThermalStorageResult,
            expectedResult: AbstractThermalStorageResult,
          ) =>
        actualResult.getqDot should equalWithTolerance(expectedResult.getqDot)
        actualResult.getEnergy should equalWithTolerance(
          expectedResult.getEnergy
        )
        actualResult.getFillLevel should equalWithTolerance(
          expectedResult.getFillLevel
        )

      case (
            actualResult: ThermalHouseResult,
            expectedResult: ThermalHouseResult,
          ) =>
        actualResult.getqDot should equalWithTolerance(expectedResult.getqDot)
        actualResult.getIndoorTemperature should equalWithTolerance(
          expectedResult.getIndoorTemperature
        )

      case (actualResult, expectedResult) =>
        fail(s"Can't compare $actualResult and $expectedResult.")
    }
  }

}
