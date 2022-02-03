/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import akka.actor.{ActorRef, ActorSystem, PoisonPill}
import akka.testkit.{TestActorRef, TestProbe}
import akka.util.Timeout
import breeze.stats.distributions.Rand
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.io.csv.timeseries.ColumnScheme
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.TimeSeriesMappingSource
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesMappingSource
import edu.ie3.datamodel.models.value.SValue
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary.{
  CouchbaseParams,
  CsvParams,
  InfluxDb1xParams,
  SqlParams
}
import edu.ie3.simona.config.SimonaConfig.Simona.Input.{
  Primary => PrimaryConfig
}
import edu.ie3.simona.exceptions.{
  InitializationException,
  InvalidConfigParameterException
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  PrimaryServiceRegistrationMessage,
  WorkerRegistrationMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.trigger.Trigger.InitializeServiceTrigger
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  InitPrimaryServiceStateData
}
import edu.ie3.simona.service.primary.PrimaryServiceProxy.{
  InitPrimaryServiceProxyStateData,
  PrimaryServiceStateData,
  SourceRef
}
import edu.ie3.simona.test.common.AgentSpec
import edu.ie3.util.TimeUtil
import org.scalatest.PartialFunctionValues
import org.scalatest.prop.TableDrivenPropertyChecks

import java.nio.file.Paths
import java.time.ZonedDateTime
import java.util.concurrent.TimeUnit
import java.util.{Objects, UUID}
import scala.util.{Failure, Success, Try}
import scala.concurrent.ExecutionContext.Implicits.global

class PrimaryServiceProxySpec
    extends AgentSpec(
      ActorSystem(
        "PrimaryServiceProxySpec",
        ConfigFactory
          .parseString("""
            |akka.loggers = ["akka.testkit.TestEventListener"]
            |akka.loglevel="OFF"
          """.stripMargin)
      )
    )
    with TableDrivenPropertyChecks
    with PartialFunctionValues {
  // this works both on Windows and Unix systems
  val baseDirectoryPath: String = Paths
    .get(
      this.getClass
        .getResource(
          "it"
        )
        .toURI
    )
    .toString
  val csvSep = ";"
  val fileNamingStrategy = new FileNamingStrategy()
  val validPrimaryConfig: PrimaryConfig =
    PrimaryConfig(
      None,
      Some(
        CsvParams(
          csvSep,
          baseDirectoryPath,
          TimeUtil.withDefaults.getDtfPattern
        )
      ),
      None,
      None
    )
  val mappingSource = new CsvTimeSeriesMappingSource(
    csvSep,
    baseDirectoryPath,
    fileNamingStrategy
  )
  val workerId: String =
    "PrimaryService_3fbfaa97-cff4-46d4-95ba-a95665e87c26"
  val modelUuid: UUID = UUID.fromString("c7ebcc6c-55fc-479b-aa6b-6fa82ccac6b8")
  val timeSeriesUuid: UUID =
    UUID.fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26")
  val simulationStart: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2021-03-17 13:14:00")
  val proxyStateData: PrimaryServiceStateData = PrimaryServiceStateData(
    Map(
      UUID.fromString("b86e95b0-e579-4a80-a534-37c7a470a409") -> UUID
        .fromString("9185b8c1-86ba-4a16-8dea-5ac898e8caa5"),
      modelUuid -> UUID.fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26"),
      UUID.fromString("90a96daa-012b-4fea-82dc-24ba7a7ab81c") -> UUID
        .fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26")
    ),
    Map(
      UUID.fromString("9185b8c1-86ba-4a16-8dea-5ac898e8caa5") -> SourceRef(
        ColumnScheme.ACTIVE_POWER,
        None
      ),
      UUID.fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26") -> SourceRef(
        ColumnScheme.APPARENT_POWER,
        None
      )
    ),
    simulationStart,
    validPrimaryConfig,
    mappingSource
  )

  private val scheduler = TestProbe("scheduler")

  "Testing a primary service config" should {
    "lead to complaining about too much source definitions" in {
      val maliciousConfig = PrimaryConfig(
        Some(CouchbaseParams("", "", "", "", "", "", "")),
        Some(CsvParams("", "", "")),
        None,
        None
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "2 time series source types defined. Please define only one type!\nAvailable types:\n\tcsv"
    }

    "lead to complaining about too few source definitions" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        None,
        None
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "No time series source type defined. Please define exactly one type!\nAvailable types:\n\tcsv"
    }

    "not let couchbase parameters pass for mapping configuration" in {
      val maliciousConfig = PrimaryConfig(
        Some(CouchbaseParams("", "", "", "", "", "", "")),
        None,
        None,
        None
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "Invalid configuration 'CouchbaseParams(,,,,,,)' for a time series source.\nAvailable types:\n\tcsv"
    }

    "let csv parameters pass for mapping configuration" in {
      val mappingConfig = PrimaryConfig(
        None,
        Some(CsvParams("", "", "")),
        None,
        None
      )

      noException shouldBe thrownBy {
        PrimaryServiceProxy.checkConfig(mappingConfig)
      }
    }

    "not let influx db parameters pass for mapping configuration" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        Some(InfluxDb1xParams("", 0, "", "")),
        None
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "Invalid configuration 'InfluxDb1xParams(,0,,)' for a time series source.\nAvailable types:\n\tcsv"
    }

    "not let sql parameters pass for mapping configuration" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        None,
        Some(SqlParams("", "", "", "", "", "", ""))
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "Invalid configuration 'SqlParams(,,,,,,)' for a time series source.\nAvailable types:\n\tcsv"
    }

    "fails on invalid time pattern" in {
      val invalidTimePatternConfig = PrimaryConfig(
        None,
        Some(CsvParams("", "", "xYz")),
        None,
        None
      )

      intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(invalidTimePatternConfig)
      ).getMessage shouldBe "Invalid timePattern 'xYz' for a time series source. " +
        "Please provide a valid pattern!\nException: java.lang.IllegalArgumentException: Illegal pattern character 'x'"

    }

    "succeeds on valid time pattern" in {
      val validTimePatternConfig = PrimaryConfig(
        None,
        Some(CsvParams("", "", "yyyy-MM-dd'T'HH:mm'Z[UTC]'")),
        None,
        None
      )

      noException shouldBe thrownBy {
        PrimaryServiceProxy.checkConfig(validTimePatternConfig)
      }
    }
  }

  val proxyRef: TestActorRef[PrimaryServiceProxy] = TestActorRef(
    new PrimaryServiceProxy(self, simulationStart)
  )
  val proxy: PrimaryServiceProxy = proxyRef.underlyingActor

  "Building state data from given config" should {
    val prepareStateData =
      PrivateMethod[Try[PrimaryServiceStateData]](Symbol("prepareStateData"))

    "fail, in case no config is given" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        None,
        None
      )

      proxy invokePrivate prepareStateData(
        maliciousConfig,
        simulationStart
      ) match {
        case Success(_) =>
          fail("Building state data with missing config should fail")
        case Failure(exception) =>
          exception.getClass shouldBe classOf[IllegalArgumentException]
          exception.getMessage shouldBe "You have to provide exactly one config for the mapping source."
      }
    }

    "fail, in case the wrong config is given" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        None,
        Some(SqlParams("", "", "", "", "", "", ""))
      )

      proxy invokePrivate prepareStateData(
        maliciousConfig,
        simulationStart
      ) match {
        case Success(_) =>
          fail("Building state data with missing config should fail")
        case Failure(exception) =>
          exception.getClass shouldBe classOf[IllegalArgumentException]
          exception.getMessage shouldBe "Unsupported config for mapping source: 'SqlParams(,,,,,,)'"
      }
    }

    "result in correct data" in {
      proxy invokePrivate prepareStateData(
        validPrimaryConfig,
        simulationStart
      ) match {
        case Success(
              PrimaryServiceStateData(
                modelToTimeSeries,
                timeSeriesToSourceRef,
                simulationStart,
                primaryConfig,
                mappingSource
              )
            ) =>
          modelToTimeSeries shouldBe Map(
            UUID.fromString("b86e95b0-e579-4a80-a534-37c7a470a409") -> UUID
              .fromString("9185b8c1-86ba-4a16-8dea-5ac898e8caa5"),
            UUID.fromString("c7ebcc6c-55fc-479b-aa6b-6fa82ccac6b8") -> UUID
              .fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26"),
            UUID.fromString("90a96daa-012b-4fea-82dc-24ba7a7ab81c") -> UUID
              .fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26")
          )
          timeSeriesToSourceRef shouldBe Map(
            UUID
              .fromString("9185b8c1-86ba-4a16-8dea-5ac898e8caa5") -> SourceRef(
              ColumnScheme.ACTIVE_POWER,
              None
            ),
            UUID
              .fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26") -> SourceRef(
              ColumnScheme.APPARENT_POWER,
              None
            )
          )
          simulationStart shouldBe this.simulationStart
          primaryConfig shouldBe validPrimaryConfig
          classOf[TimeSeriesMappingSource].isAssignableFrom(
            mappingSource.getClass
          ) shouldBe true
        case Failure(_) =>
          fail("Building state data with correct config should not fail")
      }
    }
  }

  "Sending initialization information to an uninitialized actor" should {
    "lead to a completion message without trigger requests" in {
      val initStateData = InitPrimaryServiceProxyStateData(
        validPrimaryConfig,
        simulationStart
      )

      proxyRef ! TriggerWithIdMessage(
        InitializeServiceTrigger(initStateData),
        0L,
        self
      )
      expectMsg(CompletionMessage(0L, None))
    }
  }

  "Spinning off a worker" should {
    "successfully instantiate an actor within the actor system" in {
      val classToWorkerRef = PrivateMethod[ActorRef](Symbol("classToWorkerRef"))

      val testClass =
        classOf[
          SValue
        ] // The class has to be child of edu.ie3.datamodel.models.value.Value

      val workerRef = proxy invokePrivate classToWorkerRef(
        testClass,
        workerId,
        simulationStart
      )
      Objects.nonNull(workerRef) shouldBe true

      /* Kill the worker, as we do not need it */
      workerRef ! PoisonPill
    }

    "successfully spin off an actor from supported ColumnScheme and refuse unsupported ones" in {
      val testData = Table(
        ("columnScheme", "successful"),
        (ColumnScheme.ACTIVE_POWER, true),
        (ColumnScheme.ACTIVE_POWER_AND_HEAT_DEMAND, true),
        (ColumnScheme.APPARENT_POWER, true),
        (ColumnScheme.APPARENT_POWER_AND_HEAT_DEMAND, true),
        (ColumnScheme.ENERGY_PRICE, false),
        (ColumnScheme.HEAT_DEMAND, false),
        (ColumnScheme.WEATHER, false)
      )

      val columnSchemeToActor =
        PrivateMethod[Try[ActorRef]](Symbol("columnSchemeToActor"))

      forAll(testData) {
        case (columnScheme: ColumnScheme, successFul: Boolean) =>
          proxy invokePrivate columnSchemeToActor(
            columnScheme,
            workerId + Rand.randInt.draw(),
            simulationStart
          ) match {
            case Success(workerRef) =>
              if (!successFul)
                fail(
                  s"Spinning off an worker for column scheme '$columnScheme' was meant to fail, but succeeded."
                )
              /* Kill the worker, as we do not need it */
              workerRef ! PoisonPill
            case Failure(exception) =>
              if (successFul)
                fail(
                  s"Spinning off an worker for column scheme '$columnScheme' was meant to succeed, but with message '${exception.getMessage}'."
                )
              exception.getMessage shouldBe s"Cannot build a primary source for unsupported column scheme '$columnScheme'."
          }
      }
    }

    "successfully build initialization data for the worker" in {
      val toInitData = PrivateMethod[Try[InitPrimaryServiceStateData]](
        Symbol("toInitData")
      )

      proxy invokePrivate toInitData(
        validPrimaryConfig,
        mappingSource,
        timeSeriesUuid,
        simulationStart
      ) match {
        case Success(
              CsvInitPrimaryServiceStateData(
                actualTimeSeriesUuid,
                actualSimulationStart,
                actualCsvSep,
                directoryPath,
                filePath,
                fileNamingStrategy,
                timePattern
              )
            ) =>
          actualTimeSeriesUuid shouldBe timeSeriesUuid
          actualSimulationStart shouldBe simulationStart
          actualCsvSep shouldBe csvSep
          directoryPath shouldBe baseDirectoryPath
          filePath shouldBe "its_pq_3fbfaa97-cff4-46d4-95ba-a95665e87c26"
          classOf[FileNamingStrategy].isAssignableFrom(
            fileNamingStrategy.getClass
          ) shouldBe true
          timePattern shouldBe TimeUtil.withDefaults.getDtfPattern
        case Success(wrongData) =>
          fail(s"Creation of init data lead to wrong init data '$wrongData'.")
        case Failure(exception) =>
          fail(
            "Creation of init data failed, although it was meant to succeed.",
            exception
          )
      }
    }

    "fail, if no worker actor can be initialized" in {
      val maliciousColumnScheme = ColumnScheme.WEATHER

      proxy.initializeWorker(
        maliciousColumnScheme,
        timeSeriesUuid,
        simulationStart,
        validPrimaryConfig,
        mappingSource
      ) match {
        case Failure(exception) =>
          exception.getClass shouldBe classOf[InitializationException]
          exception.getMessage shouldBe "Unable to establish a typed worker for primary data provision."
          exception.getCause.getMessage shouldBe s"Cannot build a primary source for unsupported column scheme '$maliciousColumnScheme'."
        case Success(_) =>
          fail(
            "Instantiating a worker with malicious column scheme should fail."
          )
      }
    }

    "fail, if init data cannot be established for the worker" in {
      val maliciousPrimaryConfig = PrimaryConfig(
        Some(CouchbaseParams("", "", "", "", "", "", "")),
        None,
        None,
        None
      )

      proxy.initializeWorker(
        ColumnScheme.APPARENT_POWER,
        timeSeriesUuid,
        simulationStart,
        maliciousPrimaryConfig,
        mappingSource
      ) match {
        case Failure(exception) =>
          /* Check the exception */
          exception.getClass shouldBe classOf[InitializationException]
          exception.getMessage shouldBe "Unable to build init data for worker. Kill the uninitialized worker. Goodbye my friend!"
          exception.getCause.getMessage shouldBe s"Cannot build initialization data for a worker due to unsupported source config '$maliciousPrimaryConfig'."

          /* Check, if the worker has been killed, yet. */
          implicit val timeout: Timeout = Timeout(2, TimeUnit.SECONDS)
          system.actorSelection("/user/" + workerId).resolveOne().onComplete {
            case Failure(exception) =>
              logger
                .debug("Worker actor couldn't be found. Reason: ", exception)
              succeed
            case Success(_) =>
              fail("Worker actor shouldn't be existing, but exists.")
          }
        case Success(_) =>
          fail(
            "Instantiating a worker with malicious primary config should fail."
          )
      }
    }

    "succeed on fine input data" in {
      /* We "fake" the creation of the worker to infiltrate a test probe. This empowers us to check, if a matching init
       * message is sent to the worker */
      val testProbe = TestProbe("workerTestProbe")
      val fakeProxyRef =
        TestActorRef(new PrimaryServiceProxy(scheduler.ref, simulationStart) {
          override def columnSchemeToActor(
              columnScheme: ColumnScheme,
              actorId: String,
              simulationStart: ZonedDateTime
          ): Try[ActorRef] = Success(testProbe.ref)
        })
      val fakeProxy: PrimaryServiceProxy = fakeProxyRef.underlyingActor

      fakeProxy.initializeWorker(
        ColumnScheme.APPARENT_POWER,
        timeSeriesUuid,
        simulationStart,
        validPrimaryConfig,
        mappingSource
      ) match {
        case Success(workerRef) =>
          /* Check, if expected init message has been sent */
          inside(scheduler.expectMsgClass(classOf[ScheduleTriggerMessage])) {
            case ScheduleTriggerMessage(
                  InitializeServiceTrigger(
                    CsvInitPrimaryServiceStateData(
                      actualTimeSeriesUuid,
                      actualSimulationStart,
                      actualCsvSep,
                      directoryPath,
                      filePath,
                      fileNamingStrategy,
                      timePattern
                    )
                  ),
                  actorToBeScheduled
                ) =>
              actualTimeSeriesUuid shouldBe timeSeriesUuid
              actualSimulationStart shouldBe simulationStart
              actualCsvSep shouldBe csvSep
              directoryPath shouldBe baseDirectoryPath
              filePath shouldBe "its_pq_3fbfaa97-cff4-46d4-95ba-a95665e87c26"
              classOf[FileNamingStrategy].isAssignableFrom(
                fileNamingStrategy.getClass
              ) shouldBe true
              actorToBeScheduled shouldBe workerRef
              timePattern shouldBe TimeUtil.withDefaults.getDtfPattern
          }

          /* Kill the worker aka. test probe */
          workerRef ! PoisonPill
          succeed
        case Failure(exception) =>
          fail(
            "Spinning off a worker with correct input data should be successful, but failed with:",
            exception
          )
      }
    }
  }

  "Updating state data" should {
    val updateStateData =
      PrivateMethod[PrimaryServiceStateData](Symbol("updateStateData"))
    "not work, if time series hasn't been covered before" in {
      val exception = intercept[IllegalArgumentException] {
        proxy invokePrivate updateStateData(
          proxyStateData,
          UUID.fromString("394fd072-832c-4c36-869b-c574ee37afe1"),
          self
        )
      }
      exception.getMessage shouldBe "Cannot update entry for time series '394fd072-832c-4c36-869b-c574ee37afe1', as it hasn't been part of it before."
    }

    "work otherwise" in {
      proxy invokePrivate updateStateData(
        proxyStateData,
        timeSeriesUuid,
        self
      ) match {
        case PrimaryServiceStateData(
              modelToTimeSeries,
              timeSeriesToSourceRef,
              simulationStart,
              primaryConfig,
              mappingSource
            ) =>
          modelToTimeSeries shouldBe proxyStateData.modelToTimeSeries
          timeSeriesToSourceRef shouldBe Map(
            UUID
              .fromString("9185b8c1-86ba-4a16-8dea-5ac898e8caa5") -> SourceRef(
              ColumnScheme.ACTIVE_POWER,
              None
            ),
            UUID
              .fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c26") -> SourceRef(
              ColumnScheme.APPARENT_POWER,
              Some(self)
            )
          )
          simulationStart shouldBe proxyStateData.simulationStart
          primaryConfig shouldBe proxyStateData.primaryConfig
          mappingSource shouldBe proxyStateData.mappingSource
      }
    }
  }

  "Handling of a covered model" should {
    "fail, if no information can be obtained from state data" in {
      val maliciousStateData =
        proxyStateData.copy(timeSeriesToSourceRef = Map.empty[UUID, SourceRef])

      proxy.handleCoveredModel(
        modelUuid,
        timeSeriesUuid,
        maliciousStateData,
        self
      )
      expectMsg(RegistrationFailedMessage)
    }

    "forward the registration request, if worker is already known" in {
      val adaptedStateData = proxyStateData.copy(
        timeSeriesToSourceRef = Map(
          timeSeriesUuid -> SourceRef(ColumnScheme.APPARENT_POWER, Some(self))
        )
      )

      proxy.handleCoveredModel(
        modelUuid,
        timeSeriesUuid,
        adaptedStateData,
        self
      )
      expectMsg(WorkerRegistrationMessage(self))
    }

    "fail, if worker cannot be spun off" in {
      val maliciousStateData = proxyStateData.copy(
        primaryConfig = PrimaryConfig(
          Some(CouchbaseParams("", "", "", "", "", "", "")),
          None,
          None,
          None
        )
      )

      proxy.handleCoveredModel(
        modelUuid,
        timeSeriesUuid,
        maliciousStateData,
        self
      )
      expectMsg(RegistrationFailedMessage)
    }

    "spin off a worker, if needed and forward the registration request" in {
      /* We once again fake the class, so that we can infiltrate a probe */
      val probe = TestProbe("workerTestProbe")
      val fakeProxyRef =
        TestActorRef(new PrimaryServiceProxy(self, simulationStart) {
          override def initializeWorker(
              columnScheme: ColumnScheme,
              timeSeriesUuid: UUID,
              simulationStart: ZonedDateTime,
              primaryConfig: PrimaryConfig,
              mappingSource: TimeSeriesMappingSource
          ): Try[ActorRef] = Success(probe.ref)
        })
      val fakeProxy = fakeProxyRef.underlyingActor

      fakeProxy.handleCoveredModel(
        modelUuid,
        timeSeriesUuid,
        proxyStateData,
        self
      )
      probe.expectMsg(WorkerRegistrationMessage(self))
    }
  }

  "Trying to register with a proxy" should {
    "fail, if there is no information for the requested model" in {
      val request = PrimaryServiceRegistrationMessage(
        UUID.fromString("2850a2d6-4b70-43c9-b5cc-cd823a72d860")
      )

      proxyRef ! request
      expectMsg(RegistrationFailedMessage)
    }

    "succeed, if model is handled" in {
      /* We once again fake the class, so that we can infiltrate a probe */
      val probe = TestProbe("workerTestProbe")
      val fakeProxyRef =
        TestActorRef(new PrimaryServiceProxy(self, simulationStart) {
          override def initializeWorker(
              columnScheme: ColumnScheme,
              timeSeriesUuid: UUID,
              simulationStart: ZonedDateTime,
              primaryConfig: PrimaryConfig,
              mappingSource: TimeSeriesMappingSource
          ): Try[ActorRef] = Success(probe.ref)
        })

      /* Initialize the fake proxy */
      val initStateData = InitPrimaryServiceProxyStateData(
        validPrimaryConfig,
        simulationStart
      )
      fakeProxyRef ! TriggerWithIdMessage(
        InitializeServiceTrigger(initStateData),
        0L,
        self
      )
      expectMsg(CompletionMessage(0L, None))

      /* Try to register with fake proxy */
      fakeProxyRef ! PrimaryServiceRegistrationMessage(modelUuid)
      probe.expectMsg(WorkerRegistrationMessage(self))
    }
  }
}
