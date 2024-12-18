/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.datamodel.io.csv.CsvIndividualTimeSeriesMetaInformation
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.TimeSeriesMappingSource
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesMappingSource
import edu.ie3.datamodel.models.value.SValue
import edu.ie3.simona.config.SimonaConfig.PrimaryDataCsvParams
import edu.ie3.simona.config.SimonaConfig.Simona.Input.Primary.{
  CouchbaseParams,
  InfluxDb1xParams,
}
import edu.ie3.simona.config.SimonaConfig.Simona.Input.{
  Primary => PrimaryConfig
}
import edu.ie3.simona.exceptions.{
  InitializationException,
  InvalidConfigParameterException,
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.PrimaryDataMessage
import edu.ie3.simona.ontology.messages.services.PrimaryDataMessage.{
  PrimaryServiceRegistrationMessage,
  WorkerRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.RegistrationResponseMessage.RegistrationFailedMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.{
  Create,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.service.ServiceStateData.ServiceConstantStateData
import edu.ie3.simona.service.primary.PrimaryServiceProxy.{
  InitPrimaryServiceProxyStateData,
  PrimaryServiceStateData,
  SourceRef,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.CsvInitPrimaryServiceStateData
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.test.common.{AgentTypedSpec, TestSpawnerTyped}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.testkit.typed.scaladsl.TestProbe
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.mockito.Mockito.when
import org.scalatest.Inside.inside
import org.scalatest.PartialFunctionValues
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.mockito.MockitoSugar.mock
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Path, Paths}
import java.time.ZonedDateTime
import java.util.UUID
import scala.language.implicitConversions
import scala.util.{Failure, Success}

class PrimaryServiceProxySpec
    extends AgentTypedSpec
    with TableDrivenPropertyChecks
    with PartialFunctionValues
    with TimeSeriesTestData
    with TestSpawnerTyped {

  implicit def wrap(msg: Activation): WrappedActivation =
    WrappedActivation(msg)

  implicit val constantData: ServiceConstantStateData =
    mock[ServiceConstantStateData]
  implicit val log: Logger = LoggerFactory.getLogger("PrimaryServiceProxySpec")
  implicit val ctx: ActorContext[_] = {
    val m = mock[ActorContext[_]]
    when(m.log).thenReturn(log)
    m
  }

  // this works both on Windows and Unix systems
  val baseDirectoryPath: Path = Paths
    .get(
      this.getClass
        .getResource(
          "_it"
        )
        .toURI
    )
  val csvSep = ";"
  val fileNamingStrategy = new FileNamingStrategy()
  val validPrimaryConfig: PrimaryConfig =
    PrimaryConfig(
      None,
      Some(
        PrimaryDataCsvParams(
          csvSep,
          baseDirectoryPath.toString,
          isHierarchic = false,
          TimeUtil.withDefaults.getDateTimeFormatter.toString,
        )
      ),
      None,
      None,
    )
  val mappingSource = new CsvTimeSeriesMappingSource(
    csvSep,
    baseDirectoryPath,
    fileNamingStrategy,
  )
  val workerId: String = "PrimaryService_" + uuidPq
  val modelUuid: UUID = UUID.fromString("c7ebcc6c-55fc-479b-aa6b-6fa82ccac6b8")
  val simulationStart: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2021-03-17T13:14:00Z")
  val proxyStateData: PrimaryServiceStateData = PrimaryServiceStateData(
    Map(
      UUID.fromString("b86e95b0-e579-4a80-a534-37c7a470a409") -> uuidP,
      modelUuid -> uuidPq,
      UUID.fromString("90a96daa-012b-4fea-82dc-24ba7a7ab81c") -> uuidPq,
    ),
    Map(
      uuidP -> SourceRef(metaP, None),
      uuidPq -> SourceRef(metaPq, None),
    ),
    simulationStart,
    validPrimaryConfig,
    mappingSource,
  )

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")

  "Testing a primary service config" should {
    "lead to complaining about too much source definitions" in {
      val maliciousConfig = PrimaryConfig(
        Some(CouchbaseParams("", "", "", "", "", "", "")),
        Some(PrimaryDataCsvParams("", "", isHierarchic = false, "")),
        None,
        None,
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "2 time series source types defined. Please define only one type!\nAvailable types:\n\tcsv\n\tsql"
    }

    "lead to complaining about too few source definitions" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        None,
        None,
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "No time series source type defined. Please define exactly one type!\nAvailable types:\n\tcsv\n\tsql"
    }

    "not let couchbase parameters pass for mapping configuration" in {
      val maliciousConfig = PrimaryConfig(
        Some(CouchbaseParams("", "", "", "", "", "", "")),
        None,
        None,
        None,
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "Invalid configuration 'CouchbaseParams(,,,,,,)' for a time series source.\nAvailable types:\n\tcsv\n\tsql"
    }

    "let csv parameters pass for mapping configuration" in {
      val mappingConfig = PrimaryConfig(
        None,
        Some(PrimaryDataCsvParams("", "", isHierarchic = false, "")),
        None,
        None,
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
        None,
      )

      val exception = intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(maliciousConfig)
      )
      exception.getMessage shouldBe "Invalid configuration 'InfluxDb1xParams(,0,,)' for a time series source.\nAvailable types:\n\tcsv\n\tsql"
    }

    "fails on invalid time pattern with csv" in {
      val invalidTimePatternConfig = PrimaryConfig(
        None,
        Some(PrimaryDataCsvParams("", "", isHierarchic = false, "xYz")),
        None,
        None,
      )

      intercept[InvalidConfigParameterException](
        PrimaryServiceProxy.checkConfig(invalidTimePatternConfig)
      ).getMessage shouldBe "Invalid timePattern 'xYz' for a time series source. " +
        "Please provide a valid pattern!\nException: java.lang.IllegalArgumentException: Illegal pattern character 'x'"

    }

    "succeeds on valid time pattern with csv" in {
      val validTimePatternConfig = PrimaryConfig(
        None,
        Some(
          PrimaryDataCsvParams(
            "",
            "",
            isHierarchic = false,
            "yyyy-MM-dd'T'HH:mm'Z[UTC]'",
          )
        ),
        None,
        None,
      )

      noException shouldBe thrownBy {
        PrimaryServiceProxy.checkConfig(validTimePatternConfig)
      }
    }
  }

  val initStateData: InitPrimaryServiceProxyStateData =
    InitPrimaryServiceProxyStateData(
      validPrimaryConfig,
      simulationStart,
    )
  val proxy: ActorRef[PrimaryDataMessage] = testKit.spawn(
    PrimaryServiceProxy(scheduler.ref, initStateData)
  )

  "Building state data from given config" should {
    "fail, in case no config is given" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        None,
        None,
      )

      PrimaryServiceProxy.prepareStateData(
        maliciousConfig,
        simulationStart,
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
        Some(InfluxDb1xParams("", -1, "", "")),
        None,
      )

      PrimaryServiceProxy.prepareStateData(
        maliciousConfig,
        simulationStart,
      ) match {
        case Success(_) =>
          fail("Building state data with missing config should fail")
        case Failure(exception) =>
          exception.getClass shouldBe classOf[IllegalArgumentException]
          exception.getMessage shouldBe "Unsupported config for mapping source: 'InfluxDb1xParams(,-1,,)'"
      }
    }

    "result in correct data" in {
      PrimaryServiceProxy.prepareStateData(
        validPrimaryConfig,
        simulationStart,
      ) match {
        case Success(
              PrimaryServiceStateData(
                modelToTimeSeries,
                timeSeriesToSourceRef,
                simulationStart,
                primaryConfig,
                mappingSource,
              )
            ) =>
          modelToTimeSeries shouldBe Map(
            UUID.fromString("b86e95b0-e579-4a80-a534-37c7a470a409") -> uuidP,
            UUID.fromString("c7ebcc6c-55fc-479b-aa6b-6fa82ccac6b8") -> uuidPq,
            UUID.fromString("90a96daa-012b-4fea-82dc-24ba7a7ab81c") -> uuidPq,
          )
          timeSeriesToSourceRef.get(uuidP) match {
            case Some(SourceRef(metaInformation, worker)) =>
              metaInformation shouldBe metaP
              worker shouldBe None
            case None =>
              fail(
                "Expected to receive a source ref for the active power time series"
              )
          }
          timeSeriesToSourceRef.get(uuidPq) match {
            case Some(SourceRef(metaInformation, worker)) =>
              metaInformation shouldBe metaPq
              worker shouldBe None
            case None =>
              fail(
                "Expected to receive a source ref for the apparent power time series"
              )
          }
          simulationStart shouldBe this.simulationStart
          primaryConfig shouldBe validPrimaryConfig
          classOf[TimeSeriesMappingSource].isAssignableFrom(
            mappingSource.getClass
          ) shouldBe true
        case Failure(failure) =>
          fail(
            "Building state data with correct config should not fail, but failed with:",
            failure,
          )
      }
    }
  }

  "Sending initialization information to an uninitialized actor" should {
    "lead to a completion message without trigger requests" in {
      proxy ! Activation(INIT_SIM_TICK)

      scheduler.expectMessageType[ScheduleActivation]

      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.newTick shouldBe None
    }
  }

  "Spinning off a worker" should {
    "successfully build initialization data for the worker" in {
      val metaInformation = new CsvIndividualTimeSeriesMetaInformation(
        metaPq,
        Paths.get("its_pq_" + uuidPq),
      )

      PrimaryServiceProxy.toInitData(
        metaInformation,
        simulationStart,
        validPrimaryConfig,
        classOf[SValue],
      ) match {
        case Success(
              CsvInitPrimaryServiceStateData(
                actualTimeSeriesUuid,
                actualSimulationStart,
                actualValueClass,
                actualCsvSep,
                directoryPath,
                filePath,
                fileNamingStrategy,
                timePattern,
              )
            ) =>
          actualTimeSeriesUuid shouldBe uuidPq
          actualSimulationStart shouldBe simulationStart
          actualValueClass shouldBe classOf[SValue]
          actualCsvSep shouldBe csvSep
          directoryPath shouldBe baseDirectoryPath
          filePath shouldBe metaInformation.getFullFilePath
          classOf[FileNamingStrategy].isAssignableFrom(
            fileNamingStrategy.getClass
          ) shouldBe true
          timePattern shouldBe TimeUtil.withDefaults.getDateTimeFormatter.toString
        case Success(wrongData) =>
          fail(s"Creation of init data lead to wrong init data '$wrongData'.")
        case Failure(exception) =>
          fail(
            "Creation of init data failed, although it was meant to succeed.",
            exception,
          )
      }
    }

    "fail, if init data cannot be established for the worker" in {
      val maliciousPrimaryConfig = PrimaryConfig(
        Some(CouchbaseParams("", "", "", "", "", "", "")),
        None,
        None,
        None,
      )
      PrimaryServiceProxy.initializeWorker(
        metaPq,
        simulationStart,
        maliciousPrimaryConfig,
      ) match {
        case Failure(exception) =>
          /* Check the exception */
          exception.getClass shouldBe classOf[InitializationException]
          exception.getMessage shouldBe "Unable to build init data for worker. Kill the uninitialized worker. Goodbye my friend!"
          exception.getCause.getMessage shouldBe s"Cannot build initialization data for a worker due to unsupported source config '$maliciousPrimaryConfig'."
        case Success(_) =>
          fail(
            "Instantiating a worker with malicious primary config should fail."
          )
      }
    }

    "succeed on fine input data" in {
      /* We "fake" the creation of the worker to infiltrate a test probe. This empowers us to check, if a matching init
       * message is sent to the worker */
      val worker = TestProbe[PrimaryDataMessage]("workerTestProbe")

      val metaInformation = new CsvIndividualTimeSeriesMetaInformation(
        metaPq,
        Paths.get("its_pq_" + uuidPq),
      )

      val sourceRef = SourceRef(
        metaInformation,
        Some(worker.ref),
      )

      val modifiedInitStateData = initStateData.copy(
        timeSeriesToSourceRef = Map(uuidPq -> sourceRef)
      )

      val fakeProxyRef = testKit.spawn(
        PrimaryServiceProxy(
          scheduler.ref,
          modifiedInitStateData,
        )
      )

      scheduler.expectMessageType[ScheduleActivation]

      fakeProxyRef ! Activation(INIT_SIM_TICK)
      scheduler.expectMessageType[Completion]

      fakeProxyRef ! Activation(0)

      inside(worker.expectMessageType[PrimaryDataMessage]) {
        case Create(
              CsvInitPrimaryServiceStateData(
                actualTimeSeriesUuid,
                actualSimulationStart,
                actualValueClass,
                actualCsvSep,
                directoryPath,
                filePath,
                fileNamingStrategy,
                timePattern,
              ),
              _,
            ) =>
          actualTimeSeriesUuid shouldBe uuidPq
          actualSimulationStart shouldBe simulationStart
          actualValueClass shouldBe classOf[SValue]
          actualCsvSep shouldBe csvSep
          directoryPath shouldBe baseDirectoryPath
          filePath shouldBe metaInformation.getFullFilePath
          classOf[FileNamingStrategy].isAssignableFrom(
            fileNamingStrategy.getClass
          ) shouldBe true
          timePattern shouldBe TimeUtil.withDefaults.getDateTimeFormatter.toString
      }

      // receiving schedule activation, don't know why but ok...
      scheduler.expectMessageType[ScheduleActivation]

      testKit.stop(worker.ref)
    }
  }

  private val dummyWorker = TestProbe[PrimaryDataMessage]("dummyWorker")
  private val requestingAgent = TestProbe[Any]("agent")

  "Updating state data" should {
    val updateStateData =
      PrivateMethod[PrimaryServiceStateData](Symbol("updateStateData"))
    "not work, if time series hasn't been covered before" in {
      val exception = intercept[IllegalArgumentException] {
        PrimaryServiceProxy invokePrivate updateStateData(
          proxyStateData,
          UUID.fromString("394fd072-832c-4c36-869b-c574ee37afe1"),
          dummyWorker.ref,
        )
      }
      exception.getMessage shouldBe "Cannot update entry for time series '394fd072-832c-4c36-869b-c574ee37afe1', as it hasn't been part of it before."
    }

    "work otherwise" in {
      PrimaryServiceProxy invokePrivate updateStateData(
        proxyStateData,
        uuidPq,
        dummyWorker.ref,
      ) match {
        case PrimaryServiceStateData(
              modelToTimeSeries,
              timeSeriesToSourceRef,
              simulationStart,
              primaryConfig,
              mappingSource,
            ) =>
          modelToTimeSeries shouldBe proxyStateData.modelToTimeSeries
          timeSeriesToSourceRef shouldBe Map(
            uuidP -> SourceRef(metaP, None),
            uuidPq -> SourceRef(metaPq, Some(dummyWorker.ref)),
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

      PrimaryServiceProxy.handleCoveredModel(
        modelUuid,
        uuidPq,
        maliciousStateData,
        dummyWorker.ref.toClassic,
      )
      dummyWorker.expectMessage(RegistrationFailedMessage(null))
    }

    "forward the registration request, if worker is already known" in {
      val adaptedStateData = proxyStateData.copy(
        timeSeriesToSourceRef = Map(
          uuidPq -> SourceRef(metaPq, Some(dummyWorker.ref))
        )
      )

      PrimaryServiceProxy.handleCoveredModel(
        modelUuid,
        uuidPq,
        adaptedStateData,
        requestingAgent.ref.toClassic,
      )
      dummyWorker.expectMessage(
        WorkerRegistrationMessage(requestingAgent.ref.toClassic)
      )
    }

    "fail, if worker cannot be spun off" in {
      val maliciousStateData = proxyStateData.copy(
        primaryConfig = PrimaryConfig(
          Some(CouchbaseParams("", "", "", "", "", "", "")),
          None,
          None,
          None,
        )
      )

      PrimaryServiceProxy.handleCoveredModel(
        modelUuid,
        uuidPq,
        maliciousStateData,
        requestingAgent.ref.toClassic,
      )
      requestingAgent.expectMessage(RegistrationFailedMessage(null))
    }

    "spin off a worker, if needed and forward the registration request" in {
      /* We once again fake the class, so that we can infiltrate a probe */
      val worker = TestProbe[PrimaryDataMessage]("workerTestProbe")

      PrimaryServiceProxy.handleCoveredModel(
        modelUuid,
        uuidPq,
        proxyStateData,
        requestingAgent.ref.toClassic,
      )
      worker.expectMessage(
        WorkerRegistrationMessage(requestingAgent.ref.toClassic)
      )
    }
  }

  "Trying to register with a proxy" should {
    "fail, if there is no information for the requested model" in {
      val request = PrimaryServiceRegistrationMessage(
        requestingAgent.ref.toClassic,
        UUID.fromString("2850a2d6-4b70-43c9-b5cc-cd823a72d860"),
      )

      proxy ! request
      requestingAgent.expectMessage(RegistrationFailedMessage(proxy))
    }

    "succeed, if model is handled" in {
      /* We once again fake the class, so that we can infiltrate a probe */
      val worker = TestProbe[PrimaryDataMessage]("workerTestProbe")

      val modifiedInitStateData = initStateData.copy(
        timeSeriesToSourceRef =
          Map(modelUuid -> SourceRef(metaP, Some(worker.ref)))
      )

      val fakeProxyRef =
        testKit.spawn(
          PrimaryServiceProxy(
            scheduler.ref,
            modifiedInitStateData,
          )
        )

      scheduler.expectMessageType[ScheduleActivation]

      /* Initialize the fake proxy */
      fakeProxyRef ! Activation(INIT_SIM_TICK)
      scheduler.expectMessageType[Completion]

      /* Try to register with fake proxy */
      fakeProxyRef ! PrimaryServiceRegistrationMessage(
        requestingAgent.ref.toClassic,
        modelUuid,
      )
      worker.expectMessage(
        WorkerRegistrationMessage(requestingAgent.ref.toClassic)
      )
    }
  }
}
