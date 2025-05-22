/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import edu.ie3.datamodel.io.csv.CsvIndividualTimeSeriesMetaInformation
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.naming.timeseries.ColumnScheme
import edu.ie3.datamodel.io.source.TimeSeriesMappingSource
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesMappingSource
import edu.ie3.datamodel.models.value.SValue
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.agent.participant.ParticipantAgent.RegistrationFailedMessage
import edu.ie3.simona.config.ConfigParams.{
  CouchbaseParams,
  TimeStampedCsvParams,
  TimeStampedInfluxDb1xParams,
}
import edu.ie3.simona.config.InputConfig.Primary as PrimaryConfig
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  Create,
  PrimaryServiceRegistrationMessage,
  ServiceMessages,
  WorkerRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock.LockMsg
import edu.ie3.simona.service.ServiceStateData.ServiceConstantStateData
import edu.ie3.simona.service.primary.PrimaryServiceProxy.{
  InitPrimaryServiceProxyStateData,
  PrimaryServiceStateData,
  SourceRef,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.CsvInitPrimaryServiceStateData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import org.apache.pekko.actor.testkit.typed.Effect.{
  NoEffects,
  Spawned,
  SpawnedAnonymous,
}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  BehaviorTestKit,
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.when
import org.scalatest.Inside.inside
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.wordspec.AnyWordSpecLike
import org.scalatest.{PartialFunctionValues, PrivateMethodTester}
import org.scalatestplus.mockito.MockitoSugar.mock
import org.slf4j.{Logger, LoggerFactory}

import java.nio.file.{Path, Paths}
import java.time.ZonedDateTime
import java.util.UUID
import scala.language.implicitConversions
import scala.util.{Failure, Success}

class PrimaryServiceProxySpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with PrivateMethodTester
    with TableDrivenPropertyChecks
    with PartialFunctionValues
    with TimeSeriesTestData
    with TestSpawnerTyped {
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
        TimeStampedCsvParams(
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
  )

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")

  implicit val constantData: ServiceConstantStateData = {
    val m = mock[ServiceConstantStateData]
    when(m.scheduler).thenReturn(scheduler.ref)
    m
  }

  private val service = TestProbe[ServiceMessages]("primaryServiceProxy")

  given log: Logger = LoggerFactory.getLogger("PrimaryServiceProxySpec")
  given ctx: ActorContext[ServiceMessages] = {
    val m = mock[ActorContext[ServiceMessages]]
    when(m.log).thenReturn(log)
    when(m.self).thenReturn(service.ref)
    m
  }

  val initStateData: InitPrimaryServiceProxyStateData =
    InitPrimaryServiceProxyStateData(
      validPrimaryConfig,
      simulationStart,
    )
  val proxy: ActorRef[ServiceMessages] =
    testKit.spawn(PrimaryServiceProxy(scheduler.ref, initStateData))

  "Building state data from given config" should {
    "succeed with empty state data, in case no config is given" in {
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
        case Success(emptyStateData) =>
          emptyStateData.modelToTimeSeries shouldBe Map.empty
          emptyStateData.timeSeriesToSourceRef shouldBe Map.empty
          emptyStateData.simulationStart shouldBe simulationStart
          emptyStateData.primaryConfig shouldBe maliciousConfig

        case Failure(_) =>
          fail("We expect to receive an empty state data.")
      }
    }

    "fail, in case the wrong config is given" in {
      val maliciousConfig = PrimaryConfig(
        None,
        None,
        Some(TimeStampedInfluxDb1xParams("", -1, "", "")),
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
          exception.getMessage shouldBe "Unsupported config for mapping source: 'TimeStampedInfluxDb1xParams(,-1,,)'"
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
    "successfully instantiate an actor within the actor system" in {
      val testKit = BehaviorTestKit(
        Behaviors.setup[ServiceMessages] { ctx =>
          PrimaryServiceProxy.classToWorkerRef(workerId)(using
            constantData,
            ctx,
          )
          Behaviors.stopped
        }
      )

      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe workerId
      }
    }

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
      val worker = TestProbe[ServiceMessages]("workerTestProbe")
      val lockProbe = TestProbe[LockMsg]("lockProbe")

      val metaInformation = new CsvIndividualTimeSeriesMetaInformation(
        metaPq,
        Paths.get("its_pq_" + uuidPq),
      )

      val context: ActorContext[ServiceMessages] = {
        val m = mock[ActorContext[ServiceMessages]]
        when(m.log).thenReturn(log)

        when(m.spawn(any[Behavior[ServiceMessages]], any[String], any()))
          .thenReturn(worker.ref)
        when(m.spawnAnonymous(any[Behavior[LockMsg]], any()))
          .thenReturn(lockProbe.ref)

        m
      }

      PrimaryServiceProxy.initializeWorker(
        metaInformation,
        simulationStart,
        initStateData.primaryConfig,
      )(using constantData, context)

      inside(worker.expectMessageType[Create]) {
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

      // receiving schedule activation of schedule lock
      scheduler.expectMessageType[ScheduleActivation] match {
        case ScheduleActivation(_, tick, key) =>
          tick shouldBe INIT_SIM_TICK
          key shouldBe None
      }
    }
  }

  private val dummyWorker = TestProbe[ServiceMessages]("dummyWorker")
  private val agentToBeRegistered = TestProbe[Any]("agent")

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
            ) =>
          modelToTimeSeries shouldBe proxyStateData.modelToTimeSeries
          timeSeriesToSourceRef shouldBe Map(
            uuidP -> SourceRef(metaP, None),
            uuidPq -> SourceRef(metaPq, Some(dummyWorker.ref)),
          )
          simulationStart shouldBe proxyStateData.simulationStart
          primaryConfig shouldBe proxyStateData.primaryConfig
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
        agentToBeRegistered.ref,
      )
      agentToBeRegistered.expectMessage(RegistrationFailedMessage(service.ref))
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
        agentToBeRegistered.ref,
      )
      dummyWorker.expectMessage(
        WorkerRegistrationMessage(agentToBeRegistered.ref)
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
        agentToBeRegistered.ref,
      )
      agentToBeRegistered.expectMessage(RegistrationFailedMessage(service.ref))
    }

    "spin off a worker, if needed and forward the registration request" in {
      /* We once again fake the class, so that we can infiltrate a probe */
      val worker = TestProbe[ServiceMessages]("workerTestProbe")

      val adaptedStateData = proxyStateData.copy(
        timeSeriesToSourceRef = Map(
          uuidPq -> SourceRef(metaPq, Some(worker.ref))
        )
      )

      PrimaryServiceProxy.handleCoveredModel(
        modelUuid,
        uuidPq,
        adaptedStateData,
        agentToBeRegistered.ref,
      )

      worker.expectMessage(
        WorkerRegistrationMessage(agentToBeRegistered.ref)
      )
    }

    "spin off only one worker per time series" in {
      val uuid1 = UUID.randomUUID()
      val uuid3 = UUID.randomUUID()
      val timeSeriesUUID1_3 = UUID.randomUUID()
      val meta1_3 = new CsvIndividualTimeSeriesMetaInformation(
        timeSeriesUUID1_3,
        ColumnScheme.ACTIVE_POWER,
        Path.of(""),
      )

      val uuid2 = UUID.randomUUID()
      val timeSeriesUUID2 = UUID.randomUUID()
      val meta2 = new CsvIndividualTimeSeriesMetaInformation(
        timeSeriesUUID2,
        ColumnScheme.ACTIVE_POWER,
        Path.of(""),
      )

      val stateData = PrimaryServiceStateData(
        Map(
          uuid1 -> timeSeriesUUID1_3,
          uuid2 -> timeSeriesUUID2,
          uuid3 -> timeSeriesUUID1_3,
        ),
        Map(
          timeSeriesUUID1_3 -> SourceRef(meta1_3, None),
          timeSeriesUUID2 -> SourceRef(meta2, None),
        ),
        simulationStart,
        validPrimaryConfig,
      )

      val testKit = BehaviorTestKit(PrimaryServiceProxy.onMessage(stateData))

      val participant1 = TestProbe[ParticipantAgent.Request]("participant1")
      val participant2 = TestProbe[ParticipantAgent.Request]("participant2")
      val participant3 = TestProbe[ParticipantAgent.Request]("participant3")

      testKit.run(PrimaryServiceRegistrationMessage(participant1.ref, uuid1))

      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe timeSeriesUUID1_3.toString
      }

      // some behaviors spawned by the scheduler (e.g.: schedule lock)
      testKit.expectEffectPF { case SpawnedAnonymous(_, _) => }
      testKit.expectEffectPF { case SpawnedAnonymous(_, _) => }

      // second participant uses a different input time series
      // therefore, a second worker should be spawned
      testKit.run(PrimaryServiceRegistrationMessage(participant2.ref, uuid2))

      testKit.expectEffectPF { case Spawned(_, actorName, _) =>
        actorName shouldBe timeSeriesUUID2.toString
      }

      // some behaviors spawned by the scheduler (e.g.: schedule lock)
      testKit.expectEffectPF { case SpawnedAnonymous(_, _) => }
      testKit.expectEffectPF { case SpawnedAnonymous(_, _) => }

      // the third participant uses the same time series as the first participant
      // therefore, no additional worker should be spawned
      testKit.run(PrimaryServiceRegistrationMessage(participant3.ref, uuid3))

      testKit.expectEffect(NoEffects)
    }

  }

  "Trying to register with a proxy" should {
    "fail, if there is no information for the requested model" in {
      val request = PrimaryServiceRegistrationMessage(
        agentToBeRegistered.ref,
        UUID.fromString("2850a2d6-4b70-43c9-b5cc-cd823a72d860"),
      )

      proxy ! request
      agentToBeRegistered.expectMessage(RegistrationFailedMessage(proxy))
    }

    "succeed, if model is handled" in {
      /* We once again fake the class, so that we can infiltrate a probe */
      val worker = TestProbe[ServiceMessages]("workerTestProbe")

      val adaptedStateData = proxyStateData.copy(
        modelToTimeSeries = Map(modelUuid -> uuidPq),
        timeSeriesToSourceRef = Map(
          uuidPq -> SourceRef(metaPq, Some(worker.ref))
        ),
      )

      val fakeProxy = BehaviorTestKit(
        PrimaryServiceProxy.onMessage(
          adaptedStateData
        )
      )

      /* Try to register with fake proxy */
      fakeProxy.run(
        PrimaryServiceRegistrationMessage(
          agentToBeRegistered.ref,
          modelUuid,
        )
      )

      worker.expectMessage(
        WorkerRegistrationMessage(agentToBeRegistered.ref)
      )
    }
  }
}
