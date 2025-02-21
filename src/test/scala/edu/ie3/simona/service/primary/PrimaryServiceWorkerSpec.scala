/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.factory.timeseries.TimeBasedSimpleValueFactory
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesSource
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.value.{HeatDemandValue, PValue, SValue}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ActivePower
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.PrimaryDataMessage.{
  ProvidePrimaryDataMessage,
  WorkerRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessageUniversal.{
  Create,
  WrappedActivation,
}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.ontology.messages.services.{
  PrimaryDataMessage,
  ServiceMessageUniversal,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  InitPrimaryServiceStateData,
  PrimaryServiceInitializedStateData,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorkerSpec.WrongInitPrimaryServiceStateData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.test.matchers.SquantsMatchers
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.scalatest.Inside.inside
import org.scalatest.PrivateMethodTester
import org.scalatest.wordspec.AnyWordSpecLike
import org.slf4j.{Logger, LoggerFactory}
import squants.energy.{Kilowatts, Watts}
import tech.units.indriya.quantity.Quantities

import java.nio.file.{Path, Paths}
import java.time.ZonedDateTime
import java.util.UUID
import scala.language.implicitConversions
import scala.util.{Failure, Success}

class PrimaryServiceWorkerSpec
    extends ScalaTestWithActorTestKit
    with AnyWordSpecLike
    with SquantsMatchers
    with PrivateMethodTester
    with LazyLogging
    with TimeSeriesTestData
    with TestSpawnerTyped {

  implicit def wrap(msg: Activation): ServiceMessageUniversal =
    WrappedActivation(msg)

  // this works both on Windows and Unix systems
  val baseDirectoryPath: Path = Paths
    .get(
      this.getClass
        .getResource(
          "_it"
        )
        .toURI
    )

  val validInitData: CsvInitPrimaryServiceStateData[PValue] =
    CsvInitPrimaryServiceStateData(
      valueClass = classOf[PValue],
      timeSeriesUuid = uuidP,
      csvSep = ";",
      directoryPath = baseDirectoryPath,
      filePath = Paths.get("its_p_" + uuidP),
      fileNamingStrategy = new FileNamingStrategy(),
      simulationStart =
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
      timePattern = "yyyy-MM-dd'T'HH:mm:ssX",
    )

  private implicit val powerTolerance: squants.Power = Watts(0.1)

  "A primary service actor" should {
    val scheduler = TestProbe[SchedulerMessage]("scheduler")
    val systemParticipant = TestProbe[Any]("dummySystemParticipant")

    implicit val serviceRef: ActorRef[PrimaryDataMessage] =
      testKit.spawn(PrimaryServiceWorker(scheduler.ref))
    implicit val log: Logger =
      LoggerFactory.getLogger(classOf[PrimaryServiceWorkerSpec])

    "refuse instantiation on wrong init data" in {
      val maliciousInitData = WrongInitPrimaryServiceStateData()
      PrimaryServiceWorker.init(maliciousInitData) match {
        case Failure(exception) =>
          exception.getMessage shouldBe "Provided init data 'WrongInitPrimaryServiceStateData' for primary service are invalid!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "fail to init, if time series ends with delay before simulation start" in {
      val initData = validInitData.copy(
        simulationStart = validInitData.simulationStart.plusHours(1)
      )

      PrimaryServiceWorker.init(initData) match {
        case Failure(exception) =>
          exception.getMessage shouldBe "No appropriate data found within simulation time range in timeseries '9185b8c1-86ba-4a16-8dea-5ac898e8caa5'!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "fail to init, if time series starts with delay after simulation start" in {
      val initData = validInitData.copy(
        simulationStart = validInitData.simulationStart.minusHours(1)
      )

      PrimaryServiceWorker.init(initData) match {
        case Failure(exception) =>
          exception.getMessage shouldBe "The data for the timeseries '9185b8c1-86ba-4a16-8dea-5ac898e8caa5' starts after the start of this simulation (tick: 3600)! This is not allowed!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "fail, if pointed to the wrong file" in {
      // time series exists, but is malformed
      val tsUuid = UUID.fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c27")

      val maliciousInitData = CsvInitPrimaryServiceStateData[SValue](
        valueClass = classOf[SValue],
        timeSeriesUuid = tsUuid,
        simulationStart =
          TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
        csvSep = ";",
        directoryPath = baseDirectoryPath,
        filePath = Paths.get("its_pq_" + tsUuid),
        fileNamingStrategy = new FileNamingStrategy(),
        timePattern = "yyyy-MM-dd'T'HH:mm:ssX",
      )
      PrimaryServiceWorker.init(maliciousInitData) match {
        case Failure(exception) =>
          exception.getClass shouldBe classOf[IllegalArgumentException]
          exception.getMessage shouldBe "Unable to obtain time series with UUID '3fbfaa97-cff4-46d4-95ba-a95665e87c27'. Please check arguments!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "be instantiated correctly if faced to valid init data" in {
      PrimaryServiceWorker.init(validInitData) match {
        case Success((stateData, maybeNextTick)) =>
          /* Initialisation was successful. Check state data and triggers, that will be sent to scheduler */
          stateData match {
            case PrimaryServiceInitializedStateData(
                  nextActivationTick,
                  activationTicks,
                  simulationStart,
                  source,
                  subscribers,
                ) =>
              nextActivationTick shouldBe Some(0L)
              activationTicks.toVector shouldBe Vector(
                900L,
                1800L,
              ) // The first tick should already been popped
              simulationStart shouldBe validInitData.simulationStart
              source.getClass shouldBe classOf[CsvTimeSeriesSource[PValue]]
              subscribers.isEmpty shouldBe true
          }
          /* We expect a request to be triggered in tick 0 */
          maybeNextTick shouldBe Some(0)
        case Failure(_) =>
          fail("Initialisation with supported init data is not meant to fail.")
      }
    }

    "init the service actor" in {
      val key = ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      scheduler
        .expectMessageType[ScheduleActivation] // lock activation scheduled

      serviceRef ! Create(validInitData, key)

      val activationMsg = scheduler.expectMessageType[ScheduleActivation]
      activationMsg.tick shouldBe INIT_SIM_TICK
      activationMsg.unlockKey shouldBe Some(key)

      serviceRef ! Activation(INIT_SIM_TICK)
      scheduler.expectMessage(Completion(activationMsg.actor, Some(0)))
    }

    "refuse registration for wrong registration request" in {
      serviceRef ! RegisterForWeatherMessage(
        systemParticipant.ref.toClassic,
        51.4843281,
        7.4116482,
      )
      systemParticipant.expectNoMessage()
    }

    "correctly register a forwarded request" in {
      serviceRef ! WorkerRegistrationMessage(systemParticipant.ref.toClassic)

      /* Wait for request approval */
      systemParticipant.expectMessage(
        RegistrationSuccessfulMessage(serviceRef, Some(0L))
      )

      /* We cannot directly check, if the requesting actor is among the subscribers, therefore we ask the actor to
       * provide data to all subscribed actors and check, if the subscribed probe gets one */
      serviceRef ! Activation(0)
      scheduler.expectMessageType[Completion]
      systemParticipant.expectMessageType[ProvidePrimaryDataMessage]
    }

    /* At this point, the test (self) is registered with the service */

    val validStateData = PrimaryServiceInitializedStateData(
      Some(0L),
      SortedDistinctSeq(Seq(900L)),
      validInitData.simulationStart,
      new CsvTimeSeriesSource[PValue](
        ";",
        baseDirectoryPath,
        new FileNamingStrategy(),
        uuidP,
        Paths.get("its_p_" + uuidP),
        classOf[PValue],
        new TimeBasedSimpleValueFactory[PValue](classOf[PValue]),
      ),
      Vector(systemParticipant.ref.toClassic),
    )

    "correctly distribute proper primary data" in {
      val tick = 0L
      val primaryData = ActivePower(Kilowatts(50.0))
      val serviceStateData = validStateData.copy()

      PrimaryServiceWorker.announcePrimaryData(
        tick,
        primaryData,
        serviceStateData,
      ) match {
        case (updatedStateData, maybeNextTick) =>
          /* Check updated state data */
          inside(updatedStateData) {
            case PrimaryServiceInitializedStateData(
                  nextActivationTick,
                  activationTicks,
                  _,
                  _,
                  _,
                ) =>
              nextActivationTick shouldBe Some(900L)
              activationTicks.size shouldBe 0
          }
          /* Check trigger messages */
          maybeNextTick shouldBe Some(900L)
      }
      /* Check, if correct message is sent */
      systemParticipant.expectMessageType[ProvidePrimaryDataMessage] match {
        case ProvidePrimaryDataMessage(
              actualTick,
              actualServiceRef,
              actualData,
              actualNextDataTick,
            ) =>
          actualTick shouldBe 0L
          actualServiceRef shouldBe serviceRef
          actualData shouldBe primaryData
          actualNextDataTick shouldBe Some(900L)
      }
    }

    "not sent anything, if conversion to primary data failed" in {
      val tick = 0L
      val maliciousValue = new HeatDemandValue(
        Quantities.getQuantity(50d, StandardUnits.HEAT_DEMAND)
      )
      val stateData = validStateData.copy(
        activationTicks = SortedDistinctSeq(Seq(900L))
      )

      PrimaryServiceWorker.processDataAndAnnounce(
        tick,
        maliciousValue,
        stateData,
      ) match {
        case (
              PrimaryServiceInitializedStateData(
                nextActivationTick,
                _,
                _,
                _,
                _,
              ),
              maybeNextTick,
            ) =>
          nextActivationTick shouldBe Some(900L)
          maybeNextTick shouldBe Some(900L)
      }
      systemParticipant.expectNoMessage()
    }

    "announce information, if conversion succeeds" in {
      val tick = 0L
      val value =
        new PValue(Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT))
      val serviceStateData = validStateData.copy(
        activationTicks = SortedDistinctSeq(Seq(900L))
      )

      PrimaryServiceWorker.processDataAndAnnounce(
        tick,
        value,
        serviceStateData,
      ) match {
        case (updatedStateData, _) =>
          inside(updatedStateData) {
            case PrimaryServiceInitializedStateData(
                  nextActivationTick,
                  activationTicks,
                  _,
                  _,
                  _,
                ) =>
              nextActivationTick shouldBe Some(900L)
              activationTicks.size shouldBe 0
          }
        /* Rest has already been tested */
      }

      systemParticipant.expectMessage(
        ProvidePrimaryDataMessage(
          tick,
          serviceRef,
          ActivePower(Kilowatts(50.0)),
          Some(900L),
        )
      )
    }

    "should not announce anything, if time step is not covered in source" in {

      serviceRef ! Activation(200)

      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.newTick shouldBe Some(1800)

      systemParticipant.expectNoMessage()
    }

    "should announce something, if the time step is covered in source" in {
      serviceRef ! Activation(900)
      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.newTick shouldBe None

      inside(
        systemParticipant.expectMessageType[ProvidePrimaryDataMessage]
      ) {
        case ProvidePrimaryDataMessage(
              tick,
              actualServiceRef,
              data,
              nextDataTick,
            ) =>
          tick shouldBe 900L
          actualServiceRef shouldBe serviceRef
          inside(data) {
            case ActivePower(p) =>
              p should approximate(Kilowatts(1250.0))
            case _ => fail("Expected to get active power only.")
          }
          nextDataTick shouldBe None
      }
    }
  }
}

object PrimaryServiceWorkerSpec {
  final case class WrongInitPrimaryServiceStateData(
      override val simulationStart: ZonedDateTime,
      override val timeSeriesUuid: UUID,
      override val valueClass: Class[PValue],
  ) extends InitPrimaryServiceStateData[PValue]

  object WrongInitPrimaryServiceStateData {
    def apply(): WrongInitPrimaryServiceStateData =
      new WrongInitPrimaryServiceStateData(
        ZonedDateTime.now(),
        UUID.randomUUID(),
        classOf[PValue],
      )
  }
}
