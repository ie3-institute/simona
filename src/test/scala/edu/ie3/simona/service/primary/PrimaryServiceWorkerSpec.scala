/*
 * © 2021-2026. TU Dortmund University,
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
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.PrimaryData.{ActivePower, ActivePowerExtra}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  InitPrimaryServiceStateData,
  PrimaryServiceInitializedStateData,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorkerSpec.WrongInitPrimaryServiceStateData
import edu.ie3.simona.test.common.TestSpawnerTyped
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.test.helper.TestResourceHelper
import edu.ie3.simona.test.matchers.SquantsMatchers
import edu.ie3.simona.util.Coordinate
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.collection.immutable.ActivationTickQueue
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import org.apache.pekko.actor.typed.ActorRef
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
    with TestResourceHelper
    with TestSpawnerTyped {

  val baseDirectoryPath: Path = getResourcePath("_it")

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
    )

  private given powerTolerance: squants.Power = Watts(0.1)

  "A primary service actor" should {
    val scheduler = TestProbe[SchedulerMessage]("scheduler")
    val systemParticipant = TestProbe[Any]("dummySystemParticipant")

    val serviceKey =
      ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
    // lock activation scheduled
    scheduler.expectMessageType[ScheduleActivation]
    given serviceRef: ActorRef[PrimaryServiceProxy.Message] =
      spawn(PrimaryServiceWorker(scheduler.ref, validInitData, serviceKey))
    given log: Logger =
      LoggerFactory.getLogger(classOf[PrimaryServiceWorkerSpec])

    "init the service actor" in {
      scheduler.expectMessage(
        ScheduleActivation(serviceRef, 0L, Some(serviceKey))
      )
    }

    "refuse instantiation on wrong init data" in {
      val maliciousInitData = WrongInitPrimaryServiceStateData()
      PrimaryServiceWorker.init(maliciousInitData) match {
        case Failure(exception) =>
          exception.getMessage shouldBe "Provided init data 'WrongInitPrimaryServiceStateData' for primary service are invalid!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "fail to init, if time series has no data" in {
      val initData = validInitData.copy(
        timeSeriesUuid = uuidEmpty,
        filePath = Paths.get("its_p_" + uuidEmpty),
        simulationStart = validInitData.simulationStart.plusHours(1),
      )

      PrimaryServiceWorker.init(initData) match {
        case Failure(exception) =>
          exception.getMessage shouldBe "No appropriate data found within simulation time range in timeseries 'b73a7e3f-9045-40cd-b518-c11a9a6a1025'!"
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

    "init, if there is a value before the simulation start" in {
      val initData = validInitData.copy(
        simulationStart = validInitData.simulationStart.plusHours(1)
      )

      PrimaryServiceWorker.init(initData) match {
        case Success((_, maybeNextTick)) =>
          maybeNextTick shouldBe Some(0L)

        case Failure(_) =>
          fail("Initialisation with init data is meant to succeed.")
      }
    }

    "init, if there are values before and after the simulation start" in {
      val initData = validInitData.copy(
        simulationStart = validInitData.simulationStart.plusMinutes(5)
      )

      PrimaryServiceWorker.init(initData) match {
        case Success((_, maybeNextTick)) =>
          maybeNextTick shouldBe Some(0L)

        case Failure(_) =>
          fail("Initialisation with init data is meant to succeed.")
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
                  activationTicks,
                  simulationStart,
                  valueClass,
                  source,
                  subscribers,
                ) =>
              activationTicks.nextTick shouldBe Some(0L)
              activationTicks.length shouldBe 3 // tick 0 still included
              simulationStart shouldBe validInitData.simulationStart
              valueClass shouldBe classOf[PValue]
              source.getClass shouldBe classOf[CsvTimeSeriesSource[PValue]]
              subscribers.isEmpty shouldBe true
          }
          /* We expect a request to be triggered in tick 0 */
          maybeNextTick shouldBe Some(0)
        case Failure(_) =>
          fail("Initialisation with supported init data is not meant to fail.")
      }
    }

    "refuse registration for wrong registration request" in {
      val schedulerProbe = TestProbe[SchedulerMessage]("schedulerProbe")

      // we need to create another service, since we want to continue using the other in later tests
      val serviceKey =
        ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
      // lock activation scheduled
      scheduler.expectMessageType[ScheduleActivation]
      val service =
        spawn(
          PrimaryServiceWorker(schedulerProbe.ref, validInitData, serviceKey)
        )

      service ! SecondaryServiceRegistrationMessage(
        systemParticipant.ref,
        DataTimeType.Current,
        Coordinate(51.4843281, 7.4116482),
      )

      val deathWatch = createTestProbe("deathWatch")
      deathWatch.expectTerminated(service.ref)
    }

    "correctly register a forwarded request" in {
      serviceRef ! WorkerRegistrationMessage(systemParticipant.ref)

      /* Wait for request approval */
      systemParticipant.expectMessage(
        PrimaryRegistrationSuccessfulMessage(
          serviceRef,
          0L,
          ActivePowerExtra,
        )
      )

      /* We cannot directly check, if the requesting actor is among the subscribers, therefore we ask the actor to
       * provide data to all subscribed actors and check, if the subscribed probe gets one */
      serviceRef ! Activation(0)
      scheduler.expectMessageType[Completion]
      systemParticipant.expectMessageType[DataProvision]
    }

    /* At this point, the test (self) is registered with the service */

    val validStateData = PrimaryServiceInitializedStateData(
      ActivationTickQueue(Seq(0L, 900L)),
      validInitData.simulationStart,
      classOf[PValue],
      new CsvTimeSeriesSource[PValue](
        ";",
        baseDirectoryPath,
        new FileNamingStrategy(),
        uuidP,
        Paths.get("its_p_" + uuidP),
        classOf[PValue],
        new TimeBasedSimpleValueFactory[PValue](classOf[PValue]),
      ),
      Vector(systemParticipant.ref),
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
                  activationTicks,
                  _,
                  _,
                  _,
                  _,
                ) =>
              activationTicks.nextTick shouldBe Some(900L)
              activationTicks.length shouldBe 1
          }
          /* Check trigger messages */
          maybeNextTick shouldBe Some(900L)
      }
      /* Check, if correct message is sent */
      systemParticipant.expectMessageType[DataProvision] match {
        case DataProvision(
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
      val stateData = validStateData.copy()

      PrimaryServiceWorker.processDataAndAnnounce(
        tick,
        maliciousValue,
        stateData,
      ) match {
        case (
              PrimaryServiceInitializedStateData(
                activationTicks,
                _,
                _,
                _,
                _,
              ),
              maybeNextTick,
            ) =>
          activationTicks.nextTick shouldBe Some(900L)
          maybeNextTick shouldBe Some(900L)
      }
      systemParticipant.expectNoMessage()
    }

    "announce information, if conversion succeeds" in {
      val tick = 0L
      val value =
        new PValue(Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT))
      val serviceStateData = validStateData.copy()

      PrimaryServiceWorker.processDataAndAnnounce(
        tick,
        value,
        serviceStateData,
      ) match {
        case (updatedStateData, _) =>
          inside(updatedStateData) {
            case PrimaryServiceInitializedStateData(
                  activationTicks,
                  _,
                  _,
                  _,
                  _,
                ) =>
              activationTicks.nextTick shouldBe Some(900L)
              activationTicks.length shouldBe 1
          }
        /* Rest has already been tested */
      }

      systemParticipant.expectMessage(
        DataProvision(
          tick,
          serviceRef,
          ActivePower(Kilowatts(50.0)),
          Some(900L),
        )
      )
    }

    "should announce something" in {
      serviceRef ! Activation(900)
      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.newTick shouldBe Some(1800)

      inside(
        systemParticipant.expectMessageType[DataProvision]
      ) {
        case DataProvision(
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
          nextDataTick shouldBe Some(1800)
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
