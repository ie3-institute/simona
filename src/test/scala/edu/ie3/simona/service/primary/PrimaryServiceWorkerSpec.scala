/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.io.factory.timeseries.TimeBasedSimpleValueFactory
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesSource
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.value.{HeatDemandValue, PValue}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ActivePower,
  ActivePowerExtra,
}
import edu.ie3.simona.agent.participant2.ParticipantAgent.{
  DataProvision,
  PrimaryRegistrationSuccessfulMessage,
}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.WorkerRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  InitPrimaryServiceStateData,
  PrimaryServiceInitializedStateData,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorkerSpec.WrongInitPrimaryServiceStateData
import edu.ie3.simona.test.common.{AgentSpec, TestSpawnerClassic}
import edu.ie3.simona.test.common.input.TimeSeriesTestData
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import squants.energy.{Kilowatts, Watts}
import tech.units.indriya.quantity.Quantities

import java.nio.file.{Path, Paths}
import java.time.ZonedDateTime
import java.util.UUID
import scala.util.{Failure, Success}

class PrimaryServiceWorkerSpec
    extends AgentSpec(
      ActorSystem(
        "PrimaryServiceWorkerSpec",
        ConfigFactory
          .parseString("""
                       |pekko.loglevel="OFF"
          """.stripMargin),
      )
    )
    with TimeSeriesTestData
    with TestSpawnerClassic {
  // this works both on Windows and Unix systems
  val baseDirectoryPath: Path = Paths
    .get(
      this.getClass
        .getResource(
          "_it"
        )
        .toURI
    )

  val validInitData: CsvInitPrimaryServiceStateData =
    CsvInitPrimaryServiceStateData(
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
    val scheduler = TestProbe("scheduler")

    val serviceRef =
      TestActorRef(
        new PrimaryServiceWorker[PValue](
          scheduler.ref,
          classOf[PValue],
        )
      )
    val service = serviceRef.underlyingActor

    "refuse instantiation on wrong init data" in {
      val maliciousInitData = WrongInitPrimaryServiceStateData()
      service.init(maliciousInitData) match {
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

      service.init(initData) match {
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

      service.init(initData) match {
        case Failure(exception) =>
          exception.getMessage shouldBe "The data for the timeseries '9185b8c1-86ba-4a16-8dea-5ac898e8caa5' starts after the start of this simulation (tick: 3600)! This is not allowed!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "fail, if pointed to the wrong file" in {
      // time series exists, but is malformed
      val tsUuid = UUID.fromString("3fbfaa97-cff4-46d4-95ba-a95665e87c27")

      val maliciousInitData = CsvInitPrimaryServiceStateData(
        timeSeriesUuid = tsUuid,
        simulationStart =
          TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
        csvSep = ";",
        directoryPath = baseDirectoryPath,
        filePath = Paths.get("its_pq_" + tsUuid),
        fileNamingStrategy = new FileNamingStrategy(),
        timePattern = "yyyy-MM-dd'T'HH:mm:ssX",
      )
      service.init(maliciousInitData) match {
        case Failure(exception) =>
          exception.getClass shouldBe classOf[IllegalArgumentException]
          exception.getMessage shouldBe "Unable to obtain time series with UUID '3fbfaa97-cff4-46d4-95ba-a95665e87c27'. Please check arguments!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "be instantiated correctly if faced to valid init data" in {
      service.init(validInitData) match {
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
      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        serviceRef,
        SimonaService.Create(validInitData, key),
      )
      scheduler.expectMsg(
        ScheduleActivation(serviceRef.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(serviceRef, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(serviceRef.toTyped, Some(0)))
    }

    "refuse registration for wrong registration request" in {
      serviceRef ! RegisterForWeatherMessage(51.4843281, 7.4116482)
      expectNoMessage()
    }

    val systemParticipant: TestProbe = TestProbe("dummySystemParticipant")
    "correctly register a forwarded request" in {
      serviceRef ! WorkerRegistrationMessage(systemParticipant.ref)

      /* Wait for request approval */
      systemParticipant.expectMsg(
        PrimaryRegistrationSuccessfulMessage(serviceRef, 0L, ActivePowerExtra)
      )

      /* We cannot directly check, if the requesting actor is among the subscribers, therefore we ask the actor to
       * provide data to all subscribed actors and check, if the subscribed probe gets one */
      scheduler.send(serviceRef, Activation(0))
      scheduler.expectMsgType[Completion]
      systemParticipant.expectMsgAllClassOf(classOf[DataProvision[_]])
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
      Vector(self),
    )

    "correctly distribute proper primary data" in {
      val announcePrimaryData = PrivateMethod[
        (
            PrimaryServiceInitializedStateData[PValue],
            Option[Long],
        )
      ](Symbol("announcePrimaryData"))
      val tick = 0L
      val primaryData = ActivePower(Kilowatts(50.0))
      val serviceStateData = validStateData.copy()

      service invokePrivate announcePrimaryData(
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
      expectMsgClass(classOf[DataProvision[_]]) match {
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

    val processDataAndAnnounce = PrivateMethod[
      (
          PrimaryServiceInitializedStateData[PValue],
          Option[Long],
      )
    ](Symbol("processDataAndAnnounce"))

    "not sent anything, if conversion to primary data failed" in {
      val tick = 0L
      val maliciousValue = new HeatDemandValue(
        Quantities.getQuantity(50d, StandardUnits.HEAT_DEMAND)
      )
      val stateData = validStateData.copy(
        activationTicks = SortedDistinctSeq(Seq(900L))
      )

      service invokePrivate processDataAndAnnounce(
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
      expectNoMessage()
    }

    "announce information, if conversion succeeds" in {
      val tick = 0L
      val value =
        new PValue(Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT))
      val serviceStateData = validStateData.copy(
        activationTicks = SortedDistinctSeq(Seq(900L))
      )

      service invokePrivate processDataAndAnnounce(
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

      expectMsg(
        DataProvision(
          tick,
          serviceRef,
          ActivePower(Kilowatts(50.0)),
          Some(900L),
        )
      )
    }

    "should not announce anything, if time step is not covered in source" in {

      scheduler.send(serviceRef, Activation(200))

      scheduler.expectMsg(Completion(serviceRef.toTyped, Some(1800)))
      expectNoMessage()
    }

    "should announce something, if the time step is covered in source" in {
      scheduler.send(serviceRef, Activation(900))
      scheduler.expectMsg(Completion(serviceRef.toTyped))

      inside(
        systemParticipant.expectMsgClass(classOf[DataProvision[_]])
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
          nextDataTick shouldBe None
      }
    }
  }
}

object PrimaryServiceWorkerSpec {
  final case class WrongInitPrimaryServiceStateData(
      override val simulationStart: ZonedDateTime,
      override val timeSeriesUuid: UUID,
  ) extends InitPrimaryServiceStateData

  object WrongInitPrimaryServiceStateData {
    def apply(): WrongInitPrimaryServiceStateData =
      new WrongInitPrimaryServiceStateData(
        ZonedDateTime.now(),
        UUID.randomUUID(),
      )
  }
}
