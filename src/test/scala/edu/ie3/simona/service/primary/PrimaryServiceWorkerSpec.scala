/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import akka.actor.ActorSystem
import akka.testkit.{TestActorRef, TestProbe}
import com.typesafe.config.ConfigFactory
import edu.ie3.datamodel.io.factory.timeseries.TimeBasedSimpleValueFactory
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesSource
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.value.{HeatDemandValue, PValue}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ActivePower
import edu.ie3.simona.ontology.messages.SchedulerMessage
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.ServiceMessage.WorkerRegistrationMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.ontology.trigger.Trigger.{
  ActivityStartTrigger,
  InitializeServiceTrigger
}
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  InitPrimaryServiceStateData,
  PrimaryServiceInitializedStateData,
  ProvidePrimaryDataMessage
}
import edu.ie3.simona.service.primary.PrimaryServiceWorkerSpec.WrongInitPrimaryServiceStateData
import edu.ie3.simona.test.common.AgentSpec
import edu.ie3.simona.test.common.input.TimeSeriesTestData
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
                       |akka.loglevel="OFF"
          """.stripMargin)
      )
    )
    with TimeSeriesTestData {
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
        TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00"),
      timePattern = "yyyy-MM-dd'T'HH:mm:ss'Z'"
    )

  private implicit val powerTolerance: squants.Power = Watts(0.1)

  "A primary service actor" should {
    val serviceRef =
      TestActorRef(
        new PrimaryServiceWorker[PValue](
          self,
          classOf[PValue]
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

    "fail, if pointed to the wrong file" in {
      val maliciousInitData = CsvInitPrimaryServiceStateData(
        timeSeriesUuid = uuidPq,
        simulationStart =
          TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00"),
        csvSep = ";",
        directoryPath = baseDirectoryPath,
        filePath = Paths.get("its_pq_" + uuidPq),
        fileNamingStrategy = new FileNamingStrategy(),
        timePattern = TimeUtil.withDefaults.getDtfPattern
      )
      service.init(maliciousInitData) match {
        case Failure(exception) =>
          exception.getClass shouldBe classOf[IllegalArgumentException]
          exception.getMessage shouldBe "Unable to obtain time series with UUID '3fbfaa97-cff4-46d4-95ba-a95665e87c26'. Please check arguments!"
        case Success(_) =>
          fail("Initialisation with unsupported init data is meant to fail.")
      }
    }

    "be instantiated correctly if faced to valid init data" in {
      service.init(validInitData) match {
        case Success((stateData, maybeTriggerMessages)) =>
          /* Initialisation was successful. Check state data and triggers, that will be sent to scheduler */
          stateData match {
            case PrimaryServiceInitializedStateData(
                  nextActivationTick,
                  activationTicks,
                  simulationStart,
                  source,
                  subscribers
                ) =>
              nextActivationTick shouldBe Some(0L)
              activationTicks.toVector shouldBe Vector(
                900L,
                1800L
              ) // The first tick should already been popped
              simulationStart shouldBe validInitData.simulationStart
              source.getClass shouldBe classOf[CsvTimeSeriesSource[PValue]]
              subscribers.isEmpty shouldBe true
          }
          /* We expect a request to be triggered in tick 0 */
          maybeTriggerMessages shouldBe Some(
            Seq(
              ScheduleTriggerMessage(ActivityStartTrigger(0L), serviceRef)
            )
          )
        case Failure(_) =>
          fail("Initialisation with supported init data is not meant to fail.")
      }
    }

    /* Init the service actor */
    serviceRef ! TriggerWithIdMessage(
      InitializeServiceTrigger(validInitData),
      0L,
      self
    )
    expectCompletionMessage()

    "refuse registration for wrong registration request" in {
      serviceRef ! RegisterForWeatherMessage(51.4843281, 7.4116482)
      expectNoMessage()
    }

    val systemParticipant: TestProbe = TestProbe("dummySystemParticipant")
    "correctly register a forwarded request" in {
      serviceRef ! WorkerRegistrationMessage(systemParticipant.ref)

      /* Wait for request approval */
      systemParticipant.expectMsg(RegistrationSuccessfulMessage(Some(0L)))

      /* We cannot directly check, if the requesting actor is among the subscribers, therefore we ask the actor to
       * provide data to all subscribed actors and check, if the subscribed probe gets one */
      serviceRef ! TriggerWithIdMessage(ActivityStartTrigger(0L), 1L, self)
      expectCompletionMessage()
      systemParticipant.expectMsgAllClassOf(classOf[ProvidePrimaryDataMessage])
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
        new TimeBasedSimpleValueFactory[PValue](classOf[PValue])
      ),
      Vector(self)
    )

    "correctly distribute proper primary data" in {
      val announcePrimaryData = PrivateMethod[
        (
            PrimaryServiceInitializedStateData[PValue],
            Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
        )
      ](Symbol("announcePrimaryData"))
      val tick = 0L
      val primaryData = ActivePower(Kilowatts(50.0))
      val serviceStateData = validStateData.copy()

      service invokePrivate announcePrimaryData(
        tick,
        primaryData,
        serviceStateData
      ) match {
        case (updatedStateData, maybeTriggerMessages) =>
          /* Check updated state data */
          inside(updatedStateData) {
            case PrimaryServiceInitializedStateData(
                  nextActivationTick,
                  activationTicks,
                  _,
                  _,
                  _
                ) =>
              nextActivationTick shouldBe Some(900L)
              activationTicks.size shouldBe 0
          }
          /* Check trigger messages */
          maybeTriggerMessages match {
            case Some(triggerSeq) =>
              triggerSeq.size shouldBe 1
              triggerSeq.headOption match {
                case Some(
                      ScheduleTriggerMessage(
                        ActivityStartTrigger(triggerTick),
                        actorToBeScheduled,
                        _
                      )
                    ) =>
                  triggerTick shouldBe 900L
                  actorToBeScheduled shouldBe serviceRef
                case Some(value) =>
                  fail(s"Got wrong trigger messages: '$value'.")
                case None => fail("Did expect to get at least on trigger.")
              }
            case None => fail("Expect a trigger message for tick 900.")
          }
      }
      /* Check, if correct message is sent */
      expectMsgClass(classOf[ProvidePrimaryDataMessage]) match {
        case ProvidePrimaryDataMessage(
              actualTick,
              actualData,
              actualNextDataTick
            ) =>
          actualTick shouldBe 0L
          actualData shouldBe primaryData
          actualNextDataTick shouldBe Some(900L)
      }
    }

    val processDataAndAnnounce = PrivateMethod[
      (
          PrimaryServiceInitializedStateData[PValue],
          Option[Seq[SchedulerMessage.ScheduleTriggerMessage]]
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
        stateData
      ) match {
        case (
              PrimaryServiceInitializedStateData(
                nextActivationTick,
                _,
                _,
                _,
                _
              ),
              maybeTriggerMessages
            ) =>
          nextActivationTick shouldBe Some(900L)
          maybeTriggerMessages match {
            case Some(triggerSeq) => triggerSeq.size shouldBe 1
            case None             => fail("Expect a trigger for tick 900.")
          }
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
        serviceStateData
      ) match {
        case (updatedStateData, _) =>
          inside(updatedStateData) {
            case PrimaryServiceInitializedStateData(
                  nextActivationTick,
                  activationTicks,
                  _,
                  _,
                  _
                ) =>
              nextActivationTick shouldBe Some(900L)
              activationTicks.size shouldBe 0
          }
        /* Rest has already been tested */
      }

      expectMsg(
        ProvidePrimaryDataMessage(
          tick,
          ActivePower(Kilowatts(50.0)),
          Some(900L)
        )
      )
    }

    "should not announce anything, if time step is not covered in source" in {
      val triggerId = 2L
      serviceRef ! TriggerWithIdMessage(
        ActivityStartTrigger(200L),
        triggerId,
        self
      )
      inside(expectMsgClass(classOf[CompletionMessage])) {
        case CompletionMessage(actualTriggerId, newTriggers) =>
          actualTriggerId shouldBe triggerId
          newTriggers match {
            case Some(triggerSeq) => triggerSeq.size shouldBe 1
            case None             => fail("Expect a trigger for tick 1800.")
          }
      }
      expectNoMessage()
    }

    "should announce something, if the time step is covered in source" in {
      val triggerId = 3L
      serviceRef ! TriggerWithIdMessage(
        ActivityStartTrigger(900L),
        triggerId,
        self
      )
      inside(expectMsgClass(classOf[CompletionMessage])) {
        case CompletionMessage(actualTriggerId, newTriggers) =>
          actualTriggerId shouldBe triggerId
          newTriggers shouldBe None
      }
      inside(
        systemParticipant.expectMsgClass(classOf[ProvidePrimaryDataMessage])
      ) { case ProvidePrimaryDataMessage(tick, data, nextDataTick) =>
        tick shouldBe 900L
        inside(data) {
          case ActivePower(p) =>
            (p ~= Kilowatts(1250.0)) shouldBe true
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
      override val timeSeriesUuid: UUID
  ) extends InitPrimaryServiceStateData

  object WrongInitPrimaryServiceStateData {
    def apply(): WrongInitPrimaryServiceStateData =
      new WrongInitPrimaryServiceStateData(
        ZonedDateTime.now(),
        UUID.randomUUID()
      )
  }
}
