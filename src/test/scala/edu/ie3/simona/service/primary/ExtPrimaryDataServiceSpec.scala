/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.primary

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.test.common.service.PrimaryDataFactoryDefault
import edu.ie3.datamodel.io.factory.timeseries.TimeBasedSimpleValueFactory
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.CsvTimeSeriesSource
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.value.{HeatDemandValue, PValue}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ActivePower
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.data.ev.ontology._
import edu.ie3.simona.api.data.ontology.ScheduleDataServiceMessage
import edu.ie3.simona.api.data.primarydata.ExtPrimaryData
import edu.ie3.simona.exceptions.ServiceException
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.services.EvMessage._
import edu.ie3.simona.ontology.messages.services.ServiceMessage.{
  ExtPrimaryDataServiceRegistrationMessage,
  WorkerRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.services.ServiceMessage.RegistrationResponseMessage.RegistrationSuccessfulMessage
import edu.ie3.simona.ontology.messages.services.WeatherMessage.RegisterForWeatherMessage
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.SimonaService
import edu.ie3.simona.service.ev.ExtEvDataService.InitExtEvData
import edu.ie3.simona.service.primary.ExtPrimaryDataService.InitExtPrimaryData
import edu.ie3.simona.service.primary.PrimaryServiceWorker.{
  CsvInitPrimaryServiceStateData,
  PrimaryServiceInitializedStateData,
  ProvidePrimaryDataMessage,
}
import edu.ie3.simona.service.primary.PrimaryServiceWorkerSpec.WrongInitPrimaryServiceStateData
import edu.ie3.simona.test.common.{
  EvTestData,
  TestKitWithShutdown,
  TestSpawnerClassic,
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.collection.immutable.SortedDistinctSeq
import org.apache.pekko.actor.typed.scaladsl.adapter.ClassicActorRefOps
import org.apache.pekko.actor.{ActorRef, ActorSystem}
import org.apache.pekko.testkit.{TestActorRef, TestProbe}
import org.scalatest.wordspec.AnyWordSpecLike
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities

import java.nio.file.Paths
import java.time.ZonedDateTime
import java.util.UUID
import scala.concurrent.duration.DurationInt
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success}

class ExtPrimaryDataServiceSpec
    extends TestKitWithShutdown(
      ActorSystem(
        "ExtPrimaryDataServiceSpec",
        ConfigFactory
          .parseString("""
        |pekko.loggers = ["org.apache.pekko.testkit.TestEventListener"]
        |pekko.loglevel = "INFO"
        |""".stripMargin),
      )
    )
    with TestSpawnerClassic {

  private val scheduler = TestProbe("scheduler")
  private val extSimAdapter = TestProbe("extSimAdapter")

  private val primaryDataFactory = new PrimaryDataFactoryDefault

  private val extPrimaryData = (dataService: ActorRef) =>
    new ExtPrimaryData(
      dataService,
      extSimAdapter.ref,
      primaryDataFactory,
    )

  "An uninitialized external primary data service" must {

    "send correct completion message after initialisation" in {
      val primaryDataService = TestActorRef(
        new ExtPrimaryDataService(
          scheduler.ref
        )
      )

      val key =
        ScheduleLock.singleKey(TSpawner, scheduler.ref.toTyped, INIT_SIM_TICK)
      scheduler.expectMsgType[ScheduleActivation] // lock activation scheduled

      scheduler.send(
        primaryDataService,
        SimonaService.Create(
          InitExtPrimaryData(extPrimaryData(primaryDataService)),
          key,
        ),
      )
      scheduler.expectMsg(
        ScheduleActivation(primaryDataService.toTyped, INIT_SIM_TICK, Some(key))
      )

      scheduler.send(primaryDataService, Activation(INIT_SIM_TICK))
      scheduler.expectMsg(Completion(primaryDataService.toTyped))
    }
  }

  "An external primary service actor" should {
    val serviceRef =
      TestActorRef(
        new ExtPrimaryDataService(
          scheduler.ref
        )
      )

    "refuse registration for wrong registration request" in {
      serviceRef ! RegisterForWeatherMessage(51.4843281, 7.4116482)
      expectNoMessage()
    }

    val systemParticipant: TestProbe = TestProbe("dummySystemParticipant")
    "correctly register a forwarded request" in {
      serviceRef ! ExtPrimaryDataServiceRegistrationMessage(
        UUID.randomUUID(),
        systemParticipant.ref,
      )
      println("Try to register")

      /* Wait for request approval */
      systemParticipant.expectMsg(
        RegistrationSuccessfulMessage(systemParticipant.ref, Some(0L))
      )

      /* We cannot directly check, if the requesting actor is among the subscribers, therefore we ask the actor to
       * provide data to all subscribed actors and check, if the subscribed probe gets one */
      scheduler.send(serviceRef, Activation(0))
      scheduler.expectMsgType[Completion]
      systemParticipant.expectMsgAllClassOf(classOf[ProvidePrimaryDataMessage])
    }
  }
}
