/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.price

import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.config.ConfigParams.BaseCsvParams
import edu.ie3.simona.config.InputConfig.{PriceAdjustments, PriceDatasource}
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.ontology.messages.ServiceMessage.{
  DataProvision,
  RegistrationSuccessfulMessage,
  SecondaryServiceRegistrationMessage,
}
import edu.ie3.simona.ontology.messages.{Activation, SchedulerMessage}
import edu.ie3.simona.scheduler.ScheduleLock
import edu.ie3.simona.service.Data.SecondaryData.{
  ProsumerPrice,
  SecondarySeriesData,
}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.price.EnergyPriceService.InitPriceServiceStateData
import edu.ie3.simona.test.common.{TestSpawnerTyped, UnitSpec}
import edu.ie3.simona.test.helper.TestResourceHelper
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.{EnergyPrice, EuroPerKilowattHour}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import squants.time.Hours

import java.time.ZonedDateTime
import java.util.UUID

class EnergyPriceServiceSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with TestResourceHelper
    with TestSpawnerTyped {

  private given simulationStartDate: ZonedDateTime =
    TimeUtil.withDefaults.toZonedDateTime("2025-01-01T00:00:00Z")

  private val dataSourceConfig = PriceDatasource(
    buyingPrice = PriceAdjustments(
      fees = 150d,
      tax = 0.2d,
    ),
    sellingPrice = PriceAdjustments(
      tax = 0.2d
    ),
    timeseriesUuid = UUID.fromString("2511ad45-655d-4b3c-9eea-da03281fa288"),
    csvParams = Some(
      BaseCsvParams(
        csvSep = ",",
        directoryPath = getResourcePath("_timeseries/").toString,
        isHierarchic = false,
      )
    ),
  )

  private val scheduler = TestProbe[SchedulerMessage]("scheduler")
  private val agent1 = TestProbe[ParticipantAgent.Message]("agent1")
  private val agent2 = TestProbe[ParticipantAgent.Message]("agent2")

  private given EnergyPrice = EuroPerKilowattHour(1e-3)

  "A price service" must {

    val serviceKey =
      ScheduleLock.singleKey(TSpawner, scheduler.ref, INIT_SIM_TICK)
    // lock activation scheduled
    scheduler.expectMessageType[ScheduleActivation]
    val priceService = spawn(
      EnergyPriceService(
        scheduler.ref,
        InitPriceServiceStateData(
          dataSourceConfig,
          simulationStartDate,
        ),
        serviceKey,
      )
    )

    "send correct schedule message after initialisation" in {
      scheduler.expectMessage(
        ScheduleActivation(priceService, 0L, Some(serviceKey))
      )
    }

    "announce that agent is registered for current price data" in {
      priceService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        (),
      )

      agent1.expectMessage(
        RegistrationSuccessfulMessage(priceService, 0L)
      )
    }

    "recognize that agent is already registered" in {
      priceService ! SecondaryServiceRegistrationMessage(
        agent1.ref,
        DataTimeType.Current,
        (),
      )

      agent1.expectNoMessage()
      agent2.expectNoMessage()
    }

    "announce that agent is registered for forecast data" in {
      priceService ! SecondaryServiceRegistrationMessage(
        agent2.ref,
        DataTimeType.CurrentAndForecast(
          forecastLength = Hours(6),
          forecastResolution = Hours(1),
        ),
        (),
      )

      agent2.expectMessage(
        RegistrationSuccessfulMessage(priceService, 0L)
      )

      agent1.expectNoMessage()
    }

    "send out correct weather information upon activity start trigger and request the triggering for the next tick" in {
      /* Send out an activity start trigger as the scheduler */
      priceService ! Activation(0)

      val completionMsg = scheduler.expectMessageType[Completion]
      completionMsg.newTick shouldBe Some(3600)

      agent1.expectMessageType[DataProvision] match {
        case DataProvision(tick, serviceRef, data, nextTick) =>
          tick shouldBe 0L
          serviceRef shouldBe priceService
          data match {
            case ProsumerPrice(priceSell, priceBuy) =>
              // 0.216 c/kWh minus 20% tax = 0.1728 c/kWh
              priceSell should approximate(EuroPerKilowattHour(0.001728))
              // 0.216 c/kWh plus 15 c/kWh fees plus 20% tax on top = 18.2592 c/kWh
              priceBuy should approximate(EuroPerKilowattHour(0.182592))
            case unexpected =>
              fail(s"Received unexpected data $unexpected")
          }
          nextTick shouldBe Some(3600L)
      }

      agent2.expectMessageType[DataProvision] match {
        case DataProvision(tick, serviceRef, data, nextTick) =>
          tick shouldBe 0L
          serviceRef shouldBe priceService
          data match {
            case SecondarySeriesData(series) =>
              series.size shouldBe 7
            case unexpected =>
              fail(s"Received unexpected data $unexpected")
          }
          nextTick shouldBe Some(3600L)
      }

    }

  }

}
