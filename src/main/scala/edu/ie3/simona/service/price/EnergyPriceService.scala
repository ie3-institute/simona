/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.service.price

import edu.ie3.datamodel.io.factory.timeseries.TimeBasedSimpleValueFactory
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.naming.timeseries.ColumnScheme
import edu.ie3.datamodel.io.source.TimeSeriesSource
import edu.ie3.datamodel.io.source.csv.{CsvDataSource, CsvTimeSeriesSource}
import edu.ie3.datamodel.models.value.EnergyPriceValue
import edu.ie3.simona.config.ConfigParams.BaseCsvParams
import edu.ie3.simona.config.InputConfig
import edu.ie3.simona.config.InputConfig.PriceAdjustments
import edu.ie3.simona.exceptions.WeatherServiceException.InvalidRegistrationRequestException
import edu.ie3.simona.exceptions.{InitializationException, ServiceException}
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.ontology.messages.ServiceMessage.*
import edu.ie3.simona.service.Data.SecondaryData.{
  ProsumerPrice,
  SecondarySeriesData,
}
import edu.ie3.simona.service.ServiceStateData.{
  InitializeServiceStateData,
  ServiceBaseStateData,
}
import edu.ie3.simona.service.{
  DataTimeType,
  ServiceStateData,
  SimonaService,
  TimeSeriesUtil,
}
import edu.ie3.simona.util.TickUtil.{RichZonedDateTime, TickLong}
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.collection.immutable.ActivationTickQueue
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import edu.ie3.util.scala.quantities.{EnergyPrice, EuroPerMegawattHour}
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger

import java.nio.file.Paths
import java.time.ZonedDateTime
import scala.collection.immutable.SortedMap
import scala.jdk.CollectionConverters.*
import scala.jdk.OptionConverters.RichOptional
import scala.util.{Failure, Success, Try}

/** The energy price service provides registered agents with current or
  * forecasted energy prices.
  */
object EnergyPriceService extends SimonaService {

  override type S = PriceBaseStateData

  /** The price service state data used for initialization of the price service.
    *
    * @param sourceDefinition
    *   The definition of the source to use.
    * @param startDateTime
    *   Date and time of the very first tick in the simulation.
    */
  final case class InitPriceServiceStateData(
      sourceDefinition: InputConfig.PriceDatasource,
      startDateTime: ZonedDateTime,
  ) extends InitializeServiceStateData

  /** State data of an initialized price service.
    *
    * @param activationTicks
    *   A queue of future ticks that the service will be activated at.
    * @param priceSource
    *   The source to retrieve price information from.
    * @param config
    *   The configuration data for the service.
    * @param subscribers
    *   A map of data type to agent references, which registered to receive
    *   price data throughout the simulation.
    */
  final case class PriceBaseStateData(
      activationTicks: ActivationTickQueue,
      priceSource: TimeSeriesSource[EnergyPriceValue],
      config: PriceConfig,
      subscribers: Map[DataTimeType, Set[
        ActorRef[ServiceMessage.Response]
      ]] = Map.empty,
  ) extends ServiceBaseStateData

  /** Configuration data for the service.
    *
    * @param buyingPrice
    *   Price adjustments for deriving buying prices.
    * @param sellingPrice
    *   Price adjustments for deriving selling prices.
    * @param startDateTime
    *   Date and time of the very first tick in the simulation.
    */
  final case class PriceConfig(
      buyingPrice: PriceAdjustments,
      sellingPrice: PriceAdjustments,
      startDateTime: ZonedDateTime,
  )

  override def init(
      initServiceData: ServiceStateData.InitializeServiceStateData
  )(using log: Logger): Try[(PriceBaseStateData, Option[Long])] = {
    initServiceData match {
      case InitPriceServiceStateData(
            sourceDefinition,
            startDateTime,
          ) =>
        given simulationStart: ZonedDateTime = startDateTime
        val valueClass = classOf[EnergyPriceValue]

        val factory = new TimeBasedSimpleValueFactory(valueClass)

        val priceSourceCfg = Seq(
          sourceDefinition.csvParams
        ).flatten.headOption.getOrElse {
          Failure(
            InitializationException(
              s"Expected a price source, but no source was defined in $sourceDefinition."
            )
          )
        }

        (priceSourceCfg match {
          case BaseCsvParams(csvSep, directoryPath, _) =>
            val fileNamingStrategy = new FileNamingStrategy()

            val dataSource = new CsvDataSource(
              csvSep,
              Paths.get(directoryPath),
              fileNamingStrategy,
            )

            dataSource
              .getCsvIndividualTimeSeriesMetaInformation(
                ColumnScheme.ENERGY_PRICE
              )
              .asScala
              .get(sourceDefinition.timeseriesUuid)
              .map(Success.apply)
              .getOrElse(
                Failure(
                  new InitializationException(
                    s"CSV timeseries with UUID ${sourceDefinition.timeseriesUuid} not found."
                  )
                )
              )
              .flatMap { metaData =>
                Try {
                  new CsvTimeSeriesSource(
                    csvSep,
                    Paths.get(directoryPath),
                    fileNamingStrategy,
                    metaData.getUuid,
                    metaData.getFullFilePath,
                    valueClass,
                    factory,
                  )
                }
              }

        }).flatMap { source =>
          TimeSeriesUtil
            .getTicksAdaptedToSimulation(
              source,
              simulationStart,
            )
            .map { activationTicks =>

              val initializedStateData =
                PriceBaseStateData(
                  activationTicks,
                  source,
                  PriceConfig(
                    sourceDefinition.buyingPrice,
                    sourceDefinition.sellingPrice,
                    simulationStart,
                  ),
                )

              (initializedStateData, activationTicks.nextTick)
            }
        }

    }

  }

  override protected def handleRegistrationRequest(
      registrationMessage: ServiceRegistrationMessage
  )(using
      serviceStateData: PriceBaseStateData,
      ctx: ActorContext[Message],
  ): Try[PriceBaseStateData] =
    registrationMessage match {
      case SecondaryServiceRegistrationMessage(
            agentToBeRegistered,
            dataType,
            _,
          ) =>
        Success(
          handleRegistrationRequest(
            agentToBeRegistered,
            dataType,
          )
        )
      case invalidMessage =>
        Failure(
          InvalidRegistrationRequestException(
            "Cannot register an agent for weather service with registration " +
              s"request message '$invalidMessage'!"
          )
        )
    }

  /** Try to register the sending agent for price data provision according to
    * its data type.
    *
    * @param agentToBeRegistered
    *   The agent that wants to be registered.
    * @param dataType
    *   The data type that the agent wants to receive.
    * @param serviceStateData
    *   The current service state data of this service.
    * @param ctx
    *   The actor context.
    * @return
    *   An updated state data of this service that contains registration
    *   information if the registration has been carried out successfully.
    */
  private def handleRegistrationRequest(
      agentToBeRegistered: ActorRef[ServiceMessage.Response],
      dataType: DataTimeType,
  )(using
      serviceStateData: PriceBaseStateData,
      ctx: ActorContext[Message],
  ): PriceBaseStateData = {
    ctx.log.debug(
      "Received price service registration from {}.",
      agentToBeRegistered.path.name,
    )

    val registrationResponse = serviceStateData.activationTicks.nextTick
      .map(RegistrationSuccessfulMessage(ctx.self, _))
      .getOrElse(RegistrationFailedMessage(ctx.self))

    val registeredActors =
      serviceStateData.subscribers.getOrElse(dataType, Set.empty)

    if registeredActors.contains(agentToBeRegistered) then
      ctx.log.warn(
        "Sending actor {} is already registered",
        agentToBeRegistered.path.name,
      )
    else agentToBeRegistered ! registrationResponse

    serviceStateData.copy(subscribers =
      serviceStateData.subscribers +
        (dataType -> registeredActors.incl(agentToBeRegistered))
    )

  }

  override protected def announceInformation(tick: Long)(using
      serviceStateData: PriceBaseStateData,
      ctx: ActorContext[Message],
  ): (PriceBaseStateData, Option[Long]) = {

    given simulationStart: ZonedDateTime = serviceStateData.config.startDateTime

    /* Pop the next activation tick and update the state data */
    val remainingTicks = serviceStateData.activationTicks.dropFirst
    val maybeNextTick = remainingTicks.nextTick
    val updatedStateData =
      serviceStateData.copy(activationTicks = remainingTicks)

    // get the price data and send it to the subscribed agents
    updatedStateData.subscribers.foreach { case (dataType, actors) =>
      val priceData = dataType match {
        case DataTimeType.Current =>
          val value =
            updatedStateData.priceSource
              .getValueOrLast(tick.toDateTime)
              .toScala
              .getOrElse(
                throw ServiceException(s"No price data available for $tick!")
              )

          createProsumerPrice(value)

        case DataTimeType.CurrentAndForecast(length, resolution) =>
          val endTick = tick + length.toSeconds.toLong
          val interval = ClosedInterval(tick.toDateTime, endTick.toDateTime)

          // price time series is forwarded as forecast without adding noise
          val valueSeries = updatedStateData.priceSource
            .getTimeSeries(
              interval
            )
            .getEntries
            .asScala
            .map { timeBasedValue =>
              timeBasedValue.getTime.toTick -> timeBasedValue.getValue
            }
            .to(SortedMap)

          val priceSeries =
            reduceTimeSeriesResolution(valueSeries, resolution).map {
              case (tick, value) =>
                tick -> createProsumerPrice(value)
            }

          SecondarySeriesData(priceSeries)
      }

      actors.foreach {
        _ ! DataProvision(
          tick,
          ctx.self,
          priceData,
          maybeNextTick,
        )
      }
    }

    (
      updatedStateData,
      maybeNextTick,
    )
  }

  private def createProsumerPrice(
      value: EnergyPriceValue
  )(using stateData: PriceBaseStateData): ProsumerPrice = {

    val wholesalePrice = convert(value).getOrElse(
      throw ServiceException(s"Empty price data!")
    )

    val sellingPrice =
      calculateSellingPrice(wholesalePrice, stateData.config.sellingPrice)
    val buyingPrice =
      calculateBuyingPrice(wholesalePrice, stateData.config.buyingPrice)
    ProsumerPrice(sellingPrice, buyingPrice)
  }

  private def convert(value: EnergyPriceValue): Option[EnergyPrice] =
    value.getPrice.toScala.map(_.toSquants)

  private def calculateBuyingPrice(
      wholesalePrice: EnergyPrice,
      adjustments: PriceAdjustments,
  ): EnergyPrice =
    (wholesalePrice + EuroPerMegawattHour(adjustments.fees)) *
      (1d + adjustments.tax)

  private def calculateSellingPrice(
      wholesalePrice: EnergyPrice,
      adjustments: PriceAdjustments,
  ): EnergyPrice =
    (wholesalePrice - EuroPerMegawattHour(adjustments.fees)) *
      (1d - adjustments.tax)

}
