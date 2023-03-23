package edu.ie3.simona.agent.participant.chp

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.{ChpInput, WecInput}
import edu.ie3.datamodel.models.result.system.{ChpResult, SystemParticipantResult}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.simona.agent.participant.chp.ChpAgent.{neededServices, props}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{ApparentPower, ApparentPowerAndHeat, ZERO_POWER, ZERO_POWER_HEAT}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{DataCollectionStateData, ParticipantStateData}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.{ChpRuntimeConfig, WecRuntimeConfig}
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{AgentInitializationException, InconsistentStateException, InvalidRequestException}
import edu.ie3.simona.model.participant.ChpModel.ChpData
import edu.ie3.simona.model.participant.{ChpModel, WecModel}
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}
import scala.collection.SortedSet

protected trait ChpAgentFundamentals
  extends ParticipantAgentFundamentals[
    ApparentPowerAndHeat,
    ChpData,
    ParticipantStateData[ApparentPowerAndHeat],
    ChpInput,
    ChpRuntimeConfig,
    ChpModel
  ] {
  this: ChpAgent =>
  override protected val pdClassTag: ClassTag[ApparentPowerAndHeat] =
    classTag[ApparentPowerAndHeat]
  override val alternativeResult: ApparentPowerAndHeat = ZERO_POWER_HEAT

  /** Determines the needed base state data in dependence of the foreseen
    * simulation mode of the agent.
    *
    * @param inputModel
    * Input model definition
    * @param modelConfig
    * Configuration of the model
    * @param services
    * Optional collection of services to register with
    * @param simulationStartDate
    * Real world time date time, when the simulation starts
    * @param simulationEndDate
    * Real world time date time, when the simulation ends
    * @param resolution
    * Agents regular time bin it wants to be triggered e.g one hour
    * @param requestVoltageDeviationThreshold
    * Threshold, after which two nodal voltage magnitudes from participant
    * power requests for the same tick are considered to be different
    * @param outputConfig
    * Config of the output behaviour for simulation results
    * @return
    * A child of [[ParticipantModelBaseStateData]] that reflects the behaviour
    * based on the data source definition
    */
  override def determineModelBaseStateData(
                                            inputModel: ChpInput,
                                            modelConfig: ChpRuntimeConfig,
                                            services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
                                            simulationStartDate: ZonedDateTime,
                                            simulationEndDate: ZonedDateTime,
                                            resolution: Long,
                                            requestVoltageDeviationThreshold: Double,
                                            outputConfig: ParticipantNotifierConfig
                                          ): ParticipantModelBaseStateData[ApparentPowerAndHeat, ChpData, ChpModel] = {

    // Build calculation model
    val model = buildModel(
      inputModel,
      modelConfig,
      simulationStartDate,
      simulationEndDate
    )

    // Create ModelBaseStateData
    ParticipantModelBaseStateData(
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      SortedSet.empty, // Additional activation of the wec agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution * 10,
        inputModel.getNode
          .getvTarget()
          .to(PU)
      ),
      ValueStore.forResult(resolution, 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10)
    )
  }

  override def buildModel(
                           inputModel: ChpInput,
                           modelConfig: ChpRuntimeConfig,
                           simulationStartDate: ZonedDateTime,
                           simulationEndDate: ZonedDateTime
                         ): ChpModel = ChpModel(
    inputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate
  )

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
    Long,
      ParticipantModelBaseStateData[ApparentPower, ChpData, ChpModel],
      ComparableQuantity[Dimensionless]
    ) => ApparentPower =
    (
      _: Long,
      _: ParticipantModelBaseStateData[
        ApparentPower,
        ChpData,
        ChpModel
      ],
      _: ComparableQuantity[Dimensionless]
    ) =>
      throw new InvalidRequestException(
        "Chp model cannot be run without secondary data."
      )

  /** Calculate the power output of the participant utilising secondary data.
    * However, it might appear, that not the complete set of secondary data is
    * available for the given tick. This might especially be true, if the actor
    * has been additionally activated. This method thereby has to try and fill
    * up missing data with the last known data, as this is still supposed to be
    * valid. The secondary data therefore is put to the calculation relevant
    * data store. <p>The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage]] to
    * scheduler and using update result values.</p>
    *
    * @param collectionStateData
    * State data with collected, comprehensive secondary data.
    * @param currentTick
    * Tick, the trigger belongs to
    * @param scheduler
    * [[ActorRef]] to the scheduler in the simulation
    * @return
    * [[Idle]] with updated result values
    */
  override def calculatePowerWithSecondaryDataAndGoToIdle(
                                                           collectionStateData: DataCollectionStateData[ApparentPowerAndHeat],
                                                           currentTick: Long,
                                                           scheduler: ActorRef
                                                         ): FSM.State[AgentState, ParticipantStateData[ApparentPowerAndHeat]] = {
    implicit val startDateTime: ZonedDateTime = collectionStateData.baseStateData.startDate

    val (result, relevantData) =
      collectionStateData.baseStateData match {
        case modelBaseStateData: ParticipantModelBaseStateData[
          ApparentPowerAndHeat,
          ChpData,
          ChpModel] =>




          modelBaseStateData.model match {






            case wecModel: WecModel =>
              /* extract weather data from secondary data, which should have been requested and received before */
              val weatherData =
                collectionStateData.data
                  .collectFirst {
                    // filter secondary data for weather data
                    case (_, Some(data: WeatherData)) => data
                  }
                  .getOrElse(
                    throw new InconsistentStateException(
                      s"The model ${modelBaseStateData.model} was not provided with needed weather data."
                    )
                  )

              val relevantData =
                WecRelevantData(
                  state,
                  heatDemand,
                  currentTick
                )

              val power = wecModel.calculatePower(
                currentTick,
                voltage,
                relevantData
              )

              (power, relevantData)
            case unsupportedModel =>
              throw new InconsistentStateException(
                s"Wrong model: $unsupportedModel!"
              )
          }
        case _ =>
          throw new InconsistentStateException(
            "Cannot find a model for model calculation."
          )
      }


    updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(
      scheduler,
      collectionStateData.baseStateData,
      result,
      relevantData
    )
  }


  /** Determine the average result within the given tick window
    *
    * @param tickToResults
    * Mapping from data tick to actual data
    * @param windowStart
    * First, included tick of the time window
    * @param windowEnd
    * Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    * An Option on a function, that transfers the active into reactive power
    * @return
    * The averaged result
    */
  override def averageResults(
                               tickToResults: Map[Long, ApparentPowerAndHeat],
                               windowStart: Long,
                               windowEnd: Long,
                               activeToReactivePowerFuncOpt: Option[
                                 ComparableQuantity[Power] => ComparableQuantity[Power]
                               ] = None
                             ): ApparentPowerAndHeat =
    ParticipantAgentFundamentals.averageApparentPower(
      tickToResults,
      windowStart,
      windowEnd,
      activeToReactivePowerFuncOpt,
      log
    )


  /** Determines the correct result.
    *
    * @param uuid
    * Unique identifier of the physical model
    * @param dateTime
    * Real world date of the result
    * @param result
    * The primary data to build a result model for
    * @return
    * The equivalent event
    */
  protected def buildResult(
    uuid: UUID,
    dateTime: ZonedDateTime,
    result: ApparentPowerAndHeat
  ): SystemParticipantResult =
    new ChpResult(
      dateTime,
      uuid,
      result.p,
      result.q,
      result.qDot
    )
}




