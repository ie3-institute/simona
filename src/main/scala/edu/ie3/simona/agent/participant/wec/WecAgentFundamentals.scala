/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.wec

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.result.system.{
  SystemParticipantResult,
  WecResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent._
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData._
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.participant.wec.WecAgent.neededServices
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.WecRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.WecModel.WecRelevantData
import edu.ie3.simona.model.participant.{ModelState, WecModel}
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.util.quantities.EmptyQuantity
import edu.ie3.util.quantities.PowerSystemUnits._
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units.PASCAL

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

protected trait WecAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      WecRelevantData,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      WecInput,
      WecRuntimeConfig,
      WecModel
    ] {
  this: WecAgent =>
  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]
  override val alternativeResult: ApparentPower = ZERO_POWER

  /** Determines the needed base state data in dependence of the foreseen
    * simulation mode of the agent.
    *
    * @param inputModel
    *   Input model definition
    * @param modelConfig
    *   Configuration of the model
    * @param services
    *   Optional collection of services to register with
    * @param simulationStartDate
    *   Real world time date time, when the simulation starts
    * @param simulationEndDate
    *   Real world time date time, when the simulation ends
    * @param resolution
    *   Agents regular time bin it wants to be triggered e.g one hour
    * @param requestVoltageDeviationThreshold
    *   Threshold, after which two nodal voltage magnitudes from participant
    *   power requests for the same tick are considered to be different
    * @param outputConfig
    *   Config of the output behaviour for simulation results
    * @return
    *   A child of [[ParticipantModelBaseStateData]] that reflects the behaviour
    *   based on the data source definition
    */
  override def determineModelBaseStateData(
      inputModel: WecInput,
      modelConfig: WecRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    WecRelevantData,
    ConstantState.type,
    WecModel
  ] = {
    /* Check for needed services */
    if (
      !services.exists(serviceDefinitions =>
        serviceDefinitions.map(_.getClass).containsSlice(neededServices)
      )
    )
      throw new AgentInitializationException(
        s"$actorName cannot be initialized without a weather service!"
      )

    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
      )

    ParticipantModelBaseStateData(
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      Array.emptyLongArray, // Additional activation of the wec agent is not needed
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
      ValueStore(resolution * 10),
      ValueStore(0),
      maybeEmAgent.map(FlexStateData(_, ValueStore(resolution * 10)))
    )
  }

  override def buildModel(
      inputModel: WecInput,
      modelConfig: WecRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): WecModel = WecModel(
    inputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate
  )

  override protected def createInitialState(): ModelState.ConstantState.type =
    ConstantState

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        WecRelevantData,
        ConstantState.type,
        WecModel
      ],
      tick: Long
  ): WecRelevantData = {
    // take the last weather data, not necessarily the one for the current tick:
    // we might receive flex control messages for irregular ticks
    val (_, secondaryData) = baseStateData.receivedSecondaryDataStore
      .last(tick)
      .getOrElse(
        throw new InconsistentStateException(
          s"The model ${baseStateData.model} was not provided with any secondary data so far."
        )
      )

    val weatherData =
      secondaryData
        .collectFirst {
          // filter secondary data for weather data
          case (_, data: WeatherData) => data
        }
        .getOrElse(
          throw new InconsistentStateException(
            s"The model ${baseStateData.model} was not provided with needed weather data."
          )
        )

    WecRelevantData(
      weatherData.windVel,
      weatherData.temp,
      EmptyQuantity
        .of(PASCAL) // weather data does not support air pressure
    )
  }

  override protected def calculateResult(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        WecRelevantData,
        ConstantState.type,
        WecModel
      ],
      currentTick: Long,
      activePower: ComparableQuantity[Power]
  ): ApparentPower = {
    val voltage = getAndCheckNodalVoltage(baseStateData, currentTick)

    val reactivePower =
      baseStateData.model.calculateReactivePower(
        activePower,
        voltage
      )

    ApparentPower(activePower, reactivePower)
  }

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[
        ApparentPower,
        WecRelevantData,
        ConstantState.type,
        WecModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPower =
    (
        _: Long,
        _: ParticipantModelBaseStateData[
          ApparentPower,
          WecRelevantData,
          ConstantState.type,
          WecModel
        ],
        _: ComparableQuantity[Dimensionless]
    ) =>
      throw new InvalidRequestException(
        "WEC model cannot be run without secondary data."
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
    * @param baseStateData
    *   The base state data with collected secondary data
    * @param currentTick
    *   Tick, the trigger belongs to
    * @param scheduler
    *   [[ActorRef]] to the scheduler in the simulation
    * @return
    *   [[Idle]] with updated result values
    */
  override def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        WecRelevantData,
        ConstantState.type,
        WecModel
      ],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
    implicit val startDateTime: ZonedDateTime = baseStateData.startDate

    val voltage =
      getAndCheckNodalVoltage(baseStateData, currentTick)

    val relevantData =
      createCalcRelevantData(
        baseStateData,
        currentTick
      )

    val result = baseStateData.model.calculatePower(
      currentTick,
      voltage,
      relevantData
    )

    updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(
      scheduler,
      baseStateData,
      result
    )
  }

  /** Determine the average result within the given tick window
    *
    * @param tickToResults
    *   Mapping from data tick to actual data
    * @param windowStart
    *   First, included tick of the time window
    * @param windowEnd
    *   Last, included tick of the time window
    * @param activeToReactivePowerFuncOpt
    *   An Option on a function, that transfers the active into reactive power
    * @return
    *   The averaged result
    */
  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ] = None
  ): ApparentPower =
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
    *   Unique identifier of the physical model
    * @param dateTime
    *   Real world date of the result
    * @param result
    *   The primary data to build a result model for
    * @return
    *   The equivalent event
    */
  override protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ApparentPower
  ): SystemParticipantResult =
    new WecResult(
      dateTime,
      uuid,
      result.p,
      result.q
    )
}
