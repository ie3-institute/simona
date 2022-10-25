/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.pv

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.PvInput
import edu.ie3.datamodel.models.result.system.{
  PvResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.{
  ParticipantAgentFundamentals,
  StatelessParticipantAgentFundamentals
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.pv.PvAgent.neededServices
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.InputModelContainer
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.PvRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.{
  FlexChangeIndicator,
  ModelState,
  PvModel
}
import edu.ie3.simona.model.participant.PvModel.PvRelevantData
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.simona.service.weather.WeatherService.FALLBACK_WEATHER_STEM_DISTANCE
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

protected trait PvAgentFundamentals
    extends StatelessParticipantAgentFundamentals[
      ApparentPower,
      PvRelevantData,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      PvInput,
      PvRuntimeConfig,
      PvModel
    ] {
  this: PvAgent =>
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
      inputModel: InputModelContainer[PvInput],
      modelConfig: PvRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): ParticipantModelBaseStateData[
    ApparentPower,
    PvRelevantData,
    ConstantState.type,
    PvModel
  ] = {
    /* Check for needed services */
    if (
      !services.exists(serviceDefinitions =>
        serviceDefinitions.map(_.getClass).containsSlice(neededServices)
      )
    )
      throw new AgentInitializationException(
        s"PvAgent cannot be initialized without a weather service!"
      )

    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate
      )

    ParticipantModelBaseStateData[
      ApparentPower,
      PvRelevantData,
      ConstantState.type,
      PvModel
    ](
      simulationStartDate,
      simulationEndDate,
      model,
      services,
      outputConfig,
      Array.emptyLongArray, // Additional activation of the pv agent is not needed
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution * 10,
        inputModel.electricalInputModel.getNode
          .getvTarget()
          .to(PU)
      ),
      ValueStore.forResult(resolution, 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10),
      ValueStore(resolution * 10),
      maybeEmAgent.map(FlexStateData(_, ValueStore(resolution * 10)))
    )
  }

  override def buildModel(
      inputModel: InputModelContainer[PvInput],
      modelConfig: PvRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): PvModel = PvModel(
    inputModel.electricalInputModel,
    modelConfig.scaling,
    simulationStartDate,
    simulationEndDate
  )

  override protected def createInitialState(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        PvRelevantData,
        ConstantState.type,
        PvModel
      ]
  ): ModelState.ConstantState.type =
    ConstantState

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        PvRelevantData,
        ConstantState.type,
        PvModel
      ],
      tick: Long
  ): PvRelevantData = {
    /* convert current tick to a datetime */
    implicit val startDateTime: ZonedDateTime =
      baseStateData.startDate
    val dateTime = tick.toDateTime

    val tickInterval =
      baseStateData.receivedSecondaryDataStore
        .lastKnownTick(tick - 1) match {
        case Some(dataTick) =>
          tick - dataTick
        case _ =>
          /* At the first tick, we are not able to determine the tick interval from last tick
           * (since there is none). Then we use a fall back pv stem distance. */
          FALLBACK_WEATHER_STEM_DISTANCE
      }

    // take the last weather data, not necessarily the one for the current tick:
    // we might receive flex control messages for irregular ticks
    val (_, secondaryData) = baseStateData.receivedSecondaryDataStore
      .last(tick)
      .getOrElse(
        throw new InconsistentStateException(
          s"The model ${baseStateData.model} was not provided with any secondary data so far."
        )
      )

    /* extract weather data from secondary data, which should have been requested and received before */
    val weatherData =
      secondaryData
        .collectFirst {
          // filter secondary data for weather data
          case (_, data: WeatherData) =>
            data
        }
        .getOrElse(
          throw new InconsistentStateException(
            s"The model ${baseStateData.model} was not provided with needed weather data."
          )
        )

    PvRelevantData(
      dateTime,
      tickInterval,
      weatherData.diffIrr,
      weatherData.dirIrr
    )
  }

  /** Handle an active power change by flex control.
    * @param tick
    *   Tick, in which control is issued
    * @param baseStateData
    *   Base state data of the agent
    * @param data
    *   Calculation relevant data
    * @param lastState
    *   Last known model state
    * @param setPower
    *   Setpoint active power
    * @return
    *   Updated model state, a result model and a [[FlexChangeIndicator]]
    */
  def handleControlledPowerChange(
      tick: Long,
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        PvRelevantData,
        ConstantState.type,
        PvModel
      ],
      data: PvRelevantData,
      lastState: ConstantState.type,
      setPower: ComparableQuantity[Power]
  ): (ConstantState.type, ApparentPower, FlexChangeIndicator) = {
    /* Calculate result */
    val voltage = getAndCheckNodalVoltage(baseStateData, currentTick)

    val reactivePower = baseStateData.model match {
      case model: PvModel =>
        model.calculateReactivePower(
          setPower,
          voltage
        )
    }
    val result = ApparentPower(setPower, reactivePower)

    /* Handle the request within the model */
    val (updatedState, flexChangeIndicator) =
      baseStateData.model.handleControlledPowerChange(data, lastState, setPower)
    (updatedState, result, flexChangeIndicator)
  }

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[
        ApparentPower,
        PvRelevantData,
        ConstantState.type,
        PvModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPower =
    (_, _, _) =>
      throw new InvalidRequestException(
        "Pv model cannot be run without secondary data."
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
    * @param maybeLastModelState
    *   Optional last model state
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
        PvRelevantData,
        ConstantState.type,
        PvModel
      ],
      maybeLastModelState: Option[ConstantState.type],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] = {
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
      None,
      relevantData
    )

    updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(
      scheduler,
      baseStateData,
      AccompaniedSimulationResult(result),
      relevantData
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
    new PvResult(
      dateTime,
      uuid,
      result.p,
      result.q
    )
}
