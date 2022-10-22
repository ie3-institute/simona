/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.hp

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.HpInput
import edu.ie3.datamodel.models.result.system.{
  HpResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ApparentPowerAndHeat
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.hp.HpAgent.neededServices
import edu.ie3.simona.agent.participant.statedata.BaseStateData.{
  FlexStateData,
  ParticipantModelBaseStateData
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  InputModelContainer,
  WithHeatInputContainer
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.participant.{
  ParticipantAgentFundamentals,
  StatefulParticipantAgentFundamentals
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.SimonaConfig.HpRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.{
  AgentInitializationException,
  InconsistentStateException,
  InvalidRequestException
}
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.{FlexChangeIndicator, HpModel}
import edu.ie3.simona.model.participant.HpModel.{HpRelevantData, HpState}
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.thermal.ThermalGrid
import edu.ie3.simona.ontology.messages.services.WeatherMessage.WeatherData
import edu.ie3.util.quantities.PowerSystemUnits.PU
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

trait HpAgentFundamentals
    extends StatefulParticipantAgentFundamentals[
      ApparentPowerAndHeat,
      HpRelevantData,
      HpState,
      ParticipantStateData[ApparentPowerAndHeat],
      HpInput,
      HpRuntimeConfig,
      HpModel
    ] {
  this: HpAgent =>
  override protected val pdClassTag: ClassTag[ApparentPowerAndHeat] =
    classTag[ApparentPowerAndHeat]
  override val alternativeResult: ApparentPowerAndHeat = ApparentPowerAndHeat(
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.HEAT_DEMAND)
  )

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPowerAndHeat,
        HpRelevantData,
        HpState,
        HpModel
      ],
      HpState,
      ComparableQuantity[Dimensionless]
  ) => ApparentPowerAndHeat =
    (_, _, _, _) =>
      throw new InvalidRequestException(
        "Heat pump model cannot be run without secondary data."
      )

  override protected def createInitialState(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPowerAndHeat,
        HpRelevantData,
        HpState,
        HpModel
      ]
  ): HpState = startingState(baseStateData.model.thermalGrid)

  private def startingState(
      thermalGrid: ThermalGrid
  ): HpState = HpState(
    isRunning = false,
    -1,
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
    ThermalGrid.startingState(thermalGrid),
    None
  )

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
        ApparentPowerAndHeat,
        HpRelevantData,
        HpState,
        HpModel
      ],
      data: HpRelevantData,
      lastState: HpState,
      setPower: ComparableQuantity[Power]
  ): (HpState, ApparentPowerAndHeat, FlexChangeIndicator) = {
    /* Determine needed information */
    val voltage =
      getAndCheckNodalVoltage(baseStateData, tick)
    val relevantData = createCalcRelevantData(baseStateData, tick)

    val modelState = baseStateData.stateDataStore.last(tick) match {
      case Some((lastTick, _)) if lastTick == tick =>
        /* We already updated the state for this tick, take the one before */
        baseStateData.stateDataStore.last(tick - 1) match {
          case Some((_, earlierModelState)) => earlierModelState
          case None =>
            throw new InconsistentStateException(
              s"Unable to get state for heat pump '${baseStateData.model.getUuid}' in tick ${tick - 1}."
            )
        }
      case Some((_, lastModelState)) =>
        lastModelState
      case None =>
        throw new InconsistentStateException(
          s"Unable to get state for heat pump '${baseStateData.model.getUuid}' in tick $tick."
        )
    }

    /* Handle the control request */
    val (updatedState, flexChangeIndicator) = baseStateData.model
      .handleControlledPowerChange(relevantData, modelState, setPower)

    /* Calculate power results */
    val result = baseStateData.model.calculatePower(
      tick,
      voltage,
      Some(updatedState),
      relevantData
    )

    (updatedState, result, flexChangeIndicator)
  }

  /** Abstractly calculate the power output of the participant utilising
    * secondary data. However, it might appear, that not the complete set of
    * secondary data is available for the given tick. This might especially be
    * true, if the actor has been additionally activated. This method thereby
    * has to try and fill up missing data with the last known data, as this is
    * still supposed to be valid. The secondary data therefore is put to the
    * calculation relevant data store. <p>The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.CompletionMessage]] to
    * scheduler and using update result values.</p> </p>Actual implementation
    * can be found in each participant's fundamentals.</p>
    *
    * @param baseStateData
    *   State data with collected secondary data.
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
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPowerAndHeat,
        HpRelevantData,
        HpState,
        HpModel
      ],
      maybeLastModelState: Option[HpState],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPowerAndHeat]] =
    maybeLastModelState match {
      case Some(modelState) =>
        implicit val startDateTime: ZonedDateTime =
          baseStateData.startDate

        /* Determine needed information */
        val voltage =
          getAndCheckNodalVoltage(baseStateData, currentTick)
        val relevantData = createCalcRelevantData(baseStateData, currentTick)

        /* Determine the next state */
        val updatedState =
          updateState(
            currentTick,
            modelState,
            relevantData,
            voltage,
            baseStateData.model
          )

        /* Calculate power results */
        val power = baseStateData.model.calculatePower(
          currentTick,
          voltage,
          Some(updatedState),
          relevantData
        )
        val accompanyingResults = baseStateData.model.thermalGrid.results(
          modelState.thermalGridState
        )(baseStateData.startDate)
        val result = AccompaniedSimulationResult(power, accompanyingResults)

        val updatedStateDataStore = ValueStore.updateValueStore(
          baseStateData.stateDataStore,
          currentTick,
          updatedState
        )
        val updatedBaseStateData =
          baseStateData.copy(stateDataStore = updatedStateDataStore)
        updateValueStoresInformListenersAndGoToIdleWithUpdatedBaseStateData(
          scheduler,
          updatedBaseStateData,
          result,
          relevantData
        )
      case None =>
        throw new InconsistentStateException(
          s"Cannot calculate heat pump model without model state. (tick = $currentTick)"
        )
    }

  /** Update the last known model state with the given external, relevant data
    *
    * @param tick
    *   Tick to update state for
    * @param modelState
    *   Last known model state
    * @param calcRelevantData
    *   Data, relevant for calculation
    * @param nodalVoltage
    *   Current nodal voltage of the agent
    * @param model
    *   Model for calculation
    * @return
    *   The updated state at given tick under consideration of calculation
    *   relevant data
    */
  protected override def updateState(
      tick: Long,
      modelState: HpState,
      calcRelevantData: HpRelevantData,
      nodalVoltage: ComparableQuantity[Dimensionless],
      model: HpModel
  ): HpState = model.calculateNextState(modelState, calcRelevantData)

  /** Abstract definition, individual implementations found in individual agent
    * fundamental classes
    */
  override def determineModelBaseStateData(
      inputModel: InputModelContainer[HpInput],
      modelConfig: HpRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: Data.SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPowerAndHeat,
    HpRelevantData,
    HpState,
    HpModel
  ] = {
    if (!services.exists(_.map(_.getClass).containsSlice(neededServices)))
      throw new AgentInitializationException(
        "HpAgent cannot be initialized without its needed services."
      )

    inputModel match {
      case withHeatContainer: WithHeatInputContainer[HpInput] =>
        /* Build the calculation model */
        val model = buildModel(
          withHeatContainer,
          modelConfig,
          simulationStartDate,
          simulationEndDate
        )

        /* Determine a proper starting model state and safe it into the base state data */
        val startingModelState = startingState(model.thermalGrid)
        val stateDataStore = ValueStore.updateValueStore(
          ValueStore(resolution * 10),
          -1L,
          startingModelState
        )

        ParticipantModelBaseStateData[
          ApparentPowerAndHeat,
          HpRelevantData,
          HpState,
          HpModel
        ](
          simulationStartDate,
          simulationEndDate,
          model,
          services,
          outputConfig,
          Array.emptyLongArray,
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
          stateDataStore,
          maybeEmAgent.map(FlexStateData(_, ValueStore(resolution * 10)))
        )
      case unsupported =>
        throw new AgentInitializationException(
          s"HpAgent cannot be initialized with wrong init state data of type '${unsupported.getClass.getName}'. " +
            s"Expected: '${WithHeatInputContainer.getClass.getName}[${classOf[HpInput].getName}]'."
        )
    }
  }

  override protected def createCalcRelevantData(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPowerAndHeat,
        HpRelevantData,
        HpState,
        HpModel
      ],
      tick: Long
  ): HpRelevantData = {
    /* extract weather data from secondary data, which should have been requested and received before */
    val weatherData =
      baseStateData.receivedSecondaryDataStore
        .last(tick)
        .flatMap { case (receivedTick, receivedValues) =>
          // FIXME: This fallback should check if the activation comes from an internal event. Only then it's valid to
          //   take previously received values
          if (receivedTick != tick)
            log.debug(
              s"The model ${baseStateData.model.getUuid} needs to do calculations with values received " +
                s"in tick $receivedTick, as no weather data has been received in tick $tick."
            )
          receivedValues.collectFirst {
            // filter secondary data for weather data
            case (_, data: WeatherData) => data
          }
        }
        .getOrElse(
          throw new InconsistentStateException(
            s"The model ${baseStateData.model} was not provided with needed weather data."
          )
        )

    HpRelevantData(
      tick,
      weatherData.temp
    )
  }

  /** Abstract method to build the calculation model from input
    *
    * @param inputModel
    *   Input model description
    * @param modelConfig
    *   Configuration for the model
    * @param simulationStartDate
    *   Wall clock time of first instant in simulation
    * @param simulationEndDate
    *   Wall clock time of last instant in simulation
    * @return
    */
  override def buildModel(
      inputModel: InputModelContainer[HpInput],
      modelConfig: HpRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): HpModel = inputModel match {
    case ParticipantStateData.SimpleInputContainer(_) =>
      throw new AgentInitializationException(
        s"Unable to initialize heat pump agent '${inputModel.electricalInputModel.getUuid}' without thermal grid model."
      )
    case WithHeatInputContainer(_, thermalGrid) =>
      /* Build the actual heat pump model */
      HpModel(
        inputModel.electricalInputModel,
        modelConfig.scaling,
        simulationStartDate,
        simulationEndDate,
        ThermalGrid(thermalGrid)
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
      tickToResults: Map[Long, ApparentPowerAndHeat],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ]
  ): ApparentPowerAndHeat =
    ParticipantAgentFundamentals.averageApparentPowerAndHeat(
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
      result: ApparentPowerAndHeat
  ): SystemParticipantResult = new HpResult(
    dateTime,
    uuid,
    result.p,
    result.q,
    result.qDot
  )
}
