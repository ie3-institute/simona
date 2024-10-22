/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

  package edu.ie3.simona.agent.participant.load.markov

  import edu.ie3.datamodel.models.input.system.LoadInput
  import edu.ie3.datamodel.models.result.system.{LoadResult, SystemParticipantResult}
  import edu.ie3.simona.agent.participant.ParticipantAgent.getAndCheckNodalVoltage
  import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
  import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{ApparentPower, ZERO_POWER}
  import edu.ie3.simona.agent.participant.data.Data.SecondaryData
  import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
  import edu.ie3.simona.agent.participant.load.LoadAgent
  import edu.ie3.simona.agent.participant.statedata.BaseStateData.{FlexControlledData, ParticipantModelBaseStateData}
  import edu.ie3.simona.agent.participant.statedata.ParticipantStateData
  import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.InputModelContainer
  import edu.ie3.simona.agent.state.AgentState
  import edu.ie3.simona.agent.state.AgentState.Idle
  import edu.ie3.simona.config.SimonaConfig.LoadRuntimeConfig
  import edu.ie3.simona.event.notifier.NotifierConfig
  import edu.ie3.simona.exceptions.agent.InconsistentStateException
  import edu.ie3.simona.model.participant.ModelState.ConstantState
  import edu.ie3.simona.model.participant.load.LoadReference
  import edu.ie3.simona.model.participant.load.markov.MarkovModel.MarkovRelevantData
  import edu.ie3.simona.model.participant.load.markov.{MarkovModel, MarkovRelevantData}
  import edu.ie3.simona.model.participant.{FlexChangeIndicator, ModelState}
  import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexResponse
  import edu.ie3.simona.util.TickUtil.TickLong
  import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
  import edu.ie3.util.scala.OperationInterval
  import edu.ie3.util.scala.quantities.ReactivePower
  import org.apache.pekko.actor.typed.{ActorRef => TypedActorRef}
  import org.apache.pekko.actor.{ActorRef, FSM}
  import squants.{Dimensionless, Power}

  import java.time.ZonedDateTime
  import java.util.UUID
  import scala.reflect.{ClassTag, classTag}

  protected trait MarkovAgentFundamentals
  extends ParticipantAgentFundamentals[
  ApparentPower,
  MarkovRelevantData,
  ConstantState.type,
  ParticipantStateData[ApparentPower],
  LoadInput,
  LoadRuntimeConfig,
  MarkovModel,
  ] {
  this: MarkovAgent =>
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
  *   Collection of services to register with
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
    inputModel: InputModelContainer[LoadInput],
    modelConfig: LoadRuntimeConfig,
    services: Iterable[SecondaryDataService[_ <: SecondaryData]],
    simulationStartDate: ZonedDateTime,
    simulationEndDate: ZonedDateTime,
    resolution: Long,
    requestVoltageDeviationThreshold: Double,
    outputConfig: NotifierConfig,
    maybeEmAgent: Option[TypedActorRef[FlexResponse]],
  ): ParticipantModelBaseStateData[
    ApparentPower,
    MarkovRelevantData,
    ModelState.ConstantState.type,
    MarkovModel


  ] = {
    /* Build the calculation model */
    val model =
      buildModel(
        inputModel,
        modelConfig,
        simulationStartDate,
        simulationEndDate,
      )

 def createInitialState(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        MarkovRelevantData,
        ConstantState.type,
        MarkovModel,
      ]
    ): ModelState.ConstantState.type =
      ConstantState

   def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        MarkovRelevantData,
        ConstantState.type,
        MarkovModel,
      ],
      tick: Long,
    ): MarkovRelevantData =       MarkovRelevantData(currentTick.toDateTime(baseStateData.startDate))

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
      MarkovRelevantData,
      ConstantState.type,
      MarkovModel,
      ],
      data: MarkovRelevantData,
      lastState: ConstantState.type,
      setPower: squants.Power,
      ): (ConstantState.type, ApparentPower, FlexChangeIndicator) = {
      /* Calculate result */
      val voltage = getAndCheckNodalVoltage(baseStateData, tick)

      val reactivePower = baseStateData.model.calculateReactivePower(
      setPower,
      voltage,
      )
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

        /** Partial function, that is able to transfer
          * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
          * into a pair of active and reactive power
          */
        val calculateModelPowerFunc: (
          Long,
            ParticipantModelBaseStateData[
              ApparentPower,
              MarkovRelevantData.type,
              ConstantState.type,
              MarkovModel,
            ],
            ConstantState.type,
            Dimensionless,
          ) => ApparentPower = (
        tick: Long,
        baseStateData: ParticipantModelBaseStateData[
          ApparentPower,
        MarkovRelevantData.type,
          ConstantState.type,
          MarkovModel,
        ],
        state: ConstantState.type,
        voltage: Dimensionless,
        ) =>
          baseStateData.model.calculatePower(
            tick,
            voltage,
            state,
            MarkovRelevantData,
          )

      /** Calculate the power output of the participant utilising secondary data.
  * However, it might appear, that not the complete set of secondary data is
  * available for the given tick. This might especially be true, if the actor
  * has been additionally activated. This method thereby has to try and fill
  * up missing data with the last known data, as this is still supposed to be
  * valid. The secondary data therefore is put to the calculation relevant
  * data store. <p>The next state is [[Idle]], sending a
  * [[edu.ie3.simona.ontology.messages.SchedulerMessage.Completion]] to
  * scheduler and using update result values.</p>
  *
  * @param baseStateData
  *   The base state data with collected secondary data
  * @param lastModelState
  *   Optional last model state
  * @param currentTick
  *   Tick, the trigger belongs to
  * @param scheduler
  *   [[ActorRef]] to the scheduler in the simulation
  * @return
  *   [[Idle]] with updated result values
      */
       def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: ParticipantModelBaseStateData[
      ApparentPower,
      MarkovRelevantData,
      ConstantState.type,
      MarkovModel,
      ],
      lastModelState: ConstantState.type,
      currentTick: Long,
      scheduler: ActorRef,
      ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] =
    throw new InconsistentStateException(
      s"Markov model is not able to calculate power with secondary data."
      )

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
       def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
      Power => ReactivePower
      ] = None,
      ): ApparentPower =
    ParticipantAgentFundamentals.averageApparentPower(
      tickToResults,
      windowStart,
      windowEnd,
      activeToReactivePowerFuncOpt,
      log,
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
        def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ApparentPower,
      ): SystemParticipantResult =
    new LoadResult(
      dateTime,
      uuid,
      result.p.toMegawatts.asMegaWatt,
      result.q.toMegavars.asMegaVar,
      )

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
      def updateState(
      tick: Long,
      modelState: ModelState.ConstantState.type,
      calcRelevantData: MarkovRelevantData,
      nodalVoltage: squants.Dimensionless,
      model: MarkovModel,
      ): ModelState.ConstantState.type = modelState
      }
  }


  object MarkovAgentFundamentals {
      def buildModel(
        inputModel: LoadInput,
        operationInterval: OperationInterval,
        modelConfig: LoadRuntimeConfig,
        reference: LoadReference,
      ): MarkovModel =
        MarkovModel(
          inputModel,
          modelConfig.scaling,
          operationInterval,
          reference,
        )

      protected def createCalcRelevantData(
        baseStateData: ParticipantModelBaseStateData[
          ApparentPower,
          MarkovRelevantData.type,
          ConstantState.type,
          MarkovModel,
        ],
        tick: Long,
      ): MarkovRelevantData.type =
        MarkovRelevantData

      /** Partial function, that is able to transfer
        * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
        * into a pair of active and reactive power
        */
      val calculateModelPowerFunc: (
        Long,
          ParticipantModelBaseStateData[
            ApparentPower,
            MarkovRelevantData.type,
            ConstantState.type,
            MarkovModel,
          ],
          ConstantState.type,
          Dimensionless,
        ) => ApparentPower = (
      tick: Long,
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        MarkovRelevantData.type,
        ConstantState.type,
        MarkovModel,
      ],
      state: ConstantState.type,
      voltage: Dimensionless,
      ) =>
        baseStateData.model.calculatePower(
          tick,
          voltage,
          state,
          MarkovRelevantData,
        )




  }
