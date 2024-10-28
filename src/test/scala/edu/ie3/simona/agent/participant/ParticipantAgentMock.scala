/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant

import org.apache.pekko.actor.{ActorRef, FSM, Props}
import org.apache.pekko.actor.typed.{ActorRef => TypedActorRef}
import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER,
}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.BaseStateData.ParticipantModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.{
  InputModelContainer,
  ParticipantInitializeStateData,
}
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData,
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.config.RuntimeConfig.BaseRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.InvalidRequestException
import edu.ie3.simona.io.result.AccompaniedSimulationResult
import edu.ie3.simona.model.participant.CalcRelevantData.FixedRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl.CosPhiFixed
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  FlexChangeIndicator,
  ModelState,
  SystemParticipant,
}
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.FlexResponse
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.{Megavars, ReactivePower}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito
import org.mockito.Mockito.doReturn
import org.scalatestplus.mockito.MockitoSugar.mock
import squants.Each
import squants.energy.{Kilowatts, Megawatts}

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.SortedSet
import scala.reflect.{ClassTag, classTag}

/** Creating a mocking participant agent
  *
  * @param scheduler
  *   Actor reference of the scheduler
  */
class ParticipantAgentMock(
    scheduler: ActorRef,
    initStateData: ParticipantInitializeStateData[
      SystemParticipantInput,
      SimonaConfig.BaseRuntimeConfig,
      ApparentPower,
    ],
    override val listener: Iterable[ActorRef] = Iterable.empty[ActorRef],
) extends ParticipantAgent[
      ApparentPower,
      FixedRelevantData.type,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      SystemParticipantInput,
      BaseRuntimeConfig,
      SystemParticipant[
        FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ],
    ](scheduler, initStateData)
    with ParticipantAgentFundamentals[
      ApparentPower,
      FixedRelevantData.type,
      ConstantState.type,
      ParticipantStateData[ApparentPower],
      SystemParticipantInput,
      BaseRuntimeConfig,
      SystemParticipant[
        FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ],
    ] {
  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]
  override val alternativeResult: ApparentPower = ZERO_POWER

  /** Partial function, that is able to transfer
    * [[ParticipantModelBaseStateData]] (holding the actual calculation model)
    * into a pair of active and reactive power
    */
  override val calculateModelPowerFunc: (
      Long,
      ParticipantModelBaseStateData[
        ApparentPower,
        FixedRelevantData.type,
        ConstantState.type,
        SystemParticipant[
          FixedRelevantData.type,
          ApparentPower,
          ConstantState.type,
        ],
      ],
      ConstantState.type,
      squants.Dimensionless,
  ) => ApparentPower = (_, _, _, _) =>
    // output different from default (0, 0)
    ApparentPower(
      Megawatts(2.0),
      Megavars(1.0),
    )

  /** Abstractly calculate the power output of the participant with all needed
    * secondary data apparent. The next state is [[Idle]], sending a
    * [[edu.ie3.simona.ontology.messages.SchedulerMessage.Completion]] to
    * scheduler and using update result values. Additionally, the collected
    * secondary data is also put to storage. Actual implementation can be found
    * in each participant's fundamentals.
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
        FixedRelevantData.type,
        ConstantState.type,
        SystemParticipant[
          FixedRelevantData.type,
          ApparentPower,
          ConstantState.type,
        ],
      ],
      modelState: ConstantState.type,
      currentTick: Long,
      scheduler: ActorRef,
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] =
    throw new InvalidRequestException(
      "Request to calculate power with secondary data cannot be processed for this mock agent."
    )

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
      inputModel: InputModelContainer[SystemParticipantInput],
      modelConfig: BaseRuntimeConfig,
      services: Iterable[SecondaryDataService[_ <: SecondaryData]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[TypedActorRef[FlexResponse]],
  ): ParticipantModelBaseStateData[
    ApparentPower,
    FixedRelevantData.type,
    ConstantState.type,
    SystemParticipant[FixedRelevantData.type, ApparentPower, ConstantState.type],
  ] = {
    val func = CosPhiFixed(0.95).activeToReactivePowerFunc(
      Kilowatts(0.0),
      0.95d,
      Each(1.0),
    )
    val participant: SystemParticipant[
      FixedRelevantData.type,
      ApparentPower,
      ConstantState.type,
    ] =
      mock[SystemParticipant[
        FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ]]
    doReturn(func).when(participant).activeToReactivePowerFunc(any())

    ParticipantModelBaseStateData[
      ApparentPower,
      FixedRelevantData.type,
      ConstantState.type,
      SystemParticipant[
        FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ],
    ](
      simulationStartDate,
      simulationEndDate,
      participant,
      None,
      outputConfig,
      SortedSet.empty,
      Map.empty,
      requestVoltageDeviationThreshold,
      ValueStore.forVoltage(
        resolution,
        Each(1.0),
      ),
      ValueStore.forResult(resolution, 2),
      ValueStore(resolution),
      ValueStore(resolution),
      ValueStore(resolution),
      None,
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
      inputModel: InputModelContainer[SystemParticipantInput],
      modelConfig: BaseRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): SystemParticipant[
    FixedRelevantData.type,
    ApparentPower,
    ConstantState.type,
  ] = {
    val mockModel =
      mock[SystemParticipant[
        FixedRelevantData.type,
        ApparentPower,
        ConstantState.type,
      ]]
    val uuid = inputModel.electricalInputModel.getUuid
    Mockito.when(mockModel.getUuid).thenReturn(uuid)
    mockModel
  }

  override protected def createInitialState(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        FixedRelevantData.type,
        ConstantState.type,
        SystemParticipant[
          FixedRelevantData.type,
          ApparentPower,
          ConstantState.type,
        ],
      ]
  ): ModelState.ConstantState.type =
    ConstantState

  override protected def createCalcRelevantData(
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        FixedRelevantData.type,
        ConstantState.type,
        SystemParticipant[
          FixedRelevantData.type,
          ApparentPower,
          ConstantState.type,
        ],
      ],
      tick: Long,
  ): FixedRelevantData.type =
    FixedRelevantData

  /** To clean up agent value stores after power flow convergence. This is
    * necessary for agents whose results are time dependent e.g. storage agents
    * @param baseStateData
    *   Basic state data
    * @param currentTick
    *   Tick, the trigger belongs to
    * @return
    *   [[Idle]] with updated result values
    */
  override def finalizeTickAfterPF(
      baseStateData: BaseStateData[ApparentPower],
      currentTick: Long,
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] =
    goto(Idle) using baseStateData

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
        squants.Power => ReactivePower
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
  override protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ApparentPower,
  ): SystemParticipantResult =
    new SystemParticipantResult(
      dateTime,
      uuid,
      result.p.toMegawatts.asMegaWatt,
      result.q.toMegavars.asMegaVar,
    ) {}

  /** Handle an active power change by flex control.
    *
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
  override def handleControlledPowerChange(
      tick: Long,
      baseStateData: ParticipantModelBaseStateData[
        ApparentPower,
        CalcRelevantData.FixedRelevantData.type,
        ModelState.ConstantState.type,
        SystemParticipant[
          CalcRelevantData.FixedRelevantData.type,
          ApparentPower,
          ModelState.ConstantState.type,
        ],
      ],
      data: CalcRelevantData.FixedRelevantData.type,
      lastState: ModelState.ConstantState.type,
      setPower: squants.Power,
  ): (
      ModelState.ConstantState.type,
      AccompaniedSimulationResult[ApparentPower],
      FlexChangeIndicator,
  ) = (
    ConstantState,
    AccompaniedSimulationResult(
      ApparentPower(
        Kilowatts(0.0),
        Kilovars(0.0),
      ),
      Seq.empty[ResultEntity],
    ),
    FlexChangeIndicator(),
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
  override protected def updateState(
      tick: Long,
      modelState: ModelState.ConstantState.type,
      calcRelevantData: CalcRelevantData.FixedRelevantData.type,
      nodalVoltage: squants.Dimensionless,
      model: SystemParticipant[
        CalcRelevantData.FixedRelevantData.type,
        ApparentPower,
        ModelState.ConstantState.type,
      ],
  ): ModelState.ConstantState.type = modelState
}

object ParticipantAgentMock {
  def props(
      scheduler: ActorRef,
      initStateData: ParticipantInitializeStateData[
        SystemParticipantInput,
        SimonaConfig.BaseRuntimeConfig,
        ApparentPower,
      ],
  ): Props =
    Props(
      new ParticipantAgentMock(
        scheduler,
        initStateData,
      )
    )
}
