/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.input.system.EmInput
import edu.ie3.datamodel.models.result.system.{
  EmResult,
  SystemParticipantResult
}
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.Data.SecondaryData
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.EmModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.notifier.NotifierConfig
import edu.ie3.simona.exceptions.agent.InvalidRequestException
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.em.EmModel.EmRelevantData
import edu.ie3.simona.model.participant.em.{EmModel, PrioritizedFlexStrat}
import edu.ie3.simona.model.participant.{FlexChangeIndicator, ModelState}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import edu.ie3.util.scala.quantities.ReactivePower

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.{ClassTag, classTag}

/** TODO unused methods
  */
trait EmAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      EmRelevantData,
      ConstantState.type,
      EmModelBaseStateData,
      EmInput,
      EmRuntimeConfig,
      EmModel
    ] {
  this: EmAgent =>
  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]
  override val alternativeResult: ApparentPower = ZERO_POWER

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
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        ModelState.ConstantState.type,
        EmModel
      ],
      data: EmRelevantData,
      lastState: ModelState.ConstantState.type,
      setPower: squants.Power
  ): (ModelState.ConstantState.type, ApparentPower, FlexChangeIndicator) = ???

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
      inputModel: ParticipantStateData.InputModelContainer[EmInput],
      modelConfig: EmRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EmModel = EmModel(
    inputModel.electricalInputModel,
    modelConfig,
    simulationStartDate,
    simulationEndDate,
    PrioritizedFlexStrat(modelConfig.pvFlex)
  )

  /** Abstract definition, individual implementations found in individual agent
    * fundamental classes
    */
  override def determineModelBaseStateData(
      inputModel: ParticipantStateData.InputModelContainer[EmInput],
      modelConfig: EmRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: NotifierConfig,
      maybeEmAgent: Option[ActorRef]
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPower,
    EmRelevantData,
    ModelState.ConstantState.type,
    EmModel
  ] =
    throw new InvalidRequestException(
      "Not implemented"
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
      calcRelevantData: EmRelevantData,
      nodalVoltage: squants.Dimensionless,
      model: EmModel
  ): ModelState.ConstantState.type = ???

  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        ConstantState.type,
        EmModel
      ],
      ConstantState.type,
      squants.Dimensionless
  ) => ApparentPower =
    (
        _: Long,
        _: BaseStateData.ParticipantModelBaseStateData[
          ApparentPower,
          EmRelevantData,
          ConstantState.type,
          EmModel
        ],
        _: ConstantState.type,
        _: squants.Dimensionless
    ) =>
      throw new InvalidRequestException(
        "WEC model cannot be run without secondary data."
      )

  override protected def createInitialState(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        ModelState.ConstantState.type,
        EmModel
      ]
  ): ModelState.ConstantState.type = ConstantState

  override def calculatePowerWithSecondaryDataAndGoToIdle(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        ConstantState.type,
        EmModel
      ],
      modelState: ConstantState.type,
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] =
    throw new InvalidRequestException(
      "Not implemented"
    )

  override protected def createCalcRelevantData(
      baseStateData: BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        ModelState.ConstantState.type,
        EmModel
      ],
      tick: Long
  ): EmRelevantData =
    throw new InvalidRequestException(
      "Not implemented"
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
  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        squants.Power => ReactivePower
      ]
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
    new EmResult(
      dateTime,
      uuid,
      result.p.toMegawatts.asMegaWatt,
      result.q.toMegavars.asMegaVar
    )

}
