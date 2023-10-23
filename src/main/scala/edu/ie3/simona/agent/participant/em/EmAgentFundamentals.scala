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
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.EmModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.InvalidRequestException
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.quantities.ReactivePower
import squants.{Dimensionless, Power}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import scala.reflect.{ClassTag, classTag}

/** TODO unused methods
  */
trait EmAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      EmRelevantData,
      EmModelBaseStateData,
      EmInput,
      EmRuntimeConfig,
      EmModel
    ] {
  this: EmAgent =>
  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]
  override val alternativeResult: ApparentPower = ZERO_POWER

  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        EmRelevantData,
        EmModel
      ],
      Dimensionless
  ) => ApparentPower =
    (
        _: Long,
        _: BaseStateData.ParticipantModelBaseStateData[
          ApparentPower,
          EmRelevantData,
          EmModel
        ],
        _: Dimensionless
    ) =>
      throw new InvalidRequestException(
        "WEC model cannot be run without secondary data."
      )

  override def calculatePowerWithSecondaryDataAndGoToIdle(
      collectionStateData: DataCollectionStateData[ApparentPower],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPower]] =
    throw new InvalidRequestException(
      "Not implemented"
    )

  override def buildModel(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EmModel = EmModel(
    inputModel,
    modelConfig,
    simulationStartDate,
    simulationEndDate
  )

  override def determineModelBaseStateData(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: Data.SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPower,
    EmRelevantData,
    EmModel
  ] =
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
        Power => ReactivePower
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
      Quantities.getQuantity(result.p.toMegawatts, PowerSystemUnits.MEGAWATT),
      Quantities.getQuantity(result.q.toMegavars, PowerSystemUnits.MEGAVAR)
    )

}
