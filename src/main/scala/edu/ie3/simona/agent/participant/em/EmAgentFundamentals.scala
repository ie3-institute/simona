/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import akka.actor.{ActorRef, FSM}
import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.input.system.EmInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPowerAndHeat
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.em.EmAgent.EmAgentModelBaseStateData
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.config.SimonaConfig.EmRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.model.participant.EmModel
import edu.ie3.simona.model.participant.EmModel.EmRelevantData
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

trait EmAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPowerAndHeat,
      EmRelevantData,
      EmAgentModelBaseStateData,
      EmInput,
      EmRuntimeConfig,
      EmModel
    ] {
  this: EmAgent =>
  override protected val pdClassTag: ClassTag[ApparentPowerAndHeat] =
    classTag[ApparentPowerAndHeat]
  override val alternativeResult: ApparentPowerAndHeat = ApparentPowerAndHeat(
    Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.REACTIVE_POWER_RESULT),
    Quantities.getQuantity(0d, StandardUnits.HEAT_DEMAND)
  )

  // TODO power calc
  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPowerAndHeat,
        EmRelevantData,
        EmModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPowerAndHeat = ???

  override def calculatePowerWithSecondaryDataAndGoToIdle(
      collectionStateData: DataCollectionStateData[ApparentPowerAndHeat],
      currentTick: Long,
      scheduler: ActorRef
  ): FSM.State[AgentState, ParticipantStateData[ApparentPowerAndHeat]] = ???

  override def buildModel(
      inputModel: EmInput,
      modelConfig: EmRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): EmModel = ???

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
    ApparentPowerAndHeat,
    EmRelevantData,
    EmModel
  ] = ???

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
  ): ApparentPowerAndHeat = ???

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
  ): SystemParticipantResult = ???

}
