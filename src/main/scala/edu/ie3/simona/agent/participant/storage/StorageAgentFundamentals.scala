/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.storage

import akka.actor.ActorRef
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.ParticipantAgentFundamentals
import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.{
  ApparentPower,
  ZERO_POWER
}
import edu.ie3.simona.agent.participant.data.secondary.SecondaryDataService
import edu.ie3.simona.agent.participant.statedata.{
  BaseStateData,
  DataCollectionStateData,
  ParticipantStateData
}
import edu.ie3.simona.config.SimonaConfig.StorageRuntimeConfig
import edu.ie3.simona.event.notifier.ParticipantNotifierConfig
import edu.ie3.simona.exceptions.agent.InvalidRequestException
import edu.ie3.simona.model.participant.StorageModel
import edu.ie3.simona.model.participant.StorageModel.StorageRelevantData
import tech.units.indriya.ComparableQuantity

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import scala.reflect.{ClassTag, classTag}

trait StorageAgentFundamentals
    extends ParticipantAgentFundamentals[
      ApparentPower,
      StorageRelevantData,
      ParticipantStateData[ApparentPower],
      StorageInput,
      StorageRuntimeConfig,
      StorageModel
    ] {
  this: StorageAgent =>
  override val alternativeResult: ApparentPower = ZERO_POWER

  override protected val pdClassTag: ClassTag[ApparentPower] =
    classTag[ApparentPower]

  override val calculateModelPowerFunc: (
      Long,
      BaseStateData.ParticipantModelBaseStateData[
        ApparentPower,
        StorageRelevantData,
        StorageModel
      ],
      ComparableQuantity[Dimensionless]
  ) => ApparentPower =
    throw new InvalidRequestException("...")

  override def calculatePowerWithSecondaryDataAndGoToIdle(
      collectionStateData: DataCollectionStateData[ApparentPower],
      currentTick: Long,
      scheduler: ActorRef
  ): State = ???

  override def buildModel(
      inputModel: StorageInput,
      modelConfig: StorageRuntimeConfig,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime
  ): StorageModel = ???

  override def determineModelBaseStateData(
      inputModel: StorageInput,
      modelConfig: StorageRuntimeConfig,
      services: Option[Vector[SecondaryDataService[_ <: Data.SecondaryData]]],
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
      resolution: Long,
      requestVoltageDeviationThreshold: Double,
      outputConfig: ParticipantNotifierConfig
  ): BaseStateData.ParticipantModelBaseStateData[
    ApparentPower,
    StorageRelevantData,
    StorageModel
  ] = ???

  override def averageResults(
      tickToResults: Map[Long, ApparentPower],
      windowStart: Long,
      windowEnd: Long,
      activeToReactivePowerFuncOpt: Option[
        ComparableQuantity[Power] => ComparableQuantity[Power]
      ]
  ): ApparentPower = ???

  override protected def buildResult(
      uuid: UUID,
      dateTime: ZonedDateTime,
      result: ApparentPower
  ): SystemParticipantResult = ???
}
