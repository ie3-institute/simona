/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput.SystemParticipantInputCopyBuilder
import edu.ie3.datamodel.models.input.system._
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.{PrimaryData, PrimaryDataMeta}
import edu.ie3.simona.config.SimonaConfig.{
  BaseRuntimeConfig,
  EvcsRuntimeConfig,
  LoadRuntimeConfig,
  StorageRuntimeConfig,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
}
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel.PrimaryResultFunc
import edu.ie3.simona.model.participant2.evcs.EvcsModel
import edu.ie3.simona.model.participant2.load.LoadModel

import java.time.ZonedDateTime
import scala.reflect.ClassTag

object ParticipantModelInit {

  def createModel(
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
  ): ParticipantModel[
    _ <: OperatingPoint,
    _ <: ModelState,
  ] = {

    val scaledParticipantInput =
      (participantInput.copy().scale(modelConfig.scaling) match {
        // matching needed because Scala has trouble recognizing the Java type parameter
        case copyBuilder: SystemParticipantInputCopyBuilder[_] => copyBuilder
      }).build()

    (scaledParticipantInput, modelConfig) match {
      case (input: FixedFeedInInput, _) =>
        FixedFeedInModel(input)
      case (input: LoadInput, config: LoadRuntimeConfig) =>
        LoadModel(input, config)
      case (input: PvInput, _) =>
        PvModel(input)
      case (input: WecInput, _) =>
        WecModel(input)
      case (input: StorageInput, config: StorageRuntimeConfig) =>
        StorageModel(input, config)
      case (input: EvcsInput, config: EvcsRuntimeConfig) =>
        EvcsModel(input, config)
      case (input, config) =>
        throw new CriticalFailureException(
          s"Handling the input model ${input.getClass.getSimpleName} or " +
            "the combination of the input model with model config " +
            s"${config.getClass.getSimpleName} is not implemented."
        )
    }
  }

  def createPrimaryModel[P <: PrimaryData: ClassTag](
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
      primaryDataMeta: PrimaryDataMeta[P],
  ): ParticipantModel[
    _ <: OperatingPoint,
    _ <: ModelState,
  ] = {
    // Create a fitting physical model to extract parameters from
    val physicalModel = createModel(
      participantInput,
      modelConfig,
    )

    createPrimaryModel(
      physicalModel,
      primaryDataMeta,
    )
  }

  def createPrimaryModel[P <: PrimaryData: ClassTag](
      physicalModel: ParticipantModel[_, _],
      primaryDataMeta: PrimaryDataMeta[P],
  ): ParticipantModel[
    _ <: OperatingPoint,
    _ <: ModelState,
  ] = {
    val primaryResultFunc = new PrimaryResultFunc {
      override def createResult(
          data: PrimaryData.PrimaryDataWithComplexPower[_],
          dateTime: ZonedDateTime,
      ): SystemParticipantResult =
        physicalModel.createPrimaryDataResult(data, dateTime)
    }

    new PrimaryDataParticipantModel(
      physicalModel.uuid,
      physicalModel.sRated,
      physicalModel.cosPhiRated,
      physicalModel.qControl,
      primaryResultFunc,
      primaryDataMeta,
    )
  }

}
