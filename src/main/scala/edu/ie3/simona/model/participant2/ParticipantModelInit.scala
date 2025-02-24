/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput.SystemParticipantInputCopyBuilder
import edu.ie3.datamodel.models.input.system._
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.{
  PrimaryData,
  PrimaryDataExtra,
}
import edu.ie3.simona.config.RuntimeConfig.{
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

/** Helper object for constructing all types of [[ParticipantModel]]s, including
  * [[PrimaryDataParticipantModel]].
  */
object ParticipantModelInit {

  /** Constructs the matching [[ParticipantModel]] for the given
    * [[SystemParticipantInput]]. The given [[BaseRuntimeConfig]] has to match
    * the participant input.
    *
    * @param participantInput
    *   The system participant model input.
    * @param modelConfig
    *   The model runtime config.
    * @return
    *   The [[ParticipantModel]].
    */
  def createPhysicalModel(
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
          s"Handling the input model ${input.getClass.getSimpleName} and " +
            s"model config ${config.getClass.getSimpleName} is not implemented."
        )
    }
  }

  /** Constructs a [[PrimaryDataParticipantModel]] for the given
    * [[SystemParticipantInput]] and the given primary data. The given
    * [[BaseRuntimeConfig]] has to match the participant input.
    *
    * @param participantInput
    *   The system participant model input.
    * @param modelConfig
    *   The model runtime config.
    * @param primaryDataExtra
    *   Extra functionality specific to the primary data class.
    * @return
    *   The [[PrimaryDataParticipantModel]].
    */
  def createPrimaryModel[PD <: PrimaryData: ClassTag](
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
      primaryDataExtra: PrimaryDataExtra[PD],
  ): PrimaryDataParticipantModel[PD] = {
    // Create a fitting physical model to extract parameters from
    val physicalModel = createPhysicalModel(
      participantInput,
      modelConfig,
    )

    createPrimaryModel(
      physicalModel,
      primaryDataExtra,
    )
  }

  /** Constructs a [[PrimaryDataParticipantModel]] for the given physical
    * [[ParticipantModel]] and the given primary data. The given
    * [[BaseRuntimeConfig]] has to match the participant input.
    *
    * @param physicalModel
    *   The physical participant model.
    * @param primaryDataExtra
    *   Extra functionality specific to the primary data class.
    * @return
    *   The [[PrimaryDataParticipantModel]].
    */
  def createPrimaryModel[PD <: PrimaryData: ClassTag](
      physicalModel: ParticipantModel[_, _],
      primaryDataExtra: PrimaryDataExtra[PD],
  ): PrimaryDataParticipantModel[PD] = {
    val primaryResultFunc = new PrimaryResultFunc {
      override def createResult(
          data: PrimaryData.PrimaryDataWithComplexPower[_],
          dateTime: ZonedDateTime,
      ): SystemParticipantResult =
        physicalModel.createPrimaryDataResult(data, dateTime)
    }

    new PrimaryDataParticipantModel(
      physicalModel.uuid,
      physicalModel.id,
      physicalModel.sRated,
      physicalModel.cosPhiRated,
      physicalModel.qControl,
      primaryResultFunc,
      primaryDataExtra,
    )
  }

}
