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
import edu.ie3.simona.config.SimonaConfig.BaseRuntimeConfig
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
}
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel.PrimaryResultFunc
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
      case (input: LoadInput, _) =>
        LoadModel(input)
      case (input: PvInput, _) =>
        PvModel(input)
      case (input, config) =>
        throw new CriticalFailureException(
          s"Handling the input model ${input.getClass.getSimpleName} or " +
            "the combination of the input model with model config " +
            s"${config.getClass.getSimpleName} is not implemented."
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
    * @param primaryDataMeta
    *   The primary data meta class that can be used for the data to be
    *   received.
    * @return
    *   The [[PrimaryDataParticipantModel]].
    */
  def createPrimaryModel[PD <: PrimaryData: ClassTag](
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
      primaryDataMeta: PrimaryDataMeta[PD],
  ): PrimaryDataParticipantModel[PD] = {
    // Create a fitting physical model to extract parameters from
    val physicalModel = createPhysicalModel(
      participantInput,
      modelConfig,
    )

    createPrimaryModel(
      physicalModel,
      primaryDataMeta,
    )
  }

  /** Constructs a [[PrimaryDataParticipantModel]] for the given physical
    * [[ParticipantModel]] and the given primary data. The given
    * [[BaseRuntimeConfig]] has to match the participant input.
    *
    * @param physicalModel
    *   The physical participant model.
    * @param primaryDataMeta
    *   The primary data meta class that can be used for the data to be
    *   received.
    * @return
    *   The [[PrimaryDataParticipantModel]].
    */
  def createPrimaryModel[PD <: PrimaryData: ClassTag](
      physicalModel: ParticipantModel[_, _],
      primaryDataMeta: PrimaryDataMeta[PD],
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
      primaryDataMeta,
    )
  }

}
