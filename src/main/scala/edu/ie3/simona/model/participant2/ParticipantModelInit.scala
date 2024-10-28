/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput.SystemParticipantInputCopyBuilder
import edu.ie3.datamodel.models.input.system.{
  PvInput,
  StorageInput,
  SystemParticipantInput,
}
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.participant.data.Data.PrimaryData
import edu.ie3.simona.config.SimonaConfig.{
  BaseRuntimeConfig,
  StorageRuntimeConfig,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  OperatingPoint,
  OperationRelevantData,
}
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel.PrimaryResultFunc

import java.time.ZonedDateTime
import scala.reflect.ClassTag

object ParticipantModelInit {

  def createModel(
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
  ): ParticipantModelInitContainer[
    _ <: OperatingPoint,
    _ <: ModelState,
    _ <: OperationRelevantData,
  ] = {

    val scaledParticipantInput =
      (participantInput.copy().scale(modelConfig.scaling) match {
        // matching needed because Scala has trouble recognizing the Java type parameter
        case copyBuilder: SystemParticipantInputCopyBuilder[_] => copyBuilder
      }).build()

    (scaledParticipantInput, modelConfig) match {
      case (input: PvInput, _) =>
        val model = PvModel(input)
        val state = model.getInitialState
        ParticipantModelInitContainer(model, state)
      case (input: StorageInput, config: StorageRuntimeConfig) =>
        val model = StorageModel(input, config)
        val state = model.getInitialState(config)
        ParticipantModelInitContainer(model, state)
      case (input, config) =>
        throw new CriticalFailureException(
          s"Handling the input model ${input.getClass.getSimpleName} or " +
            "the combination of the input model with model config " +
            s"${config.getClass.getSimpleName} is not implemented."
        )
    }
  }

  def createPrimaryModel[P <: PrimaryData[_]: ClassTag](
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
  ): ParticipantModelInitContainer[
    _ <: OperatingPoint,
    _ <: ModelState,
    _ <: OperationRelevantData,
  ] = {
    // Create a fitting physical model to extract parameters from
    val modelContainer = createModel(
      participantInput,
      modelConfig,
    )
    val physicalModel = modelContainer.model

    val primaryResultFunc = new PrimaryResultFunc {
      override def createResult(
          data: PrimaryData.PrimaryDataWithApparentPower[_],
          dateTime: ZonedDateTime,
      ): SystemParticipantResult =
        physicalModel.createPrimaryDataResult(data, dateTime)
    }

    val primaryDataModel = new PrimaryDataParticipantModel[P](
      physicalModel.uuid,
      physicalModel.sRated,
      physicalModel.cosPhiRated,
      physicalModel.qControl,
      primaryResultFunc,
      ???, // todo needs to be provided by primary data service?
    )

    ParticipantModelInitContainer(
      primaryDataModel,
      primaryDataModel.getInitialState,
    )
  }

  final case class ParticipantModelInitContainer[
      OP <: OperatingPoint,
      S <: ModelState,
      OR <: OperationRelevantData,
  ](
      model: ParticipantModel[OP, S, OR] with ParticipantFlexibility[OP, S, OR],
      initialState: S,
  )
}
