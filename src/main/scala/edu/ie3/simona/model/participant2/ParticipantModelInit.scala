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
import edu.ie3.simona.model.participant2.ParticipantModel.ModelState
import edu.ie3.simona.model.participant2.PrimaryDataParticipantModel.PrimaryResultFunc

import java.time.ZonedDateTime

object ParticipantModelInit {

  def createModel[S <: ModelState](
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
  ): ParticipantModelInitContainer[_] = {

    // function needed because Scala does not recognize Java type parameter
    def scale[B <: SystemParticipantInputCopyBuilder[B]](
        builder: B
    ): Double => B =
      factor => builder.scale(factor)

    val scaledParticipantInput =
      scale(participantInput.copy)(modelConfig.scaling).build()

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

  def createPrimaryModel[T <: PrimaryData](
      participantInput: SystemParticipantInput,
      modelConfig: BaseRuntimeConfig,
  ): ParticipantModelInitContainer[_] = {
    // Create a fitting physical model to extract parameters from
    val modelContainer = createModel(
      participantInput,
      modelConfig,
    )
    val physicalModel = modelContainer.model

    val primaryResultFunc = new PrimaryResultFunc[T] {
      override def createResult(
          data: T with PrimaryData.PrimaryDataWithApparentPower[_],
          dateTime: ZonedDateTime,
      ): SystemParticipantResult =
        physicalModel.createPrimaryDataResult(data, dateTime)
    }

    val primaryDataModel = new PrimaryDataParticipantModel[T](
      physicalModel.uuid,
      physicalModel.sRated,
      physicalModel.cosPhiRated,
      physicalModel.qControl,
      primaryResultFunc,
    )

    ParticipantModelInitContainer(
      primaryDataModel,
      primaryDataModel.getInitialState,
    )
  }

  final case class ParticipantModelInitContainer[S <: ModelState](
      model: ParticipantModel[_, S, _],
      initialState: S,
  )
}
