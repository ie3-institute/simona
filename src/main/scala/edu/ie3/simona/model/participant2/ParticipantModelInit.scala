/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.SystemParticipantInput.SystemParticipantInputCopyBuilder
import edu.ie3.datamodel.models.input.system._
import edu.ie3.simona.agent.participant.data.Data.{
  PrimaryData,
  PrimaryDataExtra,
}
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.InputModelContainer
import edu.ie3.simona.config.RuntimeConfig.{
  BaseRuntimeConfig,
  EvcsRuntimeConfig,
  LoadRuntimeConfig,
  StorageRuntimeConfig,
}
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModel.{
  ModelState,
  ParticipantModelFactory,
}
import edu.ie3.simona.model.participant2.evcs.EvcsModel
import edu.ie3.simona.model.participant2.load.LoadModel

import scala.reflect.ClassTag

/** Helper object for constructing all types of [[ParticipantModel]]s, including
  * [[PrimaryDataParticipantModel]].
  */
object ParticipantModelInit {

  /** Constructs the matching [[ParticipantModel]] for the given
    * [[SystemParticipantInput]]. The given [[BaseRuntimeConfig]] has to match
    * the participant input.
    *
    * @param inputContainer
    *   The input container holding the system participant model input that
    *   represents the physical model at the core of the agent.
    * @param modelConfig
    *   The model runtime config.
    * @return
    *   The [[ParticipantModel]].
    */
  def getPhysicalModelFactory(
      inputContainer: InputModelContainer[_ <: SystemParticipantInput],
      modelConfig: BaseRuntimeConfig,
  ): ParticipantModelFactory[_ <: ModelState] = {
    val scaledParticipantInput = {
      (inputContainer.electricalInputModel
        .copy()
        .scale(modelConfig.scaling) match {
        // matching needed because Scala has trouble recognizing the Java type parameter
        case copyBuilder: SystemParticipantInputCopyBuilder[_] => copyBuilder
      }).build()
    }

    (scaledParticipantInput, modelConfig) match {
      case (input: FixedFeedInInput, _) =>
        FixedFeedInModel.Factory(input)
      case (input: LoadInput, config: LoadRuntimeConfig) =>
        LoadModel.getFactory(input, config)
      case (input: PvInput, _) =>
        PvModel.Factory(input)
      case (input: WecInput, _) =>
        WecModel.Factory(input)
      case (input: StorageInput, config: StorageRuntimeConfig) =>
        StorageModel.Factory(input, config)
      case (input: EvcsInput, config: EvcsRuntimeConfig) =>
        EvcsModel.Factory(input, config)
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
    * @param inputContainer
    *   The input container holding the system participant model input that
    *   represents the physical model at the core of the agent.
    * @param modelConfig
    *   The model runtime config.
    * @param primaryDataExtra
    *   Extra functionality specific to the primary data class.
    * @return
    *   The [[PrimaryDataParticipantModel]].
    */
  def getPrimaryModelFactory[PD <: PrimaryData](
      inputContainer: InputModelContainer[_ <: SystemParticipantInput],
      modelConfig: BaseRuntimeConfig,
      primaryDataExtra: PrimaryDataExtra[PD],
  ): ParticipantModelFactory[_ <: ModelState] = {
    // Create a fitting physical model to extract parameters from
    val modelFactory = getPhysicalModelFactory(
      inputContainer,
      modelConfig,
    )

    PrimaryDataParticipantModel.Factory(
      modelFactory.create(),
      primaryDataExtra,
    )
  }

}
