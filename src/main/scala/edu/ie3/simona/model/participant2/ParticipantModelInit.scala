/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.{PvInput, SystemParticipantInput}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData

import java.time.ZonedDateTime

object ParticipantModelInit {

  def createModel(
      participantInput: SystemParticipantInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): ParticipantModel[_, _, _] =
    participantInput match {
      case pvInput: PvInput =>
        PvModel(pvInput, scalingFactor, simulationStartDate, simulationEndDate)
    }

  def createPrimaryModel[T <: PrimaryData](
      participantInput: SystemParticipantInput,
      scalingFactor: Double,
      simulationStartDate: ZonedDateTime,
      simulationEndDate: ZonedDateTime,
  ): PrimaryDataParticipantModel[T] = {
    // Create a fitting physical model to extract parameters from
    val physicalModel = createModel(
      participantInput,
      scalingFactor,
      simulationStartDate,
      simulationEndDate,
    )

    new PrimaryDataParticipantModel[T](
      physicalModel.uuid,
      physicalModel.sRated,
      physicalModel.cosPhiRated,
      physicalModel.qControl,
    )
  }
}
