/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2

import edu.ie3.datamodel.models.input.system.{PvInput, SystemParticipantInput}

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

}
