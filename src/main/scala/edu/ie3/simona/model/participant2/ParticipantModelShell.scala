/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant2.ParticipantModel.OperationRelevantData

import scala.reflect._

/** Takes care of:
  *   - activating/deactivating model
  *   - holding id information
  */
class ParticipantModelShell {

  def handleReceivedData[
      OR <: OperationRelevantData: ClassTag,
      M <: ParticipantModel[_, _, OR],
  ](receivedData: OperationRelevantData, model: M) = {

    receivedData match {
      case _: M =>

      case unexpected =>
        throw new CriticalFailureException(
          s"Received unexpected operation relevant data $unexpected"
        )
    }

  }
}
