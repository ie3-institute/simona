/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load.markov

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.{CalcRelevantData, SystemParticipant}
import edu.ie3.util.scala.OperationInterval
import squants.Power

import java.util.UUID

abstract class MarkovModel(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: Power,
    cosPhiRated: Double,
) extends SystemParticipant[
      MarkovRelevantData,
      ApparentPower,
      ConstantState.type,
    ](
      uuid = ???,
      id = ???,
      operationInterval = ???,
      qControl = ???,
      sRated = ???,
      cosPhiRated = ???,
    )

class MarkovRelevantData extends CalcRelevantData
