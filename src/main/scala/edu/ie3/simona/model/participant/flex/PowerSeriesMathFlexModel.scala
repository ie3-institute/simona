/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.flex

import edu.ie3.simona.model.participant.{
  ParticipantFlexModel,
  ParticipantModel,
  PowerSeriesMathFlexOptions,
}
import edu.ie3.simona.model.participant.ParticipantModel.{
  ModelState,
  OperatingPoint,
}
import edu.ie3.simona.ontology.messages.flex.FlexOptions

import scala.collection.immutable.SortedMap

class PowerSeriesMathFlexModel[S <: ModelState](
    model: ParticipantModel[?, S],
    determineStates: S => SortedMap[Long, S],
) extends ParticipantFlexModel[S] {

  override def determineFlexOptions(state: S): FlexOptions = {

    val powerMap = determineStates(state).map { case (tick, tickState) =>
      val (op: OperatingPoint, _) = model.determineOperatingPoint(tickState)
      tick -> op.activePower
    }

    PowerSeriesMathFlexOptions(powerMap)
  }

}
