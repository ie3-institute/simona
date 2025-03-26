/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model

import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.{
  CalcRelevantData,
  FlexChangeIndicator,
  ModelState,
  SystemParticipant,
}
import edu.ie3.simona.ontology.messages.flex.FlexOptions
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.ApparentPower
import squants.Dimensionless
import squants.energy._

import java.util.UUID

class MockParticipant(
    uuid: UUID,
    id: String,
    operationInterval: OperationInterval,
    qControl: QControl,
    sRated: ApparentPower,
    cosPhiRated: Double,
) extends SystemParticipant[
      CalcRelevantData,
      Data.PrimaryData.ComplexPower,
      ModelState,
    ](
      uuid,
      id,
      operationInterval,
      qControl,
      sRated,
      cosPhiRated,
    ) {

  override def calculatePower(
      tick: Long,
      voltage: Dimensionless,
      state: ModelState,
      data: CalcRelevantData,
  ): Data.PrimaryData.ComplexPower = {
    super.calculateApparentPower(tick, voltage, state, data)
  }

  override def calculateActivePower(
      maybeModelState: ModelState,
      data: CalcRelevantData,
  ): Power = {
    Kilowatts(0)
  }

  override def determineFlexOptions(
      data: CalcRelevantData,
      lastState: ModelState,
  ): FlexOptions = {
    null
  }

  override def handleControlledPowerChange(
      data: CalcRelevantData,
      lastState: ModelState,
      setPower: Power,
  ): (ModelState, FlexChangeIndicator) = {
    (lastState, FlexChangeIndicator(changesAtTick = None))
  }
}
