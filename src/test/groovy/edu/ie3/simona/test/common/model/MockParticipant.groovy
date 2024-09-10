/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model

import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.model.participant.CalcRelevantData
import edu.ie3.simona.model.participant.ModelState
import edu.ie3.simona.model.participant.SystemParticipant
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.ApparentPower
import edu.ie3.util.scala.quantities.Sq
import scala.Tuple2
import squants.Dimensionless
import squants.energy.*

class MockParticipant extends SystemParticipant<CalcRelevantData, Data.PrimaryData.ComplexPower, ModelState> {

  MockParticipant(
  UUID uuid,
  String id,
  OperationInterval operationInterval,
  QControl qControl,
  ApparentPower sRated,
  Double cosPhiRated
  ) {
    super(
    uuid,
    id,
    operationInterval,
    qControl,
    sRated,
    cosPhiRated
    )
  }

  @Override
  Data.PrimaryData.ComplexPower calculatePower(long tick, Dimensionless voltage, ModelState state, CalcRelevantData data) {
    return super.calculateApparentPower(tick, voltage, state, data)
  }

  @Override
  Power calculateActivePower(ModelState maybeModelState, CalcRelevantData data) {
    return Sq.create(0, Megawatts$.MODULE$)
  }

  @Override
  FlexibilityMessage.ProvideFlexOptions determineFlexOptions(CalcRelevantData data, ModelState lastState) {
    return null
  }

  @Override
  Tuple2 handleControlledPowerChange(CalcRelevantData data, ModelState lastState, Power setPower) {
    return null
  }
}
