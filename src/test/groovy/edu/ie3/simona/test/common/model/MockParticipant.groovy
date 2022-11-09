/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model

import edu.ie3.simona.model.participant.CalcRelevantData
import edu.ie3.simona.model.participant.ModelState
import edu.ie3.simona.model.participant.SystemParticipant
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.ontology.messages.FlexibilityMessage
import edu.ie3.util.scala.OperationInterval
import scala.Tuple2
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.Power

import static edu.ie3.util.quantities.PowerSystemUnits.MEGAWATT

class MockParticipant extends SystemParticipant<CalcRelevantData, ModelState> {

  MockParticipant(
  UUID uuid,
  String id,
  OperationInterval operationInterval,
  Double scalingFactor,
  QControl qControl,
  ComparableQuantity<Power> sRated,
  Double cosPhiRated
  ) {
    super(
    uuid,
    id,
    operationInterval,
    scalingFactor,
    qControl,
    sRated,
    cosPhiRated
    )
  }

  @Override
  ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
    return Quantities.getQuantity(0, MEGAWATT)
  }

  @Override
  FlexibilityMessage.ProvideFlexOptions determineFlexOptions(CalcRelevantData data, ModelState lastState) {
    return null
  }

  @Override
  Tuple2 handleControlledPowerChange(CalcRelevantData data, ModelState lastState, ComparableQuantity setPower) {
    return null
  }
}
