/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model

import edu.ie3.simona.agent.participant.data.Data
import edu.ie3.simona.model.participant.CalcRelevantData

import edu.ie3.simona.model.participant.SystemParticipant
import edu.ie3.simona.model.participant.control.QControl

import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.Sq
import squants.Dimensionless
import squants.energy.*

class MockParticipant extends SystemParticipant<CalcRelevantData, Data.PrimaryData.PrimaryDataWithApparentPower> {

  MockParticipant(
  UUID uuid,
  String id,
  OperationInterval operationInterval,
  Double scalingFactor,
  QControl qControl,
  Power sRated,
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
  Data.PrimaryData.ApparentPower calculatePower(long tick, Dimensionless voltage, CalcRelevantData data) {
    return super.calculateApparentPower(tick, voltage, data)
  }

  @Override
  Power calculateActivePower(CalcRelevantData data) {
    return Sq.create(0, Megawatts$.MODULE$)
  }
}
