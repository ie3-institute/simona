/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointTypeUtils
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.agent.participant.statedata.ParticipantStateData.SimpleInputContainer
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.util.quantities.QuantityUtils._

import java.util.UUID

trait EvcsInputTestData extends DefaultTestData with NodeInputTestData {

  protected val evcsInputModel = new EvcsInput(
    UUID.randomUUID(),
    "Dummy_EvcsModel",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
    nodeInputNoSlackNs04KvA,
    CosPhiFixed.CONSTANT_CHARACTERISTIC,
    null,
    ChargingPointTypeUtils.ChargingStationType2,
    2,
    1,
    EvcsLocationType.HOME,
    true,
  )

  protected val evcsInputContainer = SimpleInputContainer(evcsInputModel)

  protected val ev1 = new MockEvModel(
    UUID.fromString("0-0-0-1-1"),
    "TestEv1",
    5.0.asKiloWatt,
    10.0.asKiloWatt,
    10.0.asKiloWattHour,
    5.0.asKiloWattHour,
    18000L,
  )

  protected val ev2 = new MockEvModel(
    UUID.fromString("0-0-0-1-2"),
    "TestEv2",
    5.0.asKiloWatt,
    10.0.asKiloWatt,
    10.0.asKiloWattHour,
    7.5.asKiloWattHour,
    18000L,
  )

  protected val ev3 = new MockEvModel(
    UUID.fromString("0-0-0-1-3"),
    "TestEv3",
    10.0.asKiloWatt, // AC is relevant,
    20.0.asKiloWatt, // DC is not
    20.0.asKiloWattHour,
    15.0.asKiloWattHour,
    10800L,
  )

  protected val ev4 = new MockEvModel(
    UUID.fromString("0-0-0-1-4"),
    "TestEv4",
    10.0.asKiloWatt, // AC is relevant,
    20.0.asKiloWatt, // DC is not
    10.0.asKiloWattHour,
    0.0.asKiloWattHour,
    10800L,
  )

  protected val ev5 = new MockEvModel(
    UUID.fromString("0-0-0-1-5"),
    "TestEv5",
    5.0.asKiloWatt, // AC is relevant,
    10.0.asKiloWatt, // DC is not
    15.0.asKiloWattHour,
    0.0.asKiloWattHour,
    14400L,
  )

}
