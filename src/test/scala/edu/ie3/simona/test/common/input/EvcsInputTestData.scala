/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.{ElectricCurrentType, OperationTime}
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointTypeUtils
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.{ChargingStrategy, EvcsModel}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.scala.OperationInterval
import squants.energy.Kilowatts

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
    0.95,
    EvcsLocationType.HOME,
    true,
  )

  protected val evcsStandardModel: EvcsModel = EvcsModel(
    evcsInputModel,
    1.0,
    defaultSimulationStart,
    defaultSimulationEnd,
    "maxPower",
    lowestEvSoc = 0.2,
  )

  protected val evcsStandardModelWithFixedUUID: EvcsModel = EvcsModel(
    UUID.fromString("ce386961-a4db-4429-939d-e92c1edef873"),
    "Evcs Model Test",
    OperationInterval(0L, 31536000L),
    defaultSimulationStart,
    QControl.apply(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
    Kilowatts(100),
    ElectricCurrentType.AC,
    1.0d,
    4,
    EvcsLocationType.HOME,
    vehicle2grid = false,
    ChargingStrategy.MAX_POWER,
    lowestEvSoc = 0.2,
  )
}
