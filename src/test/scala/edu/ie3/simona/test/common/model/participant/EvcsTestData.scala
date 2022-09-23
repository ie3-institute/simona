/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.model.participant

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.evcs.{ChargingStrategy, EvcsModel}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.scala.OperationInterval
import tech.units.indriya.quantity.Quantities

import java.util.UUID

trait EvcsTestData {
  protected val evcsStandardModel: EvcsModel = EvcsModel(
    UUID.randomUUID(),
    "Evcs Model Test",
    OperationInterval(0L, 31536000L),
    1.0,
    QControl.apply(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
    Quantities.getQuantity(100, PowerSystemUnits.KILOVOLTAMPERE),
    ElectricCurrentType.AC,
    0.95d,
    4,
    EvcsLocationType.HOME,
    ChargingStrategy.MAX_POWER
  )
}
