/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.datamodel.models.input.container.SoilType
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits.{
  KELVIN_METRE_PER_WATT,
  JOULE_PER_CUBIC_METRE_KELVIN,
}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.CELSIUS

import java.util.UUID

trait SoilInputTestData extends DefaultTestData {

  /** A test soil type matching the soil parameters used in this test model.
    */
  protected val defaultSoilType: SoilType = new SoilType(
    UUID.fromString("a1b2c3d4-e5f6-7890-abcd-ef1234567890"),
    "SandyClay",
    Quantities.getQuantity(1.0, KELVIN_METRE_PER_WATT),
    Quantities.getQuantity(2.5, KELVIN_METRE_PER_WATT),
    Quantities.getQuantity(1.0, JOULE_PER_CUBIC_METRE_KELVIN),
    Quantities.getQuantity(15.0, CELSIUS),
  )
}
