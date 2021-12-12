/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities

import java.util.UUID

trait EvTestData {
  protected val evA: MockEvModel = new MockEvModel(
    UUID.fromString("73c041c7-68e9-470e-8ca2-21fd7dbd1797"),
    "evA",
    Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(58d, PowerSystemUnits.KILOWATTHOUR)
  )
  protected val evB: MockEvModel = new MockEvModel(
    UUID.fromString("6d7d27a1-5cbb-4b73-aecb-dfcc5a6fb22e"),
    "evB",
    Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(80d, PowerSystemUnits.KILOWATTHOUR)
  )
}
