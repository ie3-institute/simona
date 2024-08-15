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
    UUID.fromString("0-0-0-0-a"),
    "evA",
    Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(58d, PowerSystemUnits.KILOWATTHOUR),
    200,
  )
  protected val evB: MockEvModel = new MockEvModel(
    UUID.fromString("0-0-0-0-b"),
    "evB",
    Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(80d, PowerSystemUnits.KILOWATTHOUR),
    200,
  )
  protected val evC: MockEvModel = new MockEvModel(
    UUID.fromString("0-0-0-0-c"),
    "evC",
    Quantities.getQuantity(22d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(22d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(120d, PowerSystemUnits.KILOWATTHOUR),
    200,
  )
}
