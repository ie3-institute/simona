/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.SoilTypeExtensions.*
import edu.ie3.simona.test.common.input.SoilInputTestData
import org.scalatest.funsuite.AnyFunSuite
import tech.units.indriya.quantity.Quantities
import edu.ie3.util.quantities.PowerSystemUnits.{
  KELVIN_METRE_PER_WATT,
  JOULE_PER_CUBIC_METRE_KELVIN,
}
import tech.units.indriya.unit.Units.CELSIUS
import javax.measure.quantity.Temperature
import tech.units.indriya.ComparableQuantity
import java.util.UUID

class SoilTypeExtensionsSpec extends AnyFunSuite with SoilInputTestData {

  test("returns dry resistivity when temperatureDifference >= critical") {
    val tempDiff: ComparableQuantity[Temperature] =
      Quantities.getQuantity(20.0, CELSIUS)

    val res = defaultSoilType.currentThermalResistivity(tempDiff)

    assert(res == defaultSoilType.getThermalResistivityDry)
  }

  test("returns wet resistivity when temperatureDifference < critical") {
    val tempDiff: ComparableQuantity[Temperature] =
      Quantities.getQuantity(10.0, CELSIUS)

    val res = defaultSoilType.currentThermalResistivity(tempDiff)

    assert(res == defaultSoilType.getThermalResistivityWet)
  }

  test("throws IllegalArgumentException when temperatureDifference is null") {
    assertThrows[IllegalArgumentException](
      defaultSoilType.currentThermalResistivity(null)
    )
  }

  test(
    "throws IllegalStateException when criticalTemperatureDifference is null"
  ) {
    val soilNoCritical = new edu.ie3.datamodel.models.input.SoilType(
      UUID.randomUUID(),
      "noCritical",
      Quantities.getQuantity(1.0, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(2.0, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(1.0, JOULE_PER_CUBIC_METRE_KELVIN),
      null,
    )

    assertThrows[IllegalStateException](
      soilNoCritical.currentThermalResistivity(
        Quantities.getQuantity(10.0, CELSIUS)
      )
    )
  }
}
