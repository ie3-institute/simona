/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.simona.model.grid.ampacity.SoilType
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.scala.quantities.{
  JoulesPerCubicMeterKelvin,
  KelvinMetersPerWatt,
}
import squants.thermal.Celsius

import java.util.UUID

trait SoilInputTestData extends DefaultTestData {

  /** A test soil type matching the soil parameters used in this test model.
    */
  protected val defaultSoilType: SoilType = SoilType(
    UUID.fromString("a1b2c3d4-e5f6-7890-abcd-ef1234567890"),
    "SandyClay",
    KelvinMetersPerWatt(1.0),
    KelvinMetersPerWatt(2.5),
    JoulesPerCubicMeterKelvin(1.0),
    Celsius(15),
  )
}
