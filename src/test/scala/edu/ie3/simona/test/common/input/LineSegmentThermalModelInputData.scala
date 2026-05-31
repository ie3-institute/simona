/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.simona.model.grid.ampacity.{CableSetup, LineSegmentThermalModel}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  ThermalResistivity,
}
import squants.thermal.Celsius

import java.util.UUID

trait LineSegmentThermalModelInputData extends DefaultTestData {
  protected val cigreT880LandCable33kV: CableSetup =
    CigreT880LandCable33kV.cable
  protected val andersSingleCore10kV: CableSetup =
    Anders1997SingleCoreCable10kV.cable
  protected val lineSegmentThermalModel: LineSegmentThermalModel =
    LineSegmentThermalModel(
      UUID.fromString("9d62d1dd-a5a2-41e0-aaaa-dfd44365224f"),
      "testModel",
      UUID.fromString("4be05b08-08a8-49ce-a427-0655a60b5616"),
      KelvinMetersPerWatt(1.0),
      KelvinMetersPerWatt(1.0),
      KelvinMetersPerWatt(1.0),
      KelvinMetersPerWatt(1.0),
      JoulesPerMeterKelvin(1.0),
      JoulesPerMeterKelvin(1.0),
      JoulesPerMeterKelvin(1.0),
      JoulesPerMeterKelvin(1.0),
      JoulesPerMeterKelvin(1.0),
      Celsius(90d),
    )(using defaultSimulationStart)

}
