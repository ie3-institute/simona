/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import edu.ie3.simona.model.grid.ampacity.{CableSetup, LineSegmentThermalModel}
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.simona.util.Coordinate3D
import edu.ie3.util.scala.quantities.{
  JoulesPerCubicMeterKelvin,
  KelvinMetersPerWatt,
}
import squants.Meters
import squants.thermal.Celsius

import java.util.UUID

trait LineSegmentThermalModelInputData
    extends DefaultTestData
    with SoilInputTestData {
  protected val cigreT880LandCable33kVCableSetup: CableSetup =
    CigreT880LandCable33kV.cable
  protected val andersSingleCore10kVCableSetup: CableSetup =
    Anders1997SingleCoreCable10kV.cable

  /** A [[LineSegmentThermalModel]] for the CIGRE cable, including the segment
    * geometry (endpoints, depth) and the surrounding soil parameters.
    */
  protected val cigreLandCable33kVLineSegmentThermalModel
      : LineSegmentThermalModel = LineSegmentThermalModel(
    UUID.fromString("9d62d1dd-a5a2-41e0-aaaa-dfd44365224f"),
    "Cigre TB880 Land Cable 33kV",
    UUID.fromString("4be05b08-08a8-49ce-a427-0655a60b5616"),
    cigreT880LandCable33kVCableSetup,
    Coordinate3D(0.0, 0.0, -1.0),
    Coordinate3D(1.0, 0.0, -1.0),
    Meters(-1),
    Meters(0.044),
    KelvinMetersPerWatt(0.4110322351),
    KelvinMetersPerWatt(0.0),
    KelvinMetersPerWatt(0.12419418991),
    KelvinMetersPerWatt(1.8524966955),
    JoulesPerCubicMeterKelvin(1.0),
    JoulesPerCubicMeterKelvin(1.0),
    JoulesPerCubicMeterKelvin(1.0),
    JoulesPerCubicMeterKelvin(1.0),
    JoulesPerCubicMeterKelvin(1.0),
    Celsius(90d),
    defaultSoilType,
  )

  /** A [[LineSegmentThermalModel]] for the Anders single-core cable, including
    * the segment geometry (endpoints, depth) and the surrounding soil
    * parameters.
    */
  protected val andersLineSegmentThermalModel: LineSegmentThermalModel =
    LineSegmentThermalModel(
      UUID.fromString("b8152c3f-d12f-4857-9746-a30aef6aee08"),
      "AndersSingleCore_10kV",
      UUID.fromString("4be05b08-08a8-49ce-a427-0655a60b5616"),
      andersSingleCore10kVCableSetup,
      Coordinate3D(0.0, 0.0, -1.0),
      Coordinate3D(1.0, 0.0, -1.0),
      Meters(-1),
      Meters(2 * 0.0358),
      KelvinMetersPerWatt(0.214),
      KelvinMetersPerWatt(0.0),
      KelvinMetersPerWatt(0.104),
      KelvinMetersPerWatt(1.933),
      JoulesPerCubicMeterKelvin(1.0),
      JoulesPerCubicMeterKelvin(1.0),
      JoulesPerCubicMeterKelvin(1.0),
      JoulesPerCubicMeterKelvin(1.0),
      JoulesPerCubicMeterKelvin(1.0),
      Celsius(90d),
      defaultSoilType,
    )
}
