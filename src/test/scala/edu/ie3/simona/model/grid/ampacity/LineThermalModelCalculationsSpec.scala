/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.connector.`type`.CableMaterial
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.calcLayFactor
import edu.ie3.simona.test.common.input.LineSegmentThermalModelInputData
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.quantities.thermal.{
  JoulesPerCubicMeterKelvin,
  KelvinMetersPerWatt,
}
import squants.electro.OhmMeters
import squants.space.Millimeters

class LineThermalModelCalculationsSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData
    with DefaultTestData {

  implicit val doubleTolerance: Double = 1e-6

  "A LineThermalModelCalculations" should {

    "calculate correctly the lay factor of the screen layer" in {
      val cases = Table(
        (
          "innerDiameterMm",
          "wireDiameterMm",
          "lengthOfLayMm",
          "expected",
        ),
        // CIGRÉ screen: 36.8mm inner, 0.9mm wire, 240mm lay -> sqrt(1 + ((0.0377*Pi)^2)/(0.24^2))
        (36.8, 0.9, 240.0, 1.1151386),
        // No screen layer
        (0.0, 0.0, 0.0, 1.0),
      )

      forAll(cases) {
        (
            innerDiameterMm,
            wireDiameterMm,
            lengthOfLayMm,
            expected,
        ) =>
          val layer =
            if innerDiameterMm == 0.0 then None
            else
              Some(
                ScreenLayer(
                  CableMaterial.COPPER,
                  Millimeters(innerDiameterMm),
                  Millimeters(innerDiameterMm),
                  KelvinMetersPerWatt(0.0),
                  JoulesPerCubicMeterKelvin(0.0),
                  None,
                  56,
                  Millimeters(wireDiameterMm),
                  Some(Millimeters(lengthOfLayMm)),
                  OhmMeters(0.0),
                )
              )

          val actual = calcLayFactor(layer)

          actual should approximate(expected)
      }
    }

    "calculate the lay factor with a missing length of lay using the default" in {
      val screenLayer = ScreenLayer(
        CableMaterial.COPPER,
        Millimeters(36.8),
        Millimeters(36.8),
        KelvinMetersPerWatt(0.0),
        JoulesPerCubicMeterKelvin(0.0),
        None,
        56,
        Millimeters(0.9),
        None,
        OhmMeters(0.0),
      )

      val actual = calcLayFactor(Some(screenLayer))

      actual should approximate(1.0)
    }
  }
}
