/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.SoilThermalResistanceCableGroup.*

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.thermal.{KelvinMetersPerWatt, ThermalResistivity}

import squants.thermal.{Celsius, Temperature}
import squants.Meters

class SoilThermalResistanceCableGroupSpec extends UnitSpec {

  implicit val resistanceTolerance: ThermalResistivity = KelvinMetersPerWatt(
    1e-10
  )
  implicit val tolerance: Double = 1e-10
  implicit val temperatureTolerance: Temperature = Celsius(1e-3)

  "return the correct mutual thermal resistance for two cables near a vertical boundary" in {
    val cases = Table(
      (
        "boundaryResistivity",
        "distanceJ",
        "distanceK",
        "expectedRes",
      ),
      (0.4, 0.02, 0.06, 0.3705763646), // S&M 1994 Table 2, R12
      (0.4, 0.02, 0.1, 0.2879710588), // S&M 1994 Table 2, R13
      (0.4, 0.06, 0.1, 0.4175240382), // S&M 1994 Table 2, R23
      (1.5, 0.02, 0.06, 0.6570747211), // S&M 1994 Table 2, R12
      (1.5, 0.02, 0.1, 0.5341096710), // S&M 1994 Table 2, R13
      (1.5, 0.06, 0.1, 0.6351658067), // S&M 1994 Table 2, R23
      (2.5, 0.02, 0.06, 0.7612559416), // S&M 1994 Table 2, R12
    )
    forAll(cases) { (boundaryResistivity, distanceJ, distanceK, expectedRes) =>
      val soilRes = KelvinMetersPerWatt(1.0)
      val boundaryRes = KelvinMetersPerWatt(boundaryResistivity)
      val depth = Meters(-0.7)
      val distJ = Meters(distanceJ)
      val distK = Meters(distanceK)
      val expectedThermalResistance = KelvinMetersPerWatt(expectedRes)
      val actual = calcThermalResistanceMutualVerticalBoundary(
        soilRes,
        boundaryRes,
        depth,
        distJ,
        distK,
      )
      actual should approximate(expectedThermalResistance)
    }
  }

  "reduce to the homogeneous mutual resistance at a vertical boundary with equal resistivities" in {
    val soilRes = KelvinMetersPerWatt(1.0)
    val depth = Meters(-0.7)
    val distJ = Meters(0.02)
    val distK = Meters(0.06)
    val actual = calcThermalResistanceMutualVerticalBoundary(
      soilRes,
      soilRes,
      depth,
      distJ,
      distK,
    )
    actual should approximate(
      KelvinMetersPerWatt(0.5659161531)
    ) // TODO DF check value
  }

  "throw an exception if both cables of the mutual vertical boundary resistance are at the same distance" in {
    an[IllegalArgumentException] should be thrownBy {
      calcThermalResistanceMutualVerticalBoundary(
        KelvinMetersPerWatt(1.0),
        KelvinMetersPerWatt(1.5),
        Meters(-0.7),
        Meters(0.06),
        Meters(0.06),
      )
    }
  }

  "return the correct mutual thermal resistance for two cables beneath a horizontal layer" in { // TODO DF needs to be reviewed
    val soilRes = KelvinMetersPerWatt(0.7)
    val h = Meters(0.7)
    val s = Meters(0.04)
    val a = Meters(0.6)
    val t = Meters(0.1)
    val actual = calcThermalResistanceMutualHorizontalLayerBeneath(
      soilRes,
      soilRes,
      h,
      s,
      a,
      t,
    )
    val expectedUniform = KelvinMetersPerWatt(
      0.7 / (4 * scala.math.Pi) * scala.math.log(
        (4 * 0.7 * 0.7 + 0.04 * 0.04) / (0.04 * 0.04)
      )
    )
    {
      implicit val looseTol: ThermalResistivity = KelvinMetersPerWatt(1e-3)
      actual should approximate(expectedUniform)
    }
    val rLow = calcThermalResistanceMutualHorizontalLayerBeneath(
      soilRes,
      KelvinMetersPerWatt(0.5),
      h,
      s,
      a,
      t,
    )
    val rHigh = calcThermalResistanceMutualHorizontalLayerBeneath(
      soilRes,
      KelvinMetersPerWatt(2.5),
      h,
      s,
      a,
      t,
    )
    rHigh should be > rLow
  }

  "return the correct mutual thermal resistance for two cables within a horizontal layer" in { // TODO DF needs to be reviewed
    val soilRes = KelvinMetersPerWatt(0.7)
    val a = Meters(0.3)
    val h = Meters(0.7)
    val s = Meters(0.04)
    val actual = calcThermalResistanceMutualHorizontalLayerWithin(
      soilRes,
      soilRes,
      a,
      h,
      s,
    )
    val expectedUniform = KelvinMetersPerWatt(
      0.7 / (4 * scala.math.Pi) * scala.math.log(
        (4 * 0.7 * 0.7 + 0.04 * 0.04) / (0.04 * 0.04)
      )
    )
    {
      implicit val looseTol: ThermalResistivity = KelvinMetersPerWatt(1e-3)
      actual should approximate(expectedUniform)
    }
    val rLow = calcThermalResistanceMutualHorizontalLayerWithin(
      soilRes,
      KelvinMetersPerWatt(1.0),
      a,
      h,
      s,
    )
    val rHigh = calcThermalResistanceMutualHorizontalLayerWithin(
      soilRes,
      KelvinMetersPerWatt(2.5),
      a,
      h,
      s,
    )
    rHigh should be > rLow
  }

  "return the correct mutual thermal resistance for two cables near a vertical layer" in { // TODO DF needs to be reviewed
    val rho = KelvinMetersPerWatt(1.0)
    val h = Meters(0.7)
    val s = Meters(0.04)
    val a = Meters(0.3)
    val t = Meters(0.2)
    val actual = calcThermalResistanceMutualVerticalLayer(
      rho,
      rho,
      rho,
      h,
      s,
      a,
      t,
    )
    val expectedUniform = KelvinMetersPerWatt(
      1.0 / (4 * scala.math.Pi) *
        scala.math.log((4 * 0.7 * 0.7 + 0.04 * 0.04) / (0.04 * 0.04))
    )
    {
      implicit val looseTol: ThermalResistivity = KelvinMetersPerWatt(1e-3)
      actual should approximate(expectedUniform)
    }
    val rLow = calcThermalResistanceMutualVerticalLayer(
      rho,
      KelvinMetersPerWatt(0.5),
      rho,
      h,
      s,
      a,
      t,
    )
    val rHigh = calcThermalResistanceMutualVerticalLayer(
      rho,
      KelvinMetersPerWatt(2.5),
      rho,
      h,
      s,
      a,
      t,
    )
    rHigh should be > rLow
  }

  "return zero delta correction for outer pair cables in a homogeneous vertical layer" in { // TODO DF needs to be reviewed
    val rho = KelvinMetersPerWatt(1.0)
    val h = Meters(0.7)
    val s = Meters(0.04)
    val a = Meters(0.3)
    val t = Meters(0.2)
    val actual = calcThermalResistanceDeltaVerticalLayerOuter(
      rho,
      rho,
      rho,
      h,
      s,
      a,
      t,
    )
    actual should approximate(KelvinMetersPerWatt(0.0))
    val delta = calcThermalResistanceDeltaVerticalLayerOuter(
      KelvinMetersPerWatt(1.0),
      KelvinMetersPerWatt(0.5),
      KelvinMetersPerWatt(2.5),
      h,
      s,
      a,
      t,
    )
    delta should be > KelvinMetersPerWatt(0.0)
  }

  "return zero delta correction for far pair cables in a homogeneous vertical layer" in { // TODO DF needs to be reviewed
    val rho = KelvinMetersPerWatt(1.0)
    val h = Meters(0.7)
    val s = Meters(0.04)
    val a = Meters(0.3)
    val t = Meters(0.2)
    val actual = calcThermalResistanceDeltaVerticalLayerFar(
      rho,
      rho,
      rho,
      h,
      s,
      a,
      t,
    )
    actual should approximate(KelvinMetersPerWatt(0.0))
    val delta = calcThermalResistanceDeltaVerticalLayerFar(
      KelvinMetersPerWatt(1.0),
      KelvinMetersPerWatt(0.5),
      KelvinMetersPerWatt(2.5),
      h,
      s,
      a,
      t,
    )
    delta should be > KelvinMetersPerWatt(0.0)
  }
}
