/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.SoilThermalResistanceCalculations.calcThermalResistanceToSoilSingleCable
import edu.ie3.simona.model.grid.ampacity.SoilBoundaryReflection.calcBoundaryReflectionFactor
import edu.ie3.simona.model.grid.ampacity.SoilThermalResistanceSingleCable.*

import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.scala.quantities.{KelvinMetersPerWatt, ThermalResistivity}

import squants.thermal.{Celsius, Temperature}
import squants.Meters

class SoilThermalResistanceSingleCableSpec extends UnitSpec {

  implicit val resistanceTolerance: ThermalResistivity = KelvinMetersPerWatt(
    1e-10
  )
  implicit val tolerance: Double = 1e-10
  implicit val temperatureTolerance: Temperature = Celsius(1e-3)

  "calculate correctly the reflection factor for a plane boundary" in {
    val cases = Table(
      (
        "soilResistivity",
        "boundaryResistivity",
        "expectedFactor",
      ),
      (0.7, 1.5, 0.3636363636), // higher resistivity beyond boundary
      (0.7, 0.4, -0.2727272727), // lower resistivity beyond boundary
      (1.0, 1.0, 0.0), // homogeneous limit
    ) // TODO DF check values
    forAll(cases) { (soilResistivity, boundaryResistivity, expectedFactor) =>
      val actual = calcBoundaryReflectionFactor(
        KelvinMetersPerWatt(soilResistivity),
        KelvinMetersPerWatt(boundaryResistivity),
      )
      actual should approximate(expectedFactor)
    }
  }

  "return the correct thermal resistance to soil for a single cable near a vertical boundary" in {
    val cases = Table(
      (
        "soilResistivity",
        "boundaryResistivity",
        "depthCable",
        "cableDiameter",
        "distanceToBoundary",
        "expectedRes",
      ),
      (0.7, 1.5, -0.7, 0.04, 0.02, 0.6173469540), // S&M 1992 Table 3, a=0.02
      (0.7, 1.5, -0.7, 0.04, 0.05, 0.5803125863), // S&M 1992 Table 3, a=0.05
      (0.7, 1.5, -0.7, 0.04, 0.1, 0.5525378349), // S&M 1992 Table 3, a=0.1
      (0.7, 1.5, -0.7, 0.04, 0.2, 0.5256372402), // S&M 1992 Table 3, a=0.2
      (0.7, 1.5, -0.7, 0.04, 0.4, 0.5016905182), // S&M 1992 Table 3, a=0.4
      (0.7, 1.5, -0.7, 0.04, 0.6, 0.4906982257), // S&M 1992 Table 3, a=0.6
      (0.7, 1.5, -0.7, 0.04, 1.0, 0.4813732120), // S&M 1992 Table 3, a=1.0
      (0.7, 1.5, -0.7, 0.04, 1.5, 0.4772865792), // S&M 1992 Table 3, a=1.5
      (0.7, 1.5, -0.7, 0.04, 99.0,
        0.4732965822), // S&M 1992 Table 3, a->infinity
      (0.7, 0.4, -0.7, 0.04, 0.02, 0.3652570312), // lower resistivity neighbor
    )
    forAll(cases) {
      (
          soilResistivity,
          boundaryResistivity,
          depthCable,
          cableDiameter,
          distanceToBoundary,
          expectedRes,
      ) =>
        val soilRes = KelvinMetersPerWatt(soilResistivity)
        val boundaryRes = KelvinMetersPerWatt(boundaryResistivity)
        val depth = Meters(depthCable)
        val cableDia = Meters(cableDiameter)
        val distance = Meters(distanceToBoundary)
        val expectedThermalResistance = KelvinMetersPerWatt(expectedRes)
        val actual = calcThermalResistanceToSoilVerticalBoundarySingleCable(
          soilRes,
          boundaryRes,
          depth,
          cableDia,
          distance,
        )
        actual should approximate(expectedThermalResistance)
    }
  }

  "reduce to the homogeneous single cable resistance at a vertical boundary with equal resistivities" in {
    val soilRes = KelvinMetersPerWatt(0.7)
    val depth = Meters(-0.7)
    val cableDia = Meters(0.04)
    val distance = Meters(0.02)
    val actual = calcThermalResistanceToSoilVerticalBoundarySingleCable(
      soilRes,
      soilRes,
      depth,
      cableDia,
      distance,
    )
    val expected = calcThermalResistanceToSoilSingleCable(
      soilRes,
      depth,
      cableDia,
    )
    actual should approximate(expected)
  }

  "throw an exception if the distance to the vertical boundary is not positive" in {
    an[IllegalArgumentException] should be thrownBy {
      calcThermalResistanceToSoilVerticalBoundarySingleCable(
        KelvinMetersPerWatt(0.7),
        KelvinMetersPerWatt(1.5),
        Meters(-0.7),
        Meters(0.04),
        Meters(0.0),
      )
    }
  }

  "return the correct thermal resistance to soil for a cable beneath a horizontal layer" in {
    val cases = Table(
      (
        "soilResistivity",
        "layerResistivity",
        "depthCable",
        "cableDiameter",
        "layerThickness",
        "expectedRes",
      ),
      // S&M 1992 Table 1: rs=0.02, h=0.7
      (0.7, 1.5, -0.7, 0.04, 0.1,
        0.4904555111), // TODO DF check values, different from S&M 1992 Table 1
      (0.7, 1.5, -0.7, 0.04, 0.2, 0.5066438750),
      (0.7, 1.5, -0.7, 0.04, 0.3, 0.5231455686),
      (0.7, 1.5, -0.7, 0.04, 0.4, 0.5413049235),
      (0.7, 1.5, -0.7, 0.04, 0.5, 0.5635102455),
      (0.7, 1.5, -0.7, 0.04, 0.6, 0.5967974348),
      (0.7, 1.5, -0.7, 0.04, 0.65, 0.6273015281),
      (1.0, 0.7, -0.7, 0.04, 0.1, 0.6691662667),
      (1.0, 0.7, -0.7, 0.04, 0.2, 0.6616204702),
      (1.0, 0.7, -0.7, 0.04, 0.65, 0.5882828098),
    )
    forAll(cases) {
      (
          soilResistivity,
          layerResistivity,
          depthCable,
          cableDiameter,
          layerThickness,
          expectedRes,
      ) =>
        val actual = calcThermalResistanceToSoilHorizontalLayerBeneath(
          KelvinMetersPerWatt(soilResistivity),
          KelvinMetersPerWatt(layerResistivity),
          Meters(depthCable),
          Meters(cableDiameter),
          Meters(layerThickness),
        )
        actual should approximate(KelvinMetersPerWatt(expectedRes))
    }
  }

  "reduce to the homogeneous single cable resistance for a cable beneath a horizontal layer with equal resistivities" in {
    val soilRes = KelvinMetersPerWatt(0.7)
    val depth = Meters(-0.7)
    val cableDia = Meters(0.04)
    val layerThick = Meters(0.2)
    val actual = calcThermalResistanceToSoilHorizontalLayerBeneath(
      soilRes,
      soilRes,
      depth,
      cableDia,
      layerThick,
    )
    val expected =
      calcThermalResistanceToSoilSingleCable(soilRes, depth, cableDia)
    {
      // The image-source geometric factor ln(2*h/r_s) matches the Kennelly
      // factor ln(u + sqrt(u^2 - 1)) only up to O(1/u^2), so a looser
      // tolerance applies for the homogeneous limit.
      implicit val looseResistanceTolerance: ThermalResistivity =
        KelvinMetersPerWatt(1e-4)
      actual should approximate(expected)
    }
  }

  "throw an exception if the cable is not below the horizontal layer interface" in {
    an[IllegalArgumentException] should be thrownBy {
      calcThermalResistanceToSoilHorizontalLayerBeneath(
        KelvinMetersPerWatt(0.7),
        KelvinMetersPerWatt(1.5),
        Meters(-0.5),
        Meters(0.04),
        Meters(0.7),
      )
    }
  }

  "return the correct thermal resistance to soil for a cable within a horizontal layer" in {
    val cases = Table(
      (
        "layerResistivity",
        "distanceToLowerBoundary",
        "expectedRes",
      ),
      // S&M 1992 Table 2: rs=0.02, h=0.7, rho1=0.7
      (1.5, 0.05, 0.5531578988),
      (1.5, 0.1, 0.5293632277),
      (1.5, 0.2, 0.5088384700),
      (1.5, 0.4, 0.4929826797),
      (1.5, 0.6, 0.4862278640),
      (1.5, 1.0, 0.4802919054),
      (1.5, 1.5, 0.4773154155),
      (1.2, 0.05, 0.5316712047),
      (1.2, 0.1, 0.5143726969),
      (1.2, 0.2, 0.4994048492),
      (1.2, 0.4, 0.4877962148),
      (1.2, 0.6, 0.4828345623),
      (1.2, 1.0, 0.4784648093),
      (1.2, 1.5, 0.4762699585),
      (0.4, 0.05, 0.4087599175),
      (0.4, 0.1, 0.4272378551),
      (0.4, 0.2, 0.4435553044),
      (0.4, 0.4, 0.4565374277),
      (0.4, 0.6, 0.4622033377),
      (0.4, 1.0, 0.4672634417),
      (0.4, 1.5, 0.4698323905),
    )
    forAll(cases) { (layerResistivity, distanceToLowerBoundary, expectedRes) =>
      val actual = calcThermalResistanceToSoilHorizontalLayerWithin(
        KelvinMetersPerWatt(0.7),
        KelvinMetersPerWatt(layerResistivity),
        Meters(distanceToLowerBoundary),
        Meters(0.7),
        Meters(0.04),
      )
      actual should approximate(KelvinMetersPerWatt(expectedRes))
    }
  }

  "reduce to the homogeneous single cable resistance for a cable within a horizontal layer with equal resistivities" in {
    val soilRes = KelvinMetersPerWatt(0.7)
    val cableDia = Meters(0.04)
    val distToLower = Meters(0.2)
    val distToSurface = Meters(0.7)
    val actual = calcThermalResistanceToSoilHorizontalLayerWithin(
      soilRes,
      soilRes,
      distToLower,
      distToSurface,
      cableDia,
    )
    val expected =
      calcThermalResistanceToSoilSingleCable(
        soilRes,
        distToSurface * -1,
        cableDia,
      )
    {
      // The image-source geometric factor ln(2*h/r_s) matches the Kennelly
      // factor ln(u + sqrt(u^2 - 1)) only up to O(1/u^2), so a looser
      // tolerance applies for the homogeneous limit.
      implicit val looseResistanceTolerance: ThermalResistivity =
        KelvinMetersPerWatt(1e-4)
      actual should approximate(expected)
    }
  }

  "throw an exception if the distance to the lower boundary or surface of the horizontal layer is not positive" in {
    an[IllegalArgumentException] should be thrownBy {
      calcThermalResistanceToSoilHorizontalLayerWithin(
        KelvinMetersPerWatt(0.7),
        KelvinMetersPerWatt(1.5),
        Meters(0.0),
        Meters(0.7),
        Meters(0.04),
      )
    }
  }

  "return the correct thermal resistance to soil for a cable near a vertical layer" in {
    val cases = Table(
      (
        "distanceToNearBoundary",
        "layerThickness",
        "expectedRes",
      ),
      // S&M 1992 Table 4: rs=0.02, h=0.7, rho1=0.7, rho2=1.5, rho3=0.4
      (0.02, 0.1, 0.5010635620),
      (0.02, 0.2, 0.5409317820),
      (0.02, 0.3, 0.5627517013),
      (0.02, 0.4, 0.5765432710),
      (0.02, 0.5, 0.5858877110), // Typo in paper? 0.5959
      (0.02, 0.6, 0.5924976260),
      (0.02, 1.0, 0.6058273706),
      (0.02, 1.5, 0.6116451608),
      (0.05, 0.1, 0.4776383827),
      (0.05, 0.2, 0.5109887210),
      (0.05, 0.3, 0.5301368332),
      (0.05, 0.4, 0.5424864615),
      (0.05, 0.5, 0.5509520678),
      (0.05, 0.6, 0.5569897756),
      (0.05, 1.0, 0.5693220588),
      (0.05, 1.5, 0.5748054463),
      (0.10, 0.1, 0.4671899576),
      (0.10, 0.2, 0.4930186452),
      (0.10, 0.3, 0.5086744425),
      (0.10, 0.4, 0.5190403719),
      (0.10, 0.5, 0.5262649537),
      (0.10, 0.6, 0.5314817999), // 0.5323
      (0.10, 1.0, 0.5423541880),
      (0.10, 1.5, 0.5473340217),
      (0.20, 0.1, 0.4636417492),
      (0.20, 0.2, 0.4805399334),
      (0.20, 0.3, 0.4914509113),
      (0.20, 0.4, 0.4989475696),
      (0.20, 0.5, 0.5043128021),
      (0.20, 0.6, 0.5082708430),
      (0.20, 1.0, 0.5168292496),
      (0.20, 1.5, 0.5209704962),
    )
    forAll(cases) { (distanceToNearBoundary, layerThickness, expectedRes) =>
      val actual = calcThermalResistanceToSoilVerticalLayerSingleCable(
        KelvinMetersPerWatt(0.7),
        KelvinMetersPerWatt(1.5),
        KelvinMetersPerWatt(0.4),
        Meters(-0.7),
        Meters(0.04),
        Meters(distanceToNearBoundary),
        Meters(layerThickness),
      )
      actual should approximate(KelvinMetersPerWatt(expectedRes))
    }
  }

  "reduce to the homogeneous single cable resistance for a cable near a vertical layer with equal resistivities" in {
    val soilRes = KelvinMetersPerWatt(0.7)
    val depth = Meters(-0.7)
    val cableDia = Meters(0.04)
    val actual = calcThermalResistanceToSoilVerticalLayerSingleCable(
      soilRes,
      soilRes,
      soilRes,
      depth,
      cableDia,
      Meters(0.05),
      Meters(0.3),
    )
    val expected =
      calcThermalResistanceToSoilSingleCable(
        soilRes,
        depth,
        cableDia,
      )
    {
      // The image-source geometric factor ln(2*h/r_s) matches the Kennelly
      // factor ln(u + sqrt(u^2 - 1)) only up to O(1/u^2), so a looser
      // tolerance applies for the homogeneous limit.
      implicit val looseResistanceTolerance: ThermalResistivity =
        KelvinMetersPerWatt(1e-4)
      actual should approximate(expected)
    }
  }

  "throw an exception if the distance to the vertical layer or the layer thickness is not positive" in {
    an[IllegalArgumentException] should be thrownBy {
      calcThermalResistanceToSoilVerticalLayerSingleCable(
        KelvinMetersPerWatt(0.7),
        KelvinMetersPerWatt(1.5),
        KelvinMetersPerWatt(0.4),
        Meters(-0.7),
        Meters(0.04),
        Meters(0.05),
        Meters(0.0),
      )
    }
  }
}
