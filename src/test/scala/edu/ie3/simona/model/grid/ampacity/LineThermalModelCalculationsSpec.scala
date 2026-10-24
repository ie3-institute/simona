/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.model.grid.ampacity.SoilTypeExtensions.*
import edu.ie3.simona.test.common.input.{
  CigreT880LandCable33kV,
  LineSegmentThermalModelInputData,
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import edu.ie3.util.scala.quantities.*

import squants.electro.*
import squants.thermal.{Celsius, Temperature}
import squants.time.Hertz
import squants.{Amperes, Meters}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.CELSIUS
import scala.math.sqrt

class LineThermalModelCalculationsSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData
    with DefaultTestData {

  implicit val resistanceTolerance: ThermalResistivity = KelvinMetersPerWatt(
    1e-10
  )
  implicit val electricResistancePerLengthTolerance
      : ElectricalResistancePerLength = OhmsPerMeter(1e-10)
  implicit val powerPerLengthTolerance: PowerPerLength = WattsPerMeter(1e-7)
  implicit val tolerance: Double = 1e-10
  implicit val thermalCapacitanceTolerance: ThermalCapacitance =
    JoulesPerCubicMeterKelvin(1e-10)
  implicit val thermalCapacitancePerLengthTolerance: ThermalCapacityPerLength =
    JoulesPerMeterKelvin(1e-10)
  implicit val temperatureTolerance: Temperature = Celsius(1e-3)

  "A LineSegmentThermalModel" should {

    "calculate correctly the AC Resistance" in {
      val cases = Table(
        (
          "resistancePerMeter",
          "temperatureCorrection",
          "limitTemp",
          "skinEffectCoefficient",
          "proximityEffectCoefficient",
          "expected",
        ),
        (0.0754e-3, 3.93e-3, 90.0, 1.0, 1.0,
          9.762868345e-5), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 197
        (0.122, 4.03e-3, 20.0, 1.0, 1.0,
          0.122000001195), // NA2XS2Y 1x240 RM/25 12/20 kV see Simbench line types, stays as 20°C
        (0.122, 4.03e-3, 90.0, 1.0, 1.0,
          0.15641620093), // NA2XS2Y 1x240 RM/25 12/20 kV see Simbench line types, at 90°C
      )

      forAll(cases) {
        (
            resistancePerMeter,
            temperatureCorrection,
            limitTemp,
            skinEffectCoefficient,
            proximityEffectCoefficient,
            expected,
        ) =>

          val cableResistance = OhmsPerMeter(resistancePerMeter)
          val limitTemperature = Celsius(limitTemp)
          val cableSetup = CigreT880LandCable33kV.cable.copy(
            limitTemperature = limitTemperature,
            skinEffectCoefficient = skinEffectCoefficient,
            proximityEffectCoefficient = proximityEffectCoefficient,
          )

          val expectedResult = OhmsPerMeter(expected)

          val actual = calcAcResistance(
            cableResistance,
            temperatureCorrection,
            cableSetup,
            Meters(0.044),
          )

          actual should approximate(expectedResult)
      }
    }

    "calculate correctly the conductor losses" in {
      val cases = Table(
        (
          "current",
          "acResistance",
          "expected",
        ),
        (100.0, 9.759631228772626e-5, 0.975963123),
        (10000d, 1.0e-5, 1000d), // high current
        (0.1, 1.0e-5, 1e-7), // small current
      )

      forAll(cases) {
        (
            currentInAmps,
            acResistance,
            expected,
        ) =>

          val current = Amperes(currentInAmps)
          val acRes = OhmsPerMeter(acResistance)
          val expectedResult = WattsPerMeter(expected)

          val actual = calcLossesConductor(acRes, current)

          actual should approximate(expectedResult)
      }
    }

    "calculate correctly the AC resistance of the screen" in {
      val cases = Table(
        (
          "limitTemperature",
          "thermalResistanceT1",
          "conductorLosses",
          "dielectricLosses",
          "expected",
        ),
        (90.0, 0.4110322351, 28.20166532, 0.10842143853,
          6.635010635e-4), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification", TB 880, p 199ff
      )

      forAll(cases) {
        (
            limitTemperature,
            thermalResistanceT1,
            conductorLosses,
            dielectricLosses,
            expected,
        ) =>

          val screenLayer = CigreT880LandCable33kV.cable.screenLayer
          val limitTemp = Celsius(limitTemperature)
          val t1 = KelvinMetersPerWatt(thermalResistanceT1)
          val wC = WattsPerMeter(conductorLosses)
          val wD = WattsPerMeter(dielectricLosses)
          val expectedResult = OhmsPerMeter(expected)

          val actual = calcAcResistanceSheath(
            screenLayer,
            limitTemp,
            t1,
            wC,
            wD,
          )

          actual should approximate(expectedResult)
      }
    }

    "calculate correctly the dielectric losses" in {
      val cases = Table(
        (
          "phaseToGroundVoltage",
          "tanDelta",
          "dielectricCapaNanoF",
          "expected",
        ),
        (
          19052.5588,
          0.004,
          0.237683304,
          0.10842143853,
        ), // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 198f
        (
          76210.2355,
          0.001,
          0.25506991358,
          0.4654100053,
        ), // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 132f
        (
          230940.108,
          0.001,
          0.14978009691,
          2.5095896112,
        ), // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 242f
      )

      forAll(cases) {
        (
            phaseToGroundVoltage,
            tanDelta,
            dielectricCapaNanoF,
            expected,
        ) =>

          val voltageU0 = Volts(phaseToGroundVoltage)
          val frequency = Hertz(50)
          val dielectricSpecificCapacity =
            edu.ie3.util.scala.quantities
              .FaradsPerMeter(dielectricCapaNanoF * 1e-9)
          val expectedResult = WattsPerMeter(expected)

          val actual = calcDielectricLosses(
            voltageU0,
            frequency,
            tanDelta,
            dielectricSpecificCapacity,
          )

          actual should approximate(expectedResult)
      }
    }
    /*"throw an exception for unsupported material for calculation of the dielectric losses" in {
      val ex = intercept[IllegalArgumentException] {
        calcDielectricLosses(
          "PE",
          Kilovolts(6),
          Hertz(50),
          0.001,
          Nanofarads(0.1),
        )
      }
      ex.getMessage should be("Unknown material used for dielectric: PE.")
    }

     */

    "return all correct thermal resistance for cable shells" in {

      val cases = Table(
        (
          "specificThermalResistivity",
          "innerRadius",
          "outerRadius",
          "thermalResistance",
        ),
        (
          3.5,
          0.0205 / 2,
          0.0205 / 2 + 0.0034 + 0.0008 + 0.00065,
          0.21580767835674133,
        ), // Anders Cable No 1 T1 (should be 0.214)
        (
          5.0,
          0.0314 / 2,
          0.0314 / 2 + 0.0022,
          0.10435789657723259,
        ), // Anders Cable No 1 T3
        (
          6.0,
          0.04145 / 2,
          0.04145 / 2 + 0.01283,
          0.46012825568418825,
        ), // Anders Cable No 3 T1
        (
          2.5,
          0.0184 / 2,
          0.0194 / 2,
          0.02105715448,
        ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_SC
        (
          3.5,
          0.0194 / 2,
          0.0348 / 2,
          0.3255045049,
        ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_I
        (
          2.5,
          0.0348 / 2,
          0.0358 / 2,
          0.011272350425,
        ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_SI
        (
          6.0,
          0.0358 / 2,
          0.0368 / 2,
          0.02630826604,
        ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 202 T1_UWBT
        (
          6.0,
          0.0386 / 2,
          0.0392 / 2,
          0.014729284181,
        ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 203 T3_TOSW
        (
          3.5,
          0.0392 / 2,
          0.0436 / 2,
          0.05925838476,
        ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 203 T3_OC
        (
          2.5,
          0.0436 / 2,
          0.044 / 2,
          0.003633699755,
        ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 203 T3_OC_SC
      )

      forAll(cases) {
        (
            specificThermalResistivity,
            innerRadius,
            outerRadius,
            thermalResistance,
        ) =>
          val specTherRes = KelvinMetersPerWatt(specificThermalResistivity)
          val inRadius = Meters(innerRadius)
          val outRadius = Meters(outerRadius)
          val expectedThermalResistance = KelvinMetersPerWatt(thermalResistance)

          val actual =
            calcThermalResistanceCableShells(specTherRes, inRadius, outRadius)

          actual should approximate(expectedThermalResistance)
      }
    }

    "calculate correctly the losses in cable screen" in {
      val cases = Table(
        (
          "layoutFormation",
          "conductorResistance",
          "screenResistance",
          "axialDistanceCables",
          "averageSheathDiameter",
          "phase",
          "eddyCurrentsSheathLossFactor",
          "conductorLosses",
          "expectedSheathLosses",
        ),
        (
          "trefoil-touching",
          9.762868345e-5,
          6.635010635e-4,
          0.044,
          (0.0368 + 0.0386) / 2,
          "middle",
          0.0,
          28.20166532,
          1.227118334845,
        ), // Cigre TB880 p. 200
        (
          "flat-distance",
          0.0781e-3,
          0.94692081e-3, // see Anders 1997 Example 7.5
          2 * 0.0358,
          0.0312, // see Anders 1997 Example 8.2 p. 159
          "middle",
          0.0,
          30.85,
          2.7321981418997779, // (lambda1m'=0.088) * 30.85W see Anders 1997 p. 160
        ), // Anders 2005 Cable No.1 p. 311
        (
          "flat-distance",
          0.0781e-3,
          0.94692081e-3, // see Anders 1997 Example 7.5
          2 * 0.0358,
          0.0312, // see Anders 1997 Example 8.2 p. 159
          "leading",
          0.0,
          30.85,
          6.381502194, // (lambda1m'=0.206) * 30.85W see Anders 1997 p. 160
        ), // Anders 2005 Cable No.1 p. 311
        (
          "flat-distance",
          0.0781e-3,
          0.94692081e-3, // see Anders 1997 Example 7.5
          2 * 0.0358,
          0.0312, // see Anders 1997 Example 8.2 p. 159
          "lagging",
          0.0,
          30.85,
          6.868629568, // (lambda1m'=0.222) * 30.85W see Anders 1997 p. 160
        ), // Anders 2005 Cable No.1 p. 311
      )

      forAll(cases) {
        (
            layoutFormation,
            conductorResist,
            screenResist,
            axialDistance,
            averageSheathDia,
            phase,
            eddyCurrentsSheathLossFactor,
            conductorLosses,
            expectedSheathLosses,
        ) =>
          val conductorResistance = OhmsPerMeter(conductorResist)
          val screenResistance = OhmsPerMeter(screenResist)
          val axialDistanceCables = Meters(axialDistance)
          val averageSheathDiameter = Meters(averageSheathDia)
          val conductorLossesW = WattsPerMeter(conductorLosses)
          val expectedLosses = WattsPerMeter(expectedSheathLosses)

          val actual = calcLossesSheath(
            layoutFormation,
            conductorResistance,
            screenResistance,
            axialDistanceCables,
            averageSheathDiameter,
            phase,
            eddyCurrentsSheathLossFactor,
            conductorLossesW,
          )

          actual should approximate(expectedLosses)
      }
    }

    "calculate correctly the losses in cable armor" in {
      val cases = Table(
        (
          "circulatingArmorLossFactor",
          "eddyCurrentsArmorLossFactor",
          "conductorLosses",
          "expectedArmorLosses",
        ),
        (0.02, 0.03, 100.0, 5.0),
        (0.1, 0.1, 50.0, 10.0),
        (0.0, 0.0, 100.0, 0.0),
        (0.15, 0.25, 200.0, 80.0),
      )

      forAll(cases) {
        (
            circulatingArmorLossFactor,
            eddyCurrentsArmorLossFactor,
            conductorLosses,
            expectedArmorLosses,
        ) =>
          val conductorLossesW = WattsPerMeter(conductorLosses)
          val expectedLosses = WattsPerMeter(expectedArmorLosses)

          val actual = calcLossesArmor(
            circulatingArmorLossFactor,
            eddyCurrentsArmorLossFactor,
            conductorLossesW,
          )

          actual should approximate(expectedLosses)
      }
    }

    "calculate correctly the thermal capacity of cylindrical layers" in {
      val cases = Table(
        (
          "specificThermalCapacity",
          "innerDiameter",
          "outerDiameter",
          "expectedThermalCapacitance",
        ),
        (1000.0, 0.01, 0.02, 0.2356194490192345),
        (2000.0, 0.0, 0.01, 0.15707963267948966), // small inner diameter
        (1500.0, 0.02, 0.03, 0.5890486225480862),
      )

      forAll(cases) {
        (
            specificThermalCapacity,
            innerDiameter,
            outerDiameter,
            expectedThermalCapacitance,
        ) =>
          val specThermalCap =
            JoulesPerCubicMeterKelvin(specificThermalCapacity)
          val innerDia = Meters(innerDiameter)
          val outerDia = Meters(outerDiameter)
          val expectedCapacitance =
            JoulesPerMeterKelvin(expectedThermalCapacitance)

          val actual = calcThermalCapacityCylindrical(
            specThermalCap,
            innerDia,
            outerDia,
          )

          actual should approximate(expectedCapacitance)
      }
    }

    "calculate correctly the thermal capacity of rectangular layers" in {
      val actual = calcThermalCapacityRectangular(
        JoulesPerCubicMeterKelvin(1.5e6),
        Meters(0.6),
        Meters(0.4),
      )

      actual should approximate(JoulesPerMeterKelvin(360000))
    }

    "calculate correctly the cable bed equivalent radius" in {
      val cases = Table(
        ("width", "height", "expected"),
        (0.6, 0.4, 0.2538213697753768),
        (1.0, 1.0, 0.5496631897919626),
        (0.5, 1.0, 0.3412383400401008),
      ) // TODO DF check values

      forAll(cases) { (width, height, expected) =>
        val actual = calcCableBedEquivalentRadius(
          Meters(width),
          Meters(height),
        )
        actual.toMeters should approximate(expected)
      }
    }

    "reject cable bed equivalent radius for aspect ratio >= 3" in {
      an[IllegalArgumentException] should be thrownBy {
        calcCableBedEquivalentRadius(Meters(1.0), Meters(3.0))
      }
      an[IllegalArgumentException] should be thrownBy {
        calcCableBedEquivalentRadius(Meters(3.0), Meters(1.0))
      }
    }

    "calculate correctly the cable bed T4 outer correction" in {
      val cases = Table(
        (
          "soilResistivity",
          "bedResistivity",
          "bedCenterDepth",
          "bedRadius",
          "lossWeightFactor",
          "expected",
        ),
        (1.0, 1.0, 1.0, 0.2, 3.0, 0.0),
        (2.5, 1.0, 1.0, 0.2, 3.0, 1.6418332435998053),
        (0.5, 1.0, 1.0, 0.2, 3.0, -0.5472777478666018),
        (2.5, 1.0, 1.0, 0.2, 2.5, 1.3681943696665044),
      ) // TODO DF check values

      forAll(cases) {
        (
            soilResistivity,
            bedResistivity,
            bedCenterDepth,
            bedRadius,
            lossWeightFactor,
            expected,
        ) =>
          val soilRes = KelvinMetersPerWatt(soilResistivity)
          val bedRes = KelvinMetersPerWatt(bedResistivity)
          val depth = Meters(bedCenterDepth)
          val radius = Meters(bedRadius)
          val expectedCorrection = KelvinMetersPerWatt(expected)

          val actual = calcCableBedT4OuterCorrection(
            soilRes,
            bedRes,
            depth,
            radius,
            lossWeightFactor,
          )

          actual should approximate(expectedCorrection)
      }
    }

    "calculate correctly the Van-Wormer-Coefficient for long-duration transients" in {
      val cases = Table(
        (
          "diameterDielectric",
          "diameterConductor",
          "expectedCoefficient",
        ),
        (0.04, 0.02, 0.3880141871111484),
        (0.1, 0.05, 0.3880141871111484),
        (0.1, 0.01, 0.20704623085061577), // large diameter ratio
        (0.1, 0.1001, 0.5001665833820539), // small diameter ratio
      )

      forAll(cases) {
        (
            diameterDielectric,
            diameterConductor,
            expectedCoefficient,
        ) =>
          val diamDielectric = Meters(diameterDielectric)
          val diamConductor = Meters(diameterConductor)

          val actual = vanWormerCoefficientLongDurationTransients(
            diamDielectric,
            diamConductor,
          )

          actual should approximate(expectedCoefficient)
      }
    }

    "calculate correctly the Van-Wormer-Coefficient for short-duration transients" in {
      val cases = Table(
        (
          "diameterDielectric",
          "diameterConductor",
          "expectedCoefficient",
        ),
        (0.04, 0.02, 0.4426950408889634),
        (0.1, 0.05, 0.4426950408889634),
        (0.1, 0.01, 0.32318337079214066), // large diameter ratio
        (0.1, 0.1001, 0.5000832916929312), // small diameter ratio
      )

      forAll(cases) {
        (
            diameterDielectric,
            diameterConductor,
            expectedCoefficient,
        ) =>
          val diamDielectric = Meters(diameterDielectric)
          val diamConductor = Meters(diameterConductor)

          val actual = vanWormerCoefficientShortTermDurationTransients(
            diamConductor,
            diamDielectric,
          )

          actual should approximate(expectedCoefficient)
      }
    }

    "calculate correctly the geometric factor for the Kennelly method" in {

      val cases = Table(
        (
          "depthCable",
          "cableDiameter",
          "expectedResult",
        ),
        (-1.0, 1.0, 1.31695789692),
        (-1.0, 0.0358, 4.716021634569044),
        (-0.7, 0.044, 4.15293803195116),
        (-0.1, 0.01, 3.6882538673612966), // small depth
        (-2.0, 0.01, 6.6846101651642655), // large diameter ratio
      )

      forAll(cases) {
        (
            depthCable,
            cableDiameter,
            expectedResult,
        ) =>

          val depth = Meters(depthCable)
          val cableDia = Meters(cableDiameter)

          val actual = calcGeometricFactor(depth, cableDia)

          actual should approximate(expectedResult)
      }
    }
  }

  "return all correct thermal resistance for T4 / soil for a single buried cable (e.g. three core cable, or cable in ducts)" in {
    val cases = Table(
      (
        "specificThermalResistivity",
        "depthCable",
        "cableDiameter",
        "thermalResistance",
      ),
      (
        1.0,
        -1.0,
        0.0729,
        0.6373564504421266,
      ), // Anders 1997 Cable No 2 T4 p. 215
      (
        1.0,
        -1.0,
        0.055,
        0.6822220147,
      ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 323
    )

    forAll(cases) {
      (
          specificThermalResistivity,
          depthCable,
          cableDiameter,
          thermalResistance,
      ) =>
        val specTherRes = KelvinMetersPerWatt(specificThermalResistivity)
        val depth = Meters(depthCable)
        val cableDia = Meters(cableDiameter)
        val expectedThermalResistance = KelvinMetersPerWatt(thermalResistance)

        val actual =
          calcThermalResistanceToSoilSingleCable(specTherRes, depth, cableDia)

        actual should approximate(expectedThermalResistance)
    }
  }

  "return all correct thermal resistance for T4 / soil for three single core cables in flat formation" in {

    val cases = Table(
      (
        "specificThermalResistivitySoil",
        "depthCables",
        "diameterCableB",
        "distanceOfCables",
        "lossesCableA",
        "lossesCableB",
        "lossesCableC",
        "expectedRes",
      ),
      (1.0, -1.0, 0.098, 0.5, 47.0866497321, 47.5422569285, 46.8820830331,
        1.0358516741), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification", TB 880, p 161 T4
      (1.0, -1.0, 0.0358, 0.0358, 31.0, 33.59, 31.0,
        1.9324332635), // Anders 2005 p. 310-311
    )

    forAll(cases) {
      (
          specificThermalResistivity,
          depthCables,
          diameterCableB,
          distanceOfCables,
          lossesCableA,
          lossesCableB,
          lossesCableC,
          expectedRes,
      ) =>
        val specTherRes = KelvinMetersPerWatt(specificThermalResistivity)
        val depth = Meters(depthCables)
        val diameterB = Meters(diameterCableB)
        val distance = Meters(distanceOfCables)
        val lossA = WattsPerMeter(lossesCableA)
        val lossB = WattsPerMeter(lossesCableB)
        val lossC = WattsPerMeter(lossesCableC)
        val expectedThermalResistance = KelvinMetersPerWatt(expectedRes)

        val actual =
          calcThermalResistanceToSoilThreeSingleCoreFlatFormationDistance(
            specTherRes,
            depth,
            diameterB,
            distance,
            lossA,
            lossB,
            lossC,
          )

        actual should approximate(expectedThermalResistance)
    }
  }
  "return correct thermal resistance for three single-core cables in flat-touching formation" in {
    val actual = LineThermalModelCalculations
      .calcThermalResistanceToSoilThreeSingleCoreFlatFormationTouching(
        KelvinMetersPerWatt(1.0),
        Meters(-1.0),
        Meters(0.044),
      )

    actual should approximate(
      KelvinMetersPerWatt(1.7961835029372892)
    ) // TODO DF add reference, Anders?
  }
  "return all correct thermal resistance for T4 / soil for three single core cables in trefoil touching formation" in {

    val cases = Table(
      (
        "specificThermalResistivitySoil",
        "depthCables",
        "diameterCable",
        "result",
      ),
      (
        1.0,
        -1.0,
        0.044,
        1.8524966955,
      ), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 161 T4
    )

    forAll(cases) {
      (
          specificThermalResistivity,
          depthCables,
          diameterCable,
          expectedRes,
      ) =>
        val specTherRes = KelvinMetersPerWatt(specificThermalResistivity)
        val depth = Meters(depthCables)
        val diameter = Meters(diameterCable)
        val expectedThermalResistance = KelvinMetersPerWatt(expectedRes)

        val actual = calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching(
          specTherRes,
          depth,
          diameter,
        )

        actual should approximate(expectedThermalResistance)
    }
  }

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

  "test " in {

    val expected = Celsius(90) // S. 205 CIGRE
    val tick = 972000L

    val groundTemp = Celsius(20)

    val startingState =
      LineSegmentThermalModel.initState(
        cigreT880LandCable33kVCableSetup,
        cigreLandCable33kVLineSegmentThermalModel,
        groundTemp,
      )

    val current = Amperes(537.46)

    val actual =
      createAndCalcRCNetworkMvCableShortDuration(
        tick,
        startingState,
        current,
        groundTemp,
      )

    actual.currentLineTemp1 should approximate(expected)
  }

  "use the current cable voltage for dielectric losses in the RC network" in {
    val tick = 972000L
    val groundTemperature = Celsius(20)
    val initialState = LineSegmentThermalModel.initState(
      cigreT880LandCable33kVCableSetup,
      cigreLandCable33kVLineSegmentThermalModel,
      groundTemperature,
    )
    val expected = Celsius(20.94639208923625)

    val temperatureAtNominalVoltage =
      createAndCalcRCNetworkMvCableShortDuration(
        tick,
        initialState.copy(currentVoltage = Kilovolts(33)),
        Amperes(0),
        groundTemperature,
      )
    val temperatureAtDoubleVoltage =
      createAndCalcRCNetworkMvCableShortDuration(
        tick,
        initialState.copy(currentVoltage = Kilovolts(66)),
        Amperes(0),
        groundTemperature,
      )

    temperatureAtDoubleVoltage.currentLineTemp1 should be >
      temperatureAtNominalVoltage.currentLineTemp1

    temperatureAtDoubleVoltage.currentLineTemp1 should approximate(expected)
  }

  "allocate cable-bed environmental cells by material and volume" in { // TODO DF needs to be reviewed
    val cableRadius = Meters(0.05)
    val bedRadius = Meters(0.25)
    val soilOuterRadius = Meters(1.5)
    val bedResistance = KelvinMetersPerWatt(0.4)
    val soilResistance = KelvinMetersPerWatt(0.9)
    val bedHeatCapacity = JoulesPerCubicMeterKelvin(1.5e6)
    val soilHeatCapacity = JoulesPerCubicMeterKelvin(2.0e6)

    val cells = calcCableBedEnvironmentalCells(
      bedResistance,
      soilResistance,
      cableRadius,
      bedRadius,
      soilOuterRadius,
      bedHeatCapacity,
      soilHeatCapacity,
    )

    cells should have size 5
    cells
      .take(2)
      .map(_.resistance.toKelvinMetersPerWatt)
      .sum should approximate(
      bedResistance.toKelvinMetersPerWatt
    )
    cells
      .drop(2)
      .map(_.resistance.toKelvinMetersPerWatt)
      .sum should approximate(
      soilResistance.toKelvinMetersPerWatt
    )
    cells
      .take(2)
      .map(_.thermalCapacity.toJoulesPerMeterKelvin)
      .sum should approximate(
      math.Pi * (bedRadius.toMeters * bedRadius.toMeters - cableRadius.toMeters * cableRadius.toMeters) * bedHeatCapacity.toJoulesPerCubicMeterKelvin
    )
    cells
      .drop(2)
      .map(_.thermalCapacity.toJoulesPerMeterKelvin)
      .sum should approximate(
      math.Pi * (soilOuterRadius.toMeters * soilOuterRadius.toMeters - bedRadius.toMeters * bedRadius.toMeters) * soilHeatCapacity.toJoulesPerCubicMeterKelvin
    )
  }

  "preserve stationary T4 temperatures when bedding and outer soil match" in { // TODO DF needs to be reviewed
    val tick = 100000000L
    val laterTick = tick * 2
    val groundTemperature = Celsius(20)
    val bed = LineSegmentThermalModel.CableBed(
      Meters(0.6),
      Meters(0.4),
      defaultSoilType,
    )
    val modelWithoutBed = cigreLandCable33kVLineSegmentThermalModel.copy(
      cableBed = None
    )
    val modelWithBed = modelWithoutBed.copy(cableBed = Some(bed))
    val stateWithoutBed = LineSegmentThermalModel.initState(
      cigreT880LandCable33kVCableSetup,
      modelWithoutBed,
      groundTemperature,
    )
    val stateWithBed = LineSegmentThermalModel.initState(
      cigreT880LandCable33kVCableSetup,
      modelWithBed,
      groundTemperature,
    )

    val temperatureWithoutBed = createAndCalcRCNetworkMvCableShortDuration(
      tick,
      stateWithoutBed,
      Amperes(537.46),
      groundTemperature,
    )
    val temperatureWithBed = createAndCalcRCNetworkMvCableShortDuration(
      tick,
      stateWithBed,
      Amperes(537.46),
      groundTemperature,
    )
    val laterTemperatureWithBed = createAndCalcRCNetworkMvCableShortDuration(
      laterTick,
      stateWithBed,
      Amperes(537.46),
      groundTemperature,
    )

    temperatureWithBed.currentLineTemp5 should approximate(
      temperatureWithoutBed.currentLineTemp5
    )
    val temperaturesAtTick = Seq(
      temperatureWithBed.currentLineTemp1,
      temperatureWithBed.currentLineTemp2,
      temperatureWithBed.currentLineTemp3,
      temperatureWithBed.currentLineTemp4,
      temperatureWithBed.currentLineTemp5,
      temperatureWithBed.currentLineTemp6,
      temperatureWithBed.currentLineTemp7,
      temperatureWithBed.currentLineTemp8,
      temperatureWithBed.currentLineTemp9,
      temperatureWithBed.currentLineTemp10,
    )
    val temperaturesAtLaterTick = Seq(
      laterTemperatureWithBed.currentLineTemp1,
      laterTemperatureWithBed.currentLineTemp2,
      laterTemperatureWithBed.currentLineTemp3,
      laterTemperatureWithBed.currentLineTemp4,
      laterTemperatureWithBed.currentLineTemp5,
      laterTemperatureWithBed.currentLineTemp6,
      laterTemperatureWithBed.currentLineTemp7,
      laterTemperatureWithBed.currentLineTemp8,
      laterTemperatureWithBed.currentLineTemp9,
      laterTemperatureWithBed.currentLineTemp10,
    )
    temperaturesAtTick.zip(temperaturesAtLaterTick).foreach {
      case (temperature, laterTemperature) =>
        laterTemperature should approximate(temperature)
    }
    Seq(
      temperatureWithBed.currentLineTemp6,
      temperatureWithBed.currentLineTemp7,
      temperatureWithBed.currentLineTemp8,
      temperatureWithBed.currentLineTemp9,
      temperatureWithBed.currentLineTemp10,
    ).foreach(_ should not be Celsius(0))
  }
  "match the independent T4 resistance in the stationary cable-bed network" in { // TODO DF needs to be reviewed
    val tick = 100000000L
    val groundTemperature = Celsius(20)
    val current = Amperes(537.46)
    val bed = LineSegmentThermalModel.CableBed(
      Meters(0.6),
      Meters(0.4),
      defaultSoilType,
    )
    val modelWithBed =
      cigreLandCable33kVLineSegmentThermalModel.copy(cableBed = Some(bed))
    val stateWithBed = LineSegmentThermalModel.initState(
      cigreT880LandCable33kVCableSetup,
      modelWithBed,
      groundTemperature,
    )
    val cableSetup = cigreT880LandCable33kVCableSetup
    val cableOuterDiameter =
      cableSetup.layersJackElements.map(_.outerDiameter).maxOption match {
        case Some(diameter) => diameter
        case None =>
          fail("Jack layer expected for environmental resistance calculation")
      }
    val screenLayer = cableSetup.screenLayer match {
      case Some(layer) => layer
      case None => fail("Screen layer expected for sheath loss calculation")
    }

    val stationary = createAndCalcRCNetworkMvCableShortDuration(
      tick,
      stateWithBed,
      current,
      groundTemperature,
    )

    val temperatureDifference = Quantities.getQuantity(
      (stateWithBed.lineTemperatures.currentLineTemp4 - groundTemperature).toCelsiusDegrees,
      CELSIUS,
    )
    val soilResistivity =
      defaultSoilType.currentThermalResistivity(temperatureDifference).toSquants
    val bedResistivity =
      bed.soilType.currentThermalResistivity(temperatureDifference).toSquants
    val bedRadius = calcCableBedEquivalentRadius(bed.width, bed.height)
    val bedCenterDepth =
      Meters(-cigreLandCable33kVLineSegmentThermalModel.depthCables.toMeters)
    val t4Base = calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching(
      bedResistivity,
      cigreLandCable33kVLineSegmentThermalModel.depthCables,
      cableOuterDiameter,
    )
    val (bedResistance, soilResistance) = calcCableBedT4Split(
      t4Base,
      bedResistivity,
      soilResistivity,
      bedCenterDepth,
      bedRadius,
      3.0,
    )
    val cableOuterRadius = cableOuterDiameter / 2d
    val soilOuterRadius = calcSoilModelOuterRadius(bedCenterDepth, bedRadius)
    val cells = calcCableBedEnvironmentalCells(
      bedResistance,
      soilResistance,
      cableOuterRadius,
      bedRadius,
      soilOuterRadius,
      bed.soilType.getSpecificHeatCapacity.toSquants,
      defaultSoilType.getSpecificHeatCapacity.toSquants,
    )

    val phaseToGroundVoltage =
      Volts(stateWithBed.currentVoltage.toVolts / sqrt(3))
    val acResistance = calcAcResistance(
      cableSetup.electricResistance,
      cableSetup.conductor.material.getElectricalResistivityTemperatureCoefficient,
      cableSetup,
      cigreLandCable33kVLineSegmentThermalModel.distanceCables,
    )
    val conductorLosses = calcLossesConductor(acResistance, current)
    val dielectricLosses = calcDielectricLosses(
      phaseToGroundVoltage,
      cableSetup.frequency,
      cableSetup.tanDelta,
      cableSetup.electricCapacitance,
    )
    val acResistanceSheath = calcAcResistanceSheath(
      Some(screenLayer),
      cableSetup.limitTemperature,
      stateWithBed.currentLineSegmentThermalModel.thermalResistanceT1,
      conductorLosses,
      dielectricLosses,
    )
    val sheathAverageDiameter =
      (screenLayer.innerDiameter + screenLayer.outerDiameter) / 2
    val sheathLosses = Seq("leading", "middle", "lagging").map { phase =>
      calcLossesSheath(
        cableSetup.layoutFormation,
        acResistance,
        acResistanceSheath,
        cigreLandCable33kVLineSegmentThermalModel.distanceCables,
        sheathAverageDiameter,
        phase,
        cableSetup.eddyCurrentsLossFactorScreen,
        conductorLosses,
      )
    }
    val lossesPerCable =
      conductorLosses + dielectricLosses + sheathLosses(1)
    val totalEnvironmentalResistance =
      bedResistance + soilResistance

    val independentT4 = t4Base + calcCableBedT4OuterCorrection(
      soilResistivity,
      bedResistivity,
      bedCenterDepth,
      bedRadius,
      3.0,
    )
    totalEnvironmentalResistance should approximate(independentT4)
    cells.map(_.resistance.toKelvinMetersPerWatt).sum should approximate(
      totalEnvironmentalResistance.toKelvinMetersPerWatt
    )

    val expectedCableSurfaceTemperature =
      groundTemperature + Celsius(
        lossesPerCable.toWattsPerMeter * totalEnvironmentalResistance.toKelvinMetersPerWatt
      )
    stationary.currentLineTemp5 should approximate(
      expectedCableSurfaceTemperature
    )
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
