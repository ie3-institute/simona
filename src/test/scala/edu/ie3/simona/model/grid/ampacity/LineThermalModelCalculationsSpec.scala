/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.datamodel.models.input.SoilType
import edu.ie3.simona.model.grid.ampacity.CableLossCalculations.*
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.model.grid.ampacity.RCNetworkCalculations.*
import edu.ie3.simona.model.grid.ampacity.SoilThermalResistanceCalculations.*
import edu.ie3.simona.model.grid.ampacity.SoilThermalResistanceCableGroup.*
import edu.ie3.simona.model.grid.ampacity.SoilThermalResistanceSingleCable.*
import edu.ie3.simona.model.grid.ampacity.SoilTypeExtensions.*
import edu.ie3.simona.test.common.input.{
  CigreT880LandCable33kV,
  LineSegmentThermalModelInputData,
}
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.quantities.PowerSystemUnits.{
  JOULE_PER_CUBIC_METRE_KELVIN,
  KELVIN_METRE_PER_WATT,
}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import edu.ie3.util.quantities.electro.{OhmsPerMeter, SpecificResistance}
import edu.ie3.util.quantities.energy.{PowerPerLength, WattsPerMeter}
import edu.ie3.util.quantities.thermal.{
  JoulesPerCubicMeterKelvin,
  KelvinMetersPerWatt,
  ThermalCapacitance,
  ThermalResistivity,
}
import edu.ie3.util.scala.quantities.*

import squants.electro.*
import squants.thermal.{Celsius, Temperature}
import squants.time.Hertz
import squants.{Amperes, Meters}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.CELSIUS
import scala.math.sqrt

import java.util.UUID

class LineThermalModelCalculationsSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData
    with DefaultTestData {

  implicit val resistanceTolerance: ThermalResistivity = KelvinMetersPerWatt(
    1e-10
  )
  implicit val electricResistancePerLengthTolerance: SpecificResistance =
    OhmsPerMeter(1e-10)
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
          LayoutFormation.TrefoilTouching,
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
          LayoutFormation.FlatDistance,
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
          LayoutFormation.FlatDistance,
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
          LayoutFormation.FlatDistance,
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
    val actual = SoilThermalResistanceCalculations
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
      cableSetup.specificElectricResistance,
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

  "return None from the non-uniform soil T4 when fewer than two layers are present" in {
    val singleLayer = LineSegmentThermalModel.SoilLayerInfo(
      defaultSoilType,
      Meters(0.0),
      Meters(-1.0),
    )
    val temperatureDifference = Quantities.getQuantity(20.0, CELSIUS)
    calcThermalResistanceToSoilNonUniform(
      LayoutFormation.FlatDistance,
      Meters(-0.5),
      Meters(0.044),
      Meters(0.098),
      WattsPerMeter(30.0),
      WattsPerMeter(33.59),
      WattsPerMeter(31.0),
      Seq(singleLayer),
      temperatureDifference,
    ) should be(None)

    calcThermalResistanceToSoilNonUniform(
      LayoutFormation.FlatDistance,
      Meters(-0.5),
      Meters(0.044),
      Meters(0.098),
      WattsPerMeter(30.0),
      WattsPerMeter(33.59),
      WattsPerMeter(31.0),
      Seq.empty,
      temperatureDifference,
    ) should be(None)
  }

  "calculate the non-uniform soil T4 for a cable within the topmost layer" in {
    val topSoilType = new SoilType(
      UUID.randomUUID(),
      "topSoil",
      Quantities.getQuantity(1.0, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(2.5, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(1.0, JOULE_PER_CUBIC_METRE_KELVIN),
      Quantities.getQuantity(15.0, CELSIUS),
    )
    val belowSoilType = new SoilType(
      UUID.randomUUID(),
      "belowSoil",
      Quantities.getQuantity(2.0, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(1.5, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(1.0, JOULE_PER_CUBIC_METRE_KELVIN),
      Quantities.getQuantity(15.0, CELSIUS),
    )
    val soilLayers = Seq(
      LineSegmentThermalModel.SoilLayerInfo(
        topSoilType,
        Meters(0.0),
        Meters(-0.7),
      ),
      LineSegmentThermalModel.SoilLayerInfo(
        belowSoilType,
        Meters(-0.7),
        Meters(-2.0),
      ),
    )
    val temperatureDifference = Quantities.getQuantity(20.0, CELSIUS)
    val topResistivity =
      topSoilType.currentThermalResistivity(temperatureDifference).toSquants
    val belowResistivity =
      belowSoilType.currentThermalResistivity(temperatureDifference).toSquants
    val depth = Meters(-0.5)
    val diameter = Meters(0.044)
    val distance = Meters(0.098)
    val lossLeading = WattsPerMeter(30.0)
    val lossMiddle = WattsPerMeter(33.59)
    val lossLagging = WattsPerMeter(31.0)

    val expected = calcThermalResistanceToSoilHorizontalLayerWithin(
      topResistivity,
      belowResistivity,
      Meters(0.2),
      Meters(0.5),
      diameter,
    ) + calcThermalResistanceMutualHorizontalLayerWithin(
      topResistivity,
      belowResistivity,
      Meters(0.2),
      Meters(0.5),
      distance,
    ) * ((lossLeading + lossLagging) / lossMiddle)

    val actual = calcThermalResistanceToSoilNonUniform(
      LayoutFormation.FlatDistance,
      depth,
      diameter,
      distance,
      lossLeading,
      lossMiddle,
      lossLagging,
      soilLayers,
      temperatureDifference,
    )

    actual match {
      case Some(value) => value should approximate(expected)
      case None        => fail("expected Some(non-uniform T4) but got None")
    }
  }

  "calculate the non-uniform soil T4 for a cable beneath the topmost layer" in {
    val topSoilType = new SoilType(
      UUID.randomUUID(),
      "topSoil",
      Quantities.getQuantity(1.0, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(2.5, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(1.0, JOULE_PER_CUBIC_METRE_KELVIN),
      Quantities.getQuantity(15.0, CELSIUS),
    )
    val belowSoilType = new SoilType(
      UUID.randomUUID(),
      "belowSoil",
      Quantities.getQuantity(2.0, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(1.5, KELVIN_METRE_PER_WATT),
      Quantities.getQuantity(1.0, JOULE_PER_CUBIC_METRE_KELVIN),
      Quantities.getQuantity(15.0, CELSIUS),
    )
    val soilLayers = Seq(
      LineSegmentThermalModel.SoilLayerInfo(
        topSoilType,
        Meters(0.0),
        Meters(-0.4),
      ),
      LineSegmentThermalModel.SoilLayerInfo(
        belowSoilType,
        Meters(-0.4),
        Meters(-2.0),
      ),
    )
    val temperatureDifference = Quantities.getQuantity(20.0, CELSIUS)
    val topResistivity =
      topSoilType.currentThermalResistivity(temperatureDifference).toSquants
    val belowResistivity =
      belowSoilType.currentThermalResistivity(temperatureDifference).toSquants
    val diameter = Meters(0.044)
    val distance = Meters(0.098)
    val lossLeading = WattsPerMeter(30.0)
    val lossMiddle = WattsPerMeter(33.59)
    val lossLagging = WattsPerMeter(31.0)

    val expected = calcThermalResistanceToSoilHorizontalLayerBeneath(
      belowResistivity,
      topResistivity,
      Meters(-0.6),
      diameter,
      Meters(0.4),
    ) + calcThermalResistanceMutualHorizontalLayerBeneath(
      belowResistivity,
      topResistivity,
      Meters(0.6),
      distance,
      Meters(0.2),
      Meters(0.4),
    ) * ((lossLeading + lossLagging) / lossMiddle)

    val actual = calcThermalResistanceToSoilNonUniform(
      LayoutFormation.FlatDistance,
      Meters(-0.6),
      diameter,
      distance,
      lossLeading,
      lossMiddle,
      lossLagging,
      soilLayers,
      temperatureDifference,
    )

    actual match {
      case Some(value) => value should approximate(expected)
      case None        => fail("expected Some(non-uniform T4) but got None")
    }
  }
}
