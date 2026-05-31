/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.{
  CigreT880LandCable33kV,
  LineSegmentThermalModelInputData,
}
import edu.ie3.util.scala.quantities.*
import squants.electro.*
import squants.energy.{Power, Watts}
import squants.thermal.{Celsius, Temperature}
import squants.time.Hertz
import squants.{Amperes, Meters}

class LineThermalModelCalculationsSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData {

  implicit val resistanceTolerance: ThermalResistivity = KelvinMetersPerWatt(
    1e-10
  )
  implicit val electricResistancePerLengthTolerance
      : ElectricalResistancePerLength = OhmsPerMeter(1e-10)
  implicit val powerTolerance: Power = Watts(1e-7)
  implicit val tolerance: Double = 1e-10
  implicit val thermalCapacitanceTolerance: ThermalCapacitance =
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
          val expectedResult = Watts(expected)

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
          val wC = Watts(conductorLosses)
          val wD = Watts(dielectricLosses)
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
          val dielectricCapacity = Nanofarads(dielectricCapaNanoF)
          val expectedResult = Watts(expected)

          val actual = calcDielectricLosses(
            voltageU0,
            frequency,
            tanDelta,
            dielectricCapacity,
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
          val conductorLossesW = Watts(conductorLosses)
          val expectedLosses = Watts(expectedSheathLosses)

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
          val conductorLossesW = Watts(conductorLosses)
          val expectedLosses = Watts(expectedArmorLosses)

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
          val specThermalCap = JoulesPerMeterKelvin(
            specificThermalCapacity
          )
          val innerDia = Meters(innerDiameter)
          val outerDia = Meters(outerDiameter)
          val expectedCapacitance = JoulesPerMeterKelvin(
            expectedThermalCapacitance
          )

          val actual = calcThermalCapacityCylindrical(
            specThermalCap,
            innerDia,
            outerDia,
          )

          actual should approximate(expectedCapacitance)
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
        (1.0, 1.0, 1.31695789692),
        (1.0, 0.0358, 4.716021634569044),
        (0.7, 0.044, 4.15293803195116),
        (0.1, 0.01, 3.6882538673612966), // small depth
        (2.0, 0.01, 6.6846101651642655), // large diameter ratio
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
        1.0,
        0.0729,
        0.6373564504421266,
      ), // Anders 1997 Cable No 2 T4 p. 215
      (
        1.0,
        1.0,
        0.055,
        0.6822220147,
      ), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 323
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
      (1.0, 1.0, 0.098, 0.5, 47.0866497321, 47.5422569285, 46.8820830331,
        1.0358516741), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification", TB 880, p 161 T4
      (1.0, 1.0, 0.0358, 0.0358, 31.0, 33.59, 31.0,
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
        val lossA = Watts(lossesCableA)
        val lossB = Watts(lossesCableB)
        val lossC = Watts(lossesCableC)
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
        1.0,
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
      LineSegmentThermalModel.startingState(
        groundTemp,
        cigreT880LandCable33kV,
        lineSegmentThermalModel,
      )

    val current = Amperes(537.46)

    val actual =
      createAndCalcRCNetworkMvCableShortDuration(
        tick,
        startingState,
        current,
      )

    actual.currentLineTemp1 should approximate(expected)
  }
}
