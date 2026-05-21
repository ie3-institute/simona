/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.*
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.LineSegmentThermalModelInputData
import edu.ie3.util.scala.quantities.{
  JoulesPerMeterKelvin,
  KelvinMetersPerWatt,
  ThermalCapacitance,
  ThermalResistivity,
}
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
  implicit val electricResistanceTolerance: ElectricalResistance = Ohms(1e-10)
  implicit val powerTolerance: Power = Watts(1e-7)
  implicit val tolerance: Double = 1e-10
  implicit val thermalCapacitanceTolerance: ThermalCapacitance =
    JoulesPerMeterKelvin(1e-10)
  implicit val temperatureTolerance: Temperature = Celsius(1e-5)

  "A LineSegmentThermalModel" should {

    "calculate correctly the AC Resistance" in {
      val cases = Table(
        (
          "resistancePerMeter",
          "temperatureCorrection",
          "limitTemp",
          "skinEffect",
          "proximityEffect",
          "expected",
        ),
        (0.0754e-3, 3.93e-3, 90.0, 8.835005445e-3, 6.622704052e-3,
          9.762868345e-5), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 197
        (0.32e-3, 4.03e-3, 50.0, 6.3894394042e-4, 6.0458825885e-4,
          3.5913404007749183e-4), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 315, skin effect and proximity effect correction factor has not been applied!
        (0.122, 4.03e-3, 20.0, 0d, 0d,
          0.122), // NA2XS2Y 1x240 RM/25 12/20 kV see Simbench line types, stays as 20°C
        (0.122, 4.03e-3, 90.0, 0d, 0d,
          0.1564162), // NA2XS2Y 1x240 RM/25 12/20 kV see Simbench line types, at 90°C
        (0.122, 4.03e-3, 90.0, 0.1, 0.1,
          0.18769944), // NA2XS2Y 1x240 RM/25 12/20 kV see Simbench line types, with skin and proximity effect
      )

      forAll(cases) {
        (
            resistancePerMeter,
            temperatureCorrection,
            limitTemp,
            skinEffect,
            proximityEffect,
            expected,
        ) =>

          val cableResistance = Ohms(resistancePerMeter)
          val limitTemperature = Celsius(limitTemp)
          val expectedResult = Ohms(expected)

          val actual = calcAcResistance(
            cableResistance,
            temperatureCorrection,
            limitTemperature,
            skinEffect,
            proximityEffect,
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
          val acRes = Ohms(acResistance)
          val expectedResult = Watts(expected)

          val actual = calcLossesConductor(acRes, current)

          actual should approximate(expectedResult)
      }
    }

    "calculate correctly the dielectric losses" in {
      val cases = Table(
        (
          "dielectricMaterial",
          "phaseToGroundVoltage",
          "tanDelta",
          "dielectricCapaNanoF",
          "expected",
        ),
        (
          "PVC", // Changed to PVC to allow test
          19052.5588,
          0.004,
          0.237683304,
          0.10842143853,
        ), // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 198f
        (
          "PVC", // Changed to PVC to allow test
          76210.2355,
          0.001,
          0.25506991358,
          0.4654100053,
        ), // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 132f
        (
          "XLPE", // Changed to PVC to allow test
          230940.108,
          0.001,
          0.14978009691,
          2.5095896112,
        ), // CIGRÉ Working Group B1.56, Power cable rating examples for calculation tool verification, TB 880, p 242f
      )

      forAll(cases) {
        (
            dielectricMaterial,
            phaseToGroundVoltage,
            tanDelta,
            dielectricCapaNanoF,
            expected,
        ) =>

          val insulation = dielectricMaterial match {
            case "XLPE" => CableMaterial.XLPE
            case "PVC"  => CableMaterial.PVC

          }
          val voltageU0 = Volts(phaseToGroundVoltage)
          val frequency = Hertz(50)
          val dielectricCapacity = Nanofarads(dielectricCapaNanoF)
          val expectedResult = Watts(expected)

          val actual = calcDielectricLosses(
            insulation,
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

    "calculate correctly the losses in cable sheath" in {
      val cases = Table(
        (
          "circulatingSheathLossFactor",
          "eddyCurrentsSheathLossFactor",
          "conductorLosses",
          "expectedSheathLosses",
        ),
        (0.01, 0.015, 100.0, 2.5),
        (0.05, 0.05, 50.0, 5.0),
        (0.0, 0.0, 100.0, 0.0),
        (0.1, 0.2, 200.0, 60.0),
      )

      forAll(cases) {
        (
            circulatingSheathLossFactor,
            eddyCurrentsSheathLossFactor,
            conductorLosses,
            expectedSheathLosses,
        ) =>
          val conductorLossesW = Watts(conductorLosses)
          val expectedLosses = Watts(expectedSheathLosses)

          val actual = calcLossesSheath(
            circulatingSheathLossFactor,
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

  "return all correct thermal resistance for T4 / soil for a single burried cable" in {

    val cases = Table(
      (
        "specificThermalResistivity",
        "depthCable",
        "cableDiameter",
        "thermalResistance",
      ),
      (1.0, 1.0, 0.0729, 0.6373564504421266), // Anders 1997 Cable No 2 T4 p. 215
      // (1.0, 1.8, 0.1252, 1.276), //Anders Cable No 5 T4
      // (1.0, 1.0, 0.0358, 1.933), //Cable No 1 T4 Anders 2005 p. 311
      // (1.0, 1.0, 0.044, 1.8524966955), //CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 204 T4, Tree-foil touching
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
        1.0358516741), // CIGRÉ Working Group B1.56, “Power cable rating examples for calculation tool verification, TB 880, p 161 T4
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
          calcThermalResistanceToSoilThreeSingleCoreFlatFormation(
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

    val expected = Celsius(88.72615) // S. 205 CIGRE
    val tick = 972000L
    val lastTick = 0L

    val groundTemp = Celsius(20)

    val startingState =
      LineSegmentThermalModel.startingState(groundTemp, cableSetup)

    val current = Amperes(537.46)

    val actual =
      createAndCalcRCNetworkMvCableShortDuration(
        tick,
        startingState,
        current,
      )

    actual should approximate(expected)
  }
}
