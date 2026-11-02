/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import edu.ie3.simona.model.grid.ampacity.CableLossCalculations.*
import edu.ie3.simona.test.common.input.{
  CigreT880LandCable33kV,
  LineSegmentThermalModelInputData,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.electro.FaradsPerKilometer
import edu.ie3.util.quantities.electro.{OhmsPerMeter, SpecificResistance}
import edu.ie3.util.quantities.energy.{PowerPerLength, WattsPerMeter}
import edu.ie3.util.quantities.thermal.KelvinMetersPerWatt
import edu.ie3.util.scala.quantities.DefaultQuantities.europeanSystemFrequency
import edu.ie3.util.scala.quantities.SquantsUtils.RichSpecificCapacitance
import squants.electro.Volts
import squants.thermal.{Celsius, Temperature}
import squants.{Amperes, Meters}

class CableLossCalculationsSpec
    extends UnitSpec
    with LineSegmentThermalModelInputData {

  implicit val electricResistancePerLengthTolerance: SpecificResistance =
    OhmsPerMeter(1e-10)
  implicit val powerPerLengthTolerance: PowerPerLength = WattsPerMeter(1e-7)
  implicit val temperatureTolerance: Temperature = Celsius(1e-3)

  "A CableLossCalculations" should {

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
          "dielectricCapaMicroFPerKm",
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
            dielectricCapaMircoFPerKm,
            expected,
        ) =>

          val voltageU0 = Volts(phaseToGroundVoltage)
          val dielectricSpecificCapacity =
            FaradsPerKilometer(dielectricCapaMircoFPerKm * 1e-6)
          val expectedResult = WattsPerMeter(expected)

          val actual =
            dielectricSpecificCapacity.calculateSpecificDielectricLosses(
              voltageU0,
              europeanSystemFrequency,
              tanDelta,
            )

          actual should approximate(expectedResult)
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
  }
}
