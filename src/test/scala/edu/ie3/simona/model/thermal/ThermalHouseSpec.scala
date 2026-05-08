/*
 * © 2021-2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.participant.hp.HpModel.{HpOperatingPoint, HpState}
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTargetTemperatureReached,
  HouseTemperatureLowerBoundaryReached,
}
import edu.ie3.simona.model.thermal.ThermalHouse.{
  ThermalHouseState,
  startingState,
}
import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState
import edu.ie3.simona.test.common.input.HpInputTestData
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.quantities.DefaultQuantities.{zeroKW, zeroKWh}
import edu.ie3.util.scala.quantities.WattsPerKelvin
import org.scalatest.prop.{TableFor2, TableFor3, TableFor4, TableFor7}
import squants.energy.*
import squants.space.Litres
import squants.thermal.*
import squants.time.*
import squants.{Energy, Temperature, Volume}

import java.time.ZonedDateTime

class ThermalHouseSpec
    extends UnitSpec
    with HpInputTestData
    with ThermalHouseTestData
    with DefaultTestData {

  implicit val temperatureTolerance: Temperature = Celsius(1e-4)
  implicit val energyTolerance: Energy = KilowattHours(1e-9)
  implicit val volumeTolerance: Volume = Litres(0.01)

  "ThermalHouse" should {
    "Functions testing inner temperature work as expected" in {
      val thermalHouseTest = thermalHouse(18, 22)

      val testCases: TableFor3[Double, Boolean, Boolean] = Table(
        ("Inner Temperature (C)", "Is Too High", "Is Too Low"),
        (17d, false, true),
        (17.98d, false, true),
        (18d, false, true),
        (19.98d, false, false),
        (20d, true, false),
        (22d, true, false),
        (22.02d, true, false),
        (23d, true, false),
      )

      forAll(testCases) {
        (innerTemperature: Double, isTooHigh: Boolean, isTooLow: Boolean) =>
          val innerTemp = Temperature(innerTemperature, Celsius)
          val isHigher = thermalHouseTest.isInnerTemperatureTooHigh(innerTemp)
          val isLower = thermalHouseTest.isInnerTemperatureTooLow(innerTemp)

          isHigher shouldBe isTooHigh
          isLower shouldBe isTooLow
      }
    }

    "handle edge cases in newInnerTemperature" in {
      val house = thermalHouse(18, 22)
      // Test with very high power
      val highPowerTemp = house.newInnerTemperature(
        Kilowatts(1000000),
        Hours(1),
        Celsius(20),
        Celsius(10),
      )
      highPowerTemp should be > Celsius(50)

      // Test with zero power
      val zeroPowerTemp = house.newInnerTemperature(
        Kilowatts(0),
        Hours(1),
        Celsius(20),
        Celsius(10),
      )
      zeroPowerTemp should be < Celsius(20)
    }

    "calculate correct K1 and K2 factors" in {
      val house = thermalHouse(18, 22)
      val method =
        PrivateMethod[Tuple2[Double, Double]](Symbol("getFactorsK1AndK2"))

      val (k1, k2) = house invokePrivate method(Kilowatts(2), Celsius(20))

      k1 should be > 0.0
      k2 should be > 0.0
    }

    "Comprising function to calculate new inner temperature works as expected" in {
      val thermalHouseTest = thermalHouse(18, 22)
      val thermalPower = Kilowatts(100)
      val duration = Seconds(3600)
      val currentInnerTemperature = Temperature(20, Celsius)
      val ambientTemperature = Temperature(10, Celsius)

      val newInnerTemperature = thermalHouseTest.newInnerTemperature(
        thermalPower,
        duration,
        currentInnerTemperature,
        ambientTemperature,
      )

      newInnerTemperature should approximate(Temperature(28.5646, Celsius))
    }

    "calculate correct energy demand for heating" in {
      val house = thermalHouse(18, 22)

      val testCases = Table(
        ("innerTemp", "ambientTemp", "expectedRequired", "expectedPossible"),
        // Temperature to low
        (16.0, 10.0, 40.0, 40.0),
        (17.0, 10.0, 30.0, 30.0),
        (17.0, 17.0, 30.0, 30.0),
        (17.0, 30.0, 30.0, 30.0),
        (17.9, 10.0, 21.0, 21.0),
        // Temperature between boundaries
        (20.0, 10.0, 0.0, 0.0),
        (20.0, 20.0, 0.0, 0.0),
        (20.0, 30.0, 0.0, 0.0),
        (21.0, 10.0, 0.0, 0.0),
        // Temperature too high
        (22.1, 10.0, 0.0, 0.0),
        (22.1, 30.0, 0.0, 0.0),
        (23.0, 10.0, 0.0, 0.0),
      )

      forAll(testCases) {
        (
            innerTemp: Double,
            ambientTemp: Double,
            expectedRequired: Double,
            expectedPossible: Double,
        ) =>
          val state =
            ThermalHouseState(0L, Celsius(ambientTemp), Celsius(innerTemp))
          val demand = house.energyDemandHeating(state)

          demand.required should approximate(KilowattHours(expectedRequired))
          demand.possible should approximate(KilowattHours(expectedPossible))
      }
    }

    "Check for the correct state of house" in {
      val house = thermalHouse(18, 22)
      val tick = 3600
      val ambientTemperature = Temperature(15, Celsius)
      val initialHouseState = startingState(house, ambientTemperature)

      val testCases: TableFor2[Double, Double] = Table(
        ("qDotHouse", "expectedTemperature(K)"),
        // loss is higher than gain
        (0d, 292.67418),
        (1d, 292.76935),
        (2d, 292.86451),
        // Loss and gain should be equal resulting no temperature change
        (5d, 293.15),
        // gain is higher than loss
        (6d, 293.2451),
        (10d, 293.6258),
      )

      forAll(testCases) {
        (lastOperatingPoint: Double, expectedTemperature: Double) =>
          val thermalHouseState =
            house.determineState(
              tick,
              initialHouseState,
              Kilowatts(lastOperatingPoint),
            )

          thermalHouseState.tick shouldBe 3600L
          thermalHouseState.innerTemperature should approximate(
            Kelvin(expectedTemperature)
          )
      }
    }

    "Check for the correct state over multiple simulation steps" in {
      val house = thermalHouse(18, 22)
      val ambientTemperature = Celsius(5)

      val testCases
          : TableFor7[Double, Double, Long, Double, Double, Long, Double] =
        Table(
          (
            "startingTemp",
            "qDotFirstPeriod",
            "firstTick",
            "expectedTemperatureFirstPeriod",
            "qDotSecondPeriod",
            "secondTick",
            "expectedTemperatureSecondPeriod",
          ),
          (20d, 30d, 36000L, 29.4818, 30d, 72000L, 32.97),
          (20d, 30d, 18000L, 25.9020, 30d, 72000L, 32.97),
          (20d, 30d, 7200L, 22.719, 30d, 72000L, 32.97),
          (20d, 0d, 5151L, 18.0002, 30d, 72000L, 32.3454),
          (20d, 1d, 5549L, 18.0001, 20d, 72000L, 23.8948),
          (20d, 2d, 6013L, 18.0002, 10d, 72000L, 15.4799),
          (19d, 14d, 99999L, 19.0, 14d, 72000L, 19.00),
          (20d, 15d, 99999L, 20.0, 15d, 72000L, 20.00),
          (18d, 16d, 39550L, 20.0, 10d, 72000L, 17.03),
          (18d, 20d, 12113L, 20.0, 5d, 72000L, 11.8947),
          (18d, 25d, 6563L, 19.9998, 0d, 72000L, 7.436),
        )

      forAll(testCases) {
        (
            startingTemp: Double,
            qDotFirstPeriod: Double,
            firstTick: Long,
            expectedTemperatureFirstPeriod: Double,
            qDotSecondPeriod: Double,
            secondTick: Long,
            expectedTemperatureSecondPeriod: Double,
        ) =>
          val initialHouseState = startingState(house, ambientTemperature)
            .copy(innerTemperature = Celsius(startingTemp))
          val thermalHouseState = house.determineState(
            firstTick,
            initialHouseState,
            Kilowatts(qDotFirstPeriod),
          )

          thermalHouseState.tick shouldBe firstTick
          thermalHouseState.innerTemperature should approximate(
            Celsius(expectedTemperatureFirstPeriod)
          )

          val finalThermalHouseState = house.determineState(
            secondTick,
            thermalHouseState,
            Kilowatts(qDotSecondPeriod),
          )

          finalThermalHouseState.tick shouldBe secondTick
          finalThermalHouseState.innerTemperature should approximate(
            Celsius(expectedTemperatureSecondPeriod)
          )
      }
    }

    "Check if the same state is reached by different ways of simulation steps" in {
      val house = thermalHouse(18, 22)
      val ambientTemperature = Celsius(5)
      val initialHouseState = startingState(house, ambientTemperature)

      val qDot = Kilowatts(30)
      val firstTickCaseA = 18000
      val firstTickCaseB = 7200

      val finalTick = 72000

      val thermalHouseStateCaseA = house.determineState(
        firstTickCaseA,
        initialHouseState,
        qDot,
      )

      val thermalHouseStateCaseB = house.determineState(
        firstTickCaseB,
        initialHouseState,
        qDot,
      )

      val finalThermalHouseStateCaseA = house.determineState(
        finalTick,
        thermalHouseStateCaseA,
        qDot,
      )
      val finalThermalHouseStateCaseB = house.determineState(
        finalTick,
        thermalHouseStateCaseB,
        qDot,
      )

      val finalThermalHouseStateCaseC = house.determineState(
        finalTick,
        initialHouseState,
        qDot,
      )
      finalThermalHouseStateCaseA.innerTemperature should approximate(
        finalThermalHouseStateCaseC.innerTemperature
      )
      finalThermalHouseStateCaseA.innerTemperature should approximate(
        finalThermalHouseStateCaseB.innerTemperature
      )
    }

    "Check if the same threshold is determined by different ways of simulation steps" in {
      val house = thermalHouse(18, 22)
      val ambientTemperature = Celsius(5)
      val initialHouseState = startingState(house, ambientTemperature)
        .copy(innerTemperature = Celsius(6))
      val qDot = Kilowatts(30)

      val inBetweenStateCaseA =
        house.determineState(3600, initialHouseState, qDot)
      val inBetweenStateCaseB =
        house.determineState(18000, initialHouseState, qDot)

      val finalThresholdCaseA =
        house.determineNextThreshold(inBetweenStateCaseA, qDot)
      val finalThresholdCaseB =
        house.determineNextThreshold(inBetweenStateCaseB, qDot)

      val finalThresholdCaseC = house.determineNextThreshold(
        initialHouseState,
        qDot,
      )

      given tolerance: Double = 1d
      (finalThresholdCaseA, finalThresholdCaseB, finalThresholdCaseC) match {
        case (Some(thresholdA), Some(thresholdB), Some(thresholdC)) =>
          thresholdA.tick.doubleValue should approximate(
            thresholdB.tick.doubleValue
          )
          thresholdB.tick.doubleValue should approximate(
            thresholdC.tick.doubleValue
          )
          thresholdC shouldBe HouseTargetTemperatureReached(23732)
        case _ => fail("Could not match thresholds.")
      }
    }

    "Check for the correct next threshold of house with thermal feed in" in {
      val house = thermalHouse(18, 22)

      val testCases
          : TableFor4[Double, Double, Double, Option[ThermalThreshold]] =
        Table(
          (
            "ambientTemp",
            "currentInnerTemp",
            "newOperatingPoint",
            "expectedThreshold",
          ),
          // some OperatingPoints not capable to heat the house sufficient
          (5d, 20d, 0d, Some(HouseTemperatureLowerBoundaryReached(5151))),
          (5d, 20d, 1d, Some(HouseTemperatureLowerBoundaryReached(5549))),
          (5d, 20d, 2d, Some(HouseTemperatureLowerBoundaryReached(6013))),
          (5d, 20d, 10d, Some(HouseTemperatureLowerBoundaryReached(18389))),
          // OperatingPoint that keeps the house in perfect balance
          (5d, 19d, 14d, None),
          (5d, 20d, 15d, None),
          // some OperatingPoints that increase the house inner temperature after some cooling down first
          (5d, 18d, 16d, Some(HouseTargetTemperatureReached(39550))),
          (5d, 18d, 20d, Some(HouseTargetTemperatureReached(12113))),
          (5d, 18d, 25d, Some(HouseTargetTemperatureReached(6563))),
          // House is heated externally (ambient temperature)
          (30d, 18d, 25d, Some(HouseTargetTemperatureReached(2000))),
          (30d, 21.9d, 0d, None),
          (30d, 25d, 0d, None),
        )

      forAll(testCases) {
        (
            ambientTemp: Double,
            currentInnerTemp: Double,
            newOp: Double,
            expectedThreshold: Option[ThermalThreshold],
        ) =>
          val ambientTemperature = Celsius(ambientTemp)
          val initialHouseState = startingState(house, ambientTemperature)
          val newOperatingPoint = Kilowatts(newOp)
          val state =
            initialHouseState.copy(innerTemperature = Celsius(currentInnerTemp))

          val thresholdOption = house.determineNextThreshold(
            state,
            newOperatingPoint,
          )

          thresholdOption shouldBe expectedThreshold
      }
    }

    "return no next tick if house temperature is within tolerance margin" in {
      val house = thermalHouse(19, 21)
      val state = ThermalHouseState(900L, Celsius(0), Celsius(19.0001))

      house.determineNextThreshold(
        state,
        zeroKW,
      ) shouldBe None
    }

    "calculating thermal energy demand for heating water" should {

      val thermalEnergyDemandWater =
        PrivateMethod[Energy](
          Symbol("thermalEnergyDemandWater")
        )

      "calculate the thermal energy demand to heat up water correctly" in {

        val cases = Table(
          ("waterDemand", "startTemp", "endTemp", "expectedEnergy"),
          (1d, 1d, 2d, 0.00116222222),
          (1000d, -5d, 55d, 69.733333333),
          (20d, 20d, 30d, 0.2324444444),
          (55d, 100d, 100d, 0d),
          (2500d, 30d, 65d, 101.6944444444),
        )

        forAll(cases) { (waterDemand, startTemp, endTemp, expectedResult) =>
          val result =
            thermalHouse invokePrivate thermalEnergyDemandWater(
              Litres(waterDemand),
              Celsius(startTemp),
              Celsius(endTemp),
            )

          result should approximate(KilowattHours(expectedResult))
        }

      }

      "calculate correct energy demand for domestic hot water" in {
        val house = thermalHouse(18, 22)

        val testCases = Table(
          ("hours", "expectedEnergy"),
          (None, 0.0),
          (Some(Seq()), 0.0),
          (Some(Seq(8)), 0.255281315),
          (Some(Seq(7, 8, 9)), 0.6457115616),
          (Some(Seq(0, 1, 2, 3)), 0.1389030685),
        )

        forAll(testCases) { (hours: Option[Seq[Int]], expectedEnergy: Double) =>
          val demand = house.energyDemandDomesticHotWater(hours)

          demand.required should approximate(KilowattHours(expectedEnergy))
          demand.possible should approximate(KilowattHours(expectedEnergy))
          demand.required shouldBe demand.possible
        }
      }

      "throw an exception if end temperature is lower than start temperature" in {
        val waterDemand = Litres(100)
        val startTemp = Celsius(60)
        val endTemp = Celsius(20)

        intercept[RuntimeException] {
          thermalHouse invokePrivate thermalEnergyDemandWater(
            waterDemand,
            startTemp,
            endTemp,
          )
        }.getMessage shouldBe "End temperature of 20.0°C is lower than the start temperature 60.0°C for the water heating system."
      }

      "calculate the water demand correctly for a given hour" in {
        val waterDemandOfHour =
          PrivateMethod[Volume](
            Symbol("waterDemandOfHour")
          )
        val cases = Table(
          ("hour", "housingType", "noPersons", "expectedVolume"),
          (0, "house", 0d, 0d),
          (0, "house", 1d, 0.64602),
          (0, "house", 2d, 1.29205),
          (1, "house", 1d, 0.358904),
          (3, "house", 4d, 0.43068),
          (8, "house", 4d, 9.76219),
          (0, "flat", 0d, 0d),
          (0, "flat", 1d, 0.3589),
          (0, "flat", 2d, 0.7178),
          (1, "flat", 1d, 0.3589),
          (3, "flat", 4d, 0d),
          (8, "flat", 4d, 11.48493),
        )

        forAll(cases) { (hour, housingType, noPersons, expectedResult) =>
          val demand = thermalHouse invokePrivate waterDemandOfHour(
            hour,
            noPersons,
            housingType,
          )
          val expected = Litres(expectedResult)
          demand should approximate(expected)
        }
      }

      "return the correct sequence of hours to determine hot water demand for" in {
        val simulationStart = ZonedDateTime.parse("2024-01-01T00:00:00Z")
        val cases = Table(
          ("lastTick", "tick", "expectedResult"),
          (-1L, 0L, Some(Seq(0))),
          (0L, 1800L, None),
          (1800L, 1801L, None),
          (0L, 3600L, Some(Seq(1))),
          (3599L, 7200L, Some(Seq(1, 2))),
          (-1L, 7200L, Some(Seq(0, 1, 2))),
          (86000L, 86400L, Some(Seq(0))),
          (
            -1L,
            86400L,
            Some(
              Seq(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
                18, 19, 20, 21, 22, 23, 0)
            ),
          ),
        )

        forAll(cases) { (lastTick, tick, expectedResult) =>
          val thermalGridState = ThermalGridState(
            Some(
              ThermalHouseState(
                lastTick,
                testGridAmbientTemperature,
                Celsius(20),
              )
            ),
            None,
            Some(ThermalStorageState(lastTick, zeroKWh)),
          )

          val state = HpState(
            lastTick,
            defaultSimulationStart.plusSeconds(lastTick),
            thermalGridState,
            HpOperatingPoint.zero,
            noThermalDemand,
          )

          val simulationTime = tick.toDateTime(using simulationStart)

          val sequenceOfHours =
            thermalHouse.checkIfNeedToDetermineDomesticHotWaterDemand(
              tick,
              simulationTime,
              state,
            )

          expectedResult match {
            case Some(expectedSeq) => sequenceOfHours shouldBe Some(expectedSeq)
            case None              => sequenceOfHours shouldBe None
          }
        }
      }
    }

    "Check build method" in {

      val thermalTestHouse = thermalHouse(18, 22)
      val thermalHouseInput = defaultThermalHouseInput

      thermalTestHouse.id shouldBe thermalHouseInput.getId
      thermalTestHouse.operatorInput shouldBe thermalHouseInput.getOperator
      thermalTestHouse.operationTime shouldBe thermalHouseInput.getOperationTime
      thermalTestHouse.bus shouldBe null
      thermalTestHouse.ethLosses shouldBe WattsPerKelvin(1000.0)
      (thermalTestHouse.ethCapa * Temperature(
        1,
        Kelvin,
      )) shouldBe KilowattHours(10.0)
      thermalTestHouse.lowerBoundaryTemperature should approximate(
        Temperature(
          18,
          Celsius,
        )
      )
      thermalTestHouse.upperBoundaryTemperature should approximate(
        Temperature(
          22,
          Celsius,
        )
      )
    }
  }
}
