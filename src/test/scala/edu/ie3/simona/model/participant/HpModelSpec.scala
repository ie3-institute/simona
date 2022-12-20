/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.simona.model.participant.HpModel.HpState
import edu.ie3.simona.model.thermal.ThermalGrid.ThermalGridState
import edu.ie3.simona.model.thermal.ThermalHouse.ThermalHouseThreshold.{
  HouseTemperatureLowerBoundaryReached,
  HouseTemperatureUpperBoundaryReached
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

class HpModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with HpModelTestData {
  "Testing the heat pump model" when {
    "calculating the next state with different states" should {
      "deliver correct tick, power and running state" in {
        val cases = Table(
          (
            "state",
            "expectedTick",
            "expectedRunningState",
            "expectedActivePower",
            "expectedInnerTemperature",
            "expectedNextThreshold"
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31711L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30642L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(20),
              None
            ),
            7200,
            true,
            95,
            18.0,
            Some(HouseTemperatureUpperBoundaryReached(27771L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(22),
              None
            ),
            7200,
            false,
            0,
            19.6,
            Some(HouseTemperatureLowerBoundaryReached(13200L))
          ),
          (
            HpState(
              isRunning = false,
              0,
              hpData.ambientTemperature,
              Kilowatts(0.0),
              Kilowatts(0.0),
              thermalState(23),
              None
            ),
            7200,
            false,
            0,
            20.4,
            Some(HouseTemperatureLowerBoundaryReached(15508L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
              thermalState(17),
              None
            ),
            7200,
            true,
            95,
            15.6,
            Some(HouseTemperatureUpperBoundaryReached(31711L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
              thermalState(18),
              None
            ),
            7200,
            true,
            95,
            16.4,
            Some(HouseTemperatureUpperBoundaryReached(30642L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
              thermalState(20),
              None
            ),
            7200,
            true,
            95,
            18.0,
            Some(HouseTemperatureUpperBoundaryReached(27771L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
              thermalState(22),
              None
            ),
            7200,
            true,
            95,
            19.6,
            Some(HouseTemperatureUpperBoundaryReached(23200L))
          ),
          (
            HpState(
              isRunning = true,
              0,
              hpData.ambientTemperature,
              Kilowatts(95.0),
              Kilowatts(80.0),
              thermalState(25),
              None
            ),
            7200,
            false,
            0,
            22.0,
            Some(HouseTemperatureLowerBoundaryReached(19200L))
          )
        )

        forAll(cases) {
          (
              state,
              expectedTick,
              expectedRunningState,
              expectedActivePower,
              expectedInnerTemperature,
              expectedNextThreshold
          ) =>
            val data = hpData
            val house = thermalHouse(18, 22)
            val grid = thermalGrid(house)
            val hp = hpModel(grid)

            hp.calculateNextState(state, data) match {
              case HpState(
                    isRunning,
                    lastTimeTick,
                    _,
                    activePower,
                    _,
                    ThermalGridState(Some(thermalHouseState), _),
                    maybeThreshold
                  ) =>
                isRunning shouldBe expectedRunningState
                lastTimeTick shouldBe expectedTick
                activePower should equalWithTolerance(
                  Quantities.getQuantity(
                    expectedActivePower,
                    PowerSystemUnits.KILOWATT
                  )
                )

                thermalHouseState.innerTemperature should equalWithTolerance(
                  Quantities.getQuantity(
                    expectedInnerTemperature,
                    Units.CELSIUS
                  )
                )

                maybeThreshold shouldBe expectedNextThreshold
            }
        }
      }
    }

    "determining the flexibility options" when {
      "the house is heated up and storage has space" should {
        "deliver positive flexibility" in {
          val house = thermalHouse(18, 22)
            .copy(ethLosses = Kilowatts(0.2))
          val grid = thermalGrid(house, Some(thermalStorage))
          val hp = hpModel(grid)
          val relevantData = hpData.copy(currentTimeTick =
            2763L
          ) // Tick, at which the house is heated up
          val lastState = HpState(
            isRunning = true,
            0,
            hpData.ambientTemperature,
            Kilowatts(95.0),
            Kilowatts(80.0),
            thermalState(21, 80, 20, 0),
            Some(HouseTemperatureUpperBoundaryReached(7995L))
          )

          hp.determineFlexOptions(relevantData, lastState) match {
            case ProvideMinMaxFlexOptions(
                  modelUuid,
                  referencePower,
                  minPower,
                  maxPower
                ) =>
              modelUuid shouldBe hp.uuid
              referencePower should equalWithTolerance(
                Quantities.getQuantity(95d, StandardUnits.ACTIVE_POWER_IN)
              )
              minPower should equalWithTolerance(
                Quantities.getQuantity(0d, StandardUnits.ACTIVE_POWER_IN)
              )
              maxPower should equalWithTolerance(
                Quantities.getQuantity(95d, StandardUnits.ACTIVE_POWER_IN)
              )
          }
        }
      }
    }
  }
}
