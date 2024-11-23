/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import edu.ie3.simona.model.thermal.ThermalGrid.{
  ThermalDemandWrapper,
  ThermalEnergyDemand,
}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKWh
import squants.energy.{KilowattHours, Kilowatts, Power}
import squants.thermal.{Celsius, Temperature}

import java.util.UUID

trait ThermalGridTestData {
  protected val thermalBusInput = new ThermalBusInput(
    UUID.fromString("48fa6e8d-c07f-45cd-9ad7-094a1f2a7489"),
    "Thermal Bus",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited(),
  )
  protected val testGridAmbientTemperature: Temperature = Celsius(12d)
  protected val testGridQDotInfeed: Power = Kilowatts(15d)
  protected val testGridQDotConsumption: Power = Kilowatts(-42d)
  protected val testGridQDotConsumptionHigh: Power = Kilowatts(-200d)
  protected val noThermalDemand: ThermalDemandWrapper =
    ThermalDemandWrapper(
      ThermalEnergyDemand(zeroKWh, zeroKWh),
      ThermalEnergyDemand(zeroKWh, zeroKWh),
      ThermalEnergyDemand(zeroKWh, zeroKWh),
    )
  protected val onlyThermalDemandOfHouse: ThermalDemandWrapper =
    ThermalDemandWrapper(
      ThermalEnergyDemand(KilowattHours(1), KilowattHours(2)),
      ThermalEnergyDemand(zeroKWh, zeroKWh),
      ThermalEnergyDemand(zeroKWh, zeroKWh),
    )
  protected val onlyThermalDemandOfHeatStorage: ThermalDemandWrapper =
    ThermalDemandWrapper(
      ThermalEnergyDemand(zeroKWh, zeroKWh),
      ThermalEnergyDemand(KilowattHours(1), KilowattHours(2)),
      ThermalEnergyDemand(zeroKWh, zeroKWh),
    )
  protected val isRunning: Boolean = true
  protected val isNotRunning: Boolean = false
  protected val houseInhabitants = 2.0
}
