/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import squants.energy.{Kilowatts, Power}
import squants.thermal.{Celsius, Temperature}

import java.util.UUID

trait ThermalGridTestData {
  protected val thermalBusInput = new ThermalBusInput(
    UUID.randomUUID(),
    "Thermal Bus",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited()
  )
  protected val testGridambientTemperature: Temperature = Celsius(12d)
  protected val testGridQDotInfeed: Power = Kilowatts(15d)
  protected val testGridQDotConsumption: Power = Kilowatts(-42d)
  protected val testGridQDotConsumptionHigh: Power = Kilowatts(-200d)
}
