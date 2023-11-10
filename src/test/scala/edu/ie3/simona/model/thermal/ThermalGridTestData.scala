/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.thermal.ThermalBusInput
import squants.energy.Kilowatts
import squants.thermal.Celsius

import java.util.UUID

trait ThermalGridTestData {
  protected val thermalBusInput = new ThermalBusInput(
    UUID.randomUUID(),
    "Thermal Bus",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    OperationTime.notLimited()
  )
  protected val testGridambientTemperature = Celsius(12d)
  protected val testGridQDotInfeed = Kilowatts(15d)
  protected val testGridQDotConsumption = Kilowatts(-42d)
  protected val testGridQDotConsumptionHigh = Kilowatts(-200d)
}
