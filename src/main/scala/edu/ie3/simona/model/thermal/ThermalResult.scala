/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.thermal.{
  CylindricalStorageResult,
  DomesticHotWaterStorageResult,
  ThermalHouseResult,
}

sealed trait ThermalResult {
  def getResultEntity: ResultEntity
}

case class HouseResult(result: ThermalHouseResult) extends ThermalResult {
  override def getResultEntity: ResultEntity = result
}

case class HeatStorageResult(result: CylindricalStorageResult)
    extends ThermalResult {
  override def getResultEntity: ResultEntity = result
}

case class HotWaterStorageResult(result: DomesticHotWaterStorageResult)
    extends ThermalResult {
  override def getResultEntity: ResultEntity = result
}
