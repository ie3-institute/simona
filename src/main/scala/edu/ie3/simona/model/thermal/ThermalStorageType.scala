/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.thermal

import edu.ie3.simona.model.thermal.ThermalStorage.ThermalStorageState

sealed trait ThermalStorageType
case class HeatStorageType(storage: CylindricalThermalStorage)
    extends ThermalStorageType
case class HotWaterStorageType(storage: DomesticHotWaterStorage)
    extends ThermalStorageType

sealed trait StorageWithState
case class HeatStorageWithState(
    storage: CylindricalThermalStorage,
    state: ThermalStorageState,
) extends StorageWithState
case class HotWaterStorageWithState(
    storage: DomesticHotWaterStorage,
    state: ThermalStorageState,
) extends StorageWithState
