/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.VoltageLevel
import edu.ie3.simona.model.grid.VoltageLimits

object VoltageLimitsParser {

  final case class ConfigVoltageLimits(
      private val gridIdVoltageLimits: Map[Int, VoltageLimits],
      private val voltLvLVoltageLimits: Map[VoltageLevel, VoltageLimits],
  ) {
    def find(
        gridId: Int,
        voltLvl: Option[VoltageLevel] = None,
    ): Option[VoltageLimits] = gridIdVoltageLimits
      .get(gridId)
      .orElse(voltLvl.flatMap(voltLvLVoltageLimits.get))
  }

  def parse(
      configVoltageLimits: List[SimonaConfig.VoltageLimitsConfig]
  ): ConfigVoltageLimits = {
    ???
  }

}
