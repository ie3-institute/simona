/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.{
  GermanVoltageLevelUtils,
  VoltageLevel,
}
import edu.ie3.simona.config.GridConfigParser.{GridConfig, parseWithDefaults}
import edu.ie3.simona.config.SimonaConfig.VoltageLimitsConfig
import edu.ie3.simona.model.grid.VoltageLimits

/** Parser to parse [[VoltageLimits]] provided via [[SimonaConfig]]
  */
object VoltageLimitsParser {

  final case class ConfigVoltageLimits(
      private val gridIdVoltageLimits: Map[Int, VoltageLimits],
      private val voltLvLVoltageLimits: Map[VoltageLevel, VoltageLimits],
  ) extends GridConfig[VoltageLimits](gridIdVoltageLimits, voltLvLVoltageLimits)

  /** Parses the configuration based [[VoltageLimits]] information based on a
    * list of [[SimonaConfig.VoltageLimitsConfig]]
    *
    * @param configVoltageLimits
    *   the refSystems provided via configuration
    * @return
    *   object that holds two maps with mappings of gridIds and voltLvls to
    *   RefSystems
    */
  def parse(
      configVoltageLimits: Option[List[SimonaConfig.VoltageLimitsConfig]]
  ): ConfigVoltageLimits = {
    val distributionVoltageLimits = VoltageLimits(0.9, 1.1)

    val defaultVoltageLimits = ConfigVoltageLimits(
      Map.empty,
      Map(
        GermanVoltageLevelUtils.LV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.MV_10KV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.MV_20KV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.MV_30KV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.HV -> distributionVoltageLimits,
        GermanVoltageLevelUtils.EHV_220KV -> VoltageLimits(0.9, 1.118),
        GermanVoltageLevelUtils.EHV_380KV -> VoltageLimits(0.9, 1.05),
      ),
    )

    parseWithDefaults[VoltageLimitsConfig, VoltageLimits, ConfigVoltageLimits](
      configVoltageLimits,
      voltageLimit => voltageLimit.gridIds,
      voltageLimit => voltageLimit.voltLvls,
      voltageLimit => VoltageLimits(voltageLimit.vMin, voltageLimit.vMax),
      (gridIds, voltLvls) => ConfigVoltageLimits(gridIds, voltLvls),
      defaultVoltageLimits,
    )("voltage limits")
  }

}
