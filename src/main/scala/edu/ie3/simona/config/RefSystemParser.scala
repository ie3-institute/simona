/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.{
  GermanVoltageLevelUtils,
  VoltageLevel,
}
import edu.ie3.simona.config.GridConfigParser.{GridConfig, parseWithDefaults}
import edu.ie3.simona.config.SimonaConfig.RefSystemConfig
import edu.ie3.simona.model.grid.RefSystem
import squants.electro.Kilovolts
import squants.energy.{Kilowatts, Megawatts}

/** Parser to parse [[RefSystem]] provided via [[SimonaConfig]]
  */
object RefSystemParser {
  final case class ConfigRefSystems(
      private val gridIdRefSystems: Map[Int, RefSystem],
      private val voltLvLRefSystems: Map[VoltageLevel, RefSystem],
  ) extends GridConfig[RefSystem](gridIdRefSystems, voltLvLRefSystems)

  /** Parses the configuration based [[RefSystem]] information based on a list
    * of [[SimonaConfig.RefSystemConfig]]
    *
    * @param configRefSystems
    *   the refSystems provided via configuration
    * @return
    *   object that holds two maps with mappings of gridIds and voltLvls to
    *   RefSystems
    */
  def parse(
      configRefSystems: Option[List[SimonaConfig.RefSystemConfig]]
  ): ConfigRefSystems = {
    val defaultRefSystems = ConfigRefSystems(
      Map.empty,
      Map(
        GermanVoltageLevelUtils.LV -> RefSystem(Kilowatts(100), Kilovolts(0.4)),
        GermanVoltageLevelUtils.MV_10KV -> RefSystem(
          Megawatts(40),
          Kilovolts(10),
        ),
        GermanVoltageLevelUtils.MV_20KV -> RefSystem(
          Megawatts(60),
          Kilovolts(20),
        ),
        GermanVoltageLevelUtils.MV_30KV -> RefSystem(
          Megawatts(150),
          Kilovolts(30),
        ),
        GermanVoltageLevelUtils.HV -> RefSystem(Megawatts(600), Kilovolts(110)),
        GermanVoltageLevelUtils.EHV_220KV -> RefSystem(
          Megawatts(800),
          Kilovolts(220),
        ),
        GermanVoltageLevelUtils.EHV_380KV -> RefSystem(
          Megawatts(1000),
          Kilovolts(380),
        ),
      ),
    )

    parseWithDefaults[RefSystemConfig, RefSystem, ConfigRefSystems](
      configRefSystems,
      refSystem => refSystem.gridIds,
      refSystem => refSystem.voltLvls,
      refSystem => RefSystem(refSystem.sNom, refSystem.vNom),
      (gridIds, voltLvls) => ConfigRefSystems(gridIds, voltLvls),
      defaultRefSystems,
    )("refSystem")
  }
}
