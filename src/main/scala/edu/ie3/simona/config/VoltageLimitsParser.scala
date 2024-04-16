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
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.model.grid.VoltageLimits
import edu.ie3.simona.util.CollectionUtils

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

    if (configVoltageLimits.isEmpty) {
      val voltageLimit = VoltageLimits(0.9, 1.1)

      ConfigVoltageLimits(
        Map.empty,
        Map(
          GermanVoltageLevelUtils.LV -> voltageLimit,
          GermanVoltageLevelUtils.MV_10KV -> voltageLimit,
          GermanVoltageLevelUtils.MV_20KV -> voltageLimit,
          GermanVoltageLevelUtils.MV_30KV -> voltageLimit,
          GermanVoltageLevelUtils.HV -> voltageLimit,
          GermanVoltageLevelUtils.EHV_220KV -> VoltageLimits(0.9, 1.118),
          GermanVoltageLevelUtils.EHV_380KV -> VoltageLimits(0.9, 1.05),
        ),
      )

    } else {

      val voltageLimits = configVoltageLimits.map { configVoltageLimit =>
        (
          configVoltageLimit,
          VoltageLimits(configVoltageLimit.vMin, configVoltageLimit.vMax),
        )
      }

      val gridIdVoltageLimits = voltageLimits.flatMap { case (config, limits) =>
        config.gridIds
          .map {
            _.flatMap { gridId =>
              {
                val allGridIds = gridId match {
                  case ConfigConventions.gridIdDotRange(from, to) =>
                    from.toInt to to.toInt
                  case ConfigConventions.gridIdMinusRange(from, to) =>
                    from.toInt to to.toInt
                  case ConfigConventions.singleGridId(singleGridId) =>
                    Seq(singleGridId.toInt)
                  case unknownGridIdFormat =>
                    throw new InvalidConfigParameterException(
                      s"Unknown gridId format $unknownGridIdFormat provided for voltage limits $config"
                    )
                }

                allGridIds.map(gridId => (gridId, limits))
              }
            }
          }
          .getOrElse(Seq.empty[(Int, VoltageLimits)])
      }

      val voltLvlVoltageLimits = voltageLimits.flatMap {
        case (configRefSystem, parsedRefSystem) =>
          configRefSystem.voltLvls
            .map {
              _.map { voltLvlDef =>
                (VoltLvlParser.from(voltLvlDef), parsedRefSystem)
              }
            }
            .getOrElse(Seq.empty)
      }

      // check for duplicates of gridIds and voltLevels which will be the key for the following map conversion
      if (
        CollectionUtils.listHasDuplicates(
          gridIdVoltageLimits.map { case (gridId, _) => gridId }
        )
      )
        throw new InvalidConfigParameterException(
          s"The provided gridIds in simona.gridConfig.voltageLimits contains duplicates. " +
            s"Please check if there are either duplicate entries or overlapping ranges!"
        )
      if (
        CollectionUtils.listHasDuplicates(
          voltLvlVoltageLimits.map { case (voltLvl, _) => voltLvl }
        )
      )
        throw new InvalidConfigParameterException(
          s"The provided voltLvls in simona.gridConfig.voltageLimits contains duplicates. " +
            s"Please check your configuration for duplicates in voltLvl entries!"
        )

      ConfigVoltageLimits(gridIdVoltageLimits.toMap, voltLvlVoltageLimits.toMap)
    }
  }

}
