/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.{
  GermanVoltageLevelUtils,
  VoltageLevel,
}
import edu.ie3.simona.config.SimonaConfig.Simona.GridConfig
import edu.ie3.simona.config.SimonaConfig.{
  RefSystemConfig,
  VoltLvlConfig,
  VoltageLimitsConfig,
}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.model.grid.{RefSystem, VoltageLimits}
import edu.ie3.simona.util.CollectionUtils
import edu.ie3.util.quantities.PowerSystemUnits
import squants.electro.Kilovolts
import squants.energy.{Kilowatts, Megawatts}

object GridConfigParser {
  abstract class ParsedGridConfig[T](
      protected val gridIdMap: Map[Int, T],
      protected val voltLvlMap: Map[VoltageLevel, T],
  ) {

    /** Returns a [[GridConfig]] based on the provided gridId or the voltLvl as
      * fallback if available
      *
      * @param gridId
      *   the gridId the refSystem is wanted for
      * @param voltLvl
      *   the voltLvL that is valid for this grid
      * @return
      *   Some(refSystem) if available or None if unavailable
      */
    final def find(
        gridId: Int,
        voltLvl: Option[VoltageLevel] = None,
    ): Option[T] =
      gridIdMap
        .get(gridId)
        .orElse(voltLvl.flatMap(voltLvlMap.get))
  }

  final case class ConfigRefSystems(
      private val gridIdRefSystems: Map[Int, RefSystem],
      private val voltLvLRefSystems: Map[VoltageLevel, RefSystem],
  ) extends ParsedGridConfig[RefSystem](gridIdRefSystems, voltLvLRefSystems)

  final case class ConfigVoltageLimits(
      private val gridIdVoltageLimits: Map[Int, VoltageLimits],
      private val voltLvLVoltageLimits: Map[VoltageLevel, VoltageLimits],
  ) extends ParsedGridConfig[VoltageLimits](
        gridIdVoltageLimits,
        voltLvLVoltageLimits,
      )

  def parse(
      gridConfigs: GridConfig
  ): (ConfigRefSystems, ConfigVoltageLimits) =
    (
      parseRefSystems(gridConfigs.refSystems),
      parseVoltageLimits(gridConfigs.voltageLimits),
    )

  /** Parses the configuration based [[RefSystem]] information based on a list
    * of [[RefSystemConfig]]
    *
    * @param configRefSystems
    *   the refSystems provided via configuration
    * @return
    *   object that holds two maps with mappings of gridIds and voltLvls to
    *   RefSystems
    */
  def parseRefSystems(
      configRefSystems: Option[List[RefSystemConfig]]
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
    )("refSystems")
  }

  /** Parses the configuration based [[VoltageLimits]] information based on a
    * list of [[VoltageLimitsConfig]]
    *
    * @param configVoltageLimits
    *   the refSystems provided via configuration
    * @return
    *   object that holds two maps with mappings of gridIds and voltLvls to
    *   RefSystems
    */
  def parseVoltageLimits(
      configVoltageLimits: Option[List[VoltageLimitsConfig]]
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
    )("voltageLimits")
  }

  def parseWithDefaults[C, E, T <: ParsedGridConfig[_]](
      configs: Option[List[C]],
      gridIds: C => Option[List[String]],
      voltLvls: C => Option[List[VoltLvlConfig]],
      elementFcn: C => E,
      builder: (Map[Int, E], Map[VoltageLevel, E]) => T,
      defaults: T,
  )(implicit gridConfigType: String): T = configs match {
    case Some(configElements) if configElements.nonEmpty =>
      // units for parsing are not initialized by default
      // hence we call them manually
      new PowerSystemUnits

      val emptyLists = (List.empty[(Int, E)], List.empty[(VoltageLevel, E)])

      val (parsedIdList, parsedVoltLvlList) =
        configElements.foldLeft(emptyLists) { case ((ids, lvls), config) =>
          val element = elementFcn(config)

          val parsedGridIds = gridIds(config).getOrElse(List.empty).flatMap {
            case ConfigConventions.gridIdDotRange(from, to) =>
              (from.toInt to to.toInt)
                .map(gridId => (gridId, element))
            case ConfigConventions.gridIdMinusRange(from, to) =>
              (from.toInt to to.toInt)
                .map(gridId => (gridId, element))
            case ConfigConventions.singleGridId(singleGridId) =>
              Seq((singleGridId.toInt, element))
            case unknownGridIdFormat =>
              throw new InvalidConfigParameterException(
                s"Unknown gridId format $unknownGridIdFormat provided for grid config: $config"
              )
          }

          val parsedVoltLvls =
            voltLvls(config).getOrElse(List.empty).map { voltLvlDef =>
              (VoltLvlParser.from(voltLvlDef), element)
            }

          (
            ids ++ parsedGridIds,
            lvls ++ parsedVoltLvls,
          )
        }

      if CollectionUtils.listHasDuplicates(parsedIdList) then {
        throw new InvalidConfigParameterException(
          s"The provided gridIds in simona.gridConfig.$gridConfigType contain duplicates. " +
            "Please check if there are either duplicate entries or overlapping ranges!"
        )
      }

      if CollectionUtils.listHasDuplicates(parsedVoltLvlList) then
        throw new InvalidConfigParameterException(
          s"The provided voltLvls in simona.gridConfig.$gridConfigType contain duplicates. " +
            "Please check your configuration for duplicates in voltLvl entries!"
        )

      builder(parsedIdList.toMap, parsedVoltLvlList.toMap)
    case _ =>
      defaults
  }
}
