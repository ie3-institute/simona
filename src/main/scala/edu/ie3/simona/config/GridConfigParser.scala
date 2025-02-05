/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.VoltageLevel
import edu.ie3.simona.config.SimonaConfig.VoltLvlConfig
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.util.CollectionUtils
import edu.ie3.util.quantities.PowerSystemUnits

object GridConfigParser {
  abstract class GridConfig[GC](
      protected val gridIdMap: Map[Int, GC],
      protected val voltLvlMap: Map[VoltageLevel, GC],
  ) {

    /** Returns a [[GC]] based on the provided gridId or the voltLvl as fallback
      * if available
      *
      * @param gridId
      *   the gridId the refSystem is wanted for
      * @param voltLvl
      *   the voltLvL that is valid for the grid that is wanted
      * @return
      *   Some(refSystem) if available or None if unavailable
      */
    final def find(
        gridId: Int,
        voltLvl: Option[VoltageLevel] = None,
    ): Option[GC] =
      gridIdMap
        .get(gridId)
        .orElse(voltLvl.flatMap(voltLvlMap.get))
  }

  def parseWithDefaults[C, E, GC](
      configs: Option[List[C]],
      gridIds: C => Option[List[String]],
      voltLvls: C => Option[List[VoltLvlConfig]],
      elementFcn: C => E,
      builder: (Map[Int, E], Map[VoltageLevel, E]) => GC,
      defaults: GC,
  )(implicit gridConfigType: String): GC = configs match {
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
                s"Unknown gridId format $unknownGridIdFormat provided for $gridConfigType $config"
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

      if (CollectionUtils.listHasDuplicates(parsedIdList)) {
        throw new InvalidConfigParameterException(
          s"The provided gridIds in simona.gridConfig.${gridConfigType}s contain duplicates. " +
            "Please check if there are either duplicate entries or overlapping ranges!"
        )
      }

      if (CollectionUtils.listHasDuplicates(parsedVoltLvlList))
        throw new InvalidConfigParameterException(
          s"The provided voltLvls in simona.gridConfig.${gridConfigType}s contain duplicates. " +
            "Please check your configuration for duplicates in voltLvl entries!"
        )

      builder(parsedIdList.toMap, parsedVoltLvlList.toMap)
    case _ =>
      defaults
  }
}
