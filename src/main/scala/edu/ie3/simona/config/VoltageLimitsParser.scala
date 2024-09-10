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
import edu.ie3.util.quantities.PowerSystemUnits

/** Parser to parse [[VoltageLimits]] provided via [[SimonaConfig]]
  */
object VoltageLimitsParser {

  final case class ConfigVoltageLimits(
      private val gridIdVoltageLimits: Map[Int, VoltageLimits],
      private val voltLvLVoltageLimits: Map[VoltageLevel, VoltageLimits],
  ) {

    /** Returns a [[VoltageLimits]] based on the provided gridId or the voltLvl
      * as fallback if available
      *
      * @param gridId
      *   the gridId the refSystem is wanted for
      * @param voltLvl
      *   the voltLvL that is valid for the grid that is wanted
      * @return
      *   Some(voltageLimits) if available or None if unavailable
      */
    def find(
        gridId: Int,
        voltLvl: Option[VoltageLevel] = None,
    ): Option[VoltageLimits] = gridIdVoltageLimits
      .get(gridId)
      .orElse(voltLvl.flatMap(voltLvLVoltageLimits.get))
  }

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

    configVoltageLimits match {
      case Some(voltageLimits) if voltageLimits.nonEmpty =>
        // units for parsing are not initialized by default
        // hence we call them manually
        new PowerSystemUnits

        val parsedVoltageLimits = voltageLimits.flatMap { configVoltageLimit =>
          val voltageLimits =
            VoltageLimits(configVoltageLimit.vMin, configVoltageLimit.vMax)

          configVoltageLimit.gridIds.getOrElse(Seq.empty).flatMap {
            case ConfigConventions.gridIdDotRange(from, to) =>
              from.toInt to to.toInt
            case ConfigConventions.gridIdMinusRange(from, to) =>
              from.toInt to to.toInt
            case ConfigConventions.singleGridId(singleGridId) =>
              Seq(singleGridId.toInt)
            case unknownGridIdFormat =>
              throw new InvalidConfigParameterException(
                s"Unknown gridId format $unknownGridIdFormat provided for voltage limits $configVoltageLimit"
              )
          } ++ configVoltageLimit.voltLvls.getOrElse(Seq.empty).map {
            voltLvlDef =>
              (VoltLvlParser.from(voltLvlDef), voltageLimits)
          }
        }

        val gridIdVoltageLimitsList: List[(Int, VoltageLimits)] =
          parsedVoltageLimits.flatMap {
            case (gridId: Int, refSystems) =>
              refSystems match {
                case voltageLimits: VoltageLimits =>
                  Some(gridId -> voltageLimits)
                case _ => None
              }
            case _ => None
          }

        val gridIdVoltageLimits: Map[Int, VoltageLimits] =
          gridIdVoltageLimitsList.toMap

        if (CollectionUtils.listHasDuplicates(gridIdVoltageLimitsList)) {
          throw new InvalidConfigParameterException(
            s"The provided gridIds in simona.gridConfig.voltageLimits contains duplicates. " +
              s"Please check if there are either duplicate entries or overlapping ranges!"
          )
        }

        val voltLvLVoltageLimitsList: List[(VoltageLevel, VoltageLimits)] =
          parsedVoltageLimits.flatMap {
            case (voltLvl: VoltageLevel, refSystems) =>
              refSystems match {
                case voltageLimits: VoltageLimits =>
                  Some(voltLvl -> voltageLimits)
                case _ => None
              }
            case _ => None
          }

        if (CollectionUtils.listHasDuplicates(voltLvLVoltageLimitsList))
          throw new InvalidConfigParameterException(
            s"The provided voltLvls in simona.gridConfig.voltageLimits contains duplicates. " +
              s"Please check your configuration for duplicates in voltLvl entries!"
          )

        val voltLvLVoltageLimits: Map[VoltageLevel, VoltageLimits] =
          parsedVoltageLimits.collect {
            case (voltLvl: VoltageLevel, values: VoltageLimits) =>
              (voltLvl, values)
          }.toMap

        ConfigVoltageLimits(gridIdVoltageLimits, voltLvLVoltageLimits)

      case _ => defaultVoltageLimits
    }
  }

}
