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
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.util.CollectionUtils
import edu.ie3.util.quantities.PowerSystemUnits
import squants.electro.{Kilovolts, Volts}
import squants.energy.{Kilowatts, Megawatts}

/** Parser to parse [[RefSystem]] provided via [[SimonaConfig]]
  */
object RefSystemParser {
  final case class ConfigRefSystems(
      private val gridIdRefSystems: Map[Int, RefSystem],
      private val voltLvLRefSystems: Map[VoltageLevel, RefSystem],
  ) {

    /** Returns a [[RefSystem]] based on the provided gridId or the voltLvl as
      * fallback if available
      *
      * @param gridId
      *   the gridId the refSystem is wanted for
      * @param voltLvl
      *   the voltLvL that is valid for the grid that is wanted
      * @return
      *   Some(refSystem) if available or None if unavailable
      */
    def find(
        gridId: Int,
        voltLvl: Option[VoltageLevel] = None,
    ): Option[RefSystem] =
      gridIdRefSystems
        .get(gridId)
        .orElse(voltLvl.flatMap(voltLvLRefSystems.get))

  }

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
      configRefSystems: Option[Seq[SimonaConfig.RefSystemConfig]]
      /*fixme mh Option(dev) oder Seq
      configRefSystems: Seq[SimonaConfig.RefSystemConfig]

       */
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

    configRefSystems match {
      case Some(refSystems) if refSystems.nonEmpty =>
        // units for parsing are not initialized by default
        // hence we call them manually
        new PowerSystemUnits

        val parsedRefSystems = refSystems.flatMap { configRefSystem =>
          val refSystem = RefSystem(configRefSystem.sNom, configRefSystem.vNom)

          configRefSystem.gridIds.getOrElse(Seq.empty).flatMap {
            case ConfigConventions.gridIdDotRange(from, to) =>
              (from.toInt to to.toInt)
                .map(gridId => (gridId, refSystem))
            case ConfigConventions.gridIdMinusRange(from, to) =>
              (from.toInt to to.toInt)
                .map(gridId => (gridId, refSystem))
            case ConfigConventions.singleGridId(singleGridId) =>
              Seq((singleGridId.toInt, refSystem))
            case unknownGridIdFormat =>
              throw new InvalidConfigParameterException(
                s"Unknown gridId format $unknownGridIdFormat provided for refSystem $configRefSystem"
              )
          } ++ configRefSystem.voltLvls.getOrElse(Seq.empty).map { voltLvlDef =>
            (VoltLvlParser.from(voltLvlDef), refSystem)
          }
        }

        val gridIdRefSystemsList: List[(Int, RefSystem)] =
          parsedRefSystems.flatMap {
            case (gridId: Int, refSystems) =>
              refSystems match {
                case refSystem: RefSystem => Some(gridId -> refSystem)
                case _                    => None
              }
            case _ => None
          }

        val gridIdRefSystems: Map[Int, RefSystem] =
          gridIdRefSystemsList.toMap

        if (CollectionUtils.listHasDuplicates(gridIdRefSystemsList)) {
          throw new InvalidConfigParameterException(
            "The provided gridIds in simona.gridConfig.refSystems contain duplicates. " +
              "Please check if there are either duplicate entries or overlapping ranges!"
          )
        }

        val voltLvLRefSystemsList: List[(VoltageLevel, RefSystem)] =
          parsedRefSystems.flatMap {
            case (voltLvl: VoltageLevel, refSystems) =>
              refSystems match {
                case refSystem: RefSystem => Some(voltLvl -> refSystem)
                case _                    => None
              }
            case _ => None
          }

        if (CollectionUtils.listHasDuplicates(voltLvLRefSystemsList)) {
          throw new InvalidConfigParameterException(
            "The provided voltLvls in simona.gridConfig.refSystems contain duplicates. " +
              "Please check your configuration for duplicates in voltLvl entries!"
          )
          /*fixme mh für seq
        }
         check for duplicates of gridIds and voltLevels which will be the key for the following map conversion
    if (
      CollectionUtils.seqHasDuplicates(
        gridIdRefSystems.map { case (gridId, _) => gridId }
      )
    )
      throw new InvalidConfigParameterException(
        s"The provided gridIds in simona.gridConfig.refSystems contains duplicates. " +
          s"Please check if there are either duplicate entries or overlapping ranges!"
      )
    if (
      CollectionUtils.seqHasDuplicates(
        voltLvlRefSystems.map { case (voltLvl, _) => voltLvl }
      )
    )
      throw new InvalidConfigParameterException(
        s"The provided voltLvls in simona.gridConfig.refSystems contains duplicates. " +
          s"Please check your configuration for duplicates in voltLvl entries!"
      )

           */

        val voltLvLRefSys: Map[VoltageLevel, RefSystem] =
          parsedRefSystems.collect { case (voltLvl: VoltageLevel, values) =>
            (voltLvl, values)
          }.toMap

        ConfigRefSystems(gridIdRefSystems, voltLvLRefSys)
      case _ => defaultRefSystems
    }
  }
}
