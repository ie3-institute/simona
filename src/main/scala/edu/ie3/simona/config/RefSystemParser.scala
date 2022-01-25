/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.models.voltagelevels.VoltageLevel
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.util.CollectionUtils
import edu.ie3.util.quantities.PowerSystemUnits

/** Parser to parse [[RefSystem]] provided via [[SimonaConfig]]
  */
object RefSystemParser {

  final case class ConfigRefSystems(
      private val gridIdRefSystems: Map[Int, RefSystem],
      private val voltLvLRefSystems: Map[VoltageLevel, RefSystem]
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
        voltLvl: Option[VoltageLevel] = None
    ): Option[RefSystem] = {

      gridIdRefSystems.get(gridId) match {
        case None =>
          voltLvl match {
            case Some(voltLvl) => voltLvLRefSystems.get(voltLvl)
            case None          => None
          }
        case refSystemOpt @ Some(_) => refSystemOpt
      }
    }

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
      configRefSystems: List[SimonaConfig.RefSystemConfig]
  ): ConfigRefSystems = {

    // units for parsing are not initialized by default
    // hence we call them manually
    new PowerSystemUnits

    val gridIdRefSystems: Vector[(Int, RefSystem)] = configRefSystems.foldLeft(
      Vector.empty[(Int, RefSystem)]
    )((gridIdRefSystemsAccumulator, configRefSystem) => {
      val refSystem = RefSystem(configRefSystem.sNom, configRefSystem.vNom)

      val gridIdRefSystems = configRefSystem.gridIds match {
        case Some(gridIds) =>
          gridIds.foldLeft(Vector.empty[(Int, RefSystem)])(
            (gridIdRefSystems, gridId) => {

              val gridIds: Vector[Int] = gridId match {
                case ConfigConventions.gridIdDotRange(from, to) =>
                  (from.toInt to to.toInt).toVector
                case ConfigConventions.gridIdMinusRange(from, to) =>
                  (from.toInt to to.toInt).toVector
                case ConfigConventions.singleGridId(singleGridId) =>
                  Vector(singleGridId.toInt)
                case unknownGridIdFormat =>
                  throw new InvalidConfigParameterException(
                    s"Unknown gridId format $unknownGridIdFormat provided for refSystem $configRefSystem"
                  )
              }

              gridIdRefSystems ++ gridIds.map(gridId => (gridId, refSystem))
            }
          )
        case None => List.empty[(Int, RefSystem)]
      }

      gridIdRefSystemsAccumulator ++ gridIdRefSystems
    })

    val voltLvlRefSystems: Vector[(VoltageLevel, RefSystem)] =
      configRefSystems.foldLeft(
        Vector.empty[(VoltageLevel, RefSystem)]
      )((voltLvlRefSystemsAccumulator, configRefSystem) => {
        val refSystem = RefSystem(configRefSystem.sNom, configRefSystem.vNom)

        val voltLvlRefSystems = configRefSystem.voltLvls match {
          case Some(voltLvls) =>
            voltLvls.foldLeft(Vector.empty[(VoltageLevel, RefSystem)])(
              (voltLvlRefSystems, voltLvlDef) => {
                val voltLvl = voltLvlDef match {
                  case SimonaConfig.VoltLvlConfig(id, vNom) =>
                    VoltLvlParser.parse(id, vNom)
                  case invalid =>
                    throw new InvalidConfigParameterException(
                      s"Got invalid voltage level definition $invalid. Double-check your entry!"
                    )
                }
                voltLvlRefSystems :+ (voltLvl, refSystem)
              }
            )
          case None => List.empty[(VoltageLevel, RefSystem)]
        }
        voltLvlRefSystemsAccumulator ++ voltLvlRefSystems
      })

    // check for duplicates on each list tuple_.1 which will be the key for the following map conversion
    if (
      CollectionUtils.listHasDuplicates(
        gridIdRefSystems.map(gridIdRefSystem => gridIdRefSystem._1).toList
      )
    )
      throw new InvalidConfigParameterException(
        s"The provided gridIds in simona.gridConfig.refSystems contains duplicates. " +
          s"Please check if there are either duplicate entries or overlapping ranges!"
      )
    if (
      CollectionUtils.listHasDuplicates(
        voltLvlRefSystems.map(gridIdRefSystem => gridIdRefSystem._1).toList
      )
    )
      throw new InvalidConfigParameterException(
        s"The provided voltLvls in simona.gridConfig.refSystems contains duplicates. " +
          s"Please check your configuration for duplicates in voltLvl entries!"
      )

    ConfigRefSystems(gridIdRefSystems.toMap, voltLvlRefSystems.toMap)
  }

}
