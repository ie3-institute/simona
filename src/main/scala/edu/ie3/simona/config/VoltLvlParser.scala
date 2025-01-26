/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import edu.ie3.datamodel.exceptions.VoltageLevelException
import edu.ie3.datamodel.models.voltagelevels.{
  CommonVoltageLevel,
  GermanVoltageLevelUtils,
  VoltageLevel,
}
import edu.ie3.simona.exceptions.InvalidConfigParameterException
import edu.ie3.util.quantities.QuantityUtil
import javax.measure.quantity.ElectricPotential
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

/** Parses voltage level elements
  */
object VoltLvlParser {

  /** Create a voltage level definition from config entry
    *
    * @param config
    *   Config entry to parse
    * @return
    *   A suitable [[VoltageLevel]]
    */
  def from(config: SimonaConfig.VoltLvlConfig): VoltageLevel = {
    val vNom = parseNominalVoltage(config.vNom)
    parse(config.id, vNom)
  }

  /** Looks up a common voltage level with the given parameters
    *
    * @param id
    *   Identifier
    * @param vNominal
    *   Nominal voltage
    * @return
    *   The common voltage level
    */
  private def parse(
      id: String,
      vNominal: ComparableQuantity[ElectricPotential],
  ): CommonVoltageLevel =
    try {
      GermanVoltageLevelUtils.parse(id, vNominal)
    } catch {
      case vle: VoltageLevelException =>
        throw new InvalidConfigParameterException(
          s"Cannot find a common voltage level with id $id and nominal voltage $vNominal",
          vle,
        )
    }

  /** Parses a given quantity string to a nominal voltage
    *
    * @param quantString
    *   Quantity string denoting a voltage
    * @return
    *   The nominal voltage as quantity
    */
  private def parseNominalVoltage(
      quantString: String
  ): ComparableQuantity[ElectricPotential] = {
    try {
      QuantityUtil.asComparable(
        Quantities.getQuantity(quantString).asType(classOf[ElectricPotential])
      )
    } catch {
      case iae: IllegalArgumentException =>
        throw new InvalidConfigParameterException(
          s"Cannot parse the nominal voltage $quantString",
          iae,
        )
      case cce: ClassCastException =>
        throw new InvalidConfigParameterException(
          s"Cannot parse $quantString to nominal voltage, as it is no voltage.",
          cce,
        )
    }
  }
}
