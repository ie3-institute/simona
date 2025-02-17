/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant2.load

import edu.ie3.simona.config.RuntimeConfig.LoadRuntimeConfig
import edu.ie3.util.StringUtils

/** Denoting difference referencing scenarios for scaling load model output
  */
sealed trait LoadReferenceType

object LoadReferenceType {

  /** Scale the load model behaviour so that the rated power of the load model
    * serves as the maximum power consumption
    */
  case object ActivePower extends LoadReferenceType

  /** Scale the load model behaviour so that the aggregate annual energy
    * consumption corresponds to the energy set by the model input
    */
  case object EnergyConsumption extends LoadReferenceType

  /** Build a reference type, that denotes to which type of reference a load
    * model behaviour might be scaled.
    *
    * @param modelConfig
    *   Configuration of model behaviour
    * @return
    *   A [[LoadReferenceType]] for use in [[LoadModel]]
    */
  def apply(
      modelConfig: LoadRuntimeConfig
  ): LoadReferenceType =
    StringUtils.cleanString(modelConfig.reference).toLowerCase match {
      case "power" =>
        LoadReferenceType.ActivePower
      case "energy" =>
        LoadReferenceType.EnergyConsumption
      case unsupported =>
        throw new IllegalArgumentException(
          s"Unsupported load reference type '$unsupported'."
        )
    }
}
