/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.control

import breeze.math.Complex
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult
import edu.ie3.simona.model.control.TransformerControlGroup.RegulationCriterion

import java.util.UUID
import squants.Dimensionless

/** Business logic for a transformer control group. It's main purpose is to
  * determine, if there is any regulation need and if yes, to what extent (here:
  * voltage raise or reduction to achieve)
  *
  * @param nodalRegulationCriterion
  *   Mapping from nodal index to a partial function, that determines the
  *   regulation need at this node
  * @param harmonizeRegulationNeeds
  *   Partial function to harmonize different, possible contradictory regulation
  *   needs
  */
final case class TransformerControlGroup(
    nodalRegulationCriterion: Map[UUID, RegulationCriterion],
    harmonizeRegulationNeeds: Array[Dimensionless] => Option[Dimensionless]
) {

  /** Based on the given successful power flow result, determine the difference
    * in voltage magnitude, that needs to be achieved by regulating the
    * transformer tap position
    *
    * @param result
    *   Power flow result to account for
    * @param uuidToIndex
    *   Mapping from node's uuid to nodal index
    * @return
    *   Optional voltage magnitude, that a transformer tap regulation needs to
    *   achieve
    */
  def determineRegulationNeed(
      result: SuccessFullPowerFlowResult,
      uuidToIndex: Map[UUID, Int]
  ): Option[Dimensionless] = {
    val regulationNeeds = result.nodeData.flatMap {
      case StateData(resultNodeIndex, _, voltage, _) =>
        /* Find possible matching criterion and evaluate it */
        nodalRegulationCriterion
          .find { case (uuid, _) =>
            val index = uuidToIndex(uuid)
            index == resultNodeIndex
          }
          .map { case (_, criterion) =>
            criterion(voltage)
          }
    }.flatten
    Option
      .when(regulationNeeds.nonEmpty)(
        harmonizeRegulationNeeds(regulationNeeds)
      )
      .flatten
  }
}

object TransformerControlGroup {
  type RegulationCriterion =
    Complex => Option[Dimensionless]
}
