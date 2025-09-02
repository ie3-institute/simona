/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.control

import breeze.math.Complex
import edu.ie3.datamodel.models.input.MeasurementUnitInput
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.config.SimonaConfig.TransformerControlGroup
import edu.ie3.simona.model.control.TransformerControlGroupModel.{
  RegulationCriterion,
  harmonizeRegulationNeeds,
}
import squants.{Dimensionless, Each}

import java.util.UUID

/** Business logic for a transformer control group. It's main purpose is to
  * determine, if there is any regulation need and if yes, to what extent (here:
  * voltage raise or reduction to achieve)
  *
  * @param measuredNodes
  *   The nodes (with voltage measurement) to consider
  * @param regulationCriterion
  *   Function that determines the regulation need
  */
final case class TransformerControlGroupModel(
    measuredNodes: Set[UUID],
    regulationCriterion: RegulationCriterion,
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
      uuidToIndex: Map[UUID, Int],
  ): Option[Dimensionless] = {
    val regulationNeeds = result.nodeData
      .filter { case StateData(resultNodeIndex, _, _, _) =>
        measuredNodes.exists { uuid =>
          resultNodeIndex == uuidToIndex(uuid)
        }
      }
      .flatMap { case StateData(_, _, voltage, _) =>
        regulationCriterion(voltage)
      }
    harmonizeRegulationNeeds(regulationNeeds)
  }
}

object TransformerControlGroupModel {
  type RegulationCriterion =
    Complex => Option[Dimensionless]

  /** Build business models for control groups
    *
    * @param measurementUnitInput
    *   Set of [[MeasurementUnitInput]] s
    * @param config
    *   List of configs for control groups
    * @return
    *   A set of control group business models
    */
  def buildControlGroups(
      measurementUnitInput: Set[MeasurementUnitInput],
      config: Iterable[SimonaConfig.TransformerControlGroup],
  ): Set[TransformerControlGroupModel] = config.map {
    case TransformerControlGroup(measurements, _, vMax, vMin) =>
      val nodeUuids =
        determineNodeUuids(measurementUnitInput, measurements.toSet)
      TransformerControlGroupModel(nodeUuids, regulationFunction(vMax, vMin))
  }.toSet

  /** Determine the uuids of the nodes to control
    *
    * @param measurementUnitInput
    *   Collection of all known [[MeasurementUnitInput]] s
    * @param measurementConfigs
    *   Collection of all uuids, denoting which of the [[MeasurementUnitInput]]
    *   s does belong to this control group
    * @return
    *   A set of relevant nodal uuids
    */
  private def determineNodeUuids(
      measurementUnitInput: Set[MeasurementUnitInput],
      measurementConfigs: Set[String],
  ): Set[UUID] =
    measurementUnitInput
      .filter(input =>
        measurementConfigs.contains(input.getUuid.toString) && input.getVMag
      )
      .map(_.getNode.getUuid)

  /** Determine the regulation criterion of the nodes to control
    *
    * @param vMax
    *   Maximum voltage limit
    * @param vMin
    *   Minimum voltage limit
    * @return
    *   The regulation need, if applicable
    */
  private def regulationFunction(
      vMax: Double,
      vMin: Double,
  ): RegulationCriterion = { (voltage: Complex) =>
    voltage.abs match {
      case vMag if vMag > vMax =>
        Some(vMax - vMag).map(Each(_))
      case vMag if vMag < vMin =>
        Some(vMin - vMag).map(Each(_))
      case _ => None
    }
  }

  /** Function to harmonize contrary requests for regulation
    *
    * @param regulationRequests:
    *   Array of all regulation requests
    * @return
    *   None in case of contrary requests, else the highest or lowest voltage
    *   depending on the direction for regulation
    */
  private def harmonizeRegulationNeeds(
      regulationRequests: Array[Dimensionless]
  ): Option[Dimensionless] = {
    val negativeRequests = regulationRequests.filter(_ < Each(0d))
    val positiveRequests = regulationRequests.filter(_ > Each(0d))

    (negativeRequests.nonEmpty, positiveRequests.nonEmpty) match {
      case (true, true) =>
        /* There are requests for higher and lower voltages at the same time => do nothing! */
        None
      case (true, false) =>
        /* There are only requests for lower voltages => decide for the lowest required voltage */
        negativeRequests.minOption
      case (false, true) =>
        /* There are only requests for higher voltages => decide for the highest required voltage */
        positiveRequests.maxOption
      case _ =>
        None
    }

  }

}
