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
import edu.ie3.simona.model.control.TransformerControlGroupModel.RegulationCriterion
import squants.{Dimensionless, Each}

import java.util.UUID
import scala.jdk.CollectionConverters._

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
final case class TransformerControlGroupModel(
    nodalRegulationCriterion: Map[UUID, RegulationCriterion],
    harmonizeRegulationNeeds: Array[Dimensionless] => Option[Dimensionless],
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

object TransformerControlGroupModel {
  type RegulationCriterion =
    Complex => Option[Dimensionless]

  /** Build business models for control groups
    *
    * @param config
    *   List of configs for control groups
    * @param measurementUnitInput
    *   Set of [[MeasurementUnitInput]] s
    * @return
    *   A set of control group business models
    */
  def buildControlGroups(
      config: List[SimonaConfig.TransformerControlGroup],
      measurementUnitInput: java.util.Set[MeasurementUnitInput],
  ): Set[TransformerControlGroupModel] = config.map {
    case TransformerControlGroup(measurements, _, vMax, vMin) =>
      buildTransformerControlGroupModel(
        measurementUnitInput,
        measurements.toSet,
        vMax,
        vMin,
      )
  }.toSet

  /** Build a single control group model. Currently, only limit violation
    * prevention logic is captured: The nodal regulation need is equal to the
    * voltage change needed to comply with the given thresholds
    *
    * @param measurementUnitInput
    *   Collection of all known [[MeasurementUnitInput]] s
    * @param measurementConfigs
    *   Collection of all uuids, denoting which of the [[MeasurementUnitInput]]
    *   s does belong to this control group
    * @param vMax
    *   Upper permissible voltage magnitude
    * @param vMin
    *   Lower permissible voltage magnitude
    * @return
    *   A [[TransformerControlGroupModel]]
    */
  private def buildTransformerControlGroupModel(
      measurementUnitInput: java.util.Set[MeasurementUnitInput],
      measurementConfigs: Set[String],
      vMax: Double,
      vMin: Double,
  ): TransformerControlGroupModel = {
    val nodeUuids =
      determineNodeUuids(measurementUnitInput, measurementConfigs)
    buildTransformerControlModels(nodeUuids, vMax, vMin)
  }

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
      measurementUnitInput: java.util.Set[MeasurementUnitInput],
      measurementConfigs: Set[String],
  ): Set[UUID] = Set.from(
    measurementUnitInput.asScala
      .filter(input =>
        measurementConfigs.contains(input.getUuid.toString) && input.getVMag
      )
      .map(_.getNode.getUuid)
  )

  private def regulationFunction(
      complexVoltage: Complex,
      vMax: Double,
      vMin: Double,
  ): Option[Dimensionless] = {
    val vMag = complexVoltage.abs
    vMag match {
      case mag if mag > vMax =>
        Some(vMax - mag).map(Each(_))
      case mag if mag < vMin =>
        Some(vMin - mag).map(Each(_))
      case _ => None
    }
  }

  private def harmonizationFunction
      : Array[Dimensionless] => Option[Dimensionless] =
    (regulationRequests: Array[Dimensionless]) => {
      val negativeRequests = regulationRequests.array.filter(_ < Each(0d))
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

  /** Build a single control group model. Currently, only limit violation
    * prevention logic is captured: The nodal regulation need is equal to the
    * voltage change needed to comply with the given thresholds
    *
    * @param nodeUuids
    *   Collection of all relevant node uuids
    * @param vMax
    *   Upper permissible voltage magnitude
    * @param vMin
    *   Lower permissible voltage magnitude
    * @return
    *   A [[TransformerControlGroupModel]]
    */
  private def buildTransformerControlModels(
      nodeUuids: Set[UUID],
      vMax: Double,
      vMin: Double,
  ): TransformerControlGroupModel = {
    /* Determine the voltage regulation criterion for each of the available nodes */
    val voltage = Complex(1.0, 0.0)
    val nodeUuidToRegulationCriterion = nodeUuids.map { uuid =>
      uuid -> regulationFunction(voltage, vMax, vMin)
    }.toMap

    TransformerControlGroupModel(
      nodeUuidToRegulationCriterion,
      harmonizationFunction,
    )
  }
}