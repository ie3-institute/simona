/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.ActorRef

import java.util.UUID
import breeze.math.Complex
import edu.ie3.datamodel.models.input.connector.Transformer2WInput
import edu.ie3.powerflow.NewtonRaphsonPF
import edu.ie3.powerflow.model.NodeData.{PresetData, StateData}
import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.powerflow.model.StartData.WithForcedStartVoltages
import edu.ie3.powerflow.model.enums.NodeType
import edu.ie3.simona.agent.grid.ReceivedValues.ReceivedSlackValues
import edu.ie3.simona.exceptions.agent.DBFSAlgorithmException
import edu.ie3.simona.model.grid.{
  GridModel,
  NodeModel,
  RefSystem,
  Transformer3wModel,
  TransformerModel
}
import edu.ie3.simona.ontology.messages.PowerMessage
import edu.ie3.simona.ontology.messages.PowerMessage.ProvidePowerMessage
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity

import javax.measure.Quantity
import javax.measure.quantity.{Dimensionless, ElectricPotential, Power}
import tech.units.indriya.quantity.Quantities

import scala.annotation.tailrec

/** Support and helper methods for power flow calculations provided by
  * [[edu.ie3.powerflow]]
  */
trait PowerFlowSupport {
  this: GridAgent =>

  /** Composes the current operation point needed by
    * [[edu.ie3.powerflow.NewtonRaphsonPF.calculate()]]
    *
    * @param nodes
    *   node models in the grid under investigation
    * @param transformers2w
    *   two winding transformer models in the grid under investigation
    * @param transformers3w
    *   three winding transformer models in the grid under investigation
    * @param nodeUuidToIndexMap
    *   mapping of the uuid of the node models to their index in the admittance
    *   matrix
    * @param receivedValuesStore
    *   instance of [[ReceivedValuesStore]] containing all asset and grid load
    *   information as well the slack voltage information for the grid under
    *   investigation
    * @param gridMainRefSystem
    *   instance of [[RefSystem]] of the grid under investigation
    * @param targetVoltageFromReceivedData
    *   Determine target voltage for power flow from received messages
    * @param ignoreTargetVoltage
    *   trigger that allows ignoring the target voltage for slack nodes
    *   (normally used when no slack information are provided via
    *   receivedValuesStore)
    * @return
    *   current operating point of the grid to be used with
    *   [[edu.ie3.powerflow.NewtonRaphsonPF.calculate()]] as well as the complex
    *   slack node target voltages
    */
  protected def composeOperatingPoint(
      nodes: Set[NodeModel],
      transformers2w: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      nodeUuidToIndexMap: Map[UUID, Int],
      receivedValuesStore: ReceivedValuesStore,
      gridMainRefSystem: RefSystem,
      targetVoltageFromReceivedData: Boolean = true,
      ignoreTargetVoltage: Boolean = false
  ): (Array[PresetData], WithForcedStartVoltages) =
    nodes.foldLeft((Array.empty[PresetData], Array.empty[StateData])) {
      case ((operatingPoint, stateData), nodeModel) =>
        // note: currently we only support pq nodes as we not distinguish between pq/pv nodes -
        // when slack emulators or pv-node assets are added this needs to be considered here

        /* Determine the operating point for this given node */
        val nodeIdx = nodeUuidToIndexMap.getOrElse(
          nodeModel.uuid,
          throw new RuntimeException(
            s"Received data for node ${nodeModel.id} [${nodeModel.uuid}] which is not in my nodeUuidToIndexMap!"
          )
        )

        /* Determine the nodal residual apparent power */
        val apparentPower = determineNodalApparentPower(
          receivedValuesStore.nodeToReceivedPower.get(nodeModel.uuid),
          nodeModel.id,
          gridMainRefSystem
        )

        /* Determine node type, target voltage and updated state data (dependent on the node type) */
        nodeTypeTargetVoltageStateData(
          nodeModel,
          nodeIdx,
          receivedValuesStore,
          transformers2w,
          transformers3w,
          apparentPower,
          stateData,
          gridMainRefSystem,
          targetVoltageFromReceivedData,
          ignoreTargetVoltage
        ) match {
          case (nodeType, targetVoltage, stateData) =>
            (
              operatingPoint :+ PresetData(
                nodeIdx,
                nodeType,
                apparentPower,
                targetVoltage.abs
              ),
              stateData
            )
        }
    } match {
      case (operatingPoint, stateData) =>
        (operatingPoint, WithForcedStartVoltages(stateData))
    }

  /** Determine the residual apparent nodal power balance for a given node.
    *
    * @param receivedPower
    *   Optional vector of received power messages
    * @param nodeId
    *   Id of the node (for debugging reasons)
    * @param refSystem
    *   Main reference system of the grid
    * @return
    */
  private def determineNodalApparentPower(
      receivedPower: Option[
        Vector[(ActorRef, Option[PowerMessage.PowerResponseMessage])]
      ],
      nodeId: String,
      refSystem: RefSystem
  ): Complex = receivedPower match {
    case Some(actorRefsWithPower) =>
      val powerUnit = refSystem.nominalPower.getUnit
      val (p, q) = actorRefsWithPower
        .map(_._2)
        .collect {
          case Some(providePowerMessage: ProvidePowerMessage) =>
            providePowerMessage
          case Some(message) =>
            throw new RuntimeException(
              s"Received message $message which cannot processed here!"
            )
          case None =>
            throw new RuntimeException(
              s"Did not receive all power values I expected @ node $nodeId. This is a fatal and should never happen!"
            )
        }
        .foldLeft(
          (
            Quantities.getQuantity(0, powerUnit),
            Quantities.getQuantity(0, powerUnit)
          )
        )((pqSum, powerMessage) => {
          (pqSum._1.add(powerMessage.p), pqSum._2.add(powerMessage.q))
        })

      new Complex(
        refSystem.pInPu(p).getValue.doubleValue(),
        refSystem.qInPu(q).getValue.doubleValue()
      )
    case None => new Complex(0, 0)
  }

  /** Determine a target voltage if no received values are meant to be taken
    *
    * @param nodeModel
    *   The nodal model
    * @param ignoreTargetVoltage
    *   Whether or not to ignore the nodal target voltage provided by input
    *   model
    * @return
    *   A complex nodal target voltage
    */
  private def nodalTargetVoltage(
      nodeModel: NodeModel,
      ignoreTargetVoltage: Boolean
  ): Complex = Complex.one * (if (!ignoreTargetVoltage)
                                nodeModel.vTarget
                                  .to(PowerSystemUnits.PU)
                                  .getValue
                                  .doubleValue()
                              else 1.0)

  /** Determine node type, target voltage and state data (dependent of the node
    * type)
    *
    * @param nodeModel
    *   Complete node model
    * @param nodeIndex
    *   Index of the node
    * @param receivedValuesStore
    *   Store of received data
    * @param transformers2w
    *   Set of two winding transformer models
    * @param transformers3w
    *   Set of three winding transformer models
    * @param residualPower
    *   Relative nodal residual power
    * @param stateData
    *   Current collection of state data
    * @param refSystem
    *   Reference system to use
    * @param targetVoltageFromReceivedData
    *   Whether or not to take target voltage from received data
    * @param ignoreTargetVoltage
    *   Whether or not to neglect the target voltage given in node input
    * @return
    */
  private def nodeTypeTargetVoltageStateData(
      nodeModel: NodeModel,
      nodeIndex: Int,
      receivedValuesStore: ReceivedValuesStore,
      transformers2w: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      residualPower: Complex,
      stateData: Array[StateData],
      refSystem: RefSystem,
      targetVoltageFromReceivedData: Boolean,
      ignoreTargetVoltage: Boolean
  ): (NodeType, Complex, Array[StateData]) = if (nodeModel.isSlack) {
    val targetVoltage = if (targetVoltageFromReceivedData) {
      /* If the preset voltage is meant to be determined by means of received data and the node is a slack node
       * (only then there is received data), look it up and transform it */
      val receivedSlackVoltage =
        receivedValuesStore.nodeToReceivedSlackVoltage.values.flatten
          .find(_.nodeUuid == nodeModel.uuid)
          .getOrElse(
            throw new RuntimeException(
              s"No slack voltage received for node ${nodeModel.id} [${nodeModel.uuid}]!"
            )
          )

      transformVoltage(
        receivedSlackVoltage,
        nodeModel.uuid,
        transformers2w,
        transformers3w,
        refSystem
      )
    } else nodalTargetVoltage(nodeModel, ignoreTargetVoltage)
    val updatedStateData = stateData :+ StateData(
      nodeIndex,
      NodeType.SL,
      targetVoltage,
      residualPower
    )
    (NodeType.SL, targetVoltage, updatedStateData)
  } else {
    (
      NodeType.PQ,
      nodalTargetVoltage(nodeModel, ignoreTargetVoltage),
      stateData
    )
  }

  /** Composes the current operation point needed by
    * [[edu.ie3.powerflow.NewtonRaphsonPF.calculate()]] by reusing the provided
    * p/q values from the provided sweepDataValues and combines them with
    * updated receivedSlackValues. Normally used in a forward sweep phase of
    * [[DBFSAlgorithm]] as in this state only voltages are updated and a power
    * flow with new voltages but old p/q values is executed afterwards
    *
    * @param receivedSlackValues
    *   new slack voltages provided by the superior grid
    * @param sweepDataValues
    *   instance of [[SweepValueStore]] from the previous sweep
    * @param transformers2w
    *   two winding transformer models in the grid under investigation
    * @param transformers3w
    *   three winding transformer models in the grid under investigation
    * @param gridMainRefSystem
    *   instance of [[RefSystem]] of the grid under investigation
    * @return
    *   current operating point of the grid to be used with
    *   [[edu.ie3.powerflow.NewtonRaphsonPF.calculate()]] as well as the complex
    *   slack node target voltages
    */
  protected def composeOperatingPointWithUpdatedSlackVoltages(
      receivedSlackValues: ReceivedSlackValues,
      sweepDataValues: Vector[SweepValueStore.SweepValueStoreData],
      transformers2w: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      gridMainRefSystem: RefSystem
  ): (Array[PresetData], WithForcedStartVoltages) =
    sweepDataValues.foldLeft(Array.empty[PresetData], Array.empty[StateData]) {
      case ((operatingPoint, stateData), sweepValueStoreData) =>
        val nodeStateData = sweepValueStoreData.stateData
        val targetVoltage = if (nodeStateData.nodeType == NodeType.SL) {
          val receivedSlackVoltage = receivedSlackValues.values
            .flatMap(_._2)
            .find(_.nodeUuid == sweepValueStoreData.nodeUuid)
            .getOrElse(
              throw new RuntimeException(
                s"Unable to find node with uuid " +
                  s"${sweepValueStoreData.nodeUuid} in received slack voltage values!"
              )
            )

          transformVoltage(
            receivedSlackVoltage,
            sweepValueStoreData.nodeUuid,
            transformers2w,
            transformers3w,
            gridMainRefSystem
          )
        } else
          Complex.one

        val updatedStateData = if (nodeStateData.nodeType == NodeType.SL) {
          stateData :+ StateData(
            nodeStateData.index,
            nodeStateData.nodeType,
            targetVoltage,
            nodeStateData.power
          )
        } else stateData

        // note: target voltage will be ignored for slack node if provided
        (
          operatingPoint :+ PresetData(
            nodeStateData.index,
            nodeStateData.nodeType,
            nodeStateData.power,
            targetVoltage.abs
          ),
          updatedStateData
        )
    } match {
      case (operatingPoint, stateData) =>
        (operatingPoint, WithForcedStartVoltages(stateData))
    }

  /** A debug method that composes a string with voltage information (in p.u.)
    * from a [[ValidNewtonRaphsonPFResult]]
    *
    * @param validResult
    *   the result that should be converted to a human readable debug string
    * @param gridModel
    *   the grid model this result comes from
    * @return
    *   debug string with the final power flow voltage results @ each node
    */
  protected def composeValidNewtonRaphsonPFResultVoltagesDebugString(
      validResult: ValidNewtonRaphsonPFResult,
      gridModel: GridModel
  ): String = {
    val debugString = new StringBuilder("Power flow result: ")
    validResult.nodeData.foreach(nodeStateData => {
      // get idx
      val idx = nodeStateData.index
      // get nodeUUID
      val uuid = gridModel.nodeUuidToIndexMap
        .find(_._2 == idx)
        .getOrElse(throw new RuntimeException("NODE NOT FOUND REMOVE THIS "))
        ._1
      // get nodeId from UUID
      val nodeId = gridModel.gridComponents.nodes
        .find(_.uuid == uuid)
        .getOrElse(throw new RuntimeException("BLA BLA BLA SHOULD NOT HAPPEn"))
        .id
      debugString
        .append("\n\t")
        .append(nodeId)
        .append(" (")
        .append(uuid)
        .append(")")
        .append("\t-->\t")
        .append(nodeStateData.voltage)
        .append(", |v| = ")
        .append(nodeStateData.voltage.abs)
    })
    debugString.toString()
  }

  /** Depending on if the targeted node of the voltage refers to a two winding
    * or a three winding transformer, transform the received voltage to meet the
    * conventions regarding voltage exchange between different voltage levels.
    *
    * @param receivedSlackVoltage
    *   Message with received slack voltage
    * @param nodeUuid
    *   Unique identifier of the targeted node
    * @param transformers2w
    *   All known two winding transformers
    * @param transformers3w
    *   All known three winding transformers
    * @param gridRefSystem
    *   Reference system of the grid
    * @return
    *   Complex nodal voltage to use
    */
  private def transformVoltage(
      receivedSlackVoltage: ProvideSlackVoltageMessage,
      nodeUuid: UUID,
      transformers2w: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      gridRefSystem: RefSystem
  ): Complex = {
    ((
      transformers2w.find(_.hvNodeUuid == nodeUuid),
      transformers3w.find(_.nodeInternalUuid == nodeUuid)
    ) match {
      case (Some(transformer2w), None) =>
        transferToVoltageLevel(
          receivedSlackVoltage.e,
          receivedSlackVoltage.f,
          transformer2w
        )
      case (None, Some(transformer3w)) =>
        transferToVoltageLevel(
          receivedSlackVoltage.e,
          receivedSlackVoltage.f,
          transformer3w
        )
      case (Some(transformer2w), Some(transformer3w)) =>
        throw new RuntimeException(
          s"The received slack voltage for node $nodeUuid belongs to a two and a " +
            s"three winding transformer at the same time (${transformer2w.uuid} and ${transformer3w.uuid}), " +
            s"which is not possible!"
        )
      case (None, None) =>
        throw new RuntimeException(
          s"Unable to find transformer for slack node $nodeUuid!"
        )
    }) match {
      case (e, f) => toComplex(toPu(e, f, gridRefSystem))
    }
  }

  /** Transfer physical nodal voltage to correct voltage level, respecting the
    * transformer's voltage ratio
    *
    * @param e
    *   real part of the slack voltage
    * @param f
    *   imaginary part of the slack voltage
    * @param transformerModel
    *   transformer model that is connected to the slack voltage on the hv side
    * @return
    *   Transferred physical voltage
    */
  private def transferToVoltageLevel(
      e: Quantity[ElectricPotential],
      f: Quantity[ElectricPotential],
      transformerModel: TransformerModel
  ): (Quantity[ElectricPotential], Quantity[ElectricPotential]) = {
    val voltRatio = transformerModel.voltRatioNominal
    (e.divide(voltRatio), f.divide(voltRatio))
  }

  /** Transfer physical nodal voltage to correct voltage level, respecting the
    * transformer's voltage ratio
    *
    * @param e
    *   real part of the slack voltage
    * @param f
    *   imaginary part of the slack voltage
    * @param transformerModel
    *   transformer model to apply voltage ratio from
    * @return
    *   Transferred physical voltage
    */
  private def transferToVoltageLevel(
      e: Quantity[ElectricPotential],
      f: Quantity[ElectricPotential],
      transformerModel: Transformer3wModel
  ): (Quantity[ElectricPotential], Quantity[ElectricPotential]) = {
    val voltRatio = Transformer3wModel.voltRatio(transformerModel)
    (e.divide(voltRatio), f.divide(voltRatio))
  }

  /** Make the voltage dimensionless
    *
    * @param e
    *   Real part of voltage
    * @param f
    *   Imaginary part of voltage
    * @param gridRefSystem
    *   Reference system of the grid
    * @return
    *   Nodal voltage in [[edu.ie3.util.quantities.PowerSystemUnits#PU]]
    */
  private def toPu(
      e: Quantity[ElectricPotential],
      f: Quantity[ElectricPotential],
      gridRefSystem: RefSystem
  ): (ComparableQuantity[Dimensionless], ComparableQuantity[Dimensionless]) = (
    gridRefSystem.vInPu(e),
    gridRefSystem.vInPu(f)
  )

  /** Build a [[Complex]] from both parts of the voltage
    *
    * @param ef
    *   Tuple of real and imaginary voltage
    * @return
    *   A [[Complex]] from real and imaginary part
    */
  private def toComplex(
      ef: (ComparableQuantity[Dimensionless], ComparableQuantity[Dimensionless])
  ) = ef match {
    case (e, f) =>
      new Complex(e.getValue.doubleValue(), f.getValue.doubleValue())
  }

  /** Prepare input for and perform newton raphson power flow calculation
    *
    * @param gridModel
    *   Model of the grid
    * @param maxIterations
    *   Maximum permissible iterations
    * @param operatingPoint
    *   Current operation point of the grid
    * @param slackVoltages
    *   Complex target voltages of the slack nodes
    * @param epsilons
    *   Ascending ordered list of convergence thresholds for relaxation
    * @return
    *   The result of newton raphson power flow calculation
    */
  @tailrec
  protected final def newtonRaphsonPF(
      gridModel: GridModel,
      maxIterations: Int,
      operatingPoint: Array[PresetData],
      slackVoltages: WithForcedStartVoltages
  )(epsilons: Vector[Double]): PowerFlowResult = {
    epsilons.headOption match {
      case Some(epsilon) =>
        val admittanceMatrix =
          GridModel.composeAdmittanceMatrix(
            gridModel.nodeUuidToIndexMap,
            gridModel.gridComponents
          )

        // / execute
        val powerFlow =
          NewtonRaphsonPF(epsilon, maxIterations, admittanceMatrix)
        powerFlow.calculate(
          operatingPoint,
          Some(slackVoltages)
        ) match {
          case _: PowerFlowResult.FailedPowerFlowResult if epsilons.size > 1 =>
            // if we can relax, we relax
            val epsilonsLeft = epsilons.drop(1)
            log.debug(
              "NR power flow with ɛ = {} failed. Relaxing to {}.",
              epsilon,
              epsilonsLeft.headOption.getOrElse("")
            )
            newtonRaphsonPF(
              gridModel,
              maxIterations,
              operatingPoint,
              slackVoltages
            )(
              epsilonsLeft
            )
          case result =>
            result
        }
      case None =>
        throw new DBFSAlgorithmException(
          "ɛ is mandatory for a newton raphson power flow!"
        )
    }
  }
}
