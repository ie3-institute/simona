/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import breeze.linalg.DenseMatrix
import breeze.math.Complex
import edu.ie3.powerflow.NewtonRaphsonPF
import edu.ie3.powerflow.model.NodeData.{PresetData, StateData}
import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.powerflow.model.StartData.WithForcedStartVoltages
import edu.ie3.powerflow.model.enums.NodeType
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.ExchangeVoltage
import edu.ie3.simona.agent.grid.GridAgentMessages.{
  ProvidedPowerResponse,
  ReceivedSlackVoltageValues,
}
import edu.ie3.simona.exceptions.agent.DBFSAlgorithmException
import edu.ie3.simona.model.grid.*
import edu.ie3.util.scala.quantities.DefaultQuantities.*
import org.slf4j.Logger
import squants.electro.ElectricPotential

import java.util.UUID
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

/** Support and helper methods for power flow calculations provided by
  * [[edu.ie3.powerflow]]
  */
trait PowerFlowSupport {

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
      nodes: Seq[NodeModel],
      transformers2w: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      nodeUuidToIndexMap: Map[UUID, Int],
      receivedValuesStore: ReceivedValuesStore,
      gridMainRefSystem: RefSystem,
      targetVoltageFromReceivedData: Boolean = true,
      ignoreTargetVoltage: Boolean = false,
  ): (Array[PresetData], WithForcedStartVoltages) = {
    val (operatingPoints, stateData) = nodes.map { nodeModel =>
      // note: currently we only support pq nodes as we not distinguish between pq/pv nodes -
      // when slack emulators or pv-node assets are added this needs to be considered here
      val nodeType = if nodeModel.isSlack then NodeType.SL else NodeType.PQ

      /* Determine the operating point for this given node */
      val nodeIdx = nodeUuidToIndexMap.getOrElse(
        nodeModel.uuid,
        throw new RuntimeException(
          s"Received data for node ${nodeModel.id} [${nodeModel.uuid}] which is not in my nodeUuidToIndexMap!"
        ),
      )

      val apparentPower: Complex =
        receivedValuesStore.nodeToReceivedPower
          .get(nodeModel.uuid) match {
          case Some(actorRefsWithPower) =>
            val (p, q) = actorRefsWithPower
              .map { case (_, powerMsg) => powerMsg }
              .collect {
                case Some(providePowerMessage: ProvidedPowerResponse) =>
                  providePowerMessage
                case Some(message) =>
                  throw new RuntimeException(
                    s"Received message $message which cannot processed here!"
                  )
                case None =>
                  throw new RuntimeException(
                    s"Did not receive all power values I expected @ node ${nodeModel.id}. This is a fatal and should never happen!"
                  )
              }
              .foldLeft(
                (
                  zeroKW,
                  zeroKVAr,
                )
              ) { case ((pSum, qSum), powerMessage) =>
                (
                  pSum + powerMessage.p,
                  qSum + powerMessage.q,
                )
              }

            new Complex(
              gridMainRefSystem.pInPu(p).value.doubleValue,
              gridMainRefSystem.qInPu(q).value.doubleValue,
            )
          case None => new Complex(0, 0)
        }

      val targetVoltage =
        if targetVoltageFromReceivedData && nodeModel.isSlack then {
          /* If the preset voltage is meant to be determined by means of received data and the node is a slack node
           * (only then there is received data), look it up and transform it */
          val receivedSlackVoltage =
            receivedValuesStore.nodeToReceivedSlackVoltage
              .get(nodeModel.uuid)
              .flatten
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
            gridMainRefSystem,
          )
        } else {
          // Either the received data shall not be considered or the node is not a slack node
          Complex.one *
            (if !ignoreTargetVoltage then nodeModel.vTarget.toEach
             else 1.0)
        }

      val optStateData = Option.when(nodeModel.isSlack)(
        StateData(
          nodeIdx,
          NodeType.SL,
          targetVoltage,
          apparentPower,
        )
      )

      (
        PresetData(
          nodeIdx,
          nodeType,
          apparentPower,
          targetVoltage.abs,
        ),
        optStateData,
      )
    }.unzip

    // NOTE: Currently, only one slack node per sub grid is allowed.
    val slackNodeData = stateData.flatten
      .minByOption(_.index)
      .getOrElse(
        throw new DBFSAlgorithmException(
          s"Unable to find a slack node."
        )
      )

    /*  In case a model has more than one, set all others to PQ nodes.
    ATTENTION: This does not cover the power flow situation correctly! */
    val adaptedOperatingPoint = operatingPoints.map { nodePreset =>
      if nodePreset.nodeType == NodeType.SL then {
        // If this is the slack node we picked, leave it as a slack node.
        if nodePreset.index == slackNodeData.index then nodePreset
        // If it is not the one, make it a PQ node.
        else nodePreset.copy(nodeType = NodeType.PQ)
      } else nodePreset
    }

    (
      combineOperatingPoint(adaptedOperatingPoint.toArray),
      WithForcedStartVoltages(Array(slackNodeData)),
    )
  }

  /** Composes the current operation point needed by
    * [[edu.ie3.powerflow.NewtonRaphsonPF.calculate()]] by reusing the provided
    * p/q values from the provided sweepDataValues and combines them with
    * updated receivedSlackValues. Normally used in a forward sweep phase of
    * [[DBFSAlgorithm]] as in this state only voltages are updated and a power
    * flow with new voltages but old p/q values is executed afterward.
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
      receivedSlackValues: ReceivedSlackVoltageValues,
      sweepDataValues: Vector[SweepValueStore.SweepValueStoreData],
      transformers2w: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      gridMainRefSystem: RefSystem,
  ): (Array[PresetData], WithForcedStartVoltages) =
    sweepDataValues.map { sweepValueStoreData =>
      val nodeStateData = sweepValueStoreData.stateData
      val targetVoltage = if nodeStateData.nodeType == NodeType.SL then {
        val receivedSlackVoltage = receivedSlackValues.values
          .map { case (_, slackVoltageMsg) => slackVoltageMsg }
          .flatMap(_.nodalSlackVoltages)
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
          gridMainRefSystem,
        )
      } else Complex.one

      // note: target voltage will be ignored for slack node if provided
      (
        PresetData(
          nodeStateData.index,
          nodeStateData.nodeType,
          nodeStateData.power,
          targetVoltage.abs,
        ),
        Option.when(nodeStateData.nodeType == NodeType.SL)(
          StateData(
            nodeStateData.index,
            nodeStateData.nodeType,
            targetVoltage,
            nodeStateData.power,
          )
        ),
      )
    }.unzip match {
      case (operatingPoint, stateData) =>
        (
          combineOperatingPoint(operatingPoint.toArray),
          WithForcedStartVoltages(stateData.flatten.toArray),
        )
    }

  /** When nodes are connected via a closed switch, they map to the same node
    * idx, as they get fused in the power flow calculation. In this case we need
    * to combine the operating point of these two nodes.
    *
    * @param operatingPoint
    *   the operating point of all nodes
    * @return
    *   the combined operating point
    */
  private def combineOperatingPoint(
      operatingPoint: Array[PresetData]
  ): Array[PresetData] = {
    operatingPoint
      .groupBy(_.index)
      .view
      .mapValues(
        _.reduceOption(combinePresetData)
          .getOrElse(
            throw new IllegalArgumentException(
              "There are no operation points to combine."
            )
          )
      )
      .values
      .toArray
      .sortBy(_.index)
  }

  /** Combines two [[PresetData]] instances to one. This is only possible if
    * they map to the same node index and have the same node type.
    *
    * @param a
    *   first instance
    * @param b
    *   second instance
    * @return
    *   combined instance
    */
  private def combinePresetData(a: PresetData, b: PresetData): PresetData = {
    require(
      a.index == b.index,
      "Preset Data should only be combined when they map to the same index.",
    )
    require(
      a.nodeType == b.nodeType,
      "Preset Data combination is only supported for the same node types for now.",
    )
    require(
      math.abs(a.targetVoltage - b.targetVoltage) < 1e-6,
      "Nodes to be combined have to be located in the same voltage level.",
    )

    val index = a.index
    val nodeType = a.nodeType
    val power = a.power + b.power
    val targetVoltage = a.targetVoltage

    def combineOptionals(
        a: Option[Double],
        b: Option[Double],
        f: (Double, Double) => Double,
    ): Option[Double] = (a, b) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case (Some(a), None)    => Some(a)
      case (None, Some(b))    => Some(b)
      case (None, None)       => None
    }

    val activePowerMin =
      combineOptionals(a.activePowerMin, b.activePowerMin, math.max)
    val activePowerMax =
      combineOptionals(a.activePowerMax, b.activePowerMax, math.min)
    val reactivePowerMin =
      combineOptionals(a.reactivePowerMin, b.reactivePowerMin, math.max)
    val reactivePowerMax =
      combineOptionals(a.reactivePowerMax, b.reactivePowerMax, math.min)

    PresetData(
      index,
      nodeType,
      power,
      targetVoltage,
      activePowerMin,
      activePowerMax,
      reactivePowerMin,
      reactivePowerMax,
    )
  }

  /** A debug method that composes a string with voltage information (in p.u.)
    * from a [[ValidNewtonRaphsonPFResult]]
    *
    * @param validResult
    *   the result that should be converted to a human-readable debug string
    * @param gridModel
    *   the grid model this result comes from
    * @return
    *   debug string with the final power flow voltage results @ each node
    */
  protected def composeValidNewtonRaphsonPFResultVoltagesDebugString(
      validResult: ValidNewtonRaphsonPFResult,
      gridModel: GridModel,
  ): String = {
    val debugString = new mutable.StringBuilder("Power flow result: ")
    validResult.nodeData.foreach(nodeStateData => {
      // get node index
      val nodeIndex = nodeStateData.index
      // get nodeUUID
      val uuid = gridModel.nodeUuidToIndexMap
        .find { case (_, index) => index == nodeIndex }
        .map { case (uuid, _) => uuid }
        .getOrElse(throw new RuntimeException("NODE NOT FOUND REMOVE THIS "))

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
      receivedSlackVoltage: ExchangeVoltage,
      nodeUuid: UUID,
      transformers2w: Set[TransformerModel],
      transformers3w: Set[Transformer3wModel],
      gridRefSystem: RefSystem,
  ): Complex = {
    ((
      transformers2w.find(_.hvNodeUuid == nodeUuid),
      transformers3w.find(_.nodeInternalUuid == nodeUuid),
    ) match {
      case (Some(transformer2w), None) =>
        transferToVoltageLevel(
          receivedSlackVoltage.e,
          receivedSlackVoltage.f,
          transformer2w,
        )
      case (None, Some(transformer3w)) =>
        transferToVoltageLevel(
          receivedSlackVoltage.e,
          receivedSlackVoltage.f,
          transformer3w,
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
      case (e: ElectricPotential, f: ElectricPotential) =>
        toComplex(toPu(e, f, gridRefSystem))
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
      e: ElectricPotential,
      f: ElectricPotential,
      transformerModel: TransformerModel,
  ): (ElectricPotential, ElectricPotential) = {
    val voltRatio = transformerModel.voltRatioNominal
    (e.divide(voltRatio.toDouble), f.divide(voltRatio.toDouble))
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
      e: ElectricPotential,
      f: ElectricPotential,
      transformerModel: Transformer3wModel,
  ): (ElectricPotential, ElectricPotential) = {
    val voltRatio = Transformer3wModel.voltRatio(transformerModel)
    (e.divide(voltRatio.toDouble), f.divide(voltRatio.toDouble))
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
      e: ElectricPotential,
      f: ElectricPotential,
      gridRefSystem: RefSystem,
  ): (squants.Dimensionless, squants.Dimensionless) = (
    gridRefSystem.vInPu(e),
    gridRefSystem.vInPu(f),
  )

  /** Build a [[Complex]] from both parts of the voltage
    *
    * @param ef
    *   Tuple of real and imaginary voltage
    * @return
    *   A [[Complex]] from real and imaginary part
    */
  private def toComplex(
      ef: (squants.Dimensionless, squants.Dimensionless)
  ) = ef match {
    case (e, f) =>
      new Complex(e.toEach, f.toEach)
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
  protected final def newtonRaphsonPF(
      gridModel: GridModel,
      maxIterations: Int,
      operatingPoint: Array[PresetData],
      slackVoltages: WithForcedStartVoltages,
  )(epsilons: Vector[Double])(implicit log: Logger): PowerFlowResult = {
    epsilons.headOption match {
      case Some(epsilon) =>
        val admittanceMatrix =
          GridModel.composeAdmittanceMatrix(
            gridModel.nodeUuidToIndexMap,
            gridModel.gridComponents,
          )

        // / execute
        val powerFlow =
          NewtonRaphsonPF(epsilon, maxIterations, admittanceMatrix)

        Try {
          powerFlow.calculate(
            operatingPoint,
            Some(slackVoltages),
          )
        }.map {
          case _: PowerFlowResult.FailedPowerFlowResult if epsilons.size > 1 =>
            // if we can relax, we relax
            val epsilonsLeft = epsilons.drop(1)
            log.debug(
              "NR power flow with ɛ = {} failed. Relaxing to {}.",
              epsilon,
              epsilonsLeft.headOption.getOrElse(""),
            )
            newtonRaphsonPF(
              gridModel,
              maxIterations,
              operatingPoint,
              slackVoltages,
            )(
              epsilonsLeft
            )
          case result =>
            result
        } match {
          case Success(result) => result
          case Failure(exception) =>
            throw new DBFSAlgorithmException(
              s"Power flow calculation in subgrid ${gridModel.subnetNo} failed.",
              exception,
            )
        }
      case None =>
        throw new DBFSAlgorithmException(
          "ɛ is mandatory for a newton raphson power flow!"
        )
    }
  }

  /** Calculates the power flow for the grid that contains the slack node.
    * @param gridModel
    *   model of the slack grid
    * @param receivedValueStore
    *   received values
    * @param powerFlowParams
    *   parameters for the power flow calculation
    * @param log
    *   for logging
    * @return
    *   power flow results
    */
  protected final def slackGridPF(
      gridModel: GridModel,
      receivedValueStore: ReceivedValuesStore,
      powerFlowParams: PowerFlowParams,
  )(implicit log: Logger): PowerFlowResult = {
    /* This is the highest grid agent, therefore no data is received for the slack node. Suppress, that it is looked
     * up in the empty store. */
    val (operationPoint, slackNodeVoltages) = composeOperatingPoint(
      gridModel.gridComponents.nodes,
      gridModel.gridComponents.transformers,
      gridModel.gridComponents.transformers3w,
      gridModel.nodeUuidToIndexMap,
      receivedValueStore,
      gridModel.mainRefSystem,
      targetVoltageFromReceivedData = false,
    )

    def superiorPowerFlow: PowerFlowResult =
      newtonRaphsonPF(
        gridModel,
        powerFlowParams.maxIterations,
        operationPoint,
        slackNodeVoltages,
      )(powerFlowParams.epsilon) match {
        case validPowerFlowResult: ValidNewtonRaphsonPFResult =>
          log.debug(
            "{}",
            composeValidNewtonRaphsonPFResultVoltagesDebugString(
              validPowerFlowResult,
              gridModel,
            ),
          )
          validPowerFlowResult
        case result: PowerFlowResult.FailedPowerFlowResult =>
          result
      }

    /* Regarding the power flow result of this grid, there are two cases. If this is the "highest" grid in a
     * simulation without a three winding transformer, the grid consists of only one node, and we can mock the power
     * flow results. If there is a three winding transformer apparent, we actually have to perform power flow
     * calculations, as the high voltage branch of the transformer is modeled here. */
    gridModel.gridComponents.transformers3w.isEmpty match {
      case true if gridModel.gridComponents.nodes.size == 1 =>
        val nodeData = operationPoint.map(StateData(_))
        ValidNewtonRaphsonPFResult(-1, nodeData, DenseMatrix(0d, 0d))

      case true =>
        log.warn(
          "This grid contains a more than just a slack node. Perform power flow calculations before assessing the power deviations."
        )
        superiorPowerFlow

      case false =>
        log.debug(
          "This grid contains a three winding transformer. Perform power flow calculations before assessing the power deviations."
        )
        superiorPowerFlow
    }
  }
}
