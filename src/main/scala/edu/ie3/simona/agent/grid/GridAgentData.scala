/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import java.util.UUID
import akka.event.LoggingAdapter
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.container.SubGridContainer
import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.simona.agent.grid.ReceivedValues.{
  ActorPowerRequestResponse,
  ReceivedPowerValues,
  ReceivedSlackValues
}
import edu.ie3.simona.akka.SimonaActorRef
import edu.ie3.simona.model.grid.{GridModel, RefSystem}
import edu.ie3.simona.ontology.messages.PowerMessage
import edu.ie3.simona.ontology.messages.PowerMessage.{
  FailedPowerFlow,
  ProvidePowerMessage
}

sealed trait GridAgentData

/** Contains all state data of [[GridAgent]]
  */
object GridAgentData {

  /** Initial state data of the [[GridAgent]]
    */
  final case object GridAgentUninitializedData extends GridAgentData

  /** Data that is send to the [[GridAgent]] directly after startup. It contains
    * the main information for initialization. This data should include all
    * [[GridAgent]] individual data, for data that is the same for all
    * [[GridAgent]] s please use [[GridAgent.props()]]
    *
    * @param subGridContainer
    *   raw grid information in the input data format
    * @param subGridGateToActorRef
    *   information on inferior and superior grid connections [[SubGridGate]]s
    *   and [[SimonaActorRef]]s of the corresponding [[GridAgent]]s
    */
  final case class GridAgentInitData(
      subGridContainer: SubGridContainer,
      subGridGateToActorRef: Map[SubGridGate, SimonaActorRef],
      refSystem: RefSystem
  ) extends GridAgentData
      with GridAgentDataHelper {
    override protected val subnetGates: Vector[SubGridGate] =
      subGridGateToActorRef.keys.toVector
    override protected val subnetId: Int = subGridContainer.getSubnet
  }

  /** State data indicating that a power flow has been executed.
    *
    * @param gridAgentBaseData
    *   the base data of the [[GridAgent]]
    * @param powerFlowResult
    *   result of the executed power flow
    */
  final case class PowerFlowDoneData(
      gridAgentBaseData: GridAgentBaseData,
      powerFlowResult: PowerFlowResult
  ) extends GridAgentData

  /** The base data that is mainly used by the [[GridAgent]]. This data has to
    * be copied several times at several places for each state transition with
    * updated data. So be careful in adding more data on it!
    */
  final case object GridAgentBaseData extends GridAgentData {

    def apply(
        gridModel: GridModel,
        subnetGateToActorRef: Map[SubGridGate, SimonaActorRef],
        nodeToAssetAgents: Map[UUID, Set[SimonaActorRef]],
        superiorGridNodeUuids: Vector[UUID],
        inferiorGridGates: Vector[SubGridGate],
        powerFlowParams: PowerFlowParams,
        log: LoggingAdapter,
        actorName: String
    ): GridAgentBaseData = {

      val currentSweepNo = 0 // initialization is assumed to be always @ sweep 0
      val sweepValueStores: Map[Int, SweepValueStore] = Map
        .empty[
          Int,
          SweepValueStore
        ] // initialization is assumed to be always with no sweep data
      val inferiorGridGateToActorRef = subnetGateToActorRef.filter {
        case (gate, _) => inferiorGridGates.contains(gate)
      }
      GridAgentBaseData(
        GridEnvironment(gridModel, subnetGateToActorRef, nodeToAssetAgents),
        powerFlowParams,
        currentSweepNo,
        ReceivedValuesStore.empty(
          nodeToAssetAgents,
          inferiorGridGateToActorRef,
          superiorGridNodeUuids
        ),
        sweepValueStores,
        log,
        actorName
      )
    }

    /** Constructs a new object of type [[GridAgentBaseData]] with the same data
      * as the provided one but with an empty [[ReceivedValuesStore]], an empty
      * [[SweepValueStore]] map and zero current sweep number.
      *
      * Normally used when a result in the [[DBFSAlgorithm]] has been found
      *
      * @param gridAgentBaseData
      *   the [[GridAgentBaseData]] that should be cleaned
      * @param superiorGridNodeUuids
      *   the unique node ids of the superior grid nodes of this [[GridAgent]]
      * @param inferiorGridGates
      *   the gates with connections to the inferior grids of this [[GridAgent]]
      * @return
      *   a cleaned [[GridAgentBaseData]] object
      */
    def clean(
        gridAgentBaseData: GridAgentBaseData,
        superiorGridNodeUuids: Vector[UUID],
        inferiorGridGates: Vector[SubGridGate]
    ): GridAgentBaseData = {

      gridAgentBaseData.copy(
        receivedValueStore = ReceivedValuesStore.empty(
          gridAgentBaseData.gridEnv.nodeToAssetAgents,
          gridAgentBaseData.gridEnv.subnetGateToActorRef.filter {
            case (gate, _) => inferiorGridGates.contains(gate)
          },
          superiorGridNodeUuids
        ),
        currentSweepNo = 0,
        sweepValueStores = Map.empty[Int, SweepValueStore]
      )

    }

  }

  /** The base aka default data of a [[GridAgent]]. Contains information on the
    * grid, parameters for the power flow calculations, information of the
    * current sweep number needed by [[DBFSAlgorithm]], a value store for
    * received slack and power values from superior and inferior [[GridAgent]] s
    * and [[edu.ie3.simona.agent.participant.ParticipantAgent]] s (== assets).
    *
    * @param gridEnv
    *   the grid environment
    * @param powerFlowParams
    *   power flow configuration parameters
    * @param currentSweepNo
    *   the current sweep number
    * @param receivedValueStore
    *   a value store for received values
    * @param sweepValueStores
    *   a value store for sweep results
    */
  final case class GridAgentBaseData private (
      gridEnv: GridEnvironment,
      powerFlowParams: PowerFlowParams,
      currentSweepNo: Int,
      receivedValueStore: ReceivedValuesStore,
      sweepValueStores: Map[Int, SweepValueStore],
      log: LoggingAdapter,
      actorName: String
  ) extends GridAgentData
      with GridAgentDataHelper {

    override protected val subnetGates: Vector[SubGridGate] =
      gridEnv.subnetGateToActorRef.keys.toVector
    override protected val subnetId: Int = gridEnv.gridModel.subnetNo

    val allRequestedDataReceived: Boolean = {
      // we expect power values from inferior grids and assets
      val assetAndGridPowerValuesReady =
        receivedValueStore.nodeToReceivedPower.values.forall(vector =>
          vector.forall(actorRefOption => actorRefOption._2.isDefined)
        )
      // we expect slack voltages only from our superior grids (if any)
      val slackVoltageValuesReady =
        receivedValueStore.nodeToReceivedSlackVoltage.values
          .forall(_.isDefined)
      log.debug(
        "slackMap: {}",
        receivedValueStore.nodeToReceivedSlackVoltage
      )
      log.debug(
        "powerMap: {}",
        receivedValueStore.nodeToReceivedPower
      )
      assetAndGridPowerValuesReady & slackVoltageValuesReady
    }

    /** Update this [[GridAgentBaseData]] with [[ReceivedPowerValues]] and
      * return a copy of this [[GridAgentBaseData]] for further processing
      *
      * @param receivedPowerValues
      *   the node power values that should be used for the update
      * @param replace
      *   indicates if already received values should be replaced
      * @return
      *   an updated version of this [[GridAgentBaseData]] containing the
      *   receivedPowerValues
      */
    def updateWithReceivedPowerValues(
        receivedPowerValues: ReceivedPowerValues,
        replace: Boolean = false
    ): GridAgentBaseData = {
      val updatedNodeToReceivedPowersMap = receivedPowerValues.values.foldLeft(
        receivedValueStore.nodeToReceivedPower
      ) {
        case (
              updatedNodeToReceivedPowerValuesMap,
              (senderRef, powerValueMessageOpt)
            ) =>
          // extract the nodeUuid that corresponds to the sender's actorRef and check if we expect a message from the sender
          val nodeUuid = powerValueMessageOpt match {
            case Some(
                  powerValuesMessage: ProvidePowerMessage
                ) => // can either be an AssetPowerChangedMessage or an AssetPowerUnchangedMessage
              uuid(updatedNodeToReceivedPowerValuesMap, senderRef, replace)
                .getOrElse(
                  throw new RuntimeException(
                    s"$actorName Received asset power values msg $powerValuesMessage " +
                      s"from $senderRef which is not in my power values nodes map or which cannot be replaced!"
                  )
                )
                ._1
            case Some(FailedPowerFlow) =>
              uuid(updatedNodeToReceivedPowerValuesMap, senderRef, replace)
                .getOrElse(
                  throw new RuntimeException(
                    s"$actorName Received failed power flow message " +
                      s"from $senderRef which is not in my power values nodes map or which cannot be replaced!"
                  )
                )
                ._1
            case None =>
              throw new RuntimeException(
                s"Received a 'None' as provided asset power from '$senderRef'. Provision of 'None' is not supported." +
                  s"Either a changed power or an unchanged power message is expected!"
              )
            case unknownMsg =>
              throw new RuntimeException(
                s"$actorName Unknown message received. Can't process message $unknownMsg."
              )
          }

          // update the values in the received map
          val receivedVector = updatedNodeToReceivedPowerValuesMap
            .getOrElse(
              nodeUuid,
              throw new RuntimeException(
                s"NodeId $nodeUuid is not part of nodeToReceivedPowerValuesMap!"
              )
            )
            .filterNot { case (k, v) =>
              k == senderRef &
                (if (!replace) v.isEmpty else v.isDefined)
            } :+ (senderRef, powerValueMessageOpt)

          updatedNodeToReceivedPowerValuesMap
            .updated(nodeUuid, receivedVector)

      }
      this.copy(
        receivedValueStore = receivedValueStore
          .copy(nodeToReceivedPower = updatedNodeToReceivedPowersMap)
      )
    }

    /** Find the uuid of the grid node the provided actor sender ref is located
      * on.
      *
      * @param nodeToReceivedPower
      *   a mapping of a grid node uuid to all actors and their optionally
      *   already provided power responses
      * @param senderRef
      *   the actor whose node uuid should be determined
      * @param replace
      *   if true, it is checked if the sender has already provided power
      *   values, which should be replaced, if false, it is checked if the
      *   sender has no yet provided power values
      * @return
      */
    private def uuid(
        nodeToReceivedPower: Map[UUID, Vector[ActorPowerRequestResponse]],
        senderRef: SimonaActorRef,
        replace: Boolean
    ): Option[
      (
          UUID,
          Vector[(SimonaActorRef, Option[PowerMessage.PowerResponseMessage])]
      )
    ] = {
      nodeToReceivedPower
        .find(
          _._2.exists(actorRefPowerValueOpt =>
            actorRefPowerValueOpt._1 == senderRef &&
              (if (!replace)
                 actorRefPowerValueOpt._2.isEmpty
               else
                 actorRefPowerValueOpt._2.isDefined)
          )
        )
    }

    /** Update this [[GridAgentBaseData]] with [[ReceivedSlackValues]] and
      * return a copy of this [[GridAgentBaseData]] for further processing
      *
      * @param receivedSlackValues
      *   the slack voltage values that should be used for the update
      * @return
      *   an updated version of this [[GridAgentBaseData]] containing the
      *   receivedSlackValues
      */
    def updateWithReceivedSlackVoltages(
        receivedSlackValues: ReceivedSlackValues
    ): GridAgentBaseData = {
      val updatedNodeToReceivedSlackVoltageValuesMap =
        receivedSlackValues.values.foldLeft(
          receivedValueStore.nodeToReceivedSlackVoltage
        ) {
          case (
                nodeToSlackVoltageUpdated,
                (senderRef, maybeSlackValues @ Some(slackValues))
              ) =>
            val nodeUuid: UUID = slackValues.nodeUuid

            receivedValueStore.nodeToReceivedSlackVoltage
              .get(nodeUuid) match {
              case Some(None) =>
                /* Slack voltage is expected and not yet received */
                nodeToSlackVoltageUpdated + (nodeUuid -> maybeSlackValues)
              case Some(Some(_)) =>
                throw new RuntimeException(
                  s"Already received slack value for node $nodeUuid!"
                )
              case None =>
                throw new RuntimeException(
                  s"Received slack value for node $nodeUuid from $senderRef which is not in my slack values nodes list!"
                )
            }
          case (_, (senderRef, None)) =>
            throw new RuntimeException(
              s"Received an empty voltage message from $senderRef"
            )
        }
      this.copy(
        receivedValueStore = receivedValueStore.copy(
          nodeToReceivedSlackVoltage =
            updatedNodeToReceivedSlackVoltageValuesMap
        )
      )
    }

    /** Updates the [[SweepValueStore]] map of this [[GridAgentBaseData]] with
      * the provided [[PowerFlowResult]], clears the [[ReceivedValuesStore]] and
      * returns a copy of this [[GridAgentBaseData]] with updated values for
      * further processing
      *
      * @param validPowerFlowResult
      *   the valid power flow result to be stored
      * @param superiorGridNodeUuids
      *   the unique node ids of the superior grid nodes of this [[GridAgent]]
      * @param inferiorGridGates
      *   the gates with connections to the inferior grids of this [[GridAgent]]
      * @return
      *   an updated version of this [[GridAgentBaseData]] containing the
      *   updated sweep value store and a clean received values store
      */
    def storeSweepDataAndClearReceiveMaps(
        validPowerFlowResult: ValidNewtonRaphsonPFResult,
        superiorGridNodeUuids: Vector[UUID],
        inferiorGridGates: Vector[SubGridGate]
    ): GridAgentBaseData = {
      val sweepValueStore =
        SweepValueStore(
          validPowerFlowResult,
          gridEnv.gridModel.gridComponents.nodes,
          gridEnv.gridModel.nodeUuidToIndexMap
        )
      val updatedSweepValueStore =
        sweepValueStores + (currentSweepNo -> sweepValueStore)

      this.copy(
        sweepValueStores = updatedSweepValueStore,
        receivedValueStore = ReceivedValuesStore.empty(
          gridEnv.nodeToAssetAgents,
          gridEnv.subnetGateToActorRef.filter { case (gate, _) =>
            inferiorGridGates.contains(gate)
          },
          superiorGridNodeUuids
        )
      )
    }
  }

}
