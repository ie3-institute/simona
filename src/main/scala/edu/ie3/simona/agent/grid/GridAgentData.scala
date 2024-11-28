/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.container.{SubGridContainer, ThermalGrid}
import edu.ie3.datamodel.models.result.CongestionResult
import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.CongestionManagementSupport.Congestions
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.agent.grid.ReceivedValuesStore.NodeToReceivedPower
import edu.ie3.simona.agent.participant.ParticipantAgent.ParticipantMessage
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
import edu.ie3.simona.ontology.messages.Activation
import org.apache.pekko.actor.typed.ActorRef

import java.time.ZonedDateTime
import java.util.UUID

sealed trait GridAgentData

/** Contains all state data of [[GridAgent]]
  */
object GridAgentData {

  /** Class holding some [[GridAgent]] values that are immutable.
    * @param environmentRefs
    *   environment actor refs
    * @param simonaConfig
    *   config
    * @param listener
    *   listeners
    * @param resolution
    *   of the simulation
    * @param simStartTime
    *   start time of the simulation
    * @param activationAdapter
    *   adapter for [[Activation]]
    */
  final case class GridAgentConstantData private (
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      listener: Iterable[ActorRef[ResultEvent]],
      resolution: Long,
      simStartTime: ZonedDateTime,
      activationAdapter: ActorRef[Activation],
  ) {
    def notifyListeners(event: ResultEvent): Unit = {
      listener.foreach(listener => listener ! event)
    }
  }

  /** Case class that holds all received data.
    * @param inferiorGridMap
    *   map: inferior grid to received data
    * @tparam T
    *   type of data
    */
  final case class AwaitingData[T] private (
      inferiorGridMap: Map[ActorRef[GridAgent.Request], Option[T]]
  ) {

    /** Returns true if congestion data from inferior grids is expected and no
      * data was received yet.
      */
    def notDone: Boolean =
      inferiorGridMap.values.exists(_.isEmpty)

    /** Returns the received values
      */
    def values: Iterable[T] = inferiorGridMap.values.flatten.toSeq

    /** Return the mapping of all received values. This should only be called if
      * [[notDone]] == false
      */
    def mappedValues: Map[ActorRef[GridAgent.Request], T] =
      inferiorGridMap.flatMap { case (ref, option) =>
        option.map(value => ref -> value)
      }

    /** Method for updating the data with received data.
      * @param sender
      *   actor ref of the sender
      * @param data
      *   send data
      * @return
      *   an updated object
      */
    def update(sender: ActorRef[GridAgent.Request], data: T): AwaitingData[T] =
      handleReceivingData(Vector((sender, data)))

    /** Method for updating the data with the received data.
      *
      * @param receivedData
      *   data that was received
      * @return
      *   a updated copy of this data
      */
    def handleReceivingData(
        receivedData: Vector[(ActorRef[GridAgent.Request], T)]
    ): AwaitingData[T] = {
      val mappedData = receivedData.map { case (ref, value) =>
        ref -> Some(value)
      }.toMap
      copy(inferiorGridMap = inferiorGridMap ++ mappedData)
    }
  }

  object AwaitingData {
    def apply[T](
        inferiorGridRefs: Set[ActorRef[GridAgent.Request]]
    ): AwaitingData[T] = {
      AwaitingData(inferiorGridRefs.map(ref => ref -> None).toMap)
    }
  }

  /** Data that is sent to the [[GridAgent]] directly after startup. It contains
    * the main information for initialization. This data should include all
    * [[GridAgent]] individual data, for data that is the same for all
    * [[GridAgent]] s please use [[GridAgent.apply()]]
    *
    * @param subGridContainer
    *   raw grid information in the input data format
    * @param thermalIslandGrids
    *   Collection of thermal island grids (mostly one per household / building)
    *   that are of relevance to the given sub grid container
    * @param subGridGateToActorRef
    *   information on inferior and superior grid connections [[SubGridGate]] s
    *   and [[ActorRef]] s of the corresponding [[GridAgent]]s
    * @param refSystem
    *   of the grid
    * @param voltageLimits
    *   of the grid
    */
  final case class GridAgentInitData(
      subGridContainer: SubGridContainer,
      thermalIslandGrids: Seq[ThermalGrid],
      subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]],
      refSystem: RefSystem,
      voltageLimits: VoltageLimits,
  ) extends GridAgentData
      with GridAgentDataHelper {
    override protected val subgridGates: Vector[SubGridGate] =
      subGridGateToActorRef.keys.toVector
    override protected val subgridId: Int = subGridContainer.getSubnet
  }

  /** State data indicating that a power flow has been executed.
    *
    * @param gridAgentBaseData
    *   the base data of the [[GridAgent]]
    * @param powerFlowResult
    *   result of the executed power flow
    * @param pendingRequestAnswers
    *   Set of subgrid numbers of [[GridAgent]]s that don't have their request
    *   answered, yet
    */
  final case class PowerFlowDoneData private (
      gridAgentBaseData: GridAgentBaseData,
      powerFlowResult: PowerFlowResult,
      pendingRequestAnswers: Set[Int],
  ) extends GridAgentData

  object PowerFlowDoneData {
    def apply(
        gridAgentBaseData: GridAgentBaseData,
        powerFlowResult: PowerFlowResult,
    ): PowerFlowDoneData = {
      /* Determine the subgrid numbers of all superior grids */
      val superiorSubGrids = gridAgentBaseData.gridEnv.subgridGateToActorRef
        .map { case (subGridGate, _) => subGridGate.superiorNode.getSubnet }
        .filterNot(_ == gridAgentBaseData.gridEnv.gridModel.subnetNo)
        .toSet
      PowerFlowDoneData(gridAgentBaseData, powerFlowResult, superiorSubGrids)
    }
  }

  /** The base data that is mainly used by the [[GridAgent]]. This data has to
    * be copied several times at several places for each state transition with
    * updated data. So be careful in adding more data on it!
    */
  final case object GridAgentBaseData extends GridAgentData {

    def apply(
        gridModel: GridModel,
        subgridGateToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]],
        nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantMessage]]],
        superiorGridNodeUuids: Vector[UUID],
        inferiorGridGates: Vector[SubGridGate],
        powerFlowParams: PowerFlowParams,
        congestionManagementParams: CongestionManagementParams,
        actorName: String,
    ): GridAgentBaseData = {
      val gridEnv =
        GridEnvironment(gridModel, subgridGateToActorRef, nodeToAssetAgents)

      val currentSweepNo = 0 // initialization is assumed to be always @ sweep 0
      val sweepValueStores: Map[Int, SweepValueStore] = Map
        .empty[
          Int,
          SweepValueStore,
        ] // initialization is assumed to be always with no sweep data
      val inferiorGridGateToActorRef = subgridGateToActorRef.filter {
        case (gate, _) => inferiorGridGates.contains(gate)
      }

      // extracting one inferior ref for all inferior grids
      val inferiorGridRefs = inferiorGridGates
        .map { inferiorGridGate =>
          gridEnv.subgridGateToActorRef(
            inferiorGridGate
          ) -> inferiorGridGate.superiorNode.getUuid
        }
        .groupMap {
          // Group the gates by target actor, so that only one request is sent per grid agent
          case (inferiorGridAgentRef, _) =>
            inferiorGridAgentRef
        } { case (_, inferiorGridGates) =>
          inferiorGridGates
        }
        .map { case (inferiorGridAgentRef, _) =>
          inferiorGridAgentRef
        }
        .toSet

      GridAgentBaseData(
        gridEnv,
        powerFlowParams,
        congestionManagementParams,
        currentSweepNo,
        ReceivedValuesStore.empty(
          nodeToAssetAgents,
          inferiorGridGateToActorRef,
          superiorGridNodeUuids,
        ),
        sweepValueStores,
        inferiorGridRefs,
        actorName,
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
        inferiorGridGates: Vector[SubGridGate],
    ): GridAgentBaseData = {

      gridAgentBaseData.copy(
        receivedValueStore = ReceivedValuesStore.empty(
          gridAgentBaseData.gridEnv.nodeToAssetAgents,
          gridAgentBaseData.gridEnv.subgridGateToActorRef.filter {
            case (gate, _) => inferiorGridGates.contains(gate)
          },
          superiorGridNodeUuids,
        ),
        currentSweepNo = 0,
        sweepValueStores = Map.empty[Int, SweepValueStore],
        congestionManagementParams =
          gridAgentBaseData.congestionManagementParams.clean,
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
    * @param inferiorGridRefs
    *   a set of actor refs to all inferior grids
    */
  final case class GridAgentBaseData private (
      gridEnv: GridEnvironment,
      powerFlowParams: PowerFlowParams,
      congestionManagementParams: CongestionManagementParams,
      currentSweepNo: Int,
      receivedValueStore: ReceivedValuesStore,
      sweepValueStores: Map[Int, SweepValueStore],
      inferiorGridRefs: Set[ActorRef[GridAgent.Request]],
      actorName: String,
  ) extends GridAgentData
      with GridAgentDataHelper {

    override protected val subgridGates: Vector[SubGridGate] =
      gridEnv.subgridGateToActorRef.keys.toVector
    override protected val subgridId: Int = gridEnv.gridModel.subnetNo

    val allRequestedDataReceived: Boolean = {
      // we expect power values from inferior grids and assets
      val assetAndGridPowerValuesReady =
        receivedValueStore.nodeToReceivedPower.values.forall {
          _.forall { case (_, powerResponseOpt) =>
            powerResponseOpt.isDefined
          }
        }
      // we expect slack voltages only from our superior grids (if any)
      val slackVoltageValuesReady =
        receivedValueStore.nodeToReceivedSlackVoltage.values
          .forall(_.isDefined)

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
        replace: Boolean = false,
    ): GridAgentBaseData = {
      val updatedNodeToReceivedPowersMap = receivedPowerValues.values.foldLeft(
        receivedValueStore.nodeToReceivedPower
      ) {
        case (
              nodeToReceivedPowerValuesMapWithAddedPowerResponse,
              (
                senderRef,
                provideGridPowerMessage: GridPowerResponse,
              ),
            ) =>
          /* Go over all includes messages and add them. */
          provideGridPowerMessage.nodalResidualPower.foldLeft(
            nodeToReceivedPowerValuesMapWithAddedPowerResponse
          ) {
            case (
                  nodeToReceivedPowerValuesMapWithAddedExchangedPower,
                  exchangedPower,
                ) =>
              updateNodalReceivedPower(
                exchangedPower,
                nodeToReceivedPowerValuesMapWithAddedExchangedPower,
                senderRef,
                replace,
              )
          }
        case (
              nodeToReceivedPowerValuesMapWithAddedPowerResponse,
              (senderRef, powerResponseMessage),
            ) =>
          // some other singular power response message
          updateNodalReceivedPower(
            powerResponseMessage,
            nodeToReceivedPowerValuesMapWithAddedPowerResponse,
            senderRef,
            replace,
          )
      }
      this.copy(
        receivedValueStore = receivedValueStore
          .copy(nodeToReceivedPower = updatedNodeToReceivedPowersMap)
      )
    }

    /** Identify and update the vector of already received information.
      *
      * @param powerResponse
      *   Optional power response message
      * @param nodeToReceived
      *   Mapping from node uuid to received values
      * @param senderRef
      *   Reference of current sender
      * @param replace
      *   If existing values may be replaced or not
      * @return
      *   The nodal uuid as well as the updated collection of received
      *   information
      */
    private def updateNodalReceivedPower(
        powerResponse: PowerResponse,
        nodeToReceived: NodeToReceivedPower,
        senderRef: ActorRef[_],
        replace: Boolean,
    ): NodeToReceivedPower = {
      // extract the nodeUuid that corresponds to the sender's actorRef and check if we expect a message from the sender
      val nodeUuid = powerResponse match {
        case powerValuesMessage: ProvidedPowerResponse =>
          getNodeUuidForSender(nodeToReceived, senderRef, replace)
            .getOrElse(
              throw new RuntimeException(
                s"$actorName Received asset power values msg $powerValuesMessage " +
                  s"from $senderRef which is not in my power values nodes map or which cannot be replaced!"
              )
            )
        case FailedPowerFlow =>
          getNodeUuidForSender(nodeToReceived, senderRef, replace)
            .getOrElse(
              throw new RuntimeException(
                s"$actorName Received failed power flow message " +
                  s"from $senderRef which is not in my power values nodes map or which cannot be replaced!"
              )
            )
        case unknownMsg =>
          throw new RuntimeException(
            s"$actorName Unknown message received. Can't process message $unknownMsg."
          )
      }

      // update the values in the received map
      val nodeReceived = nodeToReceived
        .getOrElse(
          nodeUuid,
          throw new RuntimeException(
            s"NodeId $nodeUuid is not part of nodeToReceivedPowerValuesMap!"
          ),
        ) +
        // add or update entry in map of node entries
        (senderRef -> Some(powerResponse))

      /* Actually update the map and hand it back */
      nodeToReceived.updated(nodeUuid, nodeReceived)
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
    private def getNodeUuidForSender(
        nodeToReceivedPower: NodeToReceivedPower,
        senderRef: ActorRef[_],
        replace: Boolean,
    ): Option[UUID] =
      nodeToReceivedPower
        .find { case (_, receivedPowerMessages) =>
          receivedPowerMessages.exists { case (ref, maybePowerResponse) =>
            ref == senderRef &&
            (if (!replace)
               maybePowerResponse.isEmpty
             else
               maybePowerResponse.isDefined)
          }
        }
        .map { case (uuid, _) => uuid }

    /** Update this [[GridAgentBaseData]] with [[ReceivedSlackVoltageValues]]
      * and return a copy of this [[GridAgentBaseData]] for further processing
      *
      * @param receivedSlackValues
      *   the slack voltage values that should be used for the update
      * @return
      *   an updated version of this [[GridAgentBaseData]] containing the
      *   receivedSlackValues
      */
    def updateWithReceivedSlackVoltages(
        receivedSlackValues: ReceivedSlackVoltageValues
    ): GridAgentBaseData = {
      val updatedNodeToReceivedSlackVoltageValuesMap =
        receivedSlackValues.values.flatMap { case (senderRef, slackValues) =>
          slackValues.nodalSlackVoltages.map { exchangeVoltage =>
            receivedValueStore.nodeToReceivedSlackVoltage
              .get(exchangeVoltage.nodeUuid) match {
              case Some(None) =>
                /* Slack voltage is expected and not yet received */
                exchangeVoltage.nodeUuid -> Some(exchangeVoltage)
              case Some(Some(_)) =>
                throw new RuntimeException(
                  s"Already received slack value for node ${exchangeVoltage.nodeUuid}!"
                )
              case None =>
                throw new RuntimeException(
                  s"Received slack value for node ${exchangeVoltage.nodeUuid} from $senderRef which is not in my slack values nodes list!"
                )
            }
          }
        }.toMap
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
        inferiorGridGates: Vector[SubGridGate],
    ): GridAgentBaseData = {
      val sweepValueStore =
        SweepValueStore(
          validPowerFlowResult,
          gridEnv.gridModel.gridComponents.nodes,
          gridEnv.gridModel.nodeUuidToIndexMap,
        )
      val updatedSweepValueStore =
        sweepValueStores + (currentSweepNo -> sweepValueStore)

      this.copy(
        sweepValueStores = updatedSweepValueStore,
        receivedValueStore = ReceivedValuesStore.empty(
          gridEnv.nodeToAssetAgents,
          gridEnv.subgridGateToActorRef.filter { case (gate, _) =>
            inferiorGridGates.contains(gate)
          },
          superiorGridNodeUuids,
        ),
      )
    }
  }

  /** State data of a grid agent during the congestion management.
    * @param gridAgentBaseData
    *   agent base data
    * @param currentTick
    *   current tick used for additional power flow calculations
    * @param powerFlowResults
    *   result of the previous power flow calculation
    * @param congestions
    *   the found congestions
    */
  final case class CongestionManagementData private (
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      powerFlowResults: PowerFlowResultEvent,
      congestions: Congestions,
  ) extends GridAgentData {

    /** Builds a [[CongestionResult]] from the power flow results.
      * @param startTime
      *   of the simulation
      * @return
      *   a new [[CongestionResult]]
      */
    def getCongestionResult(startTime: ZonedDateTime): CongestionResult = {
      val gridModel = gridAgentBaseData.gridEnv.gridModel

      new CongestionResult(
        startTime.plusSeconds(currentTick),
        gridModel.subnetNo,
        gridModel.voltageLimits.vMin,
        gridModel.voltageLimits.vMax,
        congestions.voltageCongestions,
        congestions.lineCongestions,
        congestions.transformerCongestions,
      )
    }

    /** Returns the [[ActorRef]]s to all inferior grids
      */
    def inferiorGridRefs: Set[ActorRef[GridAgent.Request]] =
      gridAgentBaseData.inferiorGridRefs

    /** Returns cleaned [[GridAgentBaseData]] with updated
      * [[CongestionManagementParams]].
      */
    def cleanAfterTransformerTapping: GridAgentBaseData = {
      val params = gridAgentBaseData.congestionManagementParams
      val updatedParams = params.copy(hasRunTransformerTapping = true)

      gridAgentBaseData.copy(congestionManagementParams = updatedParams)
    }

    /** Returns cleaned [[GridAgentBaseData]] with updated
      * [[CongestionManagementParams]].
      */
    def cleanAfterTopologyChange: GridAgentBaseData = {
      val params = gridAgentBaseData.congestionManagementParams

      // updating the params to the next iteration
      val updatedParams = params.copy(
        hasRunTransformerTapping = false,
        iteration = params.iteration + 1,
      )

      gridAgentBaseData.copy(congestionManagementParams = updatedParams)
    }

    /** Returns cleaned [[GridAgentBaseData]] with updated
      * [[CongestionManagementParams]].
      */
    def cleanAfterFlexOptions: GridAgentBaseData = {
      val params = gridAgentBaseData.congestionManagementParams
      val updatedData = params.copy(
        hasUsedFlexOptions = true
      )

      gridAgentBaseData.copy(congestionManagementParams = updatedData)
    }
  }

  object CongestionManagementData {
    def apply(
        gridAgentBaseData: GridAgentBaseData,
        currentTick: Long,
        powerFlowResults: PowerFlowResultEvent,
    ): CongestionManagementData = {
      val gridModel = gridAgentBaseData.gridEnv.gridModel

      CongestionManagementData(
        gridAgentBaseData,
        currentTick,
        powerFlowResults,
        Congestions(
          powerFlowResults,
          gridModel.gridComponents,
          gridModel.voltageLimits,
          gridModel.mainRefSystem.nominalVoltage,
          gridModel.subnetNo,
        ),
      )
    }

    /** Creates [[CongestionManagementData]] without power flow results. With
      * this data the congestion management is skipped.
      * @param gridAgentBaseData
      *   agent base data
      * @param currentTick
      *   of the simulation
      * @return
      *   a new [[CongestionManagementData]]
      */
    def empty(
        gridAgentBaseData: GridAgentBaseData,
        currentTick: Long,
    ): CongestionManagementData = apply(
      gridAgentBaseData,
      currentTick,
      PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
      ),
    )
  }
}
