/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.data

import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.*
import edu.ie3.simona.agent.grid.GridAgent.Message
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.powerflow.ReceivedValuesStore.NodeToReceivedPower
import edu.ie3.simona.agent.grid.powerflow.{
  PowerFlowParams,
  ReceivedValuesStore,
  SweepValueStore,
}
import edu.ie3.simona.agent.participant.ParticipantAgent
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.event.ResultEvent
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.model.grid.ampacity.{
  AmpacityCalculationParams,
  LineSegmentThermalModel,
}
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.util.ConfigUtil.{
  EmConfigUtil,
  OutputConfigUtil,
  ParticipantConfigUtil,
}
import edu.ie3.simona.util.{ConfigUtil, ReceiveDataMap}
import edu.ie3.util.scala.collection.immutable.RichMultiMap.*
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import squants.thermal.Celsius

import java.time.ZonedDateTime
import java.util.UUID

sealed trait GridAgentData

/** Contains all state data of [[GridAgent]]
  */
object GridAgentData {

  final type GridAgentRef = ActorRef[GridAgent.Message]
  final type AwaitingData[T] = ReceiveDataMap[ActorRef[Message], T]

  private[grid] trait GridAgentDataInternal extends GridAgentData

  /** Class holding some [[GridAgent]] values that can be considered constant
    * across simulation time.
    *
    * @param gridAgentCoordinator
    *   The reference of the grid agent coordinator.
    * @param environmentRefs
    *   Containing actor references, that are relevant for the environment of
    *   the grid agent.
    * @param simonaConfig
    *   Configuration of SIMONA, that is used for.
    * @param resolution
    *   That is used for the power flow. If no power flow should be carried out,
    *   this value is set to [[Long.MaxValue]].
    * @param simStartTime
    *   Start time of the simulation.
    * @param simEndTime
    *   Send time of the simulation.
    */
  final case class GridAgentConstantData(
      gridAgentCoordinator: ActorRef[GridAgentCoordinator.Message],
      environmentRefs: EnvironmentRefs,
      simonaConfig: SimonaConfig,
      resolution: Long,
      simStartTime: ZonedDateTime,
      simEndTime: ZonedDateTime,
  ) {
    def notifyListeners(event: ResultEvent): Unit =
      environmentRefs.resultProxy ! event

    val participantConfigUtil: ParticipantConfigUtil =
      ConfigUtil.ParticipantConfigUtil(simonaConfig.runtime.participant)

    val outputConfigUtil: OutputConfigUtil =
      ConfigUtil.OutputConfigUtil.participants(simonaConfig.output.participant)

    val emConfigUtil: EmConfigUtil = EmConfigUtil(simonaConfig.runtime.em)

  }

  /** Data that is sent to the [[GridAgent]] directly after startup. It contains
    * the main information for initialization. This data should include all
    * [[GridAgent]] individual data, for data that is the same for all
    * [[GridAgent]] s please use [[GridAgent.apply()]].
    *
    * @param gridModel
    *   [[GridModel]] with all asset information.
    * @param simulationStart
    *   Date of the very first tick in the simulation.
    * @param ampacityCalculationParams
    *   Parameters for the ampacity calculation.
    * @param powerFlowParams
    *   Parameters for the power flow calculation.
    * @param refToSubgrid
    *   A mapping of all known references to their subgrid id.
    * @param inferiorConnections
    *   A map of actor refs to all inferior grids.
    * @param superiorConnections
    *   A map of actor refs to all superior grids with the corresponding
    *   superior nodes.
    * @param nodeToAssetAgents
    *   A mapping of all node uuids to a set of asset [[ActorRef]] s at those
    *   nodes.
    */
  final case class GridAgentInitData(
      gridModel: GridModel,
      simulationStart: ZonedDateTime,
      ampacityCalculationParams: AmpacityCalculationParams,
      powerFlowParams: PowerFlowParams,
      refToSubgrid: Map[GridAgentRef, Int] = Map.empty,
      inferiorConnections: MultiMap[GridAgentRef, UUID] = Map.empty,
      superiorConnections: MultiMap[GridAgentRef, UUID] = Map.empty,
      nodeToAssetAgents: MultiMap[UUID, ActorRef[ParticipantAgent.Request]] =
        Map.empty,
  ) extends GridAgentData {

    /** Updates the init data to incorporate the inferior grid.
      * @param reference
      *   The actor reference of the inferior grid.
      * @param nodes
      *   All nodes of the higher side of the transformers that connect this
      *   grid to the inferior grid.
      * @param subgridNo
      *   The number of the inferior grid.
      * @return
      *   The updated init data.
      */
    def registerInferior(
        reference: GridAgentRef,
        nodes: Set[UUID],
        subgridNo: Int,
    ): GridAgentInitData =
      copy(
        inferiorConnections = inferiorConnections.added(reference, nodes),
        refToSubgrid = refToSubgrid.updated(reference, subgridNo),
      )

    /** Updates the init data to incorporate the superior grid.
      * @param reference
      *   The actor reference of the superior grid.
      * @param nodes
      *   All nodes of the higher side of the transformers that connect this
      *   grid to the superior grid.
      * @param subgridNo
      *   The number of the superior grid.
      * @return
      *   The updated init data.
      */
    def registerSuperior(
        reference: GridAgentRef,
        nodes: Set[UUID],
        subgridNo: Int,
    ): GridAgentInitData =
      copy(
        superiorConnections = superiorConnections.added(reference, nodes),
        refToSubgrid = refToSubgrid.updated(reference, subgridNo),
      )

    /** Updates the init data to incorporate connected assets.
      * @param nodeToParticipants
      *   A map: node uuid to set of participant actor references.
      * @return
      *   The updated init data.
      */
    def registerParticipants(
        nodeToParticipants: MultiMap[UUID, ActorRef[ParticipantAgent.Request]]
    ): GridAgentInitData = {
      val updated = nodeToParticipants.map { case (node, assets) =>
        nodeToAssetAgents.get(node) match {
          case Some(value) =>
            node -> (value ++ assets)
          case None =>
            node -> assets
        }
      }

      copy(nodeToAssetAgents = nodeToAssetAgents ++ updated)
    }

  }

  /** State data indicating that a power flow has been executed.
    *
    * @param gridAgentBaseData
    *   The base data of the [[GridAgent]].
    * @param powerFlowResult
    *   Result of the executed power flow.
    * @param pendingRequestAnswers
    *   Set of subgrid numbers of [[GridAgent]]s that don't have their request
    *   answered, yet.
    */
  final case class PowerFlowDoneData(
      gridAgentBaseData: GridAgentBaseData,
      powerFlowResult: PowerFlowResult,
      pendingRequestAnswers: Set[Int],
  ) extends GridAgentData {
    def clearAssetPower: PowerFlowDoneData = copy(
      gridAgentBaseData = gridAgentBaseData.clearAssetPower
    )
  }

  object PowerFlowDoneData {
    def apply(
        gridAgentBaseData: GridAgentBaseData,
        powerFlowResult: PowerFlowResult,
    ): PowerFlowDoneData = {
      /* Determine the subgrid numbers of all superior grids */
      PowerFlowDoneData(
        gridAgentBaseData,
        powerFlowResult,
        gridAgentBaseData.gridEnv.superiorGridIds.values.toSet,
      )
    }
  }

  /** The base data that is mainly used by the [[GridAgent]]. This data has to
    * be copied several times at several places for each state transition with
    * updated data. So be careful in adding more data on it!
    */
  object GridAgentBaseData extends GridAgentData {

    def apply(
        gridModel: GridModel,
        inferiorConnections: MultiMap[GridAgentRef, UUID],
        superiorConnections: MultiMap[GridAgentRef, UUID],
        nodeToAssetAgents: MultiMap[UUID, ActorRef[ParticipantAgent.Request]],
        refToSubgrid: Map[GridAgentRef, Int],
        simulationStart: ZonedDateTime,
        ampacityCalculationParams: AmpacityCalculationParams,
        powerFlowParams: PowerFlowParams,
        actorName: String,
    ): GridAgentBaseData = {
      val superiorGridIds = superiorConnections.flatMap { case (ref, nodes) =>
        val subgrid = refToSubgrid(ref)
        nodes.map(_ -> subgrid)
      }

      val gridEnv = GridEnvironment(
        gridModel,
        inferiorConnections,
        superiorConnections,
        nodeToAssetAgents,
        refToSubgrid,
        superiorGridIds,
      )

      val currentSweepNo = 0 // initialization is assumed to be always @ sweep 0
      val sweepValueStores: Map[Int, SweepValueStore] = Map
        .empty[
          Int,
          SweepValueStore,
        ] // initialization is assumed to be always with no sweep data

      val groundTemperature = Celsius(20) // FIXME DF

      val thermalLineStates =
        if ampacityCalculationParams.activateAmpacityCalculation
        then
          gridModel.gridComponents.thermalLineSegments.map { lineSeg =>
            lineSeg.uuid -> LineSegmentThermalModel.initState(
              groundTemperature,
              lineSeg.cableSetup,
              lineSeg,
            )
          }.toMap
        else Map.empty[UUID, LineState]

      GridAgentBaseData(
        gridEnv,
        powerFlowParams,
        currentSweepNo,
        ReceivedValuesStore.empty(gridEnv),
        sweepValueStores,
        thermalLineStates,
        simulationStart,
        actorName,
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
    *   The grid environment.
    * @param powerFlowParams
    *   Power flow configuration parameters.
    * @param currentSweepNo
    *   The current sweep number.
    * @param receivedValueStore
    *   A value store for received values.
    * @param sweepValueStores
    *   A value store for sweep results.
    * @param thermalLineStates
    *   A map [[UUID]] of [[LineSegmentThermalModel]] and the current
    *   [[LineState]].
    * @param simulationStart
    *   Date of the very first tick in the simulation.
    * @param actorName
    *   The name of the actor.
    */
  final case class GridAgentBaseData(
      gridEnv: GridEnvironment,
      powerFlowParams: PowerFlowParams,
      currentSweepNo: Int,
      receivedValueStore: ReceivedValuesStore,
      sweepValueStores: Map[Int, SweepValueStore],
      thermalLineStates: Map[UUID, LineState],
      simulationStart: ZonedDateTime,
      actorName: String,
  ) extends GridAgentData {

    val assets: Seq[UUID] = {
      val components = gridEnv.gridModel.gridComponents

      components.nodes.map(_.uuid)
        ++ components.lines.map(_.uuid)
        ++ components.switches.map(_.uuid)
        ++ components.transformers.map(_.uuid)
        ++ components.transformers3w.map(_.uuid)
    }

    val allRequestedDataReceived: Boolean = {
      // we expect power values from inferior grids and assets
      val assetAndGridPowerValuesReady =
        receivedValueStore.nodeToReceivedPower.values.forall {
          _.forall { case (_, powerResponseOpt) =>
            powerResponseOpt.isDefined
          }
        }
      // we expect slack voltages only from our superior grids (if any)
      assetAndGridPowerValuesReady & allSlackVoltagesReceived
    }

    val isSuperior: Boolean = gridEnv.superiorConnections.isEmpty

    lazy val inferiorGridRefs: MultiMap[GridAgentRef, UUID] =
      gridEnv.inferiorConnections

    lazy val superiorGridRefs: MultiMap[GridAgentRef, UUID] =
      gridEnv.superiorConnections

    lazy val inferiorGridNodeUuids: Set[UUID] = gridEnv.inferiorNodeUuids
    lazy val superiorGridNodeUuids: Set[UUID] = gridEnv.superiorNodeUuids

    /** Method to try looking up the subgrid number of the superior grid based
      * on the given node uuid.
      * @param node
      *   The uuid of one of the connecting nodes.
      * @return
      *   An option for the number.
      */
    def getSuperiorSubgridNumber(node: UUID): Option[Int] =
      gridEnv.superiorGridIds.get(node)

    /** Checks if all slack voltage have been received.
      * @return
      *   True, if all slack voltages are present.
      */
    def allSlackVoltagesReceived: Boolean =
      receivedValueStore.nodeToReceivedSlackVoltage.values
        .forall(_.isDefined)

    /** Checks if we received a [[FailedPowerFlow]] from an inferior grid.
      * @return
      *   True, if a [[FailedPowerFlow]] message is found.
      */
    def hasFailedPowerFlow: Boolean =
      receivedValueStore.nodeToReceivedGridPower.values.exists(
        _.values.exists(_.exists(_.isInstanceOf[FailedPowerFlow]))
      )

    /** Method for clearing the stored asset power.
      * @return
      *   The updated state data.
      */
    def clearAssetPower: GridAgentBaseData =
      copy(receivedValueStore = receivedValueStore.clearAssetPower)

    /** Update this [[GridAgentBaseData]] with [[PowerResponse]] and return a
      * copy of this [[GridAgentBaseData]] for further processing.
      *
      * @param receivedPowerValue
      *   The node power values that should be used for the update.
      * @param replace
      *   Indicates if already received values should be replaced.
      * @return
      *   An updated version of this [[GridAgentBaseData]] containing the
      *   receivedPowerValues.
      */
    def updateWithPowerResponse(
        receivedPowerValue: PowerResponse,
        replace: Boolean = false,
    )(using log: Logger): GridAgentBaseData = {
      receivedPowerValue match {
        case provideGridPowerMessage: GridPowerResponse =>
          /* Go over all includes messages and add them. */
          val updated = provideGridPowerMessage.nodalResidualPower.foldLeft(
            receivedValueStore.nodeToReceivedGridPower
          ) { case (currentReceivedPowerMap, exchangedPower) =>
            updateNodalReceivedPower(
              exchangedPower,
              currentReceivedPowerMap,
              replace,
            )
          }

          copy(receivedValueStore =
            receivedValueStore.copy(nodeToReceivedGridPower = updated)
          )

        case _: FailedPowerFlow =>
          // some other singular power response message
          val updated = updateNodalReceivedPower(
            receivedPowerValue,
            receivedValueStore.nodeToReceivedGridPower,
            replace,
          )

          copy(receivedValueStore =
            receivedValueStore.copy(nodeToReceivedGridPower = updated)
          )

        case _ =>
          // some other singular power response message
          val updated = updateNodalReceivedPower(
            receivedPowerValue,
            receivedValueStore.nodeToReceivedAssetPower,
            replace,
          )

          copy(receivedValueStore =
            receivedValueStore.copy(nodeToReceivedAssetPower = updated)
          )
      }
    }

    /** Identify and update the vector of already received information.
      *
      * @param powerResponse
      *   Optional power response message.
      * @param nodeToReceived
      *   Mapping from node uuid to received values.
      * @param replace
      *   If existing values may be replaced or not.
      * @return
      *   The nodal uuid as well as the updated collection of received
      *   information.
      */
    private def updateNodalReceivedPower(
        powerResponse: PowerResponse,
        nodeToReceived: NodeToReceivedPower,
        replace: Boolean,
    )(using log: Logger): NodeToReceivedPower = {
      val senderRef = powerResponse.sender

      // extract the nodeUuid that corresponds to the sender's actorRef
      val nodeUuidOption =
        getNodeUuidForSender(nodeToReceived, senderRef, replace)

      // check if we expect a message from the sender
      (powerResponse, nodeUuidOption) match {
        case (_: (ProvidedPowerResponse | FailedPowerFlow), Some(uuid)) =>
          val nodeReceived = nodeToReceived.getOrElse(
            uuid,
            throw new RuntimeException(
              s"NodeId $uuid is not part of nodeToReceivedPowerValuesMap!"
            ),
          )
          // add or update entry in map of node entries
            + (senderRef -> Some(powerResponse))

          /* Actually update the map and hand it back */
          nodeToReceived.updated(uuid, nodeReceived)

        case (powerValuesMessage: ProvidedPowerResponse, _) =>
          log.warn(
            s"$actorName Received asset power values msg $powerValuesMessage " +
              s"from $senderRef which is not in my power values nodes map or which cannot be replaced!"
          )

          nodeToReceived

        case (_: FailedPowerFlow, _) =>
          log.warn(
            s"$actorName Received failed power flow message " +
              s"from $senderRef which is not in my power values nodes map or which cannot be replaced!"
          )

          nodeToReceived

        case (unknownMsg, _) =>
          throw new RuntimeException(
            s"$actorName Unknown message received. Can't process message $unknownMsg."
          )
      }
    }

    /** Find the uuid of the grid node the provided actor sender ref is located
      * on.
      *
      * @param nodeToReceivedPower
      *   A mapping of a grid node uuid to all actors and their optionally
      *   already provided power responses.
      * @param senderRef
      *   The actor whose node uuid should be determined.
      * @param replace
      *   If true, it is checked if the sender has already provided power
      *   values, which should be replaced, if false, it is checked if the
      *   sender has no yet provided power values.
      * @return
      */
    private def getNodeUuidForSender(
        nodeToReceivedPower: NodeToReceivedPower,
        senderRef: ActorRef[?],
        replace: Boolean,
    ): Option[UUID] =
      nodeToReceivedPower
        .find { case (_, receivedPowerMessages) =>
          receivedPowerMessages.exists { case (ref, maybePowerResponse) =>
            ref == senderRef &&
            (if !replace then maybePowerResponse.isEmpty
             else maybePowerResponse.isDefined)
          }
        }
        .map { case (uuid, _) => uuid }

    /** Method to remove all previously received slack voltages.
      * @return
      *   A [[GridAgentBaseData]] with an updated [[ReceivedValuesStore]].
      */
    def clearedSlackVoltages: GridAgentBaseData = {
      val cleared = receivedValueStore.nodeToReceivedSlackVoltage.map {
        case (uuid, _) =>
          uuid -> None
      }

      copy(receivedValueStore =
        receivedValueStore.copy(nodeToReceivedSlackVoltage = cleared)
      )
    }

    /** Update this [[GridAgentBaseData]] with [[SlackVoltageResponse]] and
      * return a copy of this [[GridAgentBaseData]] for further processing.
      *
      * @param slackVoltageResponse
      *   The slack voltage value that should be used for the update.
      * @return
      *   An updated version of this [[GridAgentBaseData]] containing the
      *   receivedSlackValues.
      */
    def updateWithSlackVoltageResponse(
        slackVoltageResponse: SlackVoltageResponse
    ): GridAgentBaseData = {

      val store = receivedValueStore.nodeToReceivedSlackVoltage

      val updatedNodeToReceivedSlackVoltageValuesMap =
        store ++ slackVoltageResponse.nodalSlackVoltages.map {
          exchangeVoltage =>
            store.get(exchangeVoltage.nodeUuid) match {
              case Some(None) =>
                /* Slack voltage is expected and not yet received */
                exchangeVoltage.nodeUuid -> Some(exchangeVoltage)
              case Some(Some(_)) =>
                throw new RuntimeException(
                  s"Already received slack value for node ${exchangeVoltage.nodeUuid}!"
                )
              case None =>
                throw new RuntimeException(
                  s"Received slack value for node ${exchangeVoltage.nodeUuid} from ${slackVoltageResponse.sender} which is not in my slack values nodes list!"
                )
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
      * further processing.
      *
      * @param validPowerFlowResult
      *   The valid power flow result to be stored.
      * @return
      *   An updated version of this [[GridAgentBaseData]] containing the
      *   updated sweep value store and a clean received values store.
      */
    def storeSweepDataAndClearReceiveMaps(
        validPowerFlowResult: ValidNewtonRaphsonPFResult
    ): GridAgentBaseData = {
      val sweepValueStore =
        SweepValueStore(
          validPowerFlowResult,
          gridEnv.gridModel.gridComponents.nodes,
          gridEnv.gridModel.nodeUuidToIndexMap,
        )
      val updatedSweepValueStore =
        sweepValueStores + (currentSweepNo -> sweepValueStore)

      copy(
        sweepValueStores = updatedSweepValueStore,
        receivedValueStore = ReceivedValuesStore.empty(gridEnv),
      )
    }

    /** Constructs a new object of type [[GridAgentBaseData]] with the same data
      * as the provided one but with an empty [[ReceivedValuesStore]], an empty
      * [[SweepValueStore]] map and zero current sweep number.
      *
      * Normally used when a result in the [[DBFSAlgorithm]] has been found.
      *
      * @return
      *   A cleaned [[GridAgentBaseData]] object.
      */
    def clean: GridAgentBaseData = {
      copy(
        receivedValueStore = ReceivedValuesStore.empty(gridEnv),
        currentSweepNo = 0,
        sweepValueStores = Map.empty[Int, SweepValueStore],
      )
    }
  }

}
