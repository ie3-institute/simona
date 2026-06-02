/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.actor.SimonaActorNaming
import edu.ie3.simona.agent.grid.AmpacityCalculationMessages.DoAmpacityCalculation
import edu.ie3.simona.agent.grid.GridAgentCoordinator.{
  FinishedInitialization,
  PowerFlowResults,
}
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.congestion.CongestionManagementMessages.DoCongestionManagement
import edu.ie3.simona.agent.grid.congestion.DCMAlgorithm
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentBaseData,
  GridAgentConstantData,
  GridAgentInitData,
}
import edu.ie3.simona.agent.grid.powerflow.DBFSAlgorithm
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.service.results.ResultServiceProxy.ExpectResult
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.collection.immutable.RichMultiMap.MultiMap
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}
import org.slf4j.Logger
import squants.ElectricCurrent
import squants.electro.Amperes

import java.util.UUID

object GridAgent extends DBFSAlgorithm with DCMAlgorithm {

  /** All messages, that can be received by a [[GridAgent]]. */
  final type Message = InternalRequest | InternalReply | Activation

  /** Necessary because we want to extend messages in other classes, but we do
    * want to keep the messages only available inside this package.
    */
  private[grid] trait InternalRequest
  private[grid] trait InternalReply
  private[grid] trait InternalReplyWithSender[T] extends InternalReply {
    def sender: ActorRef[GridAgent.Message]
    def value: T
  }

  def apply(
      initData: GridAgentInitData,
      bufferSize: Int = 1000,
  )(using
      constantData: GridAgentConstantData
  ): Behavior[Message] = Behaviors.withStash(bufferSize) { buffer =>
    uninitialized(initData)(using constantData, buffer)
  }

  private def uninitialized(initData: GridAgentInitData)(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = Behaviors.receivePartial {
    case (_, RegisterInferiorGrid(gridRef, nodes, subgridNo)) =>
      uninitialized(initData.registerInferior(gridRef, nodes, subgridNo))

    case (_, RegisterSuperiorGrid(gridRef, nodes, subgridNo)) =>
      uninitialized(initData.registerSuperior(gridRef, nodes, subgridNo))

    case (_, RegisterParticipants(nodeToParticipants)) =>
      uninitialized(initData.registerParticipants(nodeToParticipants))

    case (ctx, CompleteInitialization(onlyOneSubGrid)) =>
      val actorName = SimonaActorNaming.actorName(ctx.self)

      // fail fast sanity checks
      failFast(initData, actorName, onlyOneSubGrid)

      // create the GridAgentBaseData
      val gridAgentBaseData = GridAgentBaseData(
        initData.gridModel,
        initData.inferiorConnections,
        initData.superiorConnections,
        initData.nodeToAssetAgents,
        initData.refToSubgrid,
        initData.simulationStart,
        initData.ampacityCalculationParams,
        initData.powerFlowParams,
        actorName,
      )

      constantData.gridAgentCoordinator ! FinishedInitialization(ctx.self)

      idle(gridAgentBaseData)
  }

  /** Method that defines the idle [[Behavior]] of the agent.
    *
    * @param gridAgentBaseData
    *   State data of the actor.
    * @param constantData
    *   Immutable [[GridAgent]] values.
    * @param buffer
    *   For [[GridAgent.Message]]s.
    * @return
    *   A [[Behavior]].
    */
  private[grid] def idle(
      gridAgentBaseData: GridAgentBaseData
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = Behaviors.receivePartial {
    case (ctx, doPowerFlowTrigger: DoPowerFlowTrigger) =>
      // inform the result proxy that this grid agent will send new results
      constantData.environmentRefs.resultProxy ! ExpectResult(
        gridAgentBaseData.assets,
        doPowerFlowTrigger.tick,
      )

      ctx.self ! doPowerFlowTrigger
      buffer.unstashAll(
        simulateGrid(gridAgentBaseData, doPowerFlowTrigger.tick)
      )

    case (ctx, DoAmpacityCalculation(currentTick, results)) =>
      val subGridNo =
        gridAgentBaseData.gridEnv.gridModel.subnetNo // FIXME are all subgrids check or only this one, why this one?
      startAmpacityCalculation(
        gridAgentBaseData,
        currentTick,
        subGridNo,
        results,
        ctx,
      )

    case (ctx, DoCongestionManagement(currentTick, results)) =>
      startCongestionManagement(
        gridAgentBaseData,
        currentTick,
        results,
        ctx,
      )

    case (_, msg: Message) =>
      // needs to be set here to handle if the messages arrive too early
      // before a transition to GridAgentBehaviour took place
      buffer.stash(msg)
      Behaviors.same
  }

  /** Behavior of the [[GridAgent]] after the powerflow is finished.
    *
    * @param gridAgentBaseData
    *   State data of the actor.
    * @param currentTick
    *   The current tick in the simulation.
    * @param ctx
    *   Actor context.
    * @param constantData
    *   Immutable [[GridAgent]] values.
    * @param buffer
    *   For [[GridAgent.Message]]s.
    * @return
    *   A [[Behavior]].
    */
  private[grid] def afterPowerFlow(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[Message],
  ): Behavior[Message] = {
    ctx.log.debug(
      "Calculate results ..."
    )
    val results: Option[PowerFlowResultEvent] =
      gridAgentBaseData.sweepValueStores.lastOption.map {
        case (_, valueStore) =>
          createResultModels(
            gridAgentBaseData.gridEnv.gridModel,
            valueStore,
          )(using
            currentTick.toDateTime(using constantData.simStartTime),
            ctx.log,
          )
      }

    val doAmpacityCalc =
      constantData.simonaConfig.ampacityCalculation.activateAmpacityCalculation

    val updatedThermalLineStates = {
      if doAmpacityCalc then {
        gridAgentBaseData.gridEnv.gridModel.gridComponents.thermalLineSegments.map {
          lineSegment =>
            val lastLineState =
              gridAgentBaseData.thermalLineStates.getOrElse(
                lineSegment.uuid,
                throw new RuntimeException(
                  s"No previous state for line ${lineSegment.uuid}"
                ),
              )

            val currentFromPFResults: ElectricCurrent =
              results.toSeq
                .flatMap(_.lineResults)
                .find(_.getInputModel == lineSegment.lineUuid)
                .map(_.getiAMag().toSquants)
                .getOrElse(
                  throw new RuntimeException(
                    s"No power flow result for line ${lineSegment.lineUuid}"
                  )
                )

            val lineCurrent =
              if currentFromPFResults >= Amperes(0d) then currentFromPFResults
              else currentFromPFResults * -1

            lineSegment.uuid ->
              lineSegment.determineState(
                currentTick,
                lastLineState,
                lineCurrent,
                gridAgentBaseData.simulationStart,
              )
        }.toMap
      } else {
        gridAgentBaseData.thermalLineStates
      }
    }

    val updatedBaseData =
      gridAgentBaseData.copy(thermalLineStates = updatedThermalLineStates)

    // clean up agent and go back to idle
    gotoIdle(updatedBaseData, results, ctx)
  }

  /** Method that will clean up the [[GridAgentBaseData]] and go to the
    * [[idle()]] state.
    *
    * @param gridAgentBaseData
    *   State data of the actor.
    * @param results
    *   Option for the last power flow, that should be written.
    * @param ctx
    *   Actor context.
    * @param constantData
    *   Immutable [[GridAgent]] values.
    * @param buffer
    *   For [[GridAgent.Message]]s.
    * @return
    *   A [[Behavior]].
    */
  private[grid] def gotoIdle(
      gridAgentBaseData: GridAgentBaseData,
      results: Option[PowerFlowResultEvent],
      ctx: ActorContext[Message],
  )(using
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Message],
  ): Behavior[Message] = {

    constantData.gridAgentCoordinator ! PowerFlowResults(
      ctx.self,
      results,
    )

    // do my cleanup stuff
    ctx.log.debug("Doing my cleanup stuff")

    // / clean copy of the gridAgentBaseData
    val cleanedGridAgentBaseData = gridAgentBaseData.clean

    // return to Idle
    buffer.unstashAll(idle(cleanedGridAgentBaseData))
  }

  /** Method to ask all inferior grids.
    *
    * @param inferiorGridRefs
    *   A map containing a mapping from [[ActorRef]]s to corresponding [[UUID]]s
    *   of inferior nodes.
    * @param askMsgBuilder
    *   Function to build the asked message.
    * @param ctx
    *   Actor context to use.
    * @tparam T
    *   Type of data.
    * @return
    *   True if this grids has connected inferior grids or false if this no
    *   inferior grids.
    */
  private[grid] def askInferior[T](
      inferiorGridRefs: MultiMap[ActorRef[GridAgent.Message], UUID],
      askMsgBuilder: (ActorRef[GridAgent.Message], Set[UUID]) => Message,
  )(using ctx: ActorContext[GridAgent.Message]): Boolean = {
    if inferiorGridRefs.nonEmpty then {
      inferiorGridRefs.foreach {
        case (inferiorGridAgentRef, inferiorGridNodes) =>
          inferiorGridAgentRef ! askMsgBuilder(ctx.self, inferiorGridNodes)
      }

      true
    } else false
  }

  private def failFast(
      gridAgentInitData: GridAgentInitData,
      actorName: String,
      onlyOneSubGrid: Boolean,
  ): Unit = {
    if gridAgentInitData.superiorConnections.isEmpty && gridAgentInitData.inferiorConnections.isEmpty && !onlyOneSubGrid
    then
      throw new GridAgentInitializationException(
        s"$actorName has neither superior nor inferior grids! This can either " +
          s"be cause by wrong subnetGate information or invalid parametrization of the simulation!"
      )
  }

  private[grid] def unsupported(msg: Message, log: Logger)(using
      buffer: StashBuffer[GridAgent.Message]
  ): Unit = {
    log.debug(s"Received unsupported msg: $msg. Stash away!")
    buffer.stash(msg)
  }
}
