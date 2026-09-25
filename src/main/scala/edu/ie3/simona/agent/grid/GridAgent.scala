/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.actor.SimonaActorNaming
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
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.ServiceMessage
import edu.ie3.simona.service.Data.SecondaryData.{CurrentVoltage, WeatherData}
import edu.ie3.simona.service.DataTimeType
import edu.ie3.simona.service.results.ResultServiceProxy.ExpectResult
import edu.ie3.simona.service.weather.WeatherService.WeatherRegistrationData
import edu.ie3.simona.util.TickUtil.toDateTime
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
import squants.{Dimensionless, Each}

import java.util.UUID

object GridAgent extends DBFSAlgorithm with DCMAlgorithm {

  /** All messages, that can be received by a [[GridAgent]]. */
  final type Message = InternalRequest | InternalReply | Activation |
    ServiceMessage.Response

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
      val gridAgentBaseData = GridAgentBaseData.create(
        initData.gridModel,
        initData.inferiorConnections,
        initData.superiorConnections,
        initData.nodeToAssetAgents,
        initData.refToSubgrid,
        initData.simulationStart,
        initData.powerFlowParams,
        actorName,
      )

      // Register for weather service if thermal segments exist
      val segmentCoordinates =
        initData.gridModel.gridComponents.segmentCoordinates
      if segmentCoordinates.nonEmpty then
        ctx.log.debug(
          s"Registering $actorName for weather service with {} segment coordinates",
          segmentCoordinates.size,
        )
        segmentCoordinates.foreach { (segmentUuid, coordinate) =>
          constantData.environmentRefs.weather !
            ServiceMessage.SecondaryServiceRegistrationMessage(
              ctx.self,
              DataTimeType.Current,
              WeatherRegistrationData(coordinate, Some(segmentUuid.toString)),
            )
        }

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

    case (ctx, DoCongestionManagement(currentTick, results)) =>
      startCongestionManagement(
        gridAgentBaseData,
        currentTick,
        results,
        ctx,
      )

    // Handle weather data provision
    case (
          ctx,
          ServiceMessage.DataProvision(
            _,
            _,
            weatherData: WeatherData,
            _,
            Some(key),
          ),
        ) =>
      try
        val segmentUuid = UUID.fromString(key)
        ctx.log.debug(
          s"Received weather data for segment $segmentUuid"
        )
        idle(
          gridAgentBaseData.copy(
            weatherData =
              gridAgentBaseData.weatherData.updated(segmentUuid, weatherData)
          )
        )
      catch
        case _: IllegalArgumentException =>
          // not a segment UUID key, ignore
          Behaviors.same

    // Handle registration failure (fail-fast)
    case (_, ServiceMessage.RegistrationFailedMessage(_)) =>
      throw new GridAgentInitializationException(
        s"Registration with weather service failed for grid agent ${gridAgentBaseData.actorName}. " +
          "Please ensure that a weather service is configured and the segment coordinates are within the weather data coverage area."
      )

    // Handle registration success (no-op, weather data arrives via DataProvision)
    case (_, ServiceMessage.RegistrationSuccessfulMessage(_, _, _)) =>
      Behaviors.same

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

    // Get the node voltages of the last power flow result
    val (gridModel, lastValueStore) = {
      val model = gridAgentBaseData.gridEnv.gridModel
      val storeOpt = gridAgentBaseData.sweepValueStores.lastOption
        .map { case (_, valueStore) => valueStore }
      (model, storeOpt)
    }

    val mainRefSystem = gridModel.mainRefSystem
    val nodeVoltageInPu = lastValueStore
      .map { valueStore =>
        valueStore.sweepData.map { svd =>
          val pu = Each(svd.stateData.voltage.abs)
          svd.nodeUuid -> pu
        }.toMap
      }
      .getOrElse(Map.empty)

    val updatedThermalLineStates = {
      if doAmpacityCalc then {
        ensureWeatherDataAvailable(gridAgentBaseData, gridModel)

        // Initialize the thermal line states of all segments that have not
        // been initialized yet, using the first weather data of the
        // segment's calculation point
        val lineStates =
          initializeThermalLineStates(gridAgentBaseData, gridModel)

        gridModel.gridComponents.thermalLineSegments.map { lineSegment =>
          val lastLineState = lineStates(lineSegment.uuid)

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

          val nominalVoltage = lineSegment.cableSetup.voltage
          val nominalVoltagePu = mainRefSystem.vInPu(nominalVoltage)

          val cableVoltage = gridModel.gridComponents.lines
            .find(_.uuid == lineSegment.lineUuid)
            .map { line =>
              val voltageAtNodeAPu =
                nodeVoltageInPu.getOrElse(line.nodeAUuid, nominalVoltagePu)
              val voltageAtNodeBPu =
                nodeVoltageInPu.getOrElse(line.nodeBUuid, nominalVoltagePu)

              val avgPu =
                Each((voltageAtNodeAPu.toEach + voltageAtNodeBPu.toEach) / 2d)
              mainRefSystem.vInSi(avgPu)
            }
            .getOrElse(nominalVoltage)

          val lineStateWithInput = {
            val inputs =
              gridAgentBaseData.weatherData.get(lineSegment.uuid) match {
                case Some(weather) =>
                  Seq(
                    CurrentVoltage(lineSegment.uuid, cableVoltage),
                    weather,
                  )
                case None =>
                  Seq(CurrentVoltage(lineSegment.uuid, cableVoltage))
              }
            lineSegment.handleInput(lastLineState, inputs)
          }

          lineSegment.uuid ->
            lineSegment.determineState(
              currentTick,
              lineStateWithInput,
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

    // Collect ampacity (line temperature) results and forward to ampacity writer if available
    val ampacityResults = updatedThermalLineStates.values.toSeq.flatMap {
      state =>
        val dateTime = gridAgentBaseData.simulationStart.plusSeconds(state.tick)
        state.currentLineSegmentThermalModel.createResults(state, dateTime)
    }

    // send to writer actor when configured
    constantData.environmentRefs.ampacityWriter.foreach(
      _ ! edu.ie3.simona.event.listener.AmpacityResultWriter
        .WriteLineTemps(ampacityResults)
    )

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

  /** Fails fast if the weather data of a thermal line segment is not yet
    * available at calculation time.
    *
    * @param gridAgentBaseData
    *   Current state data of the [[GridAgent]].
    * @param gridModel
    *   The grid model of the [[GridAgent]].
    * @throws GridAgentInitializationException
    *   If weather data is missing for at least one thermal line segment.
    */
  private[grid] def ensureWeatherDataAvailable(
      gridAgentBaseData: GridAgentBaseData,
      gridModel: GridModel,
  ): Unit = {
    val missingWeather =
      gridModel.gridComponents.thermalLineSegments
        .map(_.uuid)
        .filter(segmentUuid =>
          !gridAgentBaseData.weatherData.contains(segmentUuid)
        )
    if missingWeather.nonEmpty then
      throw new GridAgentInitializationException(
        s"Ampacity calculation is activated for grid agent ${gridAgentBaseData.actorName}, " +
          s"but no weather data has been received for line segment(s): " +
          missingWeather.mkString(", ") +
          ". Please ensure that a weather service is configured and provides data for the segment coordinates."
      )
  }

  /** Initializes the thermal line states of all thermal line segments that have
    * not been initialized yet, using the weather data of the segment's
    * calculation point as the initial ground temperature.
    *
    * @param gridAgentBaseData
    *   Current state data of the [[GridAgent]].
    * @param gridModel
    *   The grid model of the [[GridAgent]].
    * @return
    *   A map of all thermal line states (existing and newly initialized).
    */
  private[grid] def initializeThermalLineStates(
      gridAgentBaseData: GridAgentBaseData,
      gridModel: GridModel,
  ): Map[UUID, LineState] = {
    ensureWeatherDataAvailable(gridAgentBaseData, gridModel)

    val newlyInitializedLineStates =
      gridModel.gridComponents.thermalLineSegments
        .filter(lineSegment =>
          !gridAgentBaseData.thermalLineStates.contains(lineSegment.uuid)
        )
        .map { lineSegment =>
          val weather = gridAgentBaseData.weatherData(lineSegment.uuid)
          lineSegment.uuid -> LineSegmentThermalModel.initState(
            lineSegment.cableSetup,
            lineSegment,
            LineSegmentThermalModel.groundTemperatureFromWeather(
              lineSegment.cableSetup,
              weather,
            ),
          )
        }
        .toMap

    gridAgentBaseData.thermalLineStates ++ newlyInitializedLineStates
  }

}
