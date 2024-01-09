/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentBaseData
import edu.ie3.simona.agent.grid.GridAgentMessage.{
  ActivationAdapter,
  PMAdapter,
  VMAdapter,
  ValuesAdapter
}
import edu.ie3.simona.agent.grid.ReceivedValues._
import edu.ie3.simona.exceptions.agent.DBFSAlgorithmException
import edu.ie3.simona.model.grid.{NodeModel, RefSystem}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.PowerMessage.{
  AssetPowerChangedMessage,
  AssetPowerUnchangedMessage,
  FailedPowerFlow,
  ProvideGridPowerMessage,
  RequestAssetPowerMessage,
  RequestGridPowerMessage
}
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.ontology.messages.VoltageMessage.{
  ProvideSlackVoltageMessage,
  RequestSlackVoltageMessage
}
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.quantities.SquantsUtils.RichElectricPotential
import org.apache.pekko.actor.typed.scaladsl.AskPattern._
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Scheduler}
import org.apache.pekko.actor.{ActorRef => classicRef}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.{Timeout => PekkoTimeout}
import org.slf4j.Logger
import squants.Each

import java.time.{Duration, ZonedDateTime}
import java.util.UUID
import scala.concurrent.{ExecutionContext, Future}

trait DBFSAlgorithm extends PowerFlowSupport with GridResultsSupport {
  this: GridAgent =>

  def simulateGrid(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long
  ): Behavior[GridAgentMessage] = Behaviors.withStash(100) { buffer =>
    Behaviors.receive[GridAgentMessage] {

      // first part of the grid simulation, same for all gridAgents on all levels
      // we start with a forward-sweep by requesting the data from our child assets and grids (if any)
      case (ctx, ActivationAdapter(Activation(currentTick))) => {
        ctx.log.debug(
          "Start sweep number: {}",
          gridAgentBaseData.currentSweepNo
        )

        // we start the grid simulation by requesting the p/q values of all the nodes we are responsible for
        // as well as the slack voltage power from our superior grid
        // 1. assets p/q values
        askForAssetPowers(
          currentTick,
          gridAgentBaseData.sweepValueStores
            .get(gridAgentBaseData.currentSweepNo),
          gridAgentBaseData.gridEnv.nodeToAssetAgents,
          gridAgentBaseData.gridEnv.gridModel.mainRefSystem,
          gridAgentBaseData.powerFlowParams.sweepTimeout
        )(ctx)

        // 2. inferior grids p/q values
        askInferiorGridsForPowers(
          gridAgentBaseData.currentSweepNo,
          gridAgentBaseData.gridEnv.subgridGateToActorRef,
          gridAgentBaseData.inferiorGridGates,
          gridAgentBaseData.powerFlowParams.sweepTimeout
        )(ctx)

        // 3. superior grids slack voltage
        askSuperiorGridsForSlackVoltages(
          gridAgentBaseData.currentSweepNo,
          gridAgentBaseData.gridEnv.subgridGateToActorRef,
          gridAgentBaseData.superiorGridGates,
          gridAgentBaseData.powerFlowParams.sweepTimeout
        )(ctx)

        simulateGrid(gridAgentBaseData, currentTick)
      }

      // if we receive power values as response on our request, we process them here
      case (ctx, ValuesAdapter(receivedValues: ReceivedValues)) => {
        // we just received either all provided slack voltage values or all provided power values
        val updatedGridAgentBaseData: GridAgentBaseData = receivedValues match {
          case receivedPowers: ReceivedPowerValues =>
            /* Can be a message from an asset or a message from an inferior grid */
            gridAgentBaseData.updateWithReceivedPowerValues(receivedPowers)
          case receivedSlacks: ReceivedSlackVoltageValues =>
            gridAgentBaseData.updateWithReceivedSlackVoltages(receivedSlacks)
          case unknownReceivedValues =>
            throw new DBFSAlgorithmException(
              s"Received unknown values: $unknownReceivedValues"
            )
        }

        // check if we have enough data for a power flow calculation or a
        // power differences check (if the grid agent is a superior agent)
        // if yes, check if we have failing power flow in at least one of the inferior grids
        // if there are failing ones, escalate the failure to the superior grid (if any),
        // if not go to power flow or power differences check
        // if we haven't received everything yet, stay and wait
        val allValuesReceived =
          updatedGridAgentBaseData.allRequestedDataReceived

        ctx.log.debug(
          "{}",
          if (allValuesReceived)
            "Got answers for all my requests for Slack Voltages and Power Values."
          else
            "Still waiting for answers my requests for Slack Voltages and Power Values."
        )

        if (gridAgentBaseData.isSuperior) {
          goToCheckPowerDifferencesOrStay(
            allValuesReceived,
            updatedGridAgentBaseData
          )
        } else {
          goToPowerFlowCalculationOrStay(
            allValuesReceived,
            updatedGridAgentBaseData
          )
        }
      }

      // if we receive a request for slack voltages from our inferior grids we want to answer it
      case (
            ctx,
            VMAdapter(
              RequestSlackVoltageMessage(
                currentSweepNo,
                nodeUuids,
                sender: ActorRef[GridAgentMessage]
              )
            )
          ) => {
        ctx.log.debug(
          s"Received Slack Voltages request from {} for nodes {} and sweepNo: {}",
          sender,
          nodeUuids,
          gridAgentBaseData.currentSweepNo
        )

        nodeUuids.map { nodeUuid =>
          // we either have voltages ready calculated (not the first sweep) or we don't have them here
          // -> return calculated value or target voltage as physical value
          (gridAgentBaseData.sweepValueStores.get(currentSweepNo) match {
            case Some(result) =>
              Some(result, currentSweepNo)
            case None =>
              // this happens if this agent is either a) the superior grid agent, because it will always get a request for
              // the next sweep, as it triggers calculations for the next sweep or b) at all other
              // (non last downstream grid agents) in sweep 0
              ctx.log.debug(
                "Unable to find slack voltage for nodes '{}' in sweep '{}'. Try to get voltage of previous sweep.",
                nodeUuids,
                currentSweepNo
              )
              gridAgentBaseData.sweepValueStores
                .get(currentSweepNo - 1)
                .map((_, currentSweepNo - 1))
          }).map { case (result, sweepNo) =>
            // get nodeUUID
            result.sweepData.find(_.nodeUuid == nodeUuid) match {
              case Some(sweepValueStoreData) =>
                val slackVoltageInPu = sweepValueStoreData.stateData.voltage
                val mainRefSystem =
                  gridAgentBaseData.gridEnv.gridModel.mainRefSystem
                (
                  mainRefSystem.vInSi(slackVoltageInPu.real),
                  mainRefSystem.vInSi(slackVoltageInPu.imag)
                )
              case None =>
                throw new DBFSAlgorithmException(
                  s"Requested nodeUuid $nodeUuid " +
                    s"not found in sweep value store data for sweepNo: $sweepNo. This indicates" +
                    s"either a wrong processing of a previous sweep result or inconsistencies in grid model data!"
                )
            }
          }.getOrElse {
            ctx.log.debug(
              "Unable to get slack voltage for node '{}' in sweeps '{}' and '{}'. Returning target voltage.",
              nodeUuid,
              currentSweepNo,
              currentSweepNo - 1
            )

            val refSystem =
              gridAgentBaseData.gridEnv.gridModel.mainRefSystem

            /* Determine the slack node voltage under consideration of the target voltage set point */
            val vTarget =
              gridAgentBaseData.gridEnv.gridModel.gridComponents.nodes
                .find { case NodeModel(uuid, _, _, isSlack, _, _) =>
                  uuid == nodeUuid && isSlack
                }
                .map(_.vTarget)
                .getOrElse(Each(1d))
            val vSlack =
              refSystem.nominalVoltage.multiplyWithDimensionles(vTarget)

            (
              vSlack,
              refSystem.vInSi(0d)
            )
          } match {
            case (slackE, slackF) =>
              ctx.log.debug(
                s"Provide {} to {} for node {} and sweepNo: {}",
                s"$slackE, $slackF",
                sender,
                nodeUuid,
                gridAgentBaseData.currentSweepNo
              )

              ExchangeVoltage(nodeUuid, slackE, slackF)
          }
        } match {
          case exchangeVoltages =>
            sender ! VMAdapter(
              ProvideSlackVoltageMessage(currentSweepNo, exchangeVoltages)
            )
            Behaviors.same
        }
      }

      // receive grid power values message request from superior grids
      // before power flow calc for this sweep we either have to stash() the message to answer it later (in current sweep)
      // or trigger a new run for the next sweepNo
      case (
            ctx,
            PMAdapter(
              RequestGridPowerMessage(
                requestSweepNo,
                nodeUuids,
                sender: ActorRef[GridAgentMessage]
              )
            )
          ) => {
        if (gridAgentBaseData.currentSweepNo == requestSweepNo) {
          ctx.log.debug(
            s"Received request for grid power values for sweepNo {} before my first power flow calc. Stashing away.",
            requestSweepNo
          )

          buffer.stash(
            PMAdapter(
              RequestGridPowerMessage(requestSweepNo, nodeUuids, sender)
            )
          )

          Behaviors.same
        } else {
          ctx.log.debug(
            s"Received request for grid power values for a NEW sweep (request: {}, my: {})",
            requestSweepNo,
            gridAgentBaseData.currentSweepNo
          )
          ctx.self ! ValuesAdapter(PrepareNextSweepTrigger(currentTick))

          buffer.stash(
            PMAdapter(
              RequestGridPowerMessage(requestSweepNo, nodeUuids, sender)
            )
          )

          simulateGrid(
            gridAgentBaseData.copy(currentSweepNo = requestSweepNo),
            currentTick
          )
        }
      }

      // called when a grid power values request from a superior grid is received
      // which is similar to a new sweep and causes a) a power flow with updated slack voltage values and
      // b) afterwards a request for updated power values from inferior grids and assets with updated voltage values
      // based on the just carried out power flow
      case (ctx, ValuesAdapter(PrepareNextSweepTrigger(_))) => {
        // request the updated slack voltages from the superior grid
        askSuperiorGridsForSlackVoltages(
          gridAgentBaseData.currentSweepNo,
          gridAgentBaseData.gridEnv.subgridGateToActorRef,
          gridAgentBaseData.superiorGridGates,
          gridAgentBaseData.powerFlowParams.sweepTimeout
        )

        ctx.log.debug(s"Going to handlePowerFlowCalculations")

        handlePowerFlowCalculations(gridAgentBaseData, currentTick)
      }

      // last step which should includes a) information on inferior grids about finish and
      // b) cleanup of receiveMaps and sweepStore
      case (ctx, ValuesAdapter(FinishGridSimulationTrigger(currentTick))) => {
        // inform my child grids about the end of this grid simulation
        gridAgentBaseData.inferiorGridGates
          .map {
            gridAgentBaseData.gridEnv.subgridGateToActorRef(_)
          }
          .distinct
          .foreach(_ ! FinishGridSimulationTrigger(currentTick))

        // inform every system participant about the end of this grid simulation
        gridAgentBaseData.gridEnv.nodeToAssetAgents.foreach {
          case (_, actors) =>
            actors.foreach(actor => {
              actor ! FinishGridSimulationTrigger(
                currentTick
              )
            })
        }

        // notify listener about the results
        ctx.log.debug(
          "Calculate results and sending the results to the listener ..."
        )
        createAndSendPowerFlowResults(
          gridAgentBaseData,
          currentTick.toDateTime(simStartTime)
        )(ctx.log)

        // do my cleanup stuff
        ctx.log.debug("Doing my cleanup stuff")

        // / clean copy of the gridAgentBaseData
        val cleanedGridAgentBaseData = GridAgentBaseData.clean(
          gridAgentBaseData,
          gridAgentBaseData.superiorGridNodeUuids,
          gridAgentBaseData.inferiorGridGates
        )

        // / inform scheduler that we are done with the whole simulation and request new trigger for next time step
        environmentRefs.scheduler ! Completion(
          activationAdapter,
          Some(currentTick + resolution)
        )

        // return to Idle
        idle(cleanedGridAgentBaseData)
      }

      case (_, _) =>
        Behaviors.unhandled
    }
  }

  def handlePowerFlowCalculations(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long
  ): Behavior[GridAgentMessage] = Behaviors.receive[GridAgentMessage] {

    case (_, _) =>
      Behaviors.unhandled
  }

  def checkPowerDifferences(
      gridAgentBaseData: GridAgentBaseData
  ): Behavior[GridAgentMessage] = Behaviors.receive[GridAgentMessage] {

    case (_, _) =>
      Behaviors.unhandled
  }

  /** Triggers an execution of the pekko `ask` pattern for all power values of
    * assets (if any) of this [[GridAgent]].
    *
    * @param currentTick
    *   the current tick a data request is send for
    * @param sweepValueStore
    *   the current sweep value store containing the current node voltage for
    *   the assets
    * @param nodeToAssetAgents
    *   a map contains a mapping between nodes and the [[classicRef]] s located
    *   \@ those nodes
    * @param refSystem
    *   the reference system of the [[edu.ie3.simona.model.grid.GridModel]] of
    *   this [[GridAgent]]
    * @param askTimeout
    *   a timeout for the request
    */
  private def askForAssetPowers(
      currentTick: Long,
      sweepValueStore: Option[SweepValueStore],
      nodeToAssetAgents: Map[UUID, Set[classicRef]],
      refSystem: RefSystem,
      askTimeout: Duration
  )(implicit ctx: ActorContext[GridAgentMessage]): Unit = {
    implicit val timeout: PekkoTimeout = PekkoTimeout.create(askTimeout)
    implicit val ec: ExecutionContext = ctx.executionContext

    ctx.log.debug(s"asking assets for power values: {}", nodeToAssetAgents)

    if (nodeToAssetAgents.values.flatten.nonEmpty)
      ctx.pipeToSelf[GridAgentMessage](
        Future
          .sequence(
            nodeToAssetAgents.flatten { case (nodeUuid, assetActorRefs) =>
              assetActorRefs.map(assetAgent => {

                val (eInPu, fInPU) =
                  sweepValueStore match {
                    case Some(sweepValueStore) =>
                      val (eInSi, fInSi) = refSystem.vInSi(
                        sweepValueStore.sweepData
                          .find(_.nodeUuid == nodeUuid)
                          .getOrElse(
                            throw new DBFSAlgorithmException(
                              s"Provided Sweep value store contains no data for node with id $nodeUuid"
                            )
                          )
                          .stateData
                          .voltage
                      )
                      (
                        refSystem.vInPu(eInSi),
                        refSystem.vInPu(fInSi)
                      )
                    case None =>
                      (
                        Each(1d),
                        Each(0d)
                      )
                  }

                (assetAgent ? RequestAssetPowerMessage(
                  currentTick,
                  eInPu,
                  fInPU
                )).map {
                  case providedPowerValuesMessage: AssetPowerChangedMessage =>
                    (assetAgent, providedPowerValuesMessage)
                  case assetPowerUnchangedMessage: AssetPowerUnchangedMessage =>
                    (assetAgent, assetPowerUnchangedMessage)
                }
              })
            }.toVector
          )
          .map(res => ValuesAdapter(ReceivedAssetPowerValues(res)))
      )
  }

  /** Triggers an execution of the pekko `ask` pattern for all power values @
    * connection nodes of inferior grids (if any) of this [[GridAgent]].
    *
    * @param currentSweepNo
    *   the current sweep number the DBFS is in
    * @param subGridGateToActorRef
    *   a map containing a mapping from [[SubGridGate]] s to corresponding
    *   [[ActorRef]] s of [[GridAgent]] s @ these nodes
    * @param askTimeout
    *   a timeout for the request
    */
  private def askInferiorGridsForPowers(
      currentSweepNo: Int,
      subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgentMessage]],
      inferiorGridGates: Seq[SubGridGate],
      askTimeout: Duration
  )(implicit ctx: ActorContext[GridAgentMessage]): Unit = {
    implicit val timeout: PekkoTimeout = PekkoTimeout.create(askTimeout)
    implicit val ec: ExecutionContext = ctx.executionContext
    implicit val scheduler: Scheduler = ctx.system.scheduler

    ctx.log.debug(
      s"asking inferior grids for power values: {}",
      inferiorGridGates
    )

    Option.when(inferiorGridGates.nonEmpty) {
      ctx.pipeToSelf[GridAgentMessage](
        Future
          .sequence(
            inferiorGridGates
              .map { inferiorGridGate =>
                subGridGateToActorRef(
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
              .map { case (inferiorGridAgentRef, inferiorGridGateNodes) =>
                inferiorGridAgentRef
                  .ask[GridAgentMessage](ref =>
                    PMAdapter(
                      RequestGridPowerMessage(
                        currentSweepNo,
                        inferiorGridGateNodes.distinct,
                        ref
                      )
                    )
                  )
                  .map {
                    case PMAdapter(
                          provideGridPowerMessage: ProvideGridPowerMessage
                        ) =>
                      (inferiorGridAgentRef, provideGridPowerMessage)
                    case PMAdapter(FailedPowerFlow) =>
                      (inferiorGridAgentRef, FailedPowerFlow)
                  }
              }
              .toVector
          )
          .map(res => ValuesAdapter(ReceivedGridPowerValues(res)))
      )
    }
  }

  /** Triggers an execution of the pekko `ask` pattern for all slack voltages of
    * superior grids (if any) of this [[GridAgent]].
    *
    * @param currentSweepNo
    *   the current sweep number the DBFS is in
    * @param subGridGateToActorRef
    *   a map containing a mapping from [[SubGridGate]] s to corresponding
    *   [[ActorRef]] s of [[GridAgent]] s @ these nodes
    * @param askTimeout
    *   a timeout for the request
    */
  private def askSuperiorGridsForSlackVoltages(
      currentSweepNo: Int,
      subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgentMessage]],
      superiorGridGates: Vector[SubGridGate],
      askTimeout: Duration
  )(implicit ctx: ActorContext[GridAgentMessage]): Unit = {
    implicit val timeout: PekkoTimeout = PekkoTimeout.create(askTimeout)
    implicit val ec: ExecutionContext = ctx.executionContext
    implicit val scheduler: Scheduler = ctx.system.scheduler

    ctx.log.debug(
      s"asking superior grids for slack voltage values: {}",
      superiorGridGates
    )

    Option.when(superiorGridGates.nonEmpty) {
      ctx.pipeToSelf[GridAgentMessage](
        Future
          .sequence(
            superiorGridGates
              .groupBy(subGridGateToActorRef(_))
              .map { case (superiorGridAgent, gridGates) =>
                superiorGridAgent
                  .ask[GridAgentMessage](ref =>
                    VMAdapter(
                      RequestSlackVoltageMessage(
                        currentSweepNo,
                        gridGates.map(_.superiorNode.getUuid),
                        ref
                      )
                    )
                  )
                  .map {
                    case VMAdapter(
                          providedSlackValues: ProvideSlackVoltageMessage
                        ) =>
                      (superiorGridAgent, providedSlackValues)
                  }
              }
              .toVector
          )
          .map(res => ValuesAdapter(ReceivedSlackVoltageValues(res)))
      )
    }
  }

  /** Create an instance of
    * [[edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent]] and send it to
    * all listener Note: in the future this one could become a bottleneck for
    * power flow calculation timesteps. For performance improvements one might
    * consider putting this into a future with pipeTo. One has to consider how
    * to deal with unfinished futures in shutdown phase then
    *
    * @param gridAgentBaseData
    *   the grid agent base data
    * @param currentTimestamp
    *   the current time stamp
    */
  def createAndSendPowerFlowResults(
      gridAgentBaseData: GridAgentBaseData,
      currentTimestamp: ZonedDateTime
  )(implicit log: Logger): Unit = {
    gridAgentBaseData.sweepValueStores.lastOption.foreach {
      case (_, valueStore) =>
        notifyListener(
          this.createResultModels(
            gridAgentBaseData.gridEnv.gridModel,
            valueStore
          )(
            currentTimestamp,
            log
          )
        )
    }
  }
}
