/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.Complex
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.powerflow.model.FailureCause.CalculationFailed
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.FailedPowerFlowResult.FailedNewtonRaphsonPFResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.powerflow.model.enums.NodeType
import edu.ie3.simona.agent.grid.GridAgent.idleimport edu.ie3.simona.agent.grid.GridAgentData.{GridAgentBaseData,GridAgentConstantData, PowerFlowDoneData,
}
import edu.ie3.simona.agent.grid.GridAgentMessages.Responses.ExchangeVoltage
import edu.ie3.simona.agent.grid.GridAgentMessages._
import edu.ie3.simona.agent.participant.ParticipantAgent.{
  FinishParticipantSimulation,
  ParticipantMessage, RequestAssetPowerMessage, }
import edu.ie3.simona.event.RuntimeEvent.PowerFlowFailed
import edu.ie3.simona.exceptions.agent.DBFSAlgorithmException
import edu.ie3.simona.model.grid.{NodeModel, RefSystem}
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.Completion
import edu.ie3.simona.util.TickUtil.TickLong
import edu.ie3.util.scala.quantities.DefaultQuantities._
import edu.ie3.util.scala.quantities.SquantsUtils.RichElectricPotential
import org.apache.pekko.actor.typed.scaladsl.AskPattern._
import org.apache.pekko.actor.typed.scaladsl.adapter.TypedActorRefOps
import org.apache.pekko.actor.typed.scaladsl.{
  ActorContext,
  Behaviors,
  StashBuffer,
}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, Scheduler}
import org.apache.pekko.pattern.ask
import org.apache.pekko.util.{Timeout => PekkoTimeout}
import org.slf4j.Logger
import squants.Each

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.ElectricPotential
import scala.concurrent.duration.Duration
import java.time.{Duration => JavaDuration}
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Trait that is normally mixed into every [[GridAgent]] to enable distributed
  * forward backward sweep (DBFS) algorithm execution. It is considered to be
  * the standard behaviour of a [[GridAgent]].
  */
trait DBFSAlgorithm extends PowerFlowSupport with GridResultsSupport {

  /** Method that defines the [[Behavior]] for simulating the grid.
    * @param gridAgentData
    *   state data of the actor
    * @param currentTick
    *   current simulation tick
    * @return
    *   a [[Behavior]]
    */
  private[grid] def simulateGrid(
      gridAgentData: GridAgentData,
      currentTick: Long,
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, message) =>
      (message, gridAgentData) match {
        // first part of the grid simulation, same for all gridAgents on all levels
        // we start with a forward-sweep by requesting the data from our child assets and grids (if any)
        case (
              WrappedActivation(activation: Activation),
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          ctx.log.debug(
            "Start sweep number: {}",
            gridAgentBaseData.currentSweepNo,
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
            gridAgentBaseData.powerFlowParams.sweepTimeout,
          )(ctx)

          // 2. inferior grids p/q values
          askInferiorGridsForPowers(
            gridAgentBaseData.currentSweepNo,
            gridAgentBaseData.gridEnv.subgridGateToActorRef,
            gridAgentBaseData.inferiorGridGates,
            gridAgentBaseData.powerFlowParams.sweepTimeout,
          )(ctx)

          // 3. superior grids slack voltage
          askSuperiorGridsForSlackVoltages(
            gridAgentBaseData.currentSweepNo,
            gridAgentBaseData.gridEnv.subgridGateToActorRef,
            gridAgentBaseData.superiorGridGates,
            gridAgentBaseData.powerFlowParams.sweepTimeout,
          )(ctx)

          simulateGrid(gridAgentBaseData, activation.tick)

        // if we receive power values as response on our request, we process them here
        case (
              receivedValues: ReceivedValues,
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          // we just received either all provided slack voltage values or all provided power values
          val updatedGridAgentBaseData: GridAgentBaseData =
            receivedValues match {
              case receivedPowers: ReceivedPowerValues =>
                /* Can be a message from an asset or a message from an inferior grid */
                gridAgentBaseData.updateWithReceivedPowerValues(receivedPowers)
              case receivedSlacks: ReceivedSlackVoltageValues =>
                gridAgentBaseData.updateWithReceivedSlackVoltages(
                  receivedSlacks
                )
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
              "Still waiting for answers my requests for Slack Voltages and Power Values.",
          )

          if (gridAgentBaseData.isSuperior) {
            goToCheckPowerDifferencesOrStay(
              allValuesReceived,
              updatedGridAgentBaseData,
              currentTick,
              simulateGrid,
            )(ctx, constantData, buffer)
          } else {
            goToPowerFlowCalculationOrStay(
              allValuesReceived,
              updatedGridAgentBaseData,
              currentTick,
              simulateGrid,
            )(ctx, constantData, buffer)
          }

        // if we receive a request for slack voltages from our inferior grids we want to answer it
        case (
              SlackVoltageRequest(
                currentSweepNo,
                nodeUuids,
                sender,
              ),
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          ctx.log.debug(
            s"Received Slack Voltages request from {} for nodes {} and sweepNo: {}",
            sender,
            nodeUuids,
            gridAgentBaseData.currentSweepNo,
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
                  currentSweepNo,
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
                    mainRefSystem.vInSi(slackVoltageInPu.imag),
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
                currentSweepNo - 1,
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
                refSystem.vInSi(0d),
              )
            } match {
              case (slackE, slackF) =>
                ctx.log.debug(
                  s"Provide {} to {} for node {} and sweepNo: {}",
                  s"$slackE, $slackF",
                  sender,
                  nodeUuid,
                  gridAgentBaseData.currentSweepNo,
                )

                ExchangeVoltage(nodeUuid, slackE, slackF)
            }
          } match {
            case exchangeVoltages =>
              sender ! SlackVoltageResponse(
                currentSweepNo,
                exchangeVoltages,
              )
              Behaviors.same
          }

        // receive grid power values message request from superior grids
        // before power flow calc for this sweep we either have to stash() the message to answer it later (in current sweep)
        // or trigger a new run for the next sweepNo
        case (
              msg @ RequestGridPower(
                requestSweepNo,
                _,
                _,
              ),
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          if (gridAgentBaseData.currentSweepNo == requestSweepNo) {
            ctx.log.debug(
              s"Received request for grid power values for sweepNo {} before my first power flow calc. Stashing away.",
              requestSweepNo,
            )

            buffer.stash(msg)

            Behaviors.same
          } else {
            ctx.log.debug(
              s"Received request for grid power values for a NEW sweep (request: {}, my: {})",
              requestSweepNo,
              gridAgentBaseData.currentSweepNo,
            )
            ctx.self ! PrepareNextSweepTrigger(currentTick)

            buffer.stash(msg)

            simulateGrid(
              gridAgentBaseData.copy(currentSweepNo = requestSweepNo),
              currentTick,
            )
          }

        // after power flow calc for this sweepNo
        case (
              RequestGridPower(_, requestedNodeUuids, sender),
              powerFlowDoneData @ PowerFlowDoneData(
                gridAgentBaseData,
                powerFlowResult,
                pendingRequestAnswers,
              ),
            ) =>
          /* Determine the subgrid number of the grid agent, that has sent the request */
          val firstRequestedNodeUuid = requestedNodeUuids.headOption.getOrElse(
            throw new DBFSAlgorithmException(
              "Did receive a grid power request but without specified nodes"
            )
          )

          gridAgentBaseData.gridEnv.subgridGateToActorRef
            .map { case (subGridGate, _) => subGridGate.superiorNode }
            .find(_.getUuid == firstRequestedNodeUuid)
            .map(_.getSubnet) match {
            case Some(requestingSubgridNumber) =>
              powerFlowResult match {
                case validNewtonRaphsonPFResult: ValidNewtonRaphsonPFResult =>
                  val exchangePowers = requestedNodeUuids
                    .map { nodeUuid =>
                      /* Figure out the node index for each requested node */
                      nodeUuid -> gridAgentBaseData.gridEnv.gridModel.nodeUuidToIndexMap
                        .get(nodeUuid)
                        .flatMap { nodeIndex =>
                          /* Find matching node result */
                          validNewtonRaphsonPFResult.nodeData.find(stateData =>
                            stateData.index == nodeIndex
                          )
                        }
                        .map {
                          case StateData(_, nodeType, _, power)
                              if nodeType == NodeType.SL =>
                            val refSystem =
                              gridAgentBaseData.gridEnv.gridModel.mainRefSystem
                            val (pInPu, qInPu) =
                              (power.real, power.imag)
                            // The power flow result data provides the nodal residual power at the slack node.
                            // A feed-in case from the inferior grid TO the superior grid leads to positive residual power at the
                            // inferior grid's *slack node* (superior grid seems to be a load to the inferior grid).
                            // To model the exchanged power from the superior grid's point of view, -1 has to be multiplied.
                            // (Inferior grid is a feed in facility to superior grid, which is negative then). Analogously for load case.
                            (
                              refSystem.pInSi(pInPu) * (-1),
                              refSystem.qInSi(qInPu) * (-1),
                            )
                          case _ =>
                            /* TODO: As long as there are no multiple slack nodes, provide "real" power only for the slack node */
                            (
                              zeroMW,
                              zeroMVAr,
                            )
                        }
                        .getOrElse {
                          throw new DBFSAlgorithmException(
                            s"Got a request for power @ node with uuid $requestedNodeUuids but cannot find it in my result data!"
                          )
                        }
                    }
                    .map { case (nodeUuid, (p, q)) =>
                      Responses.ExchangePower(
                        nodeUuid,
                        p,
                        q,
                      )
                    }

                  /* Determine the remaining replies */
                  val stillPendingRequestAnswers =
                    pendingRequestAnswers.filterNot(
                      _ == requestingSubgridNumber
                    )

                  // update the sweep value store and clear all received maps
                  // note: normally it is expected that this has to be done after power flow calculations but for the sake
                  // of having it only once in the code we put this here. Otherwise it would have to been put before EVERY
                  // return with a valid power flow result (currently happens already in two situations)
                  val updatedGridAgentBaseData =
                    if (stillPendingRequestAnswers.isEmpty) {
                      gridAgentBaseData.storeSweepDataAndClearReceiveMaps(
                        validNewtonRaphsonPFResult,
                        gridAgentBaseData.superiorGridNodeUuids,
                        gridAgentBaseData.inferiorGridGates,
                      )
                    } else {
                      powerFlowDoneData.copy(pendingRequestAnswers =
                        stillPendingRequestAnswers
                      )
                    }

                  sender ! GridPowerResponse(exchangePowers)
                  simulateGrid(updatedGridAgentBaseData, currentTick)

                case _: FailedNewtonRaphsonPFResult =>
                  sender ! FailedPowerFlow
                  simulateGrid(gridAgentBaseData, currentTick)
              }
            case None =>
              /* It is not possible to determine, who has asked */
              ctx.log.error(
                "I got a grid power request from a subgrid I don't know. Can't answer it properly."
              )

              sender ! FailedPowerFlow
              Behaviors.stopped
          }

        // called when a grid power values request from a superior grid is received
        // which is similar to a new sweep and causes a) a power flow with updated slack voltage values and
        // b) afterwards a request for updated power values from inferior grids and assets with updated voltage values
        // based on the just carried out power flow
        case (
              PrepareNextSweepTrigger(_),
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          // request the updated slack voltages from the superior grid
          askSuperiorGridsForSlackVoltages(
            gridAgentBaseData.currentSweepNo,
            gridAgentBaseData.gridEnv.subgridGateToActorRef,
            gridAgentBaseData.superiorGridGates,
            gridAgentBaseData.powerFlowParams.sweepTimeout,
          )(ctx)

          ctx.log.debug(s"Going to HandlePowerFlowCalculation")

          handlePowerFlowCalculations(gridAgentBaseData, currentTick)

        // last step which should includes a) information on inferior grids about finish and
        // b) cleanup of receiveMaps and sweepStore
        case (
              FinishGridSimulationTrigger(currentTick),
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          // inform my child grids about the end of this grid simulation
          gridAgentBaseData.inferiorGridGates
            .map {
              gridAgentBaseData.gridEnv.subgridGateToActorRef(_)
            }
            .distinct
            .foreach(
              _ ! FinishGridSimulationTrigger(currentTick)
            )

          // inform every system participant about the end of this grid simulation
          gridAgentBaseData.gridEnv.nodeToAssetAgents.foreach {
            case (_, actors) =>
              actors.foreach { actor =>
                actor ! FinishParticipantSimulation(currentTick)
              }
          }

          // notify listener about the results
          ctx.log.debug(
            "Calculate results and sending the results to the listener ..."
          )
          createAndSendPowerFlowResults(
            gridAgentBaseData,
            currentTick.toDateTime(constantData.simStartTime),
          )(ctx.log, constantData)

          // do my cleanup stuff
          ctx.log.debug("Doing my cleanup stuff")

          // / clean copy of the gridAgentBaseData
          val cleanedGridAgentBaseData = GridAgentBaseData.clean(
            gridAgentBaseData,
            gridAgentBaseData.superiorGridNodeUuids,
            gridAgentBaseData.inferiorGridGates,
          )

          // / inform scheduler that we are done with the whole simulation and request new trigger for next time step
          constantData.environmentRefs.scheduler ! Completion(
            constantData.activationAdapter,
            Some(currentTick + constantData.resolution),
          )

          // return to Idle
          idle(cleanedGridAgentBaseData)

        // handles power request that arrive to early
        case (requestGridPower: RequestGridPower, _) =>
          ctx.log.debug(
            s"Received the message $requestGridPower too early. Stash away!"
          )
          buffer.stash(requestGridPower)
          Behaviors.same
      }
  }

  /** Method that defines the [[Behavior]] for handling the power flow
    * calculations. Generally used for power flow calculations only and only if
    * all data required are already received as requested.
    *
    * @param gridAgentData
    *   state data of the actor
    * @param currentTick
    *   current simulation tick
    * @return
    *   a [[Behavior]]
    */
  private def handlePowerFlowCalculations(
      gridAgentData: GridAgentData,
      currentTick: Long,
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {
    case (ctx, message) =>
      (message, gridAgentData) match {
        // main method for power flow calculations
        case (
              DoPowerFlowTrigger(currentTick, _),
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          ctx.log.debug(
            "Received the following power values to the corresponding nodes: {}",
            gridAgentBaseData.receivedValueStore.nodeToReceivedPower,
          )

          val gridModel = gridAgentBaseData.gridEnv.gridModel

          val (operatingPoint, slackNodeVoltages) = composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            gridAgentBaseData.receivedValueStore,
            gridModel.mainRefSystem,
          )

          newtonRaphsonPF(
            gridModel,
            gridAgentBaseData.powerFlowParams.maxIterations,
            operatingPoint,
            slackNodeVoltages,
          )(gridAgentBaseData.powerFlowParams.epsilon)(ctx.log) match {
            // if res is valid, ask our assets (if any) for updated power values based on the newly determined nodal voltages
            case validPowerFlowResult: ValidNewtonRaphsonPFResult =>
              ctx.log.debug(
                "{}",
                composeValidNewtonRaphsonPFResultVoltagesDebugString(
                  validPowerFlowResult,
                  gridModel,
                ),
              )

              val powerFlowDoneData =
                PowerFlowDoneData(gridAgentBaseData, validPowerFlowResult)

              val sweepValueStoreOpt = Some(
                SweepValueStore(
                  validPowerFlowResult,
                  gridModel.gridComponents.nodes,
                  gridModel.nodeUuidToIndexMap,
                )
              )

              if (
                askForAssetPowers(
                  currentTick,
                  sweepValueStoreOpt,
                  gridAgentBaseData.gridEnv.nodeToAssetAgents,
                  gridModel.mainRefSystem,
                  gridAgentBaseData.powerFlowParams.sweepTimeout,
                )(ctx)
              ) {
                // will return a future based on the `ask-pattern` which will be processed below
                handlePowerFlowCalculations(powerFlowDoneData, currentTick)
              } else {
                // when we don't have assets we can skip another request for different asset behaviour due to changed
                // voltage values and go back to SimulateGrid directly
                ctx.log.debug(
                  s"No generation or load assets in the grid. Going back to SimulateGrid."
                )

                // we can answer the stashed grid power requests now
                buffer.unstashAll(
                  simulateGrid(powerFlowDoneData, currentTick)
                )
              }

            case failedNewtonRaphsonPFResult: FailedNewtonRaphsonPFResult =>
              val powerFlowDoneData =
                PowerFlowDoneData(
                  gridAgentBaseData,
                  failedNewtonRaphsonPFResult,
                )
              ctx.log.warn(
                s"Subgrid {}: Power flow calculation before asking for updated powers did finally not converge! Cause: {}",
                gridModel.subnetNo,
                failedNewtonRaphsonPFResult.cause,
              )
              // we can answer the stashed grid power requests now and report a failed power flow back
              buffer.unstashAll(simulateGrid(powerFlowDoneData, currentTick))
          }

        // handler for the future provided by `askForAssetPowers` to check if there are any changes in generation/load
        // of assets based on updated nodal voltages
        case (
              receivedPowerValues: ReceivedPowerValues,
              powerFlowDoneData: PowerFlowDoneData,
            ) =>
          val gridAgentBaseData = powerFlowDoneData.gridAgentBaseData

          // check if we have changed values from our assets
          // if yes, we do another PF with adapted values
          // if no, we are done with the pf and ready to report to our parent grid
          val changed = receivedPowerValues.values.exists {
            case (_, _: AssetPowerChangedMessage) => true
            case _                                => false
          }

          if (changed) {
            ctx.log.debug(
              "Assets have changed their exchanged power with the grid. Update nodal powers and prepare new power flow."
            )
            val updatedGridAgentBaseData: GridAgentBaseData =
              gridAgentBaseData.updateWithReceivedPowerValues(
                receivedPowerValues,
                replace = true,
              )

            // check if we have enough data for a power flow calculation
            // if yes, go to the powerflow
            // if no, stay and wait
            val readyForPowerFlow =
              updatedGridAgentBaseData.allRequestedDataReceived
            ctx.log.debug(
              "{}",
              if (readyForPowerFlow)
                "Got answers for all my requests for Slack Voltages and Power Values."
              else
                "Still waiting for answers my requests for Slack Voltages and Power Values.",
            )

            goToPowerFlowCalculationOrStay(
              readyForPowerFlow,
              updatedGridAgentBaseData,
              currentTick,
              handlePowerFlowCalculations,
            )(ctx, constantData, buffer)

          } else {
            // no changes from assets, we want to go back to SimulateGrid and report the LF results to our parent grids if any requests
            ctx.log.debug(
              s"Assets have not changed their exchanged power or no voltage dependent behaviour. Going back to SimulateGrid."
            )
            // we can answer the stashed grid power requests now
            buffer.unstashAll(simulateGrid(powerFlowDoneData, currentTick))
          }

        // executed after request from the superior grid to execute a new sweep (forward sweep state)
        // this means we requested an update of the slack voltage values, but for now don't request (and hence don't expect)
        // updated power values for our power flow calculations
        case (
              receivedSlackValues: ReceivedSlackVoltageValues,
              gridAgentBaseData: GridAgentBaseData,
            ) =>
          ctx.log.debug(
            "Received Slack values for new forward sweep with same power but updated voltage values"
          )

          // power flow
          val gridModel = gridAgentBaseData.gridEnv.gridModel
          val previousSweepData = gridAgentBaseData.sweepValueStores.getOrElse(
            gridAgentBaseData.currentSweepNo - 1,
            throw new DBFSAlgorithmException(
              s"${gridAgentBaseData.actorName}: Unable to get results from previous sweep ${gridAgentBaseData.currentSweepNo - 1}!"
            ),
          )

          val (operatingPoint, slackNodeVoltages) =
            composeOperatingPointWithUpdatedSlackVoltages(
              receivedSlackValues,
              previousSweepData.sweepData,
              gridModel.gridComponents.transformers,
              gridModel.gridComponents.transformers3w,
              gridModel.mainRefSystem,
            )

          newtonRaphsonPF(
            gridModel,
            gridAgentBaseData.powerFlowParams.maxIterations,
            operatingPoint,
            slackNodeVoltages,
          )(gridAgentBaseData.powerFlowParams.epsilon)(ctx.log) match {
            case validPowerFlowResult: ValidNewtonRaphsonPFResult =>
              ctx.log.debug(
                "{}",
                composeValidNewtonRaphsonPFResultVoltagesDebugString(
                  validPowerFlowResult,
                  gridModel,
                ),
              )

              // update the data
              val sweepValueStore = SweepValueStore(
                validPowerFlowResult,
                gridModel.gridComponents.nodes,
                gridModel.nodeUuidToIndexMap,
              )
              val updatedSweepValueStore =
                gridAgentBaseData.sweepValueStores + (gridAgentBaseData.currentSweepNo -> sweepValueStore)

              // send request to child grids and assets for updated p/q values
              // we start the grid simulation by requesting the p/q values of all the nodes we are responsible for
              // as well as the slack voltage power from our superior grid
              // 1. assets p/q values
              val askForAssetPowersOpt =
                askForAssetPowers(
                  currentTick,
                  Some(sweepValueStore),
                  gridAgentBaseData.gridEnv.nodeToAssetAgents,
                  gridModel.mainRefSystem,
                  gridAgentBaseData.powerFlowParams.sweepTimeout,
                )(ctx)

              // 2. inferior grids p/q values
              val askForInferiorGridPowersOpt =
                askInferiorGridsForPowers(
                  gridAgentBaseData.currentSweepNo,
                  gridAgentBaseData.gridEnv.subgridGateToActorRef,
                  gridAgentBaseData.inferiorGridGates,
                  gridAgentBaseData.powerFlowParams.sweepTimeout,
                )(ctx)

              // when we don't have inferior grids and no assets both methods return None and we can skip doing another power
              // flow calculation otherwise we go back to simulate grid and wait for the answers
              if (!askForAssetPowersOpt && !askForInferiorGridPowersOpt) {
                ctx.log.debug(
                  "I don't have assets or child grids. " +
                    "Going back to SimulateGrid and provide the power flow result if there is any request left."
                )

                val powerFlowDoneData =
                  PowerFlowDoneData(gridAgentBaseData, validPowerFlowResult)

                // we can answer the stashed grid power requests now
                buffer.unstashAll(
                  simulateGrid(powerFlowDoneData, currentTick)
                )
              } else {
                ctx.log.debug(
                  "Going back to SimulateGrid and wait for my assets or inferior grids to return."
                )

                // go back to simulate grid
                simulateGrid(
                  gridAgentBaseData
                    .updateWithReceivedSlackVoltages(receivedSlackValues)
                    .copy(sweepValueStores = updatedSweepValueStore),
                  currentTick,
                )
              }

            case failedNewtonRaphsonPFResult: FailedNewtonRaphsonPFResult =>
              val powerFlowDoneData =
                PowerFlowDoneData(
                  gridAgentBaseData,
                  failedNewtonRaphsonPFResult,
                )
              ctx.log.warn(
                "Power flow with updated slack voltage did finally not converge!"
              )
              // we can answer the stashed grid power requests now and report a failed power flow back
              buffer.unstashAll(simulateGrid(powerFlowDoneData, currentTick))
          }

        // happens only when we received slack data and power values before we received a request to provide grid data
        // (only possible when first simulation triggered and this agent is faster in this state as the request
        // by a superior grid arrives)
        case (powerResponse: PowerResponse, _) =>
          ctx.log.debug(
            "Received Request for Grid Power too early. Stashing away"
          )

          buffer.stash(powerResponse)
          Behaviors.same

        case (requestGridPower: RequestGridPower, _) =>
          ctx.log.debug(
            s"Received the message $requestGridPower too early. Stashing away!"
          )
          buffer.stash(requestGridPower)
          Behaviors.same
      }
  }

  /** Method used for checking the power difference. <p> This method should only
    * be reached by the superior (dummy) grid agent.
    * @param gridAgentBaseData
    *   state data of the actor
    * @return
    *   a [[Behavior]]
    */
  private def checkPowerDifferences(
      gridAgentBaseData: GridAgentBaseData
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = Behaviors.receivePartial {

    case (ctx, CheckPowerDifferencesTrigger(currentTick)) =>
      ctx.log.debug("Starting the power differences check ...")
      val currentSweepNo = gridAgentBaseData.currentSweepNo

      val gridModel = gridAgentBaseData.gridEnv.gridModel

      /* This is the highest grid agent, therefore no data is received for the slack node. Suppress, that it is looked
       * up in the empty store. */
      val (operationPoint, slackNodeVoltages) = composeOperatingPoint(
        gridModel.gridComponents.nodes,
        gridModel.gridComponents.transformers,
        gridModel.gridComponents.transformers3w,
        gridModel.nodeUuidToIndexMap,
        gridAgentBaseData.receivedValueStore,
        gridModel.mainRefSystem,
        targetVoltageFromReceivedData = false,
      )

      /* Regarding the power flow result of this grid, there are two cases. If this is the "highest" grid in a
       * simulation without a three winding transformer, the grid consists of only one node and we can mock the power
       * flow results. If there is a three winding transformer apparent, we actually have to perform power flow
       * calculations, as the high voltage branch of the transformer is modeled here. */
      (if (gridModel.gridComponents.transformers3w.isEmpty) {
         val nodeData = operationPoint.map(StateData(_))
         ValidNewtonRaphsonPFResult(-1, nodeData, DenseMatrix(0d, 0d))
       } else {
         ctx.log.debug(
           "This grid contains a three winding transformer. Perform power flow calculations before assessing the power deviations."
         )
         newtonRaphsonPF(
           gridModel,
           gridAgentBaseData.powerFlowParams.maxIterations,
           operationPoint,
           slackNodeVoltages,
         )(gridAgentBaseData.powerFlowParams.epsilon)(ctx.log) match {
           case validPowerFlowResult: ValidNewtonRaphsonPFResult =>
             ctx.log.debug(
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
       }) match {
        case validResult: ValidNewtonRaphsonPFResult =>
          val updatedGridAgentBaseData: GridAgentBaseData =
            gridAgentBaseData
              .storeSweepDataAndClearReceiveMaps(
                validResult,
                gridAgentBaseData.superiorGridNodeUuids,
                gridAgentBaseData.inferiorGridGates,
              )
              .copy(currentSweepNo = currentSweepNo + 1)

          // the difference is checked @ the higher nodes of our transformers => the slack nodes
          // if we are either in the first backward sweep OR if the deviation is bigger as allowed, we need a second sweep
          if (gridAgentBaseData.sweepValueStores.isEmpty) {
            ctx.log.debug(
              "Sweep value store is empty. Starting a second sweep ..."
            )
            goToSimulateGridForNextSweepWith(
              updatedGridAgentBaseData,
              currentTick,
            )
          } else {
            ctx.log.debug(
              "Sweep value store is not empty. Check for deviation ..."
            )

            // calculate deviation vector for all nodes
            val previousSweepNodePower: DenseVector[Complex] =
              DenseVector(
                updatedGridAgentBaseData.sweepValueStores
                  .getOrElse(
                    currentSweepNo - 1,
                    throw new DBFSAlgorithmException(
                      s"No data for previous sweep with no ${currentSweepNo - 1} available!"
                    ),
                  )
                  .sweepData
                  .map(_.stateData.power)
                  .toArray
              )
            val currentSweepNodePower: DenseVector[Complex] =
              DenseVector(
                updatedGridAgentBaseData.sweepValueStores
                  .getOrElse(
                    currentSweepNo,
                    throw new DBFSAlgorithmException(
                      s"No data for current sweep with no $currentSweepNo available!"
                    ),
                  )
                  .sweepData
                  .map(_.stateData.power)
                  .toArray
              )

            val allowedDeviation =
              gridAgentBaseData.powerFlowParams.maxSweepPowerDeviation

            (previousSweepNodePower - currentSweepNodePower).toScalaVector
              .find(complex => {
                Math.abs(complex.real) >= allowedDeviation |
                  Math.abs(complex.imag) >= allowedDeviation
              }) match {
              case Some(deviation) => // next sweep
                ctx.log.debug(
                  "Deviation between the last two sweeps: {}",
                  deviation,
                )
                goToSimulateGridForNextSweepWith(
                  updatedGridAgentBaseData,
                  currentTick,
                )
              case None => // we're done
                ctx.log.debug("We found a result! :-)")

                ctx.log.debug(
                  "Final deviation: {}",
                  (previousSweepNodePower - currentSweepNodePower).toScalaVector,
                )

                // go back to SimulateGrid and trigger a finish
                ctx.self ! FinishGridSimulationTrigger(currentTick)
                simulateGrid(gridAgentBaseData, currentTick)
            }
          }
        case failedResult: PowerFlowResult.FailedPowerFlowResult =>
          ctx.log.warn(
            "Power flow for high voltage branch of three winding transformer failed after {} iterations. Cause: {}",
            failedResult.iteration,
            failedResult.cause,
          )
          ctx.self ! FinishGridSimulationTrigger(currentTick)
          handlePowerFlowFailure(gridAgentBaseData, currentTick, ctx)
      }
  }

  /** Checks if all data has been received and if yes checks if the there are
    * any failed power flow indications from inferior grids. If both == true,
    * then no [[Behavior]] change is triggered but the sweep value store is
    * updated with a [[FailedPowerFlow]] information as well, the now used data
    * is set to [[PowerFlowDoneData]] and this is escalated to the superior
    * grid(s). If there is no [[FailedPowerFlow]] in the [[GridAgentBaseData]] a
    * behavior transition to [[handlePowerFlowCalculations]] is triggered.
    *
    * If allReceived == false, no [[Behavior]] transition is triggered
    *
    * @param allReceived
    *   indicates if all requested data has been received
    * @param gridAgentBaseData
    *   the current or updated data of the [[GridAgent]]
    * @return
    *   either the same behavior the agent is currently in or a transition to
    *   [[handlePowerFlowCalculations]]
    */
  private def goToPowerFlowCalculationOrStay(
      allReceived: Boolean,
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      behavior: (GridAgentData, Long) => Behavior[GridAgent.Request],
  )(implicit
      ctx: ActorContext[GridAgent.Request],
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] =
    if (allReceived) {
      ctx.log.debug(
        "All power values of inferior grids, assets + voltage superior grid slack voltages received."
      )

      // if the power flow failed in one of our inferior grids, we want to report this to our superior grid
      val powerFlowFailedSomewhereInInferior =
        gridAgentBaseData.receivedValueStore.nodeToReceivedPower.values
          .exists(v => v.exists(k => k._2.contains(FailedPowerFlow)))

      if (powerFlowFailedSomewhereInInferior) {
        ctx.log.warn(
          "Received Failed Power Flow Result. Escalate to my parent."
        )

        val powerFlowDoneData = PowerFlowDoneData(
          gridAgentBaseData,
          FailedNewtonRaphsonPFResult(-1, CalculationFailed),
        )

        // we want to answer the requests from our parent
        buffer.unstashAll(simulateGrid(powerFlowDoneData, currentTick))
      } else {
        ctx.self ! DoPowerFlowTrigger(
          currentTick,
          gridAgentBaseData.currentSweepNo,
        )

        handlePowerFlowCalculations(gridAgentBaseData, currentTick)
      }

    } else {
      ctx.log.debug(
        "Still waiting for asset or grid power values or slack voltage information of inferior grids"
      )
      behavior(gridAgentBaseData, currentTick)
    }

  /** Normally only reached by the superior (dummy) agent!
    *
    * Checks if all data has been received and if yes checks if the there are
    * any failed power flow indications from inferior grids. If both == true,
    * then a finish simulation is triggered and depending on the configuration
    * this step is skipped and the simulation goes on or this leads to a
    * termination of the simulation due to a failed power flow calculation.
    *
    * If there is no [[FailedPowerFlow]] in the [[GridAgentBaseData]] a
    * [[Behavior]] transition to [[checkPowerDifferences]] is triggered.
    *
    * If allReceived == false, no state transition is triggered
    *
    * The used data is always the provided [[GridAgentBaseData]]
    *
    * @param allReceived
    *   indicates if all requested data has been received
    * @param gridAgentBaseData
    *   the current or updated data of the [[GridAgent]]
    * @return
    *   either the same behavior the agent is currently in or a transition to
    *   [[checkPowerDifferences]]
    */
  private def goToCheckPowerDifferencesOrStay(
      allReceived: Boolean,
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      behavior: (GridAgentData, Long) => Behavior[GridAgent.Request],
  )(implicit
      ctx: ActorContext[GridAgent.Request],
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
    if (allReceived) {
      ctx.log.debug(
        "All power values of child assets + inferior grids received."
      )

      // if our superior grid received a FailedPowerFlow from the inferior grids it has to trigger a finish
      // of the simulation which will either result in a skip of this time step OR a termination of the simulation run
      val powerFlowFailedSomewhere =
        gridAgentBaseData.receivedValueStore.nodeToReceivedPower.values
          .exists(actorPowerRequestResponses =>
            actorPowerRequestResponses.exists(actorPowerRequestResponse =>
              actorPowerRequestResponse._2.contains(FailedPowerFlow)
            )
          )
      if (powerFlowFailedSomewhere) {
        ctx.log.warn("Power flow failed! This incident will be reported!")
        ctx.self ! FinishGridSimulationTrigger(currentTick)

        handlePowerFlowFailure(gridAgentBaseData, currentTick, ctx)
      } else {
        ctx.self ! CheckPowerDifferencesTrigger(currentTick)
        checkPowerDifferences(gridAgentBaseData)
      }
    } else {
      ctx.log.debug(
        "Still waiting for asset or grid power values or slack voltage information of inferior grids"
      )
      behavior(gridAgentBaseData, currentTick)
    }
  }

  /** Method for handling failed power flows.
    * @param gridAgentBaseData
    *   state data of the actor
    * @param currentTick
    *   of the simulation
    */
  private def handlePowerFlowFailure(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
      ctx: ActorContext[GridAgent.Request],
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
    constantData.environmentRefs.runtimeEventListener ! PowerFlowFailed

    if (gridAgentBaseData.powerFlowParams.stopOnFailure) {
      ctx.log.error("Stopping because of failed power flow.")
      Behaviors.stopped
    } else simulateGrid(gridAgentBaseData, currentTick)
  }

  /** Normally only reached by the superior (dummy) agent!
    *
    * Triggers a [[Behavior]] transition to [[simulateGrid]], informs the
    * [[edu.ie3.simona.scheduler.Scheduler]] about the finish of this sweep and
    * requests a new trigger for itself for a new sweep
    *
    * @param gridAgentBaseData
    *   the [[GridAgentBaseData]] that should be used in the next sweep in
    *   [[simulateGrid]]
    * @param currentTick
    *   current tick the agent is in
    * @return
    *   a behavior transition to [[simulateGrid]]
    */
  private def goToSimulateGridForNextSweepWith(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long,
  )(implicit
      constantData: GridAgentConstantData,
      buffer: StashBuffer[GridAgent.Request],
  ): Behavior[GridAgent.Request] = {
    constantData.environmentRefs.scheduler ! Completion(
      constantData.activationAdapter,
      Some(currentTick),
    )

    simulateGrid(gridAgentBaseData, currentTick)
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
    *   a map contains a mapping between nodes and the [[ActorRef]] s located \@
    *   those nodes
    * @param refSystem
    *   the reference system of the [[edu.ie3.simona.model.grid.GridModel]] of
    *   this [[GridAgent]]
    * @param askTimeout
    *   a timeout for the request
    * @return
    *   true if this grids contains assets or false if no request has been send
    *   due to non-existence of assets
    */
  private def askForAssetPowers(
      currentTick: Long,
      sweepValueStore: Option[SweepValueStore],
      nodeToAssetAgents: Map[UUID, Set[ActorRef[ParticipantMessage]]],
      refSystem: RefSystem,
      askTimeout: Duration,
  )(implicit
      ctx: ActorContext[GridAgent.Request]
  ): Boolean = {
    implicit val timeout: PekkoTimeout = PekkoTimeout.create(askTimeout)
    implicit val ec: ExecutionContext = ctx.executionContext

    ctx.log.debug(s"asking assets for power values: {}", nodeToAssetAgents)

    if (nodeToAssetAgents.values.flatten.nonEmpty) {
      val future = Future
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
                      refSystem.vInPu(fInSi),
                    )
                  case None =>
                    (
                      Each(1d),
                      Each(0d),
                    )
                }

              (assetAgent.toClassic ? RequestAssetPowerMessage(
                currentTick,
                eInPu,
                fInPU,
              )).map {
                case providedPowerValuesMessage: AssetPowerChangedMessage =>
                  (assetAgent, providedPowerValuesMessage)
                case assetPowerUnchangedMessage: AssetPowerUnchangedMessage =>
                  (assetAgent, assetPowerUnchangedMessage)
              }
            })
          }.toVector
        )
        .map(res => ReceivedAssetPowerValues(res))

      pipeToSelf(future, ctx)
      true
    } else false
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
    * @return
    *   true if this grids has connected inferior grids or false if this no
    *   inferior grids
    */
  private def askInferiorGridsForPowers(
      currentSweepNo: Int,
      subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]],
      inferiorGridGates: Seq[SubGridGate],
      askTimeout: Duration,
  )(implicit
      ctx: ActorContext[GridAgent.Request]
  ): Boolean = {
    implicit val timeout: PekkoTimeout = PekkoTimeout.create(JavaDuration.ofSeconds(askTimeout.toSeconds))
    implicit val ec: ExecutionContext = ctx.executionContext
    implicit val scheduler: Scheduler = ctx.system.scheduler

    ctx.log.debug(
      s"asking inferior grids for power values: {}",
      inferiorGridGates,
    )

    if (inferiorGridGates.nonEmpty) {
      val future = Future
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
                .ask[GridAgent.Request](ref =>
                  RequestGridPower(
                    currentSweepNo,
                    inferiorGridGateNodes.distinct,
                    ref,
                  )
                )
                .map {
                  case provideGridPowerMessage: GridPowerResponse =>
                    (inferiorGridAgentRef, provideGridPowerMessage)
                  case FailedPowerFlow =>
                    (inferiorGridAgentRef, FailedPowerFlow)
                }
            }
            .toVector
        )
        .map(res => ReceivedGridPowerValues(res))
      pipeToSelf(future, ctx)
      true
    } else false
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
    * @return
    *   true if this grids has connected superior grids or false if this no
    *   superior grids
    */
  private def askSuperiorGridsForSlackVoltages(
      currentSweepNo: Int,
      subGridGateToActorRef: Map[SubGridGate, ActorRef[GridAgent.Request]],
      superiorGridGates: Vector[SubGridGate],
      askTimeout: Duration,
  )(implicit
      ctx: ActorContext[GridAgent.Request]
  ): Boolean = {
    implicit val timeout: PekkoTimeout = PekkoTimeout.create(JavaDuration.ofSeconds(askTimeout.toSeconds))
    implicit val ec: ExecutionContext = ctx.executionContext
    implicit val scheduler: Scheduler = ctx.system.scheduler

    ctx.log.debug(
      s"asking superior grids for slack voltage values: {}",
      superiorGridGates,
    )

    if (superiorGridGates.nonEmpty) {
      val future = Future
        .sequence(
          superiorGridGates
            .groupBy(subGridGateToActorRef(_))
            .map { case (superiorGridAgent, gridGates) =>
              superiorGridAgent
                .ask[GridAgent.Request](ref =>
                  SlackVoltageRequest(
                    currentSweepNo,
                    gridGates.map(_.superiorNode.getUuid),
                    ref,
                  )
                )
                .map { case providedSlackValues: SlackVoltageResponse =>
                  (superiorGridAgent, providedSlackValues)
                }
            }
            .toVector
        )
        .map(res => ReceivedSlackVoltageValues(res))
      pipeToSelf(future, ctx)
      true
    } else false
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
  private def createAndSendPowerFlowResults(
      gridAgentBaseData: GridAgentBaseData,
      currentTimestamp: ZonedDateTime,
  )(implicit
      log: Logger,
      constantData: GridAgentConstantData,
  ): Unit = {
    gridAgentBaseData.sweepValueStores.lastOption.foreach {
      case (_, valueStore) =>
        constantData.notifyListeners(
          this.createResultModels(
            gridAgentBaseData.gridEnv.gridModel,
            valueStore,
          )(
            currentTimestamp,
            log,
          )
        )
    }
  }

  /** This method uses [[ActorContext.pipeToSelf()]] to send a future message to
    * itself. If the future is a [[Success]] the message is send, else a
    * [[WrappedFailure]] with the thrown error is send.
    *
    * @param future
    *   future message that should be send to the agent after it was processed
    * @param ctx
    *   [[ActorContext]] of the receiving actor
    */
  private def pipeToSelf(
      future: Future[GridAgent.Request],
      ctx: ActorContext[GridAgent.Request],
  ): Unit = {
    ctx.pipeToSelf[GridAgent.Request](future) {
      case Success(value)     => value
      case Failure(exception) => WrappedFailure(exception)
    }
  }
}
