/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.{ActorRef, FSM}
import akka.pattern.{ask, pipe}
import akka.util.{Timeout => AkkaTimeout}
import breeze.linalg.{DenseMatrix, DenseVector}
import breeze.math.Complex
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.powerflow.model.FailureCause.CalculationFailed
import edu.ie3.powerflow.model.NodeData.StateData
import edu.ie3.powerflow.model.PowerFlowResult
import edu.ie3.powerflow.model.PowerFlowResult.FailedPowerFlowResult.FailedNewtonRaphsonPFResult
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.powerflow.model.enums.NodeType
import edu.ie3.simona.agent.grid.GridAgentData.{
  GridAgentBaseData,
  PowerFlowDoneData
}
import edu.ie3.simona.agent.grid.ReceivedValues._
import edu.ie3.simona.agent.state.AgentState
import edu.ie3.simona.agent.state.AgentState.Idle
import edu.ie3.simona.agent.state.GridAgentState.{
  CheckPowerDifferences,
  HandlePowerFlowCalculations,
  SimulateGrid
}
import edu.ie3.simona.exceptions.agent.DBFSAlgorithmException
import edu.ie3.simona.model.grid.{NodeModel, RefSystem}
import edu.ie3.simona.ontology.messages.PowerMessage._
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  CompletionMessage,
  PowerFlowFailedMessage,
  ScheduleTriggerMessage,
  TriggerWithIdMessage
}
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.ontology.messages.VoltageMessage.{
  ProvideSlackVoltageMessage,
  RequestSlackVoltageMessage
}
import edu.ie3.simona.ontology.trigger.Trigger._
import edu.ie3.simona.util.TickUtil._
import edu.ie3.util.quantities.PowerSystemUnits._
import edu.ie3.util.scala.quantities.Megavars
import squants.Each
import squants.energy.Megawatts
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.ElectricPotential
import scala.concurrent.duration.Duration
import java.time.{Duration => JavaDuration}
import scala.concurrent.{ExecutionContext, Future}

/** Trait that is normally mixed into every [[GridAgent]] to enable distributed
  * forward backward sweep (DBFS) algorithm execution. It is considered to be
  * the standard behaviour of a [[GridAgent]].
  */
trait DBFSAlgorithm extends PowerFlowSupport with GridResultsSupport {
  this: GridAgent =>
  // implicit ExecutionContext should be in scope
  // see https://doc.akka.io/docs/akka/2.5/futures.html
  implicit val ec: ExecutionContext = context.dispatcher

  when(SimulateGrid) {

    // first part of the grid simulation, same for all gridAgents on all levels
    // we start with a forward-sweep by requesting the data from our child assets and grids (if any)
    case Event(
          TriggerWithIdMessage(
            StartGridSimulationTrigger(currentTick),
            triggerId,
            _
          ),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      log.debug("Start sweep number: {}", gridAgentBaseData.currentSweepNo)
      // hold tick and trigger for the whole time the dbfs is executed or
      // at least until the the first full sweep is done (for superior grid agent only)
      holdTickAndTriggerId(currentTick, triggerId)

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
      )

      // 2. inferior grids p/q values
      askInferiorGridsForPowers(
        gridAgentBaseData.currentSweepNo,
        gridAgentBaseData.gridEnv.subgridGateToActorRef,
        gridAgentBaseData.inferiorGridGates,
        gridAgentBaseData.powerFlowParams.sweepTimeout
      )

      // 3. superior grids slack voltage
      askSuperiorGridsForSlackVoltages(
        gridAgentBaseData.currentSweepNo,
        gridAgentBaseData.gridEnv.subgridGateToActorRef,
        gridAgentBaseData.superiorGridGates,
        gridAgentBaseData.powerFlowParams.sweepTimeout
      )

      stay()

    // if we receive power values as response on our request, we process them here
    case Event(
          receivedValues: ReceivedValues,
          gridAgentBaseData: GridAgentBaseData
        ) =>
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
      val allValuesReceived = updatedGridAgentBaseData.allRequestedDataReceived

      log.debug(
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

    // if we receive a request for slack voltages from our inferior grids we want to answer it
    case Event(
          RequestSlackVoltageMessage(currentSweepNo, nodeUuids),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      log.debug(
        s"Received Slack Voltages request from {} for nodes {} and sweepNo: {}",
        sender(),
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
            log.debug(
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
          log.debug(
            "Unable to get slack voltage for node '{}' in sweeps '{}' and '{}'. Returning target voltage.",
            nodeUuid,
            currentSweepNo,
            currentSweepNo - 1
          )

          val refSystemVUnit =
            gridAgentBaseData.gridEnv.gridModel.mainRefSystem.nominalVoltage.getUnit

          /* Determine the slack node voltage under consideration of the target voltage set point */
          val vTarget =
            gridAgentBaseData.gridEnv.gridModel.gridComponents.nodes
              .find { case NodeModel(uuid, _, _, isSlack, _, _) =>
                uuid == nodeUuid && isSlack
              }
              .map(_.vTarget)
              .getOrElse(Quantities.getQuantity(1d, PU))
          val vSlack = vTarget
            .multiply(
              gridAgentBaseData.gridEnv.gridModel.mainRefSystem.nominalVoltage
            )
            .asType(classOf[ElectricPotential])
            .to(refSystemVUnit)

          (
            vSlack,
            Quantities.getQuantity(0, refSystemVUnit)
          )
        } match {
          case (slackE, slackF) =>
            log.debug(
              s"Provide {} to {} for node {} and sweepNo: {}",
              s"$slackE, $slackF",
              sender(),
              nodeUuid,
              gridAgentBaseData.currentSweepNo
            )

            ExchangeVoltage(nodeUuid, slackE, slackF)
        }
      } match {
        case exchangeVoltages =>
          stay() replying ProvideSlackVoltageMessage(
            currentSweepNo,
            exchangeVoltages
          )
      }

    // receive grid power values message request from superior grids
    // / before power flow calc for this sweep we either have to stash() the message to answer it later (in current sweep)
    // / or trigger a new run for the next sweepNo
    case Event(
          RequestGridPowerMessage(requestSweepNo, _),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      if (gridAgentBaseData.currentSweepNo == requestSweepNo) {
        log.debug(
          s"Received request for grid power values for sweepNo {} before my first power flow calc. Stashing away.",
          requestSweepNo
        )
        stash()
        stay()
      } else {
        log.debug(
          s"Received request for grid power values for a NEW sweep (request: {}, my: {})",
          requestSweepNo,
          gridAgentBaseData.currentSweepNo
        )
        self ! PrepareNextSweepTrigger(currentTick)

        stash()
        stay() using gridAgentBaseData.copy(currentSweepNo = requestSweepNo)
      }

    // / after power flow calc for this sweepNo
    case Event(
          RequestGridPowerMessage(_, requestedNodeUuids),
          powerFlowDoneData @ PowerFlowDoneData(
            gridAgentBaseData,
            powerFlowResult,
            pendingRequestAnswers
          )
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
                          refSystem.pInSi(pInPu).multiply(-1),
                          refSystem.qInSi(qInPu).multiply(-1)
                        )
                      case _ =>
                        /* TODO: As long as there are no multiple slack nodes, provide "real" power only for the slack node */
                        (
                          Quantities.getQuantity(0d, MEGAWATT),
                          Quantities.getQuantity(0d, MEGAVAR)
                        )
                    }
                    .getOrElse {
                      throw new DBFSAlgorithmException(
                        s"Got a request for power @ node with uuid $requestedNodeUuids but cannot find it in my result data!"
                      )
                    }
                }
                .map { case (nodeUuid, (p, q)) =>
                  ProvideGridPowerMessage.ExchangePower(
                    nodeUuid,
                    Megawatts(p.to(MEGAWATT).getValue.doubleValue),
                    Megavars(q.to(MEGAVAR).getValue.doubleValue)
                  )
                }

              /* Determine the remaining replies */
              val stillPendingRequestAnswers =
                pendingRequestAnswers.filterNot(_ == requestingSubgridNumber)

              // update the sweep value store and clear all received maps
              // note: normally it is expected that this has to be done after power flow calculations but for the sake
              // of having it only once in the code we put this here. Otherwise it would have to been put before EVERY
              // return with a valid power flow result (currently happens already in two situations)
              val updatedGridAgentBaseData =
                if (stillPendingRequestAnswers.isEmpty) {
                  gridAgentBaseData.storeSweepDataAndClearReceiveMaps(
                    validNewtonRaphsonPFResult,
                    gridAgentBaseData.superiorGridNodeUuids,
                    gridAgentBaseData.inferiorGridGates
                  )
                } else {
                  powerFlowDoneData.copy(pendingRequestAnswers =
                    stillPendingRequestAnswers
                  )
                }

              stay() replying
                ProvideGridPowerMessage(
                  exchangePowers
                ) using updatedGridAgentBaseData

            case _: FailedNewtonRaphsonPFResult =>
              stay() replying FailedPowerFlow using gridAgentBaseData
          }
        case None =>
          /* It is not possible to determine, who has asked */
          log.error(
            "I got a grid power request from a subgrid I don't know. Can't answer it properly."
          )
          stay() replying FailedPowerFlow using gridAgentBaseData
      }

    // called when a grid power values request from a superior grid is received
    // which is similar to a new sweep and causes a) a power flow with updated slack voltage values and
    // b) afterwards a request for updated power values from inferior grids and assets with updated voltage values
    // based on the just carried out power flow
    case Event(
          PrepareNextSweepTrigger(_),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      // request the updated slack voltages from the superior grid
      askSuperiorGridsForSlackVoltages(
        gridAgentBaseData.currentSweepNo,
        gridAgentBaseData.gridEnv.subgridGateToActorRef,
        gridAgentBaseData.superiorGridGates,
        gridAgentBaseData.powerFlowParams.sweepTimeout
      )

      log.debug(s"Going to {}", HandlePowerFlowCalculations)

      goto(HandlePowerFlowCalculations) using gridAgentBaseData

    // last step which should includes a) information on inferior grids about finish and
    // b) cleanup of receiveMaps and sweepStore
    case Event(
          FinishGridSimulationTrigger(currentTick),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      // inform my child grids about the end of this grid simulation
      gridAgentBaseData.inferiorGridGates
        .map {
          gridAgentBaseData.gridEnv.subgridGateToActorRef(_)
        }
        .distinct
        .foreach(_ ! FinishGridSimulationTrigger(currentTick))

      // inform every system participant about the end of this grid simulation
      gridAgentBaseData.gridEnv.nodeToAssetAgents.foreach { case (_, actors) =>
        actors.foreach(actor => {
          actor ! FinishGridSimulationTrigger(
            currentTick
          )
        })
      }

      // notify listener about the results
      log.debug("Calculate results and sending the results to the listener ...")
      createAndSendPowerFlowResults(
        gridAgentBaseData,
        currentTick.toDateTime(simStartTime)
      )

      // do my cleanup stuff
      log.debug("Doing my cleanup stuff")

      // / clean copy of the gridAgentBaseData
      val cleanedGridAgentBaseData = GridAgentBaseData.clean(
        gridAgentBaseData,
        gridAgentBaseData.superiorGridNodeUuids,
        gridAgentBaseData.inferiorGridGates
      )

      // / release tick and trigger for the whole simulation (StartGridSimulationTrigger)
      val (_, simTriggerId) = releaseTickAndTriggerId()

      // / inform scheduler that we are done with the whole simulation and request new trigger for next time step
      environmentRefs.scheduler ! CompletionMessage(
        simTriggerId,
        Some(
          Vector(
            ScheduleTriggerMessage(
              ActivityStartTrigger(currentTick + resolution),
              self
            )
          )
        )
      )

      // return to Idle
      goto(Idle) using cleanedGridAgentBaseData

  }

  /** Every power flow calculation should take place here. Generally used for
    * power flow calculations only and only if all data required are already
    * received as requested.
    */
  when(HandlePowerFlowCalculations) {

    // main method for power flow calculations
    case Event(
          DoPowerFlowTrigger(currentTick, _),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      log.debug(
        "Received the following power values to the corresponding nodes: {}",
        gridAgentBaseData.receivedValueStore.nodeToReceivedPower
      )

      val gridModel = gridAgentBaseData.gridEnv.gridModel

      val (operatingPoint, slackNodeVoltages) = composeOperatingPoint(
        gridModel.gridComponents.nodes,
        gridModel.gridComponents.transformers,
        gridModel.gridComponents.transformers3w,
        gridModel.nodeUuidToIndexMap,
        gridAgentBaseData.receivedValueStore,
        gridModel.mainRefSystem
      )

      newtonRaphsonPF(
        gridModel,
        gridAgentBaseData.powerFlowParams.maxIterations,
        operatingPoint,
        slackNodeVoltages
      )(gridAgentBaseData.powerFlowParams.epsilon) match {
        // if res is valid, ask our assets (if any) for updated power values based on the newly determined nodal voltages
        case validPowerFlowResult: ValidNewtonRaphsonPFResult =>
          log.debug(
            "{}",
            composeValidNewtonRaphsonPFResultVoltagesDebugString(
              validPowerFlowResult,
              gridModel
            )
          )

          val powerFlowDoneData =
            PowerFlowDoneData(gridAgentBaseData, validPowerFlowResult)

          val sweepValueStoreOpt = Some(
            SweepValueStore(
              validPowerFlowResult,
              gridModel.gridComponents.nodes,
              gridModel.nodeUuidToIndexMap
            )
          )
          askForAssetPowers(
            currentTick,
            sweepValueStoreOpt,
            gridAgentBaseData.gridEnv.nodeToAssetAgents,
            gridModel.mainRefSystem,
            gridAgentBaseData.powerFlowParams.sweepTimeout
          ) match {
            case None =>
              // when we don't have assets we can skip another request for different asset behaviour due to changed
              // voltage values and go back to SimulateGrid directly
              log.debug(
                s"No generation or load assets in the grid. Going back to {}.",
                SimulateGrid
              )
              unstashAll() // we can answer the stashed grid power requests now
              goto(SimulateGrid) using powerFlowDoneData
            case Some(_) =>
              // will return a future based on the `ask-pattern` which will be processed below
              stay() using powerFlowDoneData
          }

        case failedNewtonRaphsonPFResult: FailedNewtonRaphsonPFResult =>
          val powerFlowDoneData =
            PowerFlowDoneData(gridAgentBaseData, failedNewtonRaphsonPFResult)
          log.warning(
            "Power flow calculation before asking for updated powers did finally not converge!"
          )
          unstashAll() // we can answer the stashed grid power requests now and report a failed power flow back
          goto(SimulateGrid) using powerFlowDoneData
      }

    // handler for the future provided by `askForAssetPowers` to check if there are any changes in generation/load
    // of assets based on updated nodal voltages
    case Event(
          receivedPowerValues: ReceivedPowerValues,
          powerFlowDoneData: PowerFlowDoneData
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
        log.debug(
          "Assets have changed their exchanged power with the grid. Update nodal powers and prepare new power flow."
        )
        val updatedGridAgentBaseData: GridAgentBaseData =
          receivedPowerValues match {
            case receivedPowers: ReceivedPowerValues =>
              gridAgentBaseData.updateWithReceivedPowerValues(
                receivedPowers,
                replace = true
              )
            case unknownValuesReceived =>
              throw new DBFSAlgorithmException(
                s"Received unsuitable values: $unknownValuesReceived"
              )
          }

        // check if we have enough data for a power flow calculation
        // if yes, go to the powerflow
        // if no, stay and wait
        val readyForPowerFlow =
          updatedGridAgentBaseData.allRequestedDataReceived
        log.debug(
          "{}",
          if (readyForPowerFlow)
            "Got answers for all my requests for Slack Voltages and Power Values."
          else
            "Still waiting for answers my requests for Slack Voltages and Power Values."
        )

        goToPowerFlowCalculationOrStay(
          readyForPowerFlow,
          updatedGridAgentBaseData
        )

      } else {
        // no changes from assets, we want to go back to SimulateGrid and report the LF results to our parent grids if any requests
        log.debug(
          s"Assets have not changed their exchanged power or no voltage dependent behaviour. Going back to {}.",
          SimulateGrid
        )
        unstashAll() // we can answer the stashed grid power requests now
        goto(SimulateGrid) using powerFlowDoneData

      }

    // executed after request from the superior grid to execute a new sweep (forward sweep state)
    // this means we requested an update of the slack voltage values, but for now don't request (and hence don't expect)
    // updated power values for our power flow calculations
    case Event(
          receivedSlackValues: ReceivedSlackVoltageValues,
          gridAgentBaseData: GridAgentBaseData
        ) =>
      log.debug(
        "Received Slack values for new forward sweep with same power but updated voltage values"
      )

      // power flow
      val gridModel = gridAgentBaseData.gridEnv.gridModel
      val previousSweepData = gridAgentBaseData.sweepValueStores.getOrElse(
        gridAgentBaseData.currentSweepNo - 1,
        throw new DBFSAlgorithmException(
          s"$actorName Unable to get results from previous sweep ${gridAgentBaseData.currentSweepNo - 1}!"
        )
      )

      val (operatingPoint, slackNodeVoltages) =
        composeOperatingPointWithUpdatedSlackVoltages(
          receivedSlackValues,
          previousSweepData.sweepData,
          gridModel.gridComponents.transformers,
          gridModel.gridComponents.transformers3w,
          gridModel.mainRefSystem
        )

      newtonRaphsonPF(
        gridModel,
        gridAgentBaseData.powerFlowParams.maxIterations,
        operatingPoint,
        slackNodeVoltages
      )(gridAgentBaseData.powerFlowParams.epsilon) match {
        case validPowerFlowResult: ValidNewtonRaphsonPFResult =>
          log.debug(
            "{}",
            composeValidNewtonRaphsonPFResultVoltagesDebugString(
              validPowerFlowResult,
              gridModel
            )
          )

          // update the data
          val sweepValueStore = SweepValueStore(
            validPowerFlowResult,
            gridModel.gridComponents.nodes,
            gridModel.nodeUuidToIndexMap
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
              gridAgentBaseData.powerFlowParams.sweepTimeout
            )

          // 2. inferior grids p/q values
          val askForInferiorGridPowersOpt =
            askInferiorGridsForPowers(
              gridAgentBaseData.currentSweepNo,
              gridAgentBaseData.gridEnv.subgridGateToActorRef,
              gridAgentBaseData.inferiorGridGates,
              gridAgentBaseData.powerFlowParams.sweepTimeout
            )

          // when we don't have inferior grids and no assets both methods return None and we can skip doing another power
          // flow calculation otherwise we go back to simulate grid and wait for the answers
          (askForAssetPowersOpt, askForInferiorGridPowersOpt) match {
            case (None, None) =>
              log.debug(
                "I don't have assets or child grids. " +
                  "Going back to SimulateGrid and provide the power flow result if there is any request left."
              )

              val powerFlowDoneData =
                PowerFlowDoneData(gridAgentBaseData, validPowerFlowResult)

              unstashAll() // we can answer the stashed grid power requests now
              goto(SimulateGrid) using powerFlowDoneData

            case _ =>
              log.debug(
                "Going back to SimulateGrid and wait for my assets or inferior grids to return."
              )

              // go back to simulate grid
              goto(SimulateGrid) using gridAgentBaseData
                .updateWithReceivedSlackVoltages(receivedSlackValues)
                .copy(sweepValueStores = updatedSweepValueStore)
          }

        case failedNewtonRaphsonPFResult: FailedNewtonRaphsonPFResult =>
          val powerFlowDoneData =
            PowerFlowDoneData(
              gridAgentBaseData,
              failedNewtonRaphsonPFResult
            )
          log.warning(
            "Power flow with updated slack voltage did finally not converge!"
          )
          unstashAll() // we can answer the stashed grid power requests now and report a failed power flow back
          goto(SimulateGrid) using powerFlowDoneData

      }

    // happens only when we received slack data and power values before we received a request to provide grid data
    // (only possible when first simulation triggered and this agent is faster in this state as the request
    // by a superior grid arrives)
    case Event(
          _: RequestGridPowerMessage,
          _: GridAgentBaseData
        ) =>
      log.debug("Received Request for Grid Power too early. Stashing away")
      stash()
      stay()

    // happens only when we received slack data and power values before we received a request to provide gride
    // (only possible when first simulation triggered and this agent is faster
    // with its power flow calculation in this state as the request by a superior grid arrives)
    case Event(
          _: RequestGridPowerMessage,
          _: PowerFlowDoneData
        ) =>
      log.debug("Received Request for Grid Power too early. Stashing away")
      stash()
      stay()

  }

  // should be reached by the superior (dummy) grid agent only
  when(CheckPowerDifferences) {

    case Event(
          CheckPowerDifferencesTrigger(currentTick),
          gridAgentBaseData: GridAgentBaseData
        ) =>
      log.debug("Starting the power differences check ...")
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
        targetVoltageFromReceivedData = false
      )

      /* Regarding the power flow result of this grid, there are two cases. If this is the "highest" grid in a
       * simulation without a three winding transformer, the grid consists of only one node and we can mock the power
       * flow results. If there is a three winding transformer apparent, we actually have to perform power flow
       * calculations, as the high voltage branch of the transformer is modeled here. */
      (if (gridModel.gridComponents.transformers3w.isEmpty) {
         val nodeData = operationPoint.map(StateData(_))
         ValidNewtonRaphsonPFResult(-1, nodeData, DenseMatrix(0d, 0d))
       } else {
         log.debug(
           "This grid contains a three winding transformer. Perform power flow calculations before assessing the power deviations."
         )
         newtonRaphsonPF(
           gridModel,
           gridAgentBaseData.powerFlowParams.maxIterations,
           operationPoint,
           slackNodeVoltages
         )(gridAgentBaseData.powerFlowParams.epsilon) match {
           case validPowerFlowResult: ValidNewtonRaphsonPFResult =>
             log.debug(
               "{}",
               composeValidNewtonRaphsonPFResultVoltagesDebugString(
                 validPowerFlowResult,
                 gridModel
               )
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
                gridAgentBaseData.inferiorGridGates
              )
              .copy(currentSweepNo = currentSweepNo + 1)

          // the difference is checked @ the higher nodes of our transformers => the slack nodes
          // if we are either in the first backward sweep OR if the deviation is bigger as allowed, we need a second sweep
          if (gridAgentBaseData.sweepValueStores.isEmpty) {
            log.debug("Sweep value store is empty. Starting a second sweep ...")
            goToSimulateGridForNextSweepWith(
              updatedGridAgentBaseData,
              currentTick
            )
          } else {
            log.debug("Sweep value store is not empty. Check for deviation ...")

            // calculate deviation vector for all nodes
            val previousSweepNodePower: DenseVector[Complex] =
              DenseVector(
                updatedGridAgentBaseData.sweepValueStores
                  .getOrElse(
                    currentSweepNo - 1,
                    throw new DBFSAlgorithmException(
                      s"No data for previous sweep with no ${currentSweepNo - 1} available!"
                    )
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
                    )
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
                log.debug(
                  "Deviation between the last two sweeps: {}",
                  deviation
                )
                goToSimulateGridForNextSweepWith(
                  updatedGridAgentBaseData,
                  currentTick
                )
              case None => // we're done
                log.debug("We found a result! :-)")

                log.debug(
                  "Final deviation: {}",
                  (previousSweepNodePower - currentSweepNodePower).toScalaVector
                )

                // go back to SimulateGrid and trigger a finish
                self ! FinishGridSimulationTrigger(currentTick)
                goto(SimulateGrid)

            }
          }
        case failedResult: PowerFlowResult.FailedPowerFlowResult =>
          log.warning(
            "Power flow for high voltage branch of three winding transformer failed after {} iterations. Cause: {}",
            failedResult.iteration,
            failedResult.cause
          )
          environmentRefs.scheduler ! PowerFlowFailedMessage
          self ! FinishGridSimulationTrigger(currentTick)
          goto(SimulateGrid) using gridAgentBaseData
      }
  }

  /** Checks if all data has been received and if yes checks if the there are
    * any failed power flow indications from inferior grids. If both == true,
    * then no state change is triggered but the sweep value store is updated
    * with a [[FailedPowerFlow]] information as well, the now used data is set
    * to [[PowerFlowDoneData]] and this is escalated to the superior grid(s). If
    * there is no [[FailedPowerFlow]] in the [[GridAgentBaseData]] a state
    * transition to [[HandlePowerFlowCalculations]] is triggered.
    *
    * If allReceived == false, no state transition is triggered
    *
    * @param allReceived
    *   indicates if all requested data has been received
    * @param gridAgentBaseData
    *   the current or updated data of the [[GridAgent]]
    * @return
    *   either the same state the agent is currently in or a transition to
    *   [[HandlePowerFlowCalculations]]
    */
  private def goToPowerFlowCalculationOrStay(
      allReceived: Boolean,
      gridAgentBaseData: GridAgentBaseData
  ): FSM.State[AgentState, GridAgentData] = {

    if (allReceived) {
      log.debug(
        "All power values of inferior grids, assets + voltage superior grid slack voltages received."
      )

      // if the power flow failed in one of our inferior grids, we want to report this to our superior grid
      val powerFlowFailedSomewhereInInferior =
        gridAgentBaseData.receivedValueStore.nodeToReceivedPower.values
          .exists(v => v.exists(k => k._2.contains(FailedPowerFlow)))

      if (powerFlowFailedSomewhereInInferior) {
        log.warning("Received Failed Power Flow Result. Escalate to my parent.")
        unstashAll() // we want to answer the requests from our parent
        stay() using PowerFlowDoneData(
          gridAgentBaseData,
          FailedNewtonRaphsonPFResult(-1, CalculationFailed)
        )
      } else {
        self ! DoPowerFlowTrigger(currentTick, gridAgentBaseData.currentSweepNo)
        goto(HandlePowerFlowCalculations) using gridAgentBaseData

      }

    } else {
      log.debug(
        "Still waiting for asset or grid power values or slack voltage information of inferior grids"
      )
      stay() using gridAgentBaseData
    }

  }

  /** Normally only reached by the superior (dummy) agent!
    *
    * Checks if all data has been received and if yes checks if the there are
    * any failed power flow indications from inferior grids. If both == true,
    * then a finish simulation is triggered and depending on the configuration
    * this step is skipped and the simulation goes on or this leads to a
    * termination of the simulation due to a failed power flow calculation.
    *
    * If there is no [[FailedPowerFlow]] in the [[GridAgentBaseData]] a state
    * transition to [[CheckPowerDifferences]] is triggered.
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
    *   either the same state the agent is currently in or a transition to
    *   [[CheckPowerDifferences]]
    */
  private def goToCheckPowerDifferencesOrStay(
      allReceived: Boolean,
      gridAgentBaseData: GridAgentBaseData
  ): FSM.State[AgentState, GridAgentData] = {
    if (allReceived) {
      log.debug("All power values of child assets + inferior grids received.")

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
        log.warning("Power flow failed! This incident will be reported!")
        environmentRefs.scheduler ! PowerFlowFailedMessage
        self ! FinishGridSimulationTrigger(currentTick)
        goto(SimulateGrid) using gridAgentBaseData
      } else {
        self ! CheckPowerDifferencesTrigger(currentTick)
        goto(CheckPowerDifferences) using gridAgentBaseData
      }
    } else {
      log.debug(
        "Still waiting for asset or grid power values or slack voltage information of inferior grids"
      )
      stay() using gridAgentBaseData
    }
  }

  /** Normally only reached by the superior (dummy) agent!
    *
    * Triggers a state transition to [[SimulateGrid]], informs the
    * [[edu.ie3.simona.scheduler.SimScheduler]] about the finish of this sweep
    * and requests a new trigger for itself for a new sweep (which means a new
    * [[StartGridSimulationTrigger]])
    *
    * @param gridAgentBaseData
    *   the [[GridAgentBaseData]] that should be used in the next sweep in
    *   [[SimulateGrid]]
    * @param currentTick
    *   current tick the agent is in
    * @return
    *   a state transition to [[SimulateGrid]]
    */
  private def goToSimulateGridForNextSweepWith(
      gridAgentBaseData: GridAgentBaseData,
      currentTick: Long
  ): FSM.State[AgentState, GridAgentData] = {

    val (_, oldTrigger) = releaseTickAndTriggerId()
    environmentRefs.scheduler ! CompletionMessage(
      oldTrigger,
      Some(
        Vector(
          ScheduleTriggerMessage(StartGridSimulationTrigger(currentTick), self)
        )
      )
    )

    goto(SimulateGrid) using gridAgentBaseData

  }

  /** Triggers an execution of the akka `ask` pattern for all power values of
    * assets (if any) of this [[GridAgent]].
    *
    * @param currentTick
    *   the current tick a data request is send for
    * @param sweepValueStore
    *   the current sweep value store containing the current node voltage for
    *   the assets
    * @param nodeToAssetAgents
    *   a map contains a mapping between nodes and the [[ActorRef]] s located @
    *   those nodes
    * @param refSystem
    *   the reference system of the [[edu.ie3.simona.model.grid.GridModel]] of
    *   this [[GridAgent]]
    * @param askTimeout
    *   a timeout for the request
    * @return
    *   Some(Future[ReceivedPowerValues]) if this grids contains assets or None
    *   if no request has been send due to non-existence of assets
    */
  private def askForAssetPowers(
      currentTick: Long,
      sweepValueStore: Option[SweepValueStore],
      nodeToAssetAgents: Map[UUID, Set[ActorRef]],
      refSystem: RefSystem,
      askTimeout: Duration
  ): Option[Future[ReceivedPowerValues]] = {

    implicit val timeout: AkkaTimeout =
      AkkaTimeout.create(JavaDuration.ofSeconds(askTimeout.toSeconds))

    log.debug(s"asking assets for power values: {}", nodeToAssetAgents)

    if (nodeToAssetAgents.values.flatten.nonEmpty)
      Some(
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
                        Quantities.getQuantity(1, PU),
                        Quantities.getQuantity(0, PU)
                      )
                  }

                (assetAgent ? RequestAssetPowerMessage(
                  currentTick,
                  Each(eInPu.to(PU).getValue.doubleValue),
                  Each(fInPU.to(PU).getValue.doubleValue)
                )).map {
                  case providedPowerValuesMessage: AssetPowerChangedMessage =>
                    (assetAgent, providedPowerValuesMessage)
                  case assetPowerUnchangedMessage: AssetPowerUnchangedMessage =>
                    (assetAgent, assetPowerUnchangedMessage)
                }
              })
            }.toVector
          )
          .map(ReceivedAssetPowerValues)
          .pipeTo(self)
      )
    else
      None
  }

  /** Triggers an execution of the akka `ask` pattern for all power values @
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
    *   Some(Future[ReceivedPowerValues]) if this grids has connected inferior
    *   grids or None if this no inferior grids
    */
  private def askInferiorGridsForPowers(
      currentSweepNo: Int,
      subGridGateToActorRef: Map[SubGridGate, ActorRef],
      inferiorGridGates: Seq[SubGridGate],
      askTimeout: Duration
  ): Option[Future[ReceivedPowerValues]] = {
    implicit val timeout: AkkaTimeout =
      AkkaTimeout.create(JavaDuration.ofSeconds(askTimeout.toSeconds))
    log.debug(
      s"asking inferior grids for power values: {}",
      inferiorGridGates
    )
    Option.when(inferiorGridGates.nonEmpty) {
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
              (inferiorGridAgentRef ? RequestGridPowerMessage(
                currentSweepNo,
                inferiorGridGateNodes.distinct
              )).map {
                case provideGridPowerMessage: ProvideGridPowerMessage =>
                  (inferiorGridAgentRef, provideGridPowerMessage)
                case FailedPowerFlow =>
                  (inferiorGridAgentRef, FailedPowerFlow)
              }
            }
            .toVector
        )
        .map(ReceivedGridPowerValues)
        .pipeTo(self)
    }
  }

  /** Triggers an execution of the akka `ask` pattern for all slack voltages of
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
    *   Some(Future[ReceivedSlackValues]) if this grids has connected superior
    *   grids or None if this no superior grids
    */
  private def askSuperiorGridsForSlackVoltages(
      currentSweepNo: Int,
      subGridGateToActorRef: Map[SubGridGate, ActorRef],
      superiorGridGates: Vector[SubGridGate],
      askTimeout: Duration
  ): Option[Future[ReceivedSlackVoltageValues]] = {
    implicit val timeout: AkkaTimeout =
      AkkaTimeout.create(JavaDuration.ofSeconds(askTimeout.toSeconds))
    log.debug(
      s"asking superior grids for slack voltage values: {}",
      superiorGridGates
    )

    Option.when(superiorGridGates.nonEmpty) {
      Future
        .sequence(
          superiorGridGates
            .groupBy(subGridGateToActorRef(_))
            .map { case (superiorGridAgent, gridGates) =>
              (superiorGridAgent ? RequestSlackVoltageMessage(
                currentSweepNo,
                gridGates.map(_.superiorNode.getUuid)
              )).map { case providedSlackValues: ProvideSlackVoltageMessage =>
                (superiorGridAgent, providedSlackValues)
              }
            }
            .toVector
        )
        .map(ReceivedSlackVoltageValues)
        .pipeTo(self)
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
  ): Unit = {
    gridAgentBaseData.sweepValueStores.lastOption.foreach {
      case (_, valueStore) =>
        notifyListener(
          this.createResultModels(
            gridAgentBaseData.gridEnv.gridModel,
            valueStore
          )(
            currentTimestamp
          )
        )
    }
  }

}
