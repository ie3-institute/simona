/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  SubGridContainer,
}
import edu.ie3.datamodel.utils.ContainerUtils
import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.agent.grid.GridAgentMessages.*
import edu.ie3.simona.agent.grid.congestion.CongestionManagementParams
import edu.ie3.simona.agent.grid.data.GridAgentData.{
  GridAgentConstantData,
  GridAgentInitData,
  GridAgentRef,
}
import edu.ie3.simona.agent.grid.powerflow.PowerFlowParams
import edu.ie3.simona.config.GridConfigParser.{
  ConfigRefSystems,
  ConfigVoltageLimits,
}
import edu.ie3.simona.config.{GridConfigParser, SimonaConfig}
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.model.grid.{GridModel, RefSystem, VoltageLimits}
import edu.ie3.util.quantities.PowerSystemUnits
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.actor.typed.scaladsl.ActorContext
import org.slf4j.Logger
import squants.electro.Kilovolts

import java.util.UUID
import scala.jdk.CollectionConverters.SetHasAsScala

object GridAgentCoordinator {

  /** Method to create the grid agents.
    * @param grid
    *   The grid container with sub grids.
    * @param resolution
    *   The power flow resolution.
    * @param pfParams
    *   The power flow parameters.
    * @param cfg
    *   The simona config.
    * @param context
    *   The actor context for spawning grid agents.
    * @param environmentRefs
    *   The environment references.
    * @return
    *   A map: subgrid number to reference and a map: node uuid to subgrid.
    */
  def createGridAgents(
      grid: JointGridContainer,
      resolution: Long,
      pfParams: PowerFlowParams,
      cfg: SimonaConfig.Simona,
  )(using
      context: ActorContext[?],
      environmentRefs: EnvironmentRefs,
  ): (
      Map[Int, GridAgentRef],
      Map[UUID, Int],
  ) = {

    /* get the grid */
    val subGridTopologyGraph = grid.getSubGridTopologyGraph

    val subGrids = subGridTopologyGraph
      .vertexSet()
      .asScala

    val nodeToSubgrid = subGrids.flatMap {
      _.getRawGrid.getNodes.asScala.map(node => node.getUuid -> node.getSubnet)
    }.toMap

    given GridAgentConstantData = GridAgentConstantData(
      environmentRefs,
      cfg,
      resolution,
      cfg.time.simStartTime,
      cfg.time.simEndTime,
      CongestionManagementParams(cfg.congestionManagement.enableDetection),
    )

    /* Create all agents and map the sub grid id to their actor references */
    val (subGridToActorRefMap, actorRefToNodes) = createGridAgents(
      subGrids,
      context,
      pfParams,
    )

    // register inferior grids
    actorRefToNodes.flatMap { case (actorRef, couplingNodes) =>
      // register inferior grid by providing the superior grid with the coupling nodes
      couplingNodes.groupBy(nodeToSubgrid).foreach { case (subgridNo, nodes) =>
        val superiorGrid = subGridToActorRefMap(subgridNo)

        superiorGrid ! RegisterInferiorGrid(actorRef, nodes, subgridNo)
        actorRef ! RegisterSuperiorGrid(superiorGrid, nodes, subgridNo)
      }

      Option.when(couplingNodes.isEmpty)(actorRef)
    }

    (subGridToActorRefMap, nodeToSubgrid)
  }

  /** Method to create a map from subgrid number to grid agent reference and a
    * map of grid agent reference to coupling nodes with the superior grid.
    * @param subGrids
    *   A sequence of subgrid container.
    * @param context
    *   The actor context for spawning grid agents.
    * @param pfParams
    *   The parameter for the power flow.
    * @param constantData
    *   The grid agent constant data.
    * @return
    *   A map: subgrid number to reference and a map: reference to coupling
    *   nodes.
    */
  private def createGridAgents(
      subGrids: Iterable[SubGridContainer],
      context: ActorContext[?],
      pfParams: PowerFlowParams,
  )(using
      constantData: GridAgentConstantData
  ): (
      Map[Int, GridAgentRef],
      Map[GridAgentRef, Set[UUID]],
  ) = {
    given Logger = context.log

    val cfg = constantData.simonaConfig

    /* extract and prepare refSystem information from config */
    val (configRefSystems, configVoltageLimits) =
      GridConfigParser.parse(cfg.gridConfig)

    val (numberToRef, refToCouplingNodes) = subGrids.map { subGridContainer =>
      /* Prepare the subgrid container for the agents by adapting the transformer high voltage nodes to be slacks */
      val updatedSubGridContainer =
        ContainerUtils.withTrafoNodeAsSlack(subGridContainer)

      val rawGrid = updatedSubGridContainer.getRawGrid
      val slackNodes = rawGrid.getNodes.asScala.filter(_.isSlack)

      val couplingNodes =
        (rawGrid.getTransformer2Ws.asScala ++ rawGrid.getTransformer3Ws.asScala)
          .filter(t => slackNodes.contains(t.getNodeA))
          .map(_.getNodeA.getUuid)
          .toSet

      val subgridNumber = subGridContainer.getSubnet

      // get the [[GridModel]]
      val gridModel = GridModel(
        updatedSubGridContainer,
        getRefSystem(configRefSystems, subGridContainer),
        getVoltageLimits(configVoltageLimits, subGridContainer),
        constantData.simStartTime,
        constantData.simEndTime,
        cfg,
      )

      // create the GridAgentInitData
      val gridAgentInitData = GridAgentInitData(gridModel, pfParams)

      val gridAgentRef =
        context.spawn(
          GridAgent(gridAgentInitData),
          s"GridAgent-$subgridNumber",
        )

      context.watch(gridAgentRef)

      (subgridNumber -> gridAgentRef, gridAgentRef -> couplingNodes)
    }.unzip

    (numberToRef.toMap, refToCouplingNodes.toMap)
  }

  /** Searches for the reference system to be used with the given
    * [[SubGridContainer]] within the information provided by config.
    *
    * @param configRefSystems
    *   Collection of reference systems definitions from config.
    * @param subGridContainer
    *   Container model for the respective sub grid.
    * @return
    *   The reference system to use.
    */
  private def getRefSystem(
      configRefSystems: ConfigRefSystems,
      subGridContainer: SubGridContainer,
  )(using log: Logger): RefSystem = {
    val refSystem = configRefSystems
      .find(
        subGridContainer.getSubnet,
        Some(subGridContainer.getPredominantVoltageLevel),
      )
      .getOrElse(
        throw new InitializationException(
          s"Unable to determine refSystem for grid with id ${subGridContainer.getSubnet} @ " +
            s"volt level ${subGridContainer.getPredominantVoltageLevel}. Please either provide a refSystem for the grid id or the whole volt level!"
        )
      )

    val containerPotential = Kilovolts(
      subGridContainer.getPredominantVoltageLevel.getNominalVoltage
        .to(PowerSystemUnits.KILOVOLT)
        .getValue
        .doubleValue
    )

    if refSystem.nominalVoltage != containerPotential then
      log.warn(
        s"The configured RefSystem for subGrid ${subGridContainer.getSubnet} differs in its nominal voltage (${refSystem.nominalVoltage}) from the grids" +
          s"predominant voltage level nominal voltage ($containerPotential). If this is by intention and still valid, this warning can be just ignored!"
      )

    refSystem
  }

  /** Searches for the voltage limits to be used with the given
    * [[SubGridContainer]] within the information provided by config.
    *
    * @param configVoltageLimits
    *   Collection of voltage limits definitions from config.
    * @param subGridContainer
    *   Container model for the respective sub grid.
    * @return
    *   The voltage limits to use.
    */
  private def getVoltageLimits(
      configVoltageLimits: ConfigVoltageLimits,
      subGridContainer: SubGridContainer,
  ): VoltageLimits = configVoltageLimits
    .find(
      subGridContainer.getSubnet,
      Some(subGridContainer.getPredominantVoltageLevel),
    )
    .getOrElse(
      throw new InitializationException(
        s"Unable to determine voltage limits for grid with id ${subGridContainer.getSubnet} @ " +
          s"volt level ${subGridContainer.getPredominantVoltageLevel}. Please either provide voltage limits for the grid id or the whole volt level!"
      )
    )
}
