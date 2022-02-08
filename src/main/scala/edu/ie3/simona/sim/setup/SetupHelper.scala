/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import akka.actor.ActorRef
import com.typesafe.config.{Config => TypesafeConfig}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.graph.SubGridGate
import edu.ie3.datamodel.models.input.container.SubGridContainer
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.utils.ContainerUtils
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.RefSystemParser.ConfigRefSystems
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.exceptions.InitializationException
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.util.ConfigUtil.{
  BaseOutputConfigUtil,
  GridOutputConfigUtil
}
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import edu.ie3.simona.util.{EntityMapperUtil, ResultFileHierarchy}

/** Methods to support the setup of a simona simulation
  *
  * @version 0.1
  * @since 02.07.20
  */
trait SetupHelper extends LazyLogging {

  /** Build the [[GridAgentInitData]]. This also includes the determination of a
    * mapping from [[SubGridGate]] to [[ActorRef]] of the representing
    * [[edu.ie3.simona.agent.grid.GridAgent]] as well as the look up of the
    * [[RefSystem]] to use being defined in the config.
    *
    * @param subGridContainer
    *   Container of all models for this sub grid
    * @param subGridToActorRef
    *   Mapping from sub grid number to [[edu.ie3.simona.agent.grid.GridAgent]]
    *   's [[ActorRef]]
    * @param gridGates
    *   [[Set]] of all [[SubGridGate]] s connecting this sub grid with it's
    *   ancestors and children
    * @param configRefSystems
    *   Collection of reference systems defined in config
    * @return
    *   Initialization data for the [[edu.ie3.simona.agent.grid.GridAgent]]
    *   representing this sub grid
    */
  def buildGridAgentInitData(
      subGridContainer: SubGridContainer,
      subGridToActorRef: Map[Int, ActorRef],
      gridGates: Set[SubGridGate],
      configRefSystems: ConfigRefSystems
  ): GridAgentInitData = {
    val subGridGateToActorRef = buildGateToActorRef(
      subGridToActorRef,
      gridGates,
      subGridContainer.getSubnet
    )

    /* Find the matching reference system */
    val refSystem =
      getRefSystem(configRefSystems, subGridContainer)

    /* Prepare the subgrid container for the agents by adapting the transformer high voltage nodes to be slacks */
    val updatedSubGridContainer =
      ContainerUtils.withTrafoNodeAsSlack(subGridContainer)

    // build the grid agent data and check for its validity
    GridAgentInitData(updatedSubGridContainer, subGridGateToActorRef, refSystem)
  }

  /** Maps the [[SubGridGate]] s of a given sub grid to the corresponding actor
    * references
    *
    * @param subGridToActorRefMap
    *   Mapping from sub grid number to actor reference
    * @param subGridGates
    *   Gates from the given sub grid to other sub grids
    * @param currentSubGrid
    *   Current grid number (only for building exception message)
    * @return
    *   A mapping from [[SubGridGate]] to corresponding actor reference
    */
  def buildGateToActorRef(
      subGridToActorRefMap: Map[Int, ActorRef],
      subGridGates: Set[SubGridGate],
      currentSubGrid: Int
  ): Map[SubGridGate, ActorRef] =
    subGridGates
      .groupBy(gate => (gate.getSuperiorNode, gate.getInferiorNode))
      .flatMap(_._2.headOption)
      .map(gate => {
        val superiorSubGrid = gate.getSuperiorSubGrid
        val inferiorSubGrid = gate.getInferiorSubGrid
        if (inferiorSubGrid == currentSubGrid) {
          /* This is a gate to a superior sub grid */
          gate -> getActorRef(
            subGridToActorRefMap,
            currentSubGrid,
            superiorSubGrid
          )
        } else if (superiorSubGrid == currentSubGrid) {
          /* This is a gate to an inferior sub grid */
          gate -> getActorRef(
            subGridToActorRefMap,
            currentSubGrid,
            inferiorSubGrid
          )
        } else {
          throw new GridAgentInitializationException(
            "I am supposed to connect sub grid " + currentSubGrid + " with itself, which is not allowed."
          )
        }
      })
      .toMap

  /** Get the actor reference from the map or throw an exception, if it is not
    * apparent
    *
    * @param subGridToActorRefMap
    *   Mapping from sub grid number to actor reference
    * @param currentSubGrid
    *   Current grid number (only for building exception message)
    * @param queriedSubGrid
    *   The sub grid to look for
    * @return
    *   The actor reference of the sub grid to look for
    */
  def getActorRef(
      subGridToActorRefMap: Map[Int, ActorRef],
      currentSubGrid: Int,
      queriedSubGrid: Int
  ): ActorRef = {
    subGridToActorRefMap.get(queriedSubGrid) match {
      case Some(hit) => hit
      case _ =>
        throw new GridAgentInitializationException(
          "I am supposed to connect sub grid " + currentSubGrid + " with " + queriedSubGrid + ", but I cannot find the matching actor reference."
        )
    }
  }

  /** Searches for the reference system to be used with the given
    * [[SubGridContainer]] within the information provided by config.
    *
    * @param configRefSystems
    *   Collection of reference systems definitions from config
    * @param subGridContainer
    *   Container model for the respective sub grid
    * @return
    *   The reference system to use
    */
  def getRefSystem(
      configRefSystems: ConfigRefSystems,
      subGridContainer: SubGridContainer
  ): RefSystem = {
    val refSystem = configRefSystems
      .find(
        subGridContainer.getSubnet,
        Some(subGridContainer.getPredominantVoltageLevel)
      )
      .getOrElse(
        throw new InitializationException(
          s"Unable to determine refSystem for grid with id ${subGridContainer.getSubnet} @ " +
            s"volt level ${subGridContainer.getPredominantVoltageLevel}. Please either provide a refSystem for the grid id or the whole volt level!"
        )
      )

    if (
      !refSystem.nominalVoltage.equals(
        subGridContainer.getPredominantVoltageLevel.getNominalVoltage
      )
    )
      logger.warn(
        s"The configured RefSystem for subGrid ${subGridContainer.getSubnet} differs in its nominal voltage (${refSystem.nominalVoltage}) from the grids" +
          s"predominant voltage level nominal voltage (${subGridContainer.getPredominantVoltageLevel.getNominalVoltage}). If this is by intention and still valid, this " +
          s"warning can be just ignored!"
      )

    refSystem
  }

  /** Build the result file hierarchy based on the provided configuration file.
    * The provided type safe config must be able to be parsed as
    * [[SimonaConfig]], otherwise an exception is thrown
    * @param config
    *   the configuration file
    * @param createDirs
    *   if directories of the result file hierarchy should be created or not
    * @return
    *   the resulting result file hierarchy
    */
  def buildResultFileHierarchy(
      config: TypesafeConfig,
      createDirs: Boolean = true
  ): ResultFileHierarchy = {

    val simonaConfig = SimonaConfig(config)

    /* Determine the result models to write */
    val modelsToWrite =
      SetupHelper.allResultEntitiesToWrite(simonaConfig.simona.output)

    val resultFileHierarchy = ResultFileHierarchy(
      simonaConfig.simona.output.base.dir,
      simonaConfig.simona.simulationName,
      ResultEntityPathConfig(
        modelsToWrite,
        ResultSinkType(
          simonaConfig.simona.output.sink,
          simonaConfig.simona.simulationName
        )
      ),
      addTimeStampToOutputDir =
        simonaConfig.simona.output.base.addTimestampToOutputDir,
      createDirs = createDirs
    )

    // copy config data to output directory
    ResultFileHierarchy.prepareDirectories(config, resultFileHierarchy)

    resultFileHierarchy
  }
}

case object SetupHelper {

  /** Determine a comprehensive collection of all [[ResultEntity]] classes, that
    * will have to be considered
    *
    * @param outputConfig
    *   configuration to consider
    * @return
    *   Set of [[ResultEntity]] classes
    */
  def allResultEntitiesToWrite(
      outputConfig: SimonaConfig.Simona.Output
  ): Set[Class[_ <: ResultEntity]] =
    GridOutputConfigUtil(
      outputConfig.grid
    ).simulationResultEntitiesToConsider ++
      BaseOutputConfigUtil(
        outputConfig.participant
      ).simulationResultIdentifiersToConsider
        .map(notifierId => EntityMapperUtil.getResultEntityClass(notifierId))
}
