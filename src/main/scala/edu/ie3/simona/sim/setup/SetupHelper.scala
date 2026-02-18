/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import com.typesafe.config.Config
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.result.ResultEntity
import edu.ie3.datamodel.models.result.system.FlexOptionsResult
import edu.ie3.simona.config.{OutputConfig, SimonaConfig}
import edu.ie3.simona.io.result.ResultSinkType
import edu.ie3.simona.logging.LogbackConfiguration
import edu.ie3.simona.util.ConfigUtil.{GridOutputConfigUtil, OutputConfigUtil}
import edu.ie3.simona.util.ResultFileHierarchy.ResultEntityPathConfig
import edu.ie3.simona.util.{EntityMapperUtil, ResultFileHierarchy}

/** Methods to support the setup of a simona simulation.
  *
  * @version 0.1
  * @since 02.07.20
  */
trait SetupHelper extends LazyLogging {

  /** Build the result file hierarchy based on the provided configuration file.
    * The provided type safe config must be able to be parsed as
    * [[SimonaConfig]], otherwise an exception is thrown.
    *
    * @param typeSafeConfig
    *   All configuration parameters.
    * @param simonaConfig
    *   The configuration for SIMONA.
    * @return
    *   The resulting result file hierarchy.
    */
  def buildResultFileHierarchy(
      typeSafeConfig: Config,
      simonaConfig: SimonaConfig,
  ): ResultFileHierarchy = {

    /* Determine the result models to write */
    val modelsToWrite =
      SetupHelper.allResultEntitiesToWrite(simonaConfig.simona.output)

    val simonaLogConfig = simonaConfig.simona.output.log

    ResultFileHierarchy(
      simonaConfig.simona.output.base.dir,
      simonaConfig.simona.simulationName,
      ResultEntityPathConfig(
        modelsToWrite,
        ResultSinkType(
          simonaConfig.simona.output.sink,
          simonaConfig.simona.simulationName,
        ),
      ),
      configureLogger = LogbackConfiguration
        .default(simonaLogConfig.level, simonaLogConfig.consoleLevel),
      config = Some((typeSafeConfig, simonaConfig)),
      addTimeStampToOutputDir =
        simonaConfig.simona.output.base.addTimestampToOutputDir,
    )
  }
}

object SetupHelper {

  /** Determine a comprehensive collection of all [[ResultEntity]] classes, that
    * will have to be considered.
    *
    * @param outputConfig
    *   configuration to consider.
    * @return
    *   Set of [[ResultEntity]] classes.
    */
  private def allResultEntitiesToWrite(
      outputConfig: OutputConfig
  ): Set[Class[? <: ResultEntity]] =
    GridOutputConfigUtil(
      outputConfig.grid
    ).simulationResultEntitiesToConsider ++
      (OutputConfigUtil
        .participants(
          outputConfig.participant
        )
        .simulationResultIdentifiersToConsider(thermal =
          false
        ) ++ OutputConfigUtil
        .thermal(
          outputConfig.thermal
        )
        .simulationResultIdentifiersToConsider(thermal = true))
        .map(notifierId => EntityMapperUtil.getResultEntityClass(notifierId)) ++
      (if OutputConfigUtil
           .participants(
             outputConfig.participant
           )
           ._1
           .flexResult
       then Seq(classOf[FlexOptionsResult])
       else Seq.empty)
}
