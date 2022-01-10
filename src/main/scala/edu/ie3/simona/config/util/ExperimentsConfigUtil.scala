/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config.util

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.config.ExperimentsConfig
import edu.ie3.simona.logging.logback.LogbackConfiguration

object ExperimentsConfigUtil extends LazyLogging {

  private val consoleLoggingClasses = Seq(
    "edu.ie3.simona.main.RunSimonaStandalone",
    "edu.ie3.simona.util.ResultFileHierarchy",
    "edu.ie3.simona.sim.SimonaSim",
    "edu.ie3.simona.main.RunSimona",
    "edu.ie3.simona.logging.logback.LogbackConfiguration"
  )

  def configureSimulation(experimentsCfg: ExperimentsConfig): Unit = {

    // disable console log on request
    if (!experimentsCfg.simona.experiments.log.console) {
      logger.info(
        s"Console only logs the following classes:\n\t${consoleLoggingClasses.mkString("\n\t")}.\n" +
          s"If configured, full simulation logs will be in the corresponding output dirs."
      )
      LogbackConfiguration.disableConsoleLogging(consoleLoggingClasses)
    }

  }

}
