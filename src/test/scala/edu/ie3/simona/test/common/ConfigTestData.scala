/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import org.apache.pekko.actor.ActorRef
import com.typesafe.config.{Config, ConfigFactory}
import edu.ie3.simona.config.SimonaConfig

/** Simple (empty) configuration data. Furthermore, it would make sense to
  * implement another class which reads a config and provides config based
  * values in the future.
  */
trait ConfigTestData {
  protected val typesafeConfig: Config = ConfigFactory.parseString(
    """
      |simona.simulationName = "ConfigTestDataSimulation"
      |simona.input.grid.datasource.id = "csv"
      |simona.input.grid.datasource.csvParams = {
      |  directoryPath: "input/samples/vn_simona/fullGrid"
      |  isHierarchic: false
      |  csvSep: ","
      |}
      |simona.input.primary.csvParams = {
      |  directoryPath: "input/samples/two_winding"
      |  isHierarchic: false
      |  csvSep: ","
      |}
      |simona.input.weather.datasource = {
      |  scheme = "icon"
      |  sampleParams.use = true
      |  coordinateSource.sampleParams.use = true
      |}
      |
      |simona.output.base.dir = "testOutput/"
      |simona.output.sink.csv {
      |  fileFormat = ".csv"
      |  filePrefix = ""
      |  fileSuffix = ""
      |}
      |
      |simona.output.grid = {
      |  notifier = "grid"
      |  nodes = false
      |  lines = false
      |  switches = false
      |  transformers2w = false
      |  transformers3w = false
      |}
      |simona.output.participant.defaultConfig = {
      |    notifier = "default"
      |    powerRequestReply = false
      |    simulationResult = false
      |}
      |simona.output.participant.individualConfigs = []
      |simona.output.thermal = {
      |  defaultConfig = {
      |    notifier = "default",
      |    simulationResult = false
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.runtime.participant.requestVoltageDeviationThreshold = 1E-14
      |simona.runtime.participant.load = {
      |  defaultConfig = {
      |    calculateMissingReactivePowerWithModel = false
      |    uuids = ["default"]
      |    scaling = 1.0
      |    modelBehaviour = "fix"
      |    reference = "power"
      |  }
      |  individualConfigs = []
      |}
      |simona.runtime.participant.fixedFeedIn = {
      |  defaultConfig = {
      |       calculateMissingReactivePowerWithModel = false
      |       uuids = ["default"]
      |       scaling = 1.0
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.runtime.participant.pv = {
      |  defaultConfig = {
      |       calculateMissingReactivePowerWithModel = false
      |       uuids = ["default"]
      |       scaling = 1.0
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.runtime.participant.wec = {
      |  defaultConfig = {
      |       calculateMissingReactivePowerWithModel = false
      |       uuids = ["default"]
      |       scaling = 1.0
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.runtime.participant.evcs = {
      |  defaultConfig = {
      |       calculateMissingReactivePowerWithModel = false
      |       uuids = ["default"]
      |       scaling = 1.0
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.runtime.participant.hp = {
      |  defaultConfig = {
      |       calculateMissingReactivePowerWithModel = false
      |       uuids = ["default"]
      |       scaling = 1.0
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.runtime.participant.storage = {
      |  defaultConfig = {
      |       calculateMissingReactivePowerWithModel = false
      |       uuids = ["default"]
      |       scaling = 1.0
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.runtime.participant.em = {
      |  defaultConfig = {
      |       calculateMissingReactivePowerWithModel = false
      |       uuids = ["default"]
      |       scaling = 1.0
      |  }
      |  individualConfigs = []
      |}
      |
      |simona.powerflow.maxSweepPowerDeviation = 1E-5 // the maximum allowed deviation in power between two sweeps, before overall convergence is assumed
      |simona.powerflow.stopOnFailure = true
      |simona.powerflow.newtonraphson.epsilon = [1E-12]
      |simona.powerflow.newtonraphson.iterations = 50
      |
      |simona.gridConfig.refSystems = []
      |""".stripMargin
  )
  protected val simonaConfig: SimonaConfig = SimonaConfig(typesafeConfig)

  protected val listener: Iterable[ActorRef] = Iterable.empty[ActorRef]
}
