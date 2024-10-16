/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common

import com.typesafe.config.{Config, ConfigFactory}
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.simona.config.SimonaConfig
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.model.participant.load.{LoadModelBehaviour, LoadReference}
import edu.ie3.util.scala.OperationInterval
import org.locationtech.jts.geom.{Coordinate, GeometryFactory, Point}
import squants.electro.Kilovolts
import squants.energy.Kilowatts

import java.time.{ZoneId, ZonedDateTime}

/** Default values to be used in tests. Should be extended as needed.
  */
trait DefaultTestData {
  // default power flow resolution
  protected val defaultResolution = 3600L

  // Default start and end date of operation for building input models
  protected val defaultSimulationStart: ZonedDateTime =
    ZonedDateTime.of(2019, 1, 1, 0, 0, 0, 0, ZoneId.of("UTC"))
  protected val defaultSimulationEnd: ZonedDateTime =
    ZonedDateTime.of(2019, 12, 31, 0, 0, 0, 0, ZoneId.of("UTC"))

  private val operationTimeBuilder = OperationTime.builder()
  operationTimeBuilder.withStart(defaultSimulationStart)
  operationTimeBuilder.withEnd(defaultSimulationEnd)
  protected val defaultOperationTime: OperationTime =
    operationTimeBuilder.build()

  operationTimeBuilder.withStart(defaultSimulationStart.withHour(1))
  operationTimeBuilder.withEnd(defaultSimulationEnd)
  protected val postponedOperationTime: OperationTime =
    operationTimeBuilder.build()

  protected val defaultOperationInterval: OperationInterval =
    SystemComponent.determineOperationInterval(
      defaultSimulationStart,
      defaultSimulationEnd,
      defaultOperationTime,
    )

  // default Lat/Long
  protected val defaultLatitude = 52.02083574
  protected val defaultLongitude = 7.40110716

  private val geometryFactory = new GeometryFactory()
  protected val defaultLatLong: Point = geometryFactory.createPoint(
    new Coordinate(defaultLongitude, defaultLatitude)
  )

  protected val default400Kva10KvRefSystem: RefSystem = RefSystem(
    Kilowatts(400d),
    Kilovolts(10d),
  )

  /** Creates a [[SimonaConfig]], that provides the desired participant model
    * configurations
    *
    * @param modelBehaviour
    *   Desired behaviour of the load model
    * @param reference
    *   Desired reference
    * @return
    *   Suitable configuration
    */
  def createSimonaConfig(
      modelBehaviour: LoadModelBehaviour.Value,
      reference: LoadReference,
  ): SimonaConfig = {
    val typesafeConfig: Config = ConfigFactory.parseString(
      s"""
         |simona.simulationName = "ParticipantAgentTest"
         |
         |simona.time.startDateTime = "01/01/2020 00:00:00Z"
         |simona.time.endDateTime = "01/01/2020 01:00:00Z"
         |
         |simona.input.grid.datasource.id = "csv"
         |simona.output.base.dir = "testOutput/"
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
         |    flexResult = false
         |}
         |simona.output.participant.individualConfigs = []
         |
         |simona.runtime.participant.load = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |    modelBehaviour = "fix"
         |    reference = "power"
         |  }
         |  individualConfigs = [
         |    {
         |      calculateMissingReactivePowerWithModel = false
         |      uuids = ["4eeaf76a-ec17-4fc3-872d-34b7d6004b03"]
         |      scaling = 1.0
         |      modelBehaviour = "${modelBehaviour.toString}"
         |      reference = "${reference.key}"
         |    }
         |  ]
         |}
         |
         |simona.runtime.participant.fixedFeedIn = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |  }
         |  individualConfigs = [
         |    {
         |      calculateMissingReactivePowerWithModel = false
         |      uuids = ["9abe950d-362e-4efe-b686-500f84d8f368"]
         |      scaling = 1.0
         |    }
         |  ]
         |}
         |
         |simona.runtime.participant.pv = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |  }
         |  individualConfigs = [
         |    {
         |      calculateMissingReactivePowerWithModel = false
         |      uuids = ["9abe950d-362e-4ffe-b686-500f84d8f368"]
         |      scaling = 1.0
         |    }
         |  ]
         |}
         |simona.output.thermal = {
         |  defaultConfig = {
         |    notifier = "default",
         |    simulationResult = false
         |  }
         |  individualConfigs = []
         |}
         |
         |simona.runtime.participant.wec = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |  }
         |  individualConfigs = [
         |    {
         |      calculateMissingReactivePowerWithModel = false
         |      uuids = ["9abe950d-362e-3ffe-b686-500f84d8f368"]
         |      scaling = 1.0
         |    }
         |  ]
         |}
         |
         |simona.runtime.participant.evcs = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |  }
         |  individualConfigs = [
         |    {
         |      calculateMissingReactivePowerWithModel = false
         |      uuids = ["9abe950d-362e-4ffe-b686-500f84d8f368"]
         |      scaling = 1.0
         |    }
         |  ]
         |}
         |
         |simona.runtime.participant.hp = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |  }
         |  individualConfigs = [
         |    {
         |      calculateMissingReactivePowerWithModel = false
         |      uuids = ["9abe950d-362e-4ffe-b686-500f84d8f368"]
         |      scaling = 1.0
         |    }
         |  ]
         |}
         |
         |simona.runtime.participant.storage = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |  }
         |  individualConfigs = [
         |    {
         |      calculateMissingReactivePowerWithModel = false
         |      uuids = ["9abe950d-362e-4ffe-b686-500f84d8f368"]
         |      scaling = 1.0
         |    }
         |  ]
         |}
         |
         |simona.runtime.participant.em = {
         |  defaultConfig = {
         |    calculateMissingReactivePowerWithModel = false
         |    uuids = ["default"]
         |    scaling = 1.0
         |  }
         |  individualConfigs = []
         |}
         |
         |simona.powerflow.maxSweepPowerDeviation = 1E-5 // the maximum allowed deviation in power between two sweeps, before overall convergence is assumed
         |simona.powerflow.stopOnFailure = true
         |simona.powerflow.newtonraphson.epsilon = [1E-12]
         |simona.powerflow.newtonraphson.iterations = 50
         |simona.powerflow.resolution = "3600s"
         |
         |simona.gridConfig.refSystems = []
         |""".stripMargin
    )
    SimonaConfig(typesafeConfig)
  }
}
