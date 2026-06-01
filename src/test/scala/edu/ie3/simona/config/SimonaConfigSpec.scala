/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.ConfigParams.{BaseCsvParams, PsdmSinkCsvParams}
import edu.ie3.simona.config.InputConfig.{
  CoordinateSource,
  Grid,
  GridDatasource,
  LoadProfile,
  WeatherDatasource,
}
import edu.ie3.simona.config.RuntimeConfig.*
import edu.ie3.simona.config.SimonaConfig.Powerflow.Newtonraphson
import edu.ie3.simona.config.SimonaConfig.{
  AmpacityCalculation,
  CongestionManagement,
  GridConfig,
  Powerflow,
  Time,
}
import edu.ie3.simona.test.common.UnitSpec

import scala.concurrent.duration.DurationInt

class SimonaConfigSpec extends UnitSpec {

  "The SimonaConfig" should {

    "fill in all the default values correctly" in {
      // minimal necessary config
      val minimalConfig = ConfigFactory.parseString(
        """
          |simona.simulationName = "ConfigTestDataSimulation"
          |
          |simona.time.startDateTime = "2011-05-01T00:00:00Z"
          |simona.time.endDateTime = "2011-05-01T01:00:00Z"
          |
          |simona.input.grid.datasource.id = "csv"
          |simona.input.grid.datasource.csvParams = {
          |  directoryPath: "input/samples/vn_simona/fullGrid"
          |  isHierarchic: false
          |  csvSep: ","
          |}
          |
          |simona.output.base.dir = "testOutput/"
          |simona.output.sink.csv.csvSep = ","
          |simona.powerflow.newtonraphson.epsilon = [1E-12]
          |""".stripMargin
      )

      val simonaConfig = SimonaConfig(minimalConfig)

      // simulation name
      simonaConfig.simulationName shouldBe "ConfigTestDataSimulation"

      // time config
      simonaConfig.time shouldBe Time(
        endDateTime = "2011-05-01T01:00:00Z",
        schedulerReadyCheckWindow = None,
        startDateTime = "2011-05-01T00:00:00Z",
      )

      // ampacity config
      simonaConfig.ampacityCalculation shouldBe AmpacityCalculation(
        activateAmpacityCalculation = false
      )

      // congestion management config
      simonaConfig.congestionManagement shouldBe CongestionManagement(
        enableDetection = false
      )

      // control config
      simonaConfig.control shouldBe None

      // grid config
      simonaConfig.gridConfig shouldBe GridConfig(
        refSystems = None,
        voltageLimits = None,
      )

      // input config
      simonaConfig.input shouldBe InputConfig(
        extSimDir = None,
        grid = Grid(
          GridDatasource(
            Some(
              BaseCsvParams(
                ",",
                "input/samples/vn_simona/fullGrid",
                false,
              )
            ),
            "csv",
          )
        ),
        loadProfile = LoadProfile(
          LoadProfile.Datasource(
            None,
            None,
          )
        ),
        primary = InputConfig.Primary(
          None,
          None,
          None,
          None,
        ),
        weather = InputConfig.Weather(
          WeatherDatasource(
            coordinateSource = CoordinateSource(None, "icon", None, None),
            couchbaseParams = None,
            csvParams = None,
            influxDb1xParams = None,
            maxCoordinateDistance = 50000,
            resolution = 3600L,
            sampleParams = None,
            scheme = "icon",
            sqlParams = None,
          )
        ),
      )

      // output config
      simonaConfig.output shouldBe OutputConfig(
        base = OutputConfig
          .Base(addTimestampToOutputDir = true, dir = "testOutput/"),
        grid = OutputConfig.GridOutputConfig(
          congestions = false,
          lines = false,
          nodes = false,
          switches = false,
          transformers2w = false,
          transformers3w = false,
        ),
        log = OutputConfig.Log(level = "INFO", consoleLevel = None),
        participant = OutputConfig.ParticipantOutputConfigs(),
        sink = OutputConfig.Sink(
          csv = Some(
            PsdmSinkCsvParams(
              compressOutputs = false,
              csvSep = ",",
              fileFormat = ".csv",
              filePrefix = "",
              fileSuffix = "",
              isHierarchic = false,
            )
          ),
          influxDb1x = None,
          kafka = None,
        ),
        thermal = OutputConfig.ThermalOutputConfigs(),
      )

      // powerflow config
      simonaConfig.powerflow shouldBe Some(
        Powerflow(
          maxSweepPowerDeviation = 1e-5,
          newtonraphson = Newtonraphson(epsilon = List(1e-12), iterations = 50),
          resolution = 1.hours,
          stopOnFailure = false,
        )
      )

      // runtime config
      simonaConfig.runtime shouldBe RuntimeConfig(
        em = EmRuntimeConfigs(
          defaultConfig = EmRuntimeConfig(
            calculateMissingReactivePowerWithModel = false,
            scaling = 1.0,
            uuids = Nil,
            aggregateFlex = "SELF_OPT_EXCL_REG",
            curtailRegenerative = false,
          ),
          individualConfigs = Nil,
        ),
        listener = RuntimeConfig.Listener(eventsToProcess = None, kafka = None),
        participant = RuntimeConfig.Participant(
          bm = BmRuntimeConfigs(
            defaultConfig = BmRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
            ),
            individualConfigs = Nil,
          ),
          evcs = EvcsRuntimeConfigs(
            defaultConfig = EvcsRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
              chargingStrategy = "maxPower",
              departureTargetSoc = 0.75,
            ),
            individualConfigs = Nil,
          ),
          fixedFeedIn = FixedFeedInRuntimeConfigs(
            defaultConfig = FixedFeedInRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
            ),
            individualConfigs = Nil,
          ),
          hp = HpRuntimeConfigs(
            defaultConfig = HpRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
            ),
            individualConfigs = Nil,
          ),
          load = LoadRuntimeConfigs(
            defaultConfig = LoadRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
              modelBehaviour = "fix",
              reference = "power",
            ),
            individualConfigs = Nil,
          ),
          pv = PvRuntimeConfigs(
            defaultConfig = PvRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
            ),
            individualConfigs = Nil,
          ),
          requestVoltageDeviationThreshold = 1e-14,
          storage = StorageRuntimeConfigs(
            defaultConfig = StorageRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
              initialSoc = 0.0,
              targetSoc = None,
            ),
            individualConfigs = Nil,
          ),
          wec = WecRuntimeConfigs(
            defaultConfig = WecRuntimeConfig(
              calculateMissingReactivePowerWithModel = false,
              scaling = 1.0,
              uuids = Nil,
            ),
            individualConfigs = Nil,
          ),
        ),
        selectedSubgrids = None,
        selectedVoltLvls = None,
      )
    }

  }
}
