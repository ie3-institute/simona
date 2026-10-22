/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.config

import com.typesafe.config.ConfigFactory
import edu.ie3.simona.config.ConfigParams.{BaseCsvParams, PsdmSinkCsvParams}
import edu.ie3.simona.config.InputConfig.{Grid, GridDatasource}
import edu.ie3.simona.config.SimonaConfig.Powerflow.Newtonraphson
import edu.ie3.simona.config.SimonaConfig.{
  CongestionManagement,
  GridConfig,
  Powerflow,
  Time,
}
import edu.ie3.simona.test.common.UnitSpec

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

      // congestion management config
      simonaConfig.congestionManagement shouldBe CongestionManagement()

      // control config
      simonaConfig.control shouldBe None

      // grid config
      simonaConfig.gridConfig shouldBe GridConfig()

      // input config
      simonaConfig.input shouldBe InputConfig(
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
        )
      )

      // output config
      simonaConfig.output shouldBe OutputConfig(
        base = OutputConfig
          .Base(dir = "testOutput/"),
        sink = OutputConfig.Sink(
          csv = Some(
            PsdmSinkCsvParams()
          )
        ),
      )

      // powerflow config
      simonaConfig.powerflow shouldBe Some(
        Powerflow(
          newtonraphson = Newtonraphson(epsilon = List(1e-12))
        )
      )

      // runtime config
      simonaConfig.runtime shouldBe RuntimeConfig()
    }

  }
}
