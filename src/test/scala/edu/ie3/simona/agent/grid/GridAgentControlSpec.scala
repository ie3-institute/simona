/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.simona.agent.grid.GridAgentData.GridAgentInitData
import edu.ie3.simona.config.SimonaConfig.TransformerControlGroup
import edu.ie3.simona.exceptions.agent.GridAgentInitializationException
import edu.ie3.simona.model.grid.RefSystem
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.GridInputTestData

class GridAgentControlSpec extends UnitSpec with GridInputTestData {

  "GridAgentControl" should {
    val checkBoundariesOfControlGroup = PrivateMethod[Unit](
      Symbol("checkBoundariesOfControlGroup")
    )

    val gridAgentInitData = GridAgentInitData(
      validTestGridInputModel,
      Map.empty,
      RefSystem("400 kVA", "400 V")
    )

    "throw Exception when vMin is lower then -21% of nominal Voltage" in {
      val transformerControlGroup = Seq(
        TransformerControlGroup(
          List.empty[String],
          List.empty[String],
          1.1,
          0.79
        )
      )

      intercept[GridAgentInitializationException] {
        GridAgent invokePrivate checkBoundariesOfControlGroup(
          transformerControlGroup,
          gridAgentInitData
        )
      }.getMessage shouldBe "TestGrid has a control group which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
        "by invalid parametrization of one control groups where vMin is lower than the lower boundary (0.8 of nominal Voltage)!"

    }

    "throw Exception when vMax is higher then +21% of nominal Voltage" in {
      val transformerControlGroup = Seq(
        TransformerControlGroup(
          List.empty[String],
          List.empty[String],
          1.21,
          0.9
        )
      )

      intercept[GridAgentInitializationException] {
        GridAgent invokePrivate checkBoundariesOfControlGroup(
          transformerControlGroup,
          gridAgentInitData
        )
      }.getMessage shouldBe "TestGrid has a control group which control boundaries exceed the limit of +- 20% of nominal voltage! This may be caused " +
        "by invalid parametrization of one control groups where vMax is higher than the upper boundary (1.2 of nominal Voltage)!"

    }

    "should run through when vMin and vMax are within the boundaries of +-20% of nominal Voltage" in {
      val transformerControlGroup = Seq(
        TransformerControlGroup(
          List.empty[String],
          List.empty[String],
          1.1,
          0.9
        )
      )

      GridAgent invokePrivate checkBoundariesOfControlGroup(
        transformerControlGroup,
        gridAgentInitData
      )
    }

  }
}
