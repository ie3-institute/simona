/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.datamodel.models.StandardUnits
import edu.ie3.datamodel.models.result.NodeResult
import edu.ie3.datamodel.models.result.connector.{
  LineResult,
  Transformer2WResult,
}
import edu.ie3.simona.agent.grid.congestion.VoltageRange.calculatePossibleVoltageDeltaForLines
import edu.ie3.simona.agent.grid.congestion.{Congestions, VoltageRange}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{
  GridModel,
  RefSystem,
  TransformerTapping,
  VoltageLimits,
}
import edu.ie3.simona.test.common.model.grid.{
  DbfsTestGrid,
  GridComponentsMokka,
  SubGridGateMokka,
}
import edu.ie3.simona.test.common.result.ResultMokka
import edu.ie3.simona.test.common.{ConfigTestData, UnitSpec}
import edu.ie3.util.quantities.PowerSystemUnits.PU
import edu.ie3.util.quantities.QuantityUtils.{asPu, asDegreeGeom}
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}
import squants.electro.Kilovolts
import squants.energy.Kilowatts
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime

class CongestionSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridComponentsMokka
    with ResultMokka
    with SubGridGateMokka
    with DbfsTestGrid
    with ConfigTestData {

  "A Congestion" should {
    val startTime = ZonedDateTime.now()

    val gridModel = GridModel(
      hvGridContainer,
      RefSystem(Kilowatts(600), Kilovolts(110)),
      VoltageLimits(0.9, 1.1),
      startTime,
      startTime.plusHours(2),
      simonaConfig,
    )

    "find congestions correctly for empty results" in {
      val emptyResults = PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
      )

      val congestedComponents = CongestedComponents(
        emptyResults,
        gridModel.gridComponents,
        gridModel.voltageLimits,
        gridModel.mainRefSystem.nominalVoltage,
        gridModel.subnetNo,
      )

      Congestions(congestedComponents) shouldBe Congestions(
        voltageCongestions = false,
        lineCongestions = false,
        transformerCongestions = false,
      )
    }

    "find voltage congestions correctly" in {
      val nodeResult1 = new NodeResult(
        startTime,
        node1.getUuid,
        Quantities.getQuantity(1d, PU),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val nodeResult2 = new NodeResult(
        startTime,
        node2.getUuid,
        Quantities.getQuantity(0.9d, PU),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val nodeResult3 = new NodeResult(
        startTime,
        node3.getUuid,
        Quantities.getQuantity(1.1d, PU),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val nodeResult4 = new NodeResult(
        startTime,
        node4.getUuid,
        Quantities.getQuantity(0.89d, PU),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val results = PowerFlowResultEvent(
        Seq(nodeResult1, nodeResult2, nodeResult3, nodeResult4),
        Seq.empty,
        Seq.empty,
        Seq.empty,
        Seq.empty,
      )

      val congestedComponents = CongestedComponents(
        results,
        gridModel.gridComponents,
        gridModel.voltageLimits,
        gridModel.mainRefSystem.nominalVoltage,
        gridModel.subnetNo,
      )

      Congestions(congestedComponents) shouldBe Congestions(
        voltageCongestions = true,
        lineCongestions = false,
        transformerCongestions = false,
      )
    }

    "find line congestions correctly" in {
      val lineResult1to2 = new LineResult(
        startTime,
        line1To2.getUuid,
        Quantities.getQuantity(1360d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        Quantities.getQuantity(1360d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val lineResult1to3 = new LineResult(
        startTime,
        line1To3.getUuid,
        Quantities.getQuantity(500d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        Quantities.getQuantity(500d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val lineResult1to4 = new LineResult(
        startTime,
        line1To4.getUuid,
        Quantities.getQuantity(801d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        Quantities.getQuantity(799d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val lineResult2to3 = new LineResult(
        startTime,
        line2To3.getUuid,
        Quantities.getQuantity(801d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        Quantities.getQuantity(799d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
      )

      val results = PowerFlowResultEvent(
        Seq.empty,
        Seq.empty,
        Seq(lineResult1to2, lineResult1to3, lineResult1to4, lineResult2to3),
        Seq.empty,
        Seq.empty,
      )

      val congestedComponents = CongestedComponents(
        results,
        gridModel.gridComponents,
        gridModel.voltageLimits,
        gridModel.mainRefSystem.nominalVoltage,
        gridModel.subnetNo,
      )

      Congestions(congestedComponents) shouldBe Congestions(
        voltageCongestions = false,
        lineCongestions = true,
        transformerCongestions = false,
      )
    }

    "find transformer2w congestions correctly" in {

      val nodeResult1 = new NodeResult(
        startTime,
        node1.getUuid,
        0.9.asPu,
        0.asDegreeGeom,
      )

      val nodeResult2 = new NodeResult(
        startTime,
        node2.getUuid,
        1.0.asPu,
        0.asDegreeGeom,
      )

      val transformerResult1 = new Transformer2WResult(
        startTime,
        transformer1.getUuid,
        Quantities.getQuantity(308d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        Quantities
          .getQuantity(1064, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        0,
      )

      val transformerResult2 = new Transformer2WResult(
        startTime,
        transformer2.getUuid,
        Quantities.getQuantity(310d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        Quantities.getQuantity(1071d, StandardUnits.ELECTRIC_CURRENT_MAGNITUDE),
        Quantities.getQuantity(0, StandardUnits.VOLTAGE_ANGLE),
        0,
      )

      val results = PowerFlowResultEvent(
        Seq(nodeResult1, nodeResult2),
        Seq.empty,
        Seq.empty,
        Seq(transformerResult1, transformerResult2),
        Seq.empty,
      )

      val congestedComponents = CongestedComponents(
        results,
        gridModel.gridComponents,
        gridModel.voltageLimits,
        gridModel.mainRefSystem.nominalVoltage,
        gridModel.subnetNo,
      )

      Congestions(congestedComponents) shouldBe Congestions(
        voltageCongestions = false,
        lineCongestions = false,
        transformerCongestions = true,
      )
    }
  }

}
