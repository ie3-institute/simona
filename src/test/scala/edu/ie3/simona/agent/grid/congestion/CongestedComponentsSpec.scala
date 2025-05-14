/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.simona.agent.grid.congestion.CongestedComponents
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.result.CongestedComponentsTestData
import edu.ie3.util.TimeUtil
import edu.ie3.util.scala.quantities.{ApparentPower, Kilovoltamperes}
import squants.electro.{Amperes, Kilovolts}

import java.time.ZonedDateTime

class CongestedComponentsSpec
    extends UnitSpec
    with CongestedComponentsTestData {

  private implicit val tolerance: ApparentPower = Kilovoltamperes(1)

  "A CongestedComponents" should {

    "find voltage congestions correctly" in {
      val results = PowerFlowResultEvent(
        Iterable(
          nodeResultA,
          nodeResultB,
          nodeResult1,
          nodeResult2,
          nodeResult3,
          nodeResult4,
        ),
        Iterable.empty,
        Iterable.empty,
        Iterable.empty,
        Iterable.empty,
      )

      val congested = CongestedComponents(
        results,
        gridModel.gridComponents,
        voltageLimits,
        Kilovolts(110),
        1,
      )

      congested.voltages shouldBe Iterable(nodeResult3, nodeResult2)
      congested.lines shouldBe Iterable.empty
      congested.transformer2Ws shouldBe Iterable.empty
      congested.transformer3Ws shouldBe Iterable.empty
    }

    "find line congestions correctly" in {
      val results = PowerFlowResultEvent(
        Iterable.empty,
        Iterable.empty,
        Iterable(lineResult12, lineResult23),
        Iterable.empty,
        Iterable.empty,
      )

      val congested = CongestedComponents(
        results,
        gridModel.gridComponents,
        voltageLimits,
        Kilovolts(110),
        1,
      )

      val lineMap = gridModel.gridComponents.lines.map(l => l.uuid -> l).toMap

      congested.voltages shouldBe Iterable.empty
      congested.lines shouldBe Iterable(
        (lineMap(line2To3.getUuid), Amperes(849d))
      )
      congested.transformer2Ws shouldBe Iterable.empty
      congested.transformer3Ws shouldBe Iterable.empty
    }

    "find Transformer2W congestions correctly" in {
      val results = PowerFlowResultEvent(
        Iterable(nodeResult1, nodeResult2),
        Iterable.empty,
        Iterable.empty,
        Iterable(transformerResult1, transformerResult2),
        Iterable.empty,
      )

      val congested = CongestedComponents(
        results,
        gridModel.gridComponents,
        voltageLimits,
        Kilovolts(110),
        1,
      )

      val transformerMap =
        gridModel.gridComponents.transformers.map(t => t.uuid -> t).toMap

      congested.voltages shouldBe Iterable(nodeResult2)
      congested.lines shouldBe Iterable.empty

      congested.transformer2Ws.size shouldBe 1
      val (model, power) = congested.transformer2Ws.toSeq(0)
      model shouldBe transformerMap(transformer1.getUuid)
      power should approximate(Kilovoltamperes(202000))

      congested.transformer3Ws shouldBe Iterable.empty
    }

    "find Transformer3W congestions correctly" in {
      val results = PowerFlowResultEvent(
        Iterable(nodeResult2),
        Iterable.empty,
        Iterable.empty,
        Iterable.empty,
        Iterable(transformerResult3W),
      )

      val congested = CongestedComponents(
        results,
        gridModel.gridComponents,
        voltageLimits,
        Kilovolts(110),
        1,
      )

      val transformerMap =
        gridModel.gridComponents.transformers3w.map(t => t.uuid -> t).toMap

      congested.voltages shouldBe Iterable(nodeResult2)
      congested.lines shouldBe Iterable.empty
      congested.transformer2Ws shouldBe Iterable.empty

      congested.transformer3Ws.size shouldBe 1
      val (model, power) = congested.transformer3Ws.toSeq(0)
      model shouldBe transformerMap(transformer3W.getUuid)
      power should approximate(Kilovoltamperes(301000))
    }
  }
}
