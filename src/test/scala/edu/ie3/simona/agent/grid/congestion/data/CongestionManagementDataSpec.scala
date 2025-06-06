/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion.data

import edu.ie3.datamodel.models.result.CongestionResult
import edu.ie3.datamodel.models.result.CongestionResult.InputModelType
import edu.ie3.simona.agent.grid.GridAgentData.GridAgentBaseData
import edu.ie3.simona.agent.grid.GridEnvironment
import edu.ie3.simona.agent.grid.congestion.{CongestedComponents, Congestions}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.result.CongestedComponentsTestData
import edu.ie3.util.quantities.QuantityUtils.asPercent
import edu.ie3.util.scala.quantities.Kilovoltamperes
import org.mockito.Mockito.when
import squants.electro.Amperes

class CongestionManagementDataSpec
    extends UnitSpec
    with CongestedComponentsTestData {

  private val baseData: GridAgentBaseData = {
    val data = mock[GridAgentBaseData]

    val gridEnv = mock[GridEnvironment]
    when(gridEnv.gridModel).thenReturn(gridModel)

    when(data.gridEnv).thenReturn(gridEnv)

    data
  }

  "The CongestionManagementData" should {

    val lineMap = gridModel.gridComponents.lines.map(l => l.uuid -> l).toMap
    val transformerMap =
      gridModel.gridComponents.transformers.map(t => t.uuid -> t).toMap

    "return the congestion result correctly" in {
      val stateData = CongestionManagementData(
        baseData,
        3600,
        2,
        PowerFlowResultEvent(
          Iterable.empty,
          Iterable.empty,
          Iterable.empty,
          Iterable.empty,
          Iterable.empty,
        ),
        Congestions(true, true, true),
        CongestedComponents(
          Iterable(nodeResult2, nodeResult3),
          Iterable(
            (lineMap(line2To3.getUuid), Amperes(849d))
          ),
          Iterable(
            (transformerMap(transformer1.getUuid), Kilovoltamperes(202000))
          ),
          Iterable.empty,
        ),
      )

      val results = stateData
        .getAllResults(startTime)
        .congestionResults
        .map(res => res.getInputModel -> res)
        .toMap
      results.size shouldBe 4

      val time = startTime.plusHours(1)

      val congestionNode2 = results(node2.getUuid)
      congestionNode2.getTime shouldBe time
      congestionNode2.getInputModel shouldBe node2.getUuid
      congestionNode2.getType shouldBe InputModelType.NODE
      congestionNode2.getSubgrid shouldBe 2
      congestionNode2.getValue should equalWithTolerance(111.asPercent)
      congestionNode2.getMin should equalWithTolerance(90.asPercent)
      congestionNode2.getMax should equalWithTolerance(110.asPercent)

      val congestionNode3 = results(node3.getUuid)
      congestionNode3.getTime shouldBe time
      congestionNode3.getInputModel shouldBe node3.getUuid
      congestionNode3.getType shouldBe InputModelType.NODE
      congestionNode3.getSubgrid shouldBe 2
      congestionNode3.getValue should equalWithTolerance(89.asPercent)
      congestionNode3.getMin should equalWithTolerance(90.asPercent)
      congestionNode3.getMax should equalWithTolerance(110.asPercent)

      val congestionLine23 = results(line2To3.getUuid)
      congestionLine23.getTime shouldBe time
      congestionLine23.getInputModel shouldBe line2To3.getUuid
      congestionLine23.getType shouldBe InputModelType.LINE
      congestionLine23.getSubgrid shouldBe 2
      congestionLine23.getValue should equalWithTolerance(106.125.asPercent)
      congestionLine23.getMin should equalWithTolerance(0.asPercent)
      congestionLine23.getMax should equalWithTolerance(100.asPercent)

      val congestionTransformer1 = results(transformer1.getUuid)
      congestionTransformer1.getTime shouldBe time
      congestionTransformer1.getInputModel shouldBe transformer1.getUuid
      congestionTransformer1.getType shouldBe InputModelType.TRANSFORMER_2W
      congestionTransformer1.getSubgrid shouldBe 2
      congestionTransformer1.getValue should equalWithTolerance(101.asPercent)
      congestionTransformer1.getMin should equalWithTolerance(0.asPercent)
      congestionTransformer1.getMax should equalWithTolerance(100.asPercent)
    }
  }
}
