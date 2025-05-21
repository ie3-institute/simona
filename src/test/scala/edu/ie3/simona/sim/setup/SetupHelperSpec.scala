/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim.setup

import java.util.UUID
import org.apache.pekko.actor.typed.ActorRef
import org.apache.pekko.testkit.TestException
import edu.ie3.datamodel.models.input.MeasurementUnitInput
import edu.ie3.datamodel.models.input.connector.{
  Transformer2WInput,
  Transformer3WInput,
}
import edu.ie3.datamodel.models.input.container.{
  JointGridContainer,
  RawGridElements,
}
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.input.GridInputTestData
import org.apache.pekko.actor.testkit.typed.scaladsl.{
  ScalaTestWithActorTestKit,
  TestProbe,
}

import scala.jdk.CollectionConverters.*

class SetupHelperSpec
    extends ScalaTestWithActorTestKit
    with UnitSpec
    with GridInputTestData {

  private final object SetupHelperInstance extends SetupHelper

  "A setup helper" should {
    val actorRef: ActorRef[GridAgent.Request] =
      TestProbe[GridAgent.Request]("mock_grid_agent").ref

    "reduce multiple SubGridGates between the same superior and inferior nodes to one unique SubGridGate" in {

      // build dummy grid with two transformers between the same nodes based on the basic grid input test data
      val secondTransformer = new Transformer2WInput(
        UUID.fromString("4ea3f965-53b8-4bb6-a6a9-4eaf55131c5d"),
        adaptedTransformerInputModel.getId + "_second",
        adaptedTransformerInputModel.getOperator,
        adaptedTransformerInputModel.getOperationTime,
        adaptedTransformerInputModel.getNodeA,
        adaptedTransformerInputModel.getNodeB,
        adaptedTransformerInputModel.getParallelDevices,
        adaptedTransformerInputModel.getType,
        adaptedTransformerInputModel.getTapPos,
        adaptedTransformerInputModel.isAutoTap,
      )
      val adaptedTransformers = transformers + secondTransformer

      val rawGridElements = new RawGridElements(
        validTestGridInputModel.getRawGrid.getNodes,
        lines.asJava,
        adaptedTransformers.asJava,
        Set.empty[Transformer3WInput].asJava,
        switches.asJava,
        Set.empty[MeasurementUnitInput].asJava,
      )

      val gridModel = new JointGridContainer(
        "TestGrid",
        rawGridElements,
        validTestGridInputModel.getSystemParticipants,
        validTestGridInputModel.getGraphics,
      )

      val subGrids = gridModel.getSubGridTopologyGraph
        .vertexSet()
        .asScala
        .groupBy(_.getSubnet)
        .flatMap(_._2.headOption)
        .map(grid => grid.getSubnet -> grid)
        .toMap

      val superiorGrid = subGrids
        .getOrElse(
          1,
          throw TestException("Cannot get subGrid with id 1 from test data!"),
        )

      val inferiorGrid = subGrids
        .getOrElse(
          100,
          throw TestException("Cannot get subGrid with id 100 from test data!"),
        )

      val subGridToActorRefMap =
        Map(1 -> actorRef, 100 -> actorRef)

      // subGrid gates should be the same for this case
      gridModel.getSubGridTopologyGraph.edgesOf(
        superiorGrid
      ) == gridModel.getSubGridTopologyGraph
        .edgesOf(inferiorGrid) shouldBe true
      val subGridGates = Set.from(
        gridModel.getSubGridTopologyGraph.edgesOf(superiorGrid).asScala
      )

      // gates size shouldBe 2
      subGridGates.size shouldBe 2

      // gate to actor ref should reduce subGrid gates according to unique gates with superiorGridNode, inferiorGridNode uniqueness
      SetupHelperInstance
        .buildGateToActorRef(
          subGridToActorRefMap,
          subGridGates,
          superiorGrid.getSubnet,
        )
        .size shouldBe 1
      SetupHelperInstance
        .buildGateToActorRef(
          subGridToActorRefMap,
          subGridGates,
          inferiorGrid.getSubnet,
        )
        .size shouldBe 1

    }
  }

}
