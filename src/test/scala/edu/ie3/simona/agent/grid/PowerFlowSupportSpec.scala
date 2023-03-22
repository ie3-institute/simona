/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.actor.ActorRef
import akka.event.{LoggingAdapter, NoLogging}
import edu.ie3.powerflow.model.PowerFlowResult.SuccessFullPowerFlowResult.ValidNewtonRaphsonPFResult
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.BasicGridWithSwitches
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble

import java.time.ZonedDateTime

/** Please refer to the diagram of the grid in [[BasicGridWithSwitches]] in
  * order to comprehend the expected test results
  */
class PowerFlowSupportSpec
    extends UnitSpec
    with BasicGridWithSwitches
    with PowerFlowSupport
    with GridResultsSupport {

  override val log: LoggingAdapter = NoLogging

  /** Setting voltage at slack node to 110 kV and introducing a load of 1 MW at
    * node 1
    */
  private val receivedValuesStore = ReceivedValuesStore
    .empty(Map.empty, Map.empty, Vector.empty)
    .copy(
      nodeToReceivedSlackVoltage = Map(
        node6.uuid -> Some(
          ExchangeVoltage(
            node6.uuid,
            110d.asKiloVolt,
            0d.asKiloVolt
          )
        )
      ),
      nodeToReceivedPower = Map(
        node1.uuid -> Map(
          ActorRef.noSender -> Some(
            ExchangePower(
              node1.uuid,
              1d.asMegaWatt,
              0d.asMegaVar
            )
          )
        )
      )
    )

  "PowerFlowSupport" when {
    "all switches are closed" must {
      "calculate power flow correctly" in {

        // switches are closed per default
        val gridModel = createGridCopy()

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem
          )

        operatingPoint.length shouldBe 10 withClue "safety check: 13 nodes minus 3 closed switches"

        val result = newtonRaphsonPF(
          gridModel,
          3,
          operatingPoint,
          slackNodeVoltages
        )(Vector(1e-12)) match {
          case successResult: ValidNewtonRaphsonPFResult => successResult
          case failure => fail(s"Newton-Raphson failed: $failure")
        }

        val sweepValueStore = SweepValueStore(
          result,
          gridModel.gridComponents.nodes,
          gridModel.nodeUuidToIndexMap
        )

        val pfResult =
          createResultModels(gridModel, sweepValueStore)(ZonedDateTime.now())

        // all lines should have some load
        val expectedLoadLines =
          Iterable(
            line18To1,
            line0To17,
            line0To15,
            line16To3,
            line1To13,
            line14To2,
            line2To3
          ).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => expectedLoadLines.contains(lineRes.getInputModel))
          .forall(_.getiAMag().isGreaterThan(25d.asAmpere)) shouldBe true
      }
    }

    "switch s3 on the left/top side is open" must {
      "calculate a load flow on the right side" in {

        val gridModel = createGridCopy()
        gridModel.gridComponents.switches
          .find(_.uuid == switch3.uuid)
          .value
          .open()
        GridModel.updateUuidToIndexMap(gridModel)

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem
          )

        operatingPoint.length shouldBe 11 withClue "safety check: 13 nodes minus 2 closed switches"

        val result = newtonRaphsonPF(
          gridModel,
          50,
          operatingPoint,
          slackNodeVoltages
        )(Vector(1e-12)) match {
          case successResult: ValidNewtonRaphsonPFResult => successResult
          case failure => fail(s"Newton-Raphson failed: $failure")
        }

        val pfResult = createResultModels(
          gridModel,
          SweepValueStore(
            result,
            gridModel.gridComponents.nodes,
            gridModel.nodeUuidToIndexMap
          )
        )(ZonedDateTime.now())

        // lines that are adjacent to the open switch should have no load
        val expectedNoLoadLines =
          Iterable(line18To1, line0To17, line0To15, line16To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes =>
            expectedNoLoadLines.contains(lineRes.getInputModel)
          )
          .forall(_.getiAMag().isLessThan(1e-3.asAmpere)) shouldBe true

        // lines that are adjacent to the closed switch should have load
        val expectedLoadLines =
          Iterable(line1To13, line14To2, line2To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => expectedLoadLines.contains(lineRes.getInputModel))
          .forall(_.getiAMag().isGreaterThan(50d.asAmpere)) shouldBe true
      }
    }

    "switch s1 on the bottom/right side is open" must {
      "calculate a power flow on the left side" in {

        val gridModel = createGridCopy()
        gridModel.gridComponents.switches
          .find(_.uuid == switch1.uuid)
          .value
          .open()
        GridModel.updateUuidToIndexMap(gridModel)

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem
          )

        operatingPoint.length shouldBe 11 withClue "safety check: 13 nodes minus 2 closed switches"

        val result = newtonRaphsonPF(
          gridModel,
          50,
          operatingPoint,
          slackNodeVoltages
        )(Vector(1e-12)) match {
          case successResult: ValidNewtonRaphsonPFResult => successResult
          case failure => fail(s"Newton-Raphson failed: $failure")
        }

        val pfResult = createResultModels(
          gridModel,
          SweepValueStore(
            result,
            gridModel.gridComponents.nodes,
            gridModel.nodeUuidToIndexMap
          )
        )(ZonedDateTime.now())

        // lines that are adjacent to the open switch should have no load
        val expectedNoLoadLines =
          Iterable(line1To13, line14To2, line2To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes =>
            expectedNoLoadLines.contains(lineRes.getInputModel)
          )
          .forall(_.getiAMag().isLessThan(1e-3.asAmpere)) shouldBe true

        // lines that are adjacent to the closed switch should have load
        val expectedLoadLines =
          Iterable(line18To1, line0To17, line0To15, line16To3).map(_.uuid).toSet
        pfResult.lineResults
          .filter(lineRes => expectedLoadLines.contains(lineRes.getInputModel))
          .forall(_.getiAMag().isGreaterThan(50d.asAmpere)) shouldBe true
      }
    }
  }

}
