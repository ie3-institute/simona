/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import akka.event.{LoggingAdapter, NoLogging}
import edu.ie3.simona.model.grid.GridModel
import edu.ie3.simona.ontology.messages.VoltageMessage.ProvideSlackVoltageMessage.ExchangeVoltage
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.common.model.grid.BasicGridWithSwitches
import tech.units.indriya.quantity.Quantities
import edu.ie3.util.quantities.PowerSystemUnits._

class PowerFlowSupportSpec
    extends UnitSpec
    with BasicGridWithSwitches
    with PowerFlowSupport {

  override val log: LoggingAdapter = NoLogging

  "PowerFlowSupport" when {
    "switches are closed" must {
      "calculate power flow correctly" in {

        val gridModel = createGridCopy()

        val receivedValuesStore = ReceivedValuesStore
          .empty(Map.empty, Map.empty, Vector.empty)
          .copy(
            nodeToReceivedSlackVoltage = Map(
              node6.uuid -> Some(
                ExchangeVoltage(
                  node6.uuid,
                  Quantities.getQuantity(110, KILOVOLT),
                  Quantities.getQuantity(0, KILOVOLT)
                )
              )
            )
          )

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem
          )

        val result = newtonRaphsonPF(
          gridModel,
          3,
          operatingPoint,
          slackNodeVoltages
        )(Vector(1e-12))

        // TODO investigate result
      }
    }

    "switches are opened" must {
      "calculate power flow correctly" in {

        val gridModel = createGridCopy()
        gridModel.gridComponents.switches.foreach(_.open())
        GridModel.updateUuidToIndexMap(gridModel)

        val receivedValuesStore = ReceivedValuesStore
          .empty(Map.empty, Map.empty, Vector.empty)
          .copy(
            nodeToReceivedSlackVoltage = Map(
              node6.uuid -> Some(
                ExchangeVoltage(
                  node6.uuid,
                  Quantities.getQuantity(110, KILOVOLT),
                  Quantities.getQuantity(0, KILOVOLT)
                )
              )
            )
          )

        val (operatingPoint, slackNodeVoltages) =
          composeOperatingPoint(
            gridModel.gridComponents.nodes,
            gridModel.gridComponents.transformers,
            gridModel.gridComponents.transformers3w,
            gridModel.nodeUuidToIndexMap,
            receivedValuesStore,
            gridModel.mainRefSystem
          )

        val result = newtonRaphsonPF(
          gridModel,
          50,
          operatingPoint,
          slackNodeVoltages
        )(Vector(1e-12))

        // TODO investigate result
      }
    }
  }

}
