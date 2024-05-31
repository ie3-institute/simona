/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.WecInput
import edu.ie3.datamodel.models.input.system.`type`.WecTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.{
  ReactivePowerCharacteristic,
  WecCharacteristicInput,
}
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.test.common.{DefaultTestData, UnitSpec}
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{METRE, PERCENT, SQUARE_METRE}

import java.util.UUID

class WecModelSpec extends UnitSpec with DefaultTestData {

  val TOLERANCE: Double = 1e-5

  def setupSpec(): Unit = {
    val nodeInput = new NodeInput(
      UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
      "NodeInput",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      Quantities.getQuantity(1, PowerSystemUnits.PU),
      false,
      NodeInput.DEFAULT_GEO_POSITION,
      GermanVoltageLevelUtils.LV,
      -1,
    )

    val typeInput = new WecTypeInput(
      UUID.randomUUID(),
      "WecTypeInput",
      Quantities.getQuantity(1000, PowerSystemUnits.EURO),
      Quantities.getQuantity(1000, PowerSystemUnits.EURO_PER_MEGAWATTHOUR),
      Quantities.getQuantity(1200, PowerSystemUnits.KILOWATT),
      0.95,
      // Using the characteristics of the Enercon E-82 wind turbine.
      new WecCharacteristicInput(
        "cP:{(0.0,0.0), (1.0,0.0), (2.0,0.115933516), (3.0,0.286255595), (4.0,0.39610618), " +
          "(5.0,0.430345211), (6.0,0.45944023), (7.0,0.479507331), (8.0,0.492113623), " +
          "(9.0,0.500417188), (10.0,0.488466547), (11.0,0.420415054), (12.0,0.354241299), " +
          "(13.0,0.288470591), (14.0,0.230965702), (15.0,0.18778367), (16.0,0.154728976), " +
          "(17.0,0.128998552), (18.0,0.108671106), (19.0,0.09239975), (20.0,0.079221236), " +
          "(21.0,0.068434282), (22.0,0.059520087), (23.0,0.052089249), (24.0,0.045845623), " +
          "(25.0,0.040561273), (26.0,0.036058824), (27.0,0.032198846), (28.0,0.016618264), " +
          "(29.0,0.010330976), (30.0,0.006091519), (31.0,0.003331177), (32.0,0.001641637), " +
          "(33.0,0.000705423), (34.0,0.000196644), (35.0,0.0), (36.0,0.0), (37.0,0.0), (38.0,0.0), " +
          "(39.0,0.0), (40.0,0.0), (41.0,0.0), (42.0,0.0), (43.0,0.0), (44.0,0.0), (45.0,0.0), " +
          "(46.0,0.0), (47.0,0.0), (48.0,0.0), (49.0,0.0), (50.0,0.0)}"
      ),
      Quantities.getQuantity(15, PERCENT),
      Quantities.getQuantity(5281, SQUARE_METRE),
      Quantities.getQuantity(20, METRE),
    )

    val wecInput = new WecInput(
      UUID.randomUUID(),
      "WecTypeInput",
      nodeInput,
      ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.00,0.95)}"),
      null,
      typeInput,
      false,
    )

    val wecModel = WecModel.apply(
      wecInput,
      1,
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z"),
    )
  }
}
