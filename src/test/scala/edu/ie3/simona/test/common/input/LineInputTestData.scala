/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.test.common.input

import java.util.UUID

import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.connector.LineInput
import edu.ie3.datamodel.models.input.connector.`type`.LineTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.OlmCharacteristicInput
import edu.ie3.datamodel.utils.GridAndGeoUtils
import edu.ie3.simona.test.common.DefaultTestData
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOMETRE,
  KILOVOLT,
  OHM_PER_KILOMETRE,
  SIEMENS_PER_KILOMETRE,
}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.AMPERE

/** //ToDo: Class Description
  *
  * @version 0.1
  * @since 23.06.20
  */
trait LineInputTestData extends DefaultTestData with NodeInputTestData {

  // 10 kV line input models
  // / standard 10 kV line type
  private val standard10kVLineType = new LineTypeInput(
    UUID.randomUUID(),
    "Standard 10kV line type",
    Quantities.getQuantity(0.00000000322, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.437, OHM_PER_KILOMETRE),
    Quantities.getQuantity(0.356, OHM_PER_KILOMETRE),
    Quantities.getQuantity(300d, AMPERE),
    Quantities.getQuantity(10, KILOVOLT),
  )

  // / 10 kV line models
  protected val lineInputMs10Kv = new LineInput(
    UUID.fromString("7625f4cb-e08e-4bbd-96fe-ec14b4727c1a"),
    "Single, enabled 20km line input model MS 10kV",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackMs10Kv,
    nodeInputNoSlackMs10Kv,
    1,
    standard10kVLineType,
    Quantities.getQuantity(0.75, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(
      nodeInputNoSlackMs10Kv,
      nodeInputNoSlackMs10Kv,
    ),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC,
  )

  // 20 kV line input models
  // / standard 20kV line type
  private val standard20kVLineType = new LineTypeInput(
    UUID.randomUUID(),
    "Standard 20kV line type",
    Quantities.getQuantity(191.636993408203, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE),
    Quantities.getQuantity(0.207000002264977, OHM_PER_KILOMETRE),
    Quantities.getQuantity(0.0691149979829788, OHM_PER_KILOMETRE),
    Quantities.getQuantity(300, AMPERE),
    Quantities.getQuantity(20, KILOVOLT),
  )

  // / 20 kV line models
  protected val lineInputMs20Kv = new LineInput(
    UUID.fromString("f9a3a5f9-a5e7-4566-a3a3-d4991d6b0b12"),
    "Single, enabled 20km line input model MS 20kV with NodeA = NodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackMs20Kv,
    nodeInputNoSlackMs20Kv,
    1,
    standard20kVLineType,
    Quantities.getQuantity(20, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(
      nodeInputNoSlackMs20Kv,
      nodeInputNoSlackMs20Kv,
    ),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC,
  )
  protected val lineInputWithTooHighVoltLvlA = new LineInput(
    UUID.fromString("da55ed58-e3b1-4cc3-b9e7-997082a2a624"),
    "Single, enabled 20km line input model MS 20kV with NodeA = NodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackMs110Kv,
    nodeInputNoSlackMs20Kv,
    1,
    standard20kVLineType,
    Quantities.getQuantity(20, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(
      nodeInputNoSlackNs04KvA,
      nodeInputNoSlackMs20Kv,
    ),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC,
  )
  protected val lineInputWithTooLowVoltLvlA = new LineInput(
    UUID.fromString("8c712c17-2f6f-4ae3-beb9-eb66563ffd32"),
    "Single, enabled 20km line input model MS 20kV with NodeA = NodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackNs04KvA,
    nodeInputNoSlackMs20Kv,
    1,
    standard20kVLineType,
    Quantities.getQuantity(20, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(
      nodeInputNoSlackNs04KvA,
      nodeInputNoSlackMs20Kv,
    ),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC,
  )
  protected val lineInputWithTooHighVoltLvlB = new LineInput(
    UUID.fromString("cc9d9547-42ab-4613-93c0-17e6ac11cd9c"),
    "Single, enabled 20km line input model MS 20kV with NodeA = NodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackMs20Kv,
    nodeInputNoSlackMs110Kv,
    1,
    standard20kVLineType,
    Quantities.getQuantity(20, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(
      nodeInputNoSlackMs20Kv,
      nodeInputNoSlackNs04KvA,
    ),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC,
  )
  protected val lineInputWithTooLowVoltLvlB = new LineInput(
    UUID.fromString("cb910fee-b7cb-4b14-8609-f85e83c6973b"),
    "Single, enabled 20km line input model MS 20kV with NodeA = NodeB",
    OperatorInput.NO_OPERATOR_ASSIGNED,
    defaultOperationTime,
    nodeInputNoSlackMs20Kv,
    nodeInputNoSlackNs04KvB,
    1,
    standard20kVLineType,
    Quantities.getQuantity(20, KILOMETRE),
    GridAndGeoUtils.buildSafeLineStringBetweenNodes(
      nodeInputNoSlackMs20Kv,
      nodeInputNoSlackNs04KvA,
    ),
    OlmCharacteristicInput.CONSTANT_CHARACTERISTIC,
  )
}
