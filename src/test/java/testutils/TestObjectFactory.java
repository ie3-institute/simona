/*
 * © 2019. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package testutils;

import static edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils.*;
import static edu.ie3.util.quantities.PowerSystemUnits.*;
import static tech.units.indriya.unit.Units.METRE;

import edu.ie3.datamodel.models.OperationTime;
import edu.ie3.datamodel.models.input.NodeInput;
import edu.ie3.datamodel.models.input.OperatorInput;
import edu.ie3.datamodel.models.input.connector.LineInput;
import edu.ie3.datamodel.models.input.connector.SwitchInput;
import edu.ie3.datamodel.models.input.connector.type.LineTypeInput;
import edu.ie3.datamodel.models.input.system.characteristic.OlmCharacteristicInput;
import edu.ie3.datamodel.models.voltagelevels.CommonVoltageLevel;
import edu.ie3.datamodel.models.voltagelevels.VoltageLevel;
import edu.ie3.datamodel.utils.GridAndGeoUtils;
import edu.ie3.util.quantities.interfaces.SpecificConductance;
import edu.ie3.util.quantities.interfaces.SpecificResistance;
import java.util.UUID;
import javax.measure.quantity.ElectricCurrent;
import javax.measure.quantity.ElectricPotential;
import tech.units.indriya.ComparableQuantity;
import tech.units.indriya.quantity.Quantities;

/** This class can be used to create various test objects */
public class TestObjectFactory {

  public static int TEST_OBJECT_COUNTER = 1;

  public static NodeInput buildNodeInput(
      boolean isSlack, CommonVoltageLevel voltageLvl, int subnet) {
    return new NodeInput(
        UUID.randomUUID(),
        "TEST_NODE_" + TEST_OBJECT_COUNTER++,
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        Quantities.getQuantity(1d, PU),
        isSlack,
        NodeInput.DEFAULT_GEO_POSITION,
        voltageLvl,
        subnet);
  }

  public static LineInput buildLineInput(NodeInput nodeA, NodeInput nodeB) {
    return new LineInput(
        UUID.randomUUID(),
        "TEST_LINE_" + TEST_OBJECT_COUNTER++,
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        nodeA,
        nodeB,
        1,
        buildLineTypeInput(nodeA.getVoltLvl()),
        Quantities.getQuantity(1000, METRE),
        GridAndGeoUtils.buildSafeLineStringBetweenNodes(nodeA, nodeB),
        OlmCharacteristicInput.CONSTANT_CHARACTERISTIC);
  }

  public static LineTypeInput buildLineTypeInput(VoltageLevel voltageLvl) {
    ComparableQuantity<SpecificResistance> r, x;
    ComparableQuantity<SpecificConductance> g, b;
    ComparableQuantity<ElectricCurrent> iMax;
    ComparableQuantity<ElectricPotential> vRated = voltageLvl.getNominalVoltage();

    if (voltageLvl == LV) {
      r = Quantities.getQuantity(0.4429999888, OHM_PER_KILOMETRE);
      x = Quantities.getQuantity(0.0722566023, OHM_PER_KILOMETRE);
      g = Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE);
      b = Quantities.getQuantity(163.3630065918, MICRO_SIEMENS_PER_KILOMETRE);
      iMax = Quantities.getQuantity(195, AMPERE);
    } else if (voltageLvl == MV_10KV || voltageLvl == MV_20KV) {
      r = Quantities.getQuantity(0.2070000023, OHM_PER_KILOMETRE);
      x = Quantities.getQuantity(0.069114998, OHM_PER_KILOMETRE);
      g = Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE);
      b = Quantities.getQuantity(191.6369934082, MICRO_SIEMENS_PER_KILOMETRE);
      iMax = Quantities.getQuantity(300, AMPERE);
    } else if (voltageLvl == MV_30KV) {
      r = Quantities.getQuantity(0.13, OHM_PER_KILOMETRE);
      x = Quantities.getQuantity(0.12, OHM_PER_KILOMETRE);
      g = Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE);
      b = Quantities.getQuantity(0, MICRO_SIEMENS_PER_KILOMETRE);
      iMax = Quantities.getQuantity(423, AMPERE);
    } else if (voltageLvl == HV) {
      r = Quantities.getQuantity(0.109999999403954, OHM_PER_KILOMETRE);
      x = Quantities.getQuantity(0.379999995231628, OHM_PER_KILOMETRE);
      g = Quantities.getQuantity(0, SIEMENS_PER_KILOMETRE);
      b = Quantities.getQuantity(3, MICRO_SIEMENS_PER_KILOMETRE);
      iMax = Quantities.getQuantity(550, AMPERE);
    } else {
      throw new IllegalArgumentException("Unknown voltage level " + voltageLvl);
    }

    return new LineTypeInput(
        UUID.randomUUID(), "TEST_LINE_TYPE_" + TEST_OBJECT_COUNTER++, b, g, r, x, iMax, vRated);
  }

  public static SwitchInput buildSwitchInput(NodeInput nodeA, NodeInput nodeB) {
    return new SwitchInput(
        UUID.randomUUID(),
        "TEST_SWITCH" + TEST_OBJECT_COUNTER++,
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        nodeA,
        nodeB,
        false);
  }
}
