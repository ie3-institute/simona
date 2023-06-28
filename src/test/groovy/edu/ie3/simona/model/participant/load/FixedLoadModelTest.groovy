/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import static edu.ie3.simona.model.participant.load.LoadReference.ActivePower
import static edu.ie3.simona.model.participant.load.LoadReference.EnergyConsumption
import static edu.ie3.util.quantities.PowerSystemUnits.*
import static org.apache.commons.math3.util.FastMath.abs
import static tech.units.indriya.unit.Units.WATT

import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.TimeUtil
import spock.lang.Specification
import tech.units.indriya.quantity.Quantities



class FixedLoadModelTest extends Specification {
  def loadInput =
  new LoadInput(
  UUID.fromString("4eeaf76a-ec17-4fc3-872d-34b7d6004b03"),
  "testLoad",
  OperatorInput.NO_OPERATOR_ASSIGNED,
  OperationTime.notLimited(),
  new NodeInput(
  UUID.fromString("e5c1cde5-c161-4a4f-997f-fcf31fecbf57"),
  "TestNodeInputModel",
  OperatorInput.NO_OPERATOR_ASSIGNED,
  OperationTime.notLimited(),
  Quantities.getQuantity(1d, PU),
  false,
  NodeInput.DEFAULT_GEO_POSITION,
  GermanVoltageLevelUtils.LV,
  -1
  ),
  new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
  BdewStandardLoadProfile.H0,
  false,
  Quantities.getQuantity(3000d, KILOWATTHOUR),
  Quantities.getQuantity(282.74d, VOLTAMPERE),
  0.95
  )

  def simulationStartDate = TimeUtil.withDefaults.toZonedDateTime("2020-01-01 00:00:00")
  def simulationEndDate = TimeUtil.withDefaults.toZonedDateTime("2020-12-31 23:59:00")
  def foreSeenOperationInterval =
  SystemComponent.determineOperationInterval(
  simulationStartDate,
  simulationEndDate,
  loadInput.operationTime
  )
  def testingTolerance = 1e-6 // Equals to 1 W power

  def "A fixed load model should be instantiated from valid input correctly"() {
    when:
    def actual = new FixedLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        reference
        )

    then:
    abs(actual.activePower().subtract(expectedReferenceActivePower).to(MEGAWATT).value.doubleValue()) < testingTolerance

    where:
    reference                                                          | expectedReferenceActivePower
    new ActivePower(Quantities.getQuantity(268.6, WATT))               | Quantities.getQuantity(268.6, WATT)
    new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR)) | Quantities.getQuantity(342.24, WATT)
  }

  def "A fixed load model should return approximately the same power in 10.000 calculations"() {
    when:
    def dut = new FixedLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        reference
        )

    then:
    for (cnt in 0..10000) {
      abs((dut.calculateActivePower(FixedLoadModel.FixedLoadRelevantData$.MODULE$)).subtract(expectedPower).to(MEGAWATT).value.doubleValue()) < testingTolerance
    }

    where:
    reference                                                          | expectedPower
    new ActivePower(Quantities.getQuantity(268.6, WATT))               | Quantities.getQuantity(268.6, WATT)
    new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR)) | Quantities.getQuantity(342.24, WATT)
  }

  def "A fixed load model considers the (global) scaling factor correctly"() {
    when:
    def relevantData = FixedLoadModel.FixedLoadRelevantData$.MODULE$

    then:
    for (double scale = 0.0; scale <= 2.0; scale += 0.1) {
      def dut = new FixedLoadModel(
          loadInput.uuid,
          loadInput.id,
          foreSeenOperationInterval,
          scale,
          QControl.apply(loadInput.qCharacteristics),
          loadInput.sRated,
          loadInput.cosPhiRated,
          reference
          )

      abs((dut.calculateActivePower(relevantData)).subtract(expectedPower * scale).to(MEGAWATT).value.doubleValue()) < testingTolerance
    }

    where:
    reference                                                          | expectedPower
    new ActivePower(Quantities.getQuantity(268.6, WATT))               | Quantities.getQuantity(268.6, WATT)
    new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR)) | Quantities.getQuantity(342.24, WATT)
  }
}
