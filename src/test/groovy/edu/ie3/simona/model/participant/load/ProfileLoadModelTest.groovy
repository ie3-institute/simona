/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.ModelState
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.util.TimeUtil
import scala.Option
import spock.lang.Specification
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.Energy
import java.time.temporal.ChronoUnit
import java.util.stream.Collectors

import static edu.ie3.datamodel.models.profile.BdewStandardLoadProfile.*
import static edu.ie3.simona.model.participant.load.LoadReference.ActivePower
import static edu.ie3.simona.model.participant.load.LoadReference.EnergyConsumption
import static edu.ie3.util.quantities.PowerSystemUnits.*
import static org.apache.commons.math3.util.FastMath.abs
import static tech.units.indriya.unit.Units.MINUTE
import static tech.units.indriya.unit.Units.WATT

class ProfileLoadModelTest extends Specification {
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
  H0,
  false,
  Quantities.getQuantity(3000d, KILOWATTHOUR),
  Quantities.getQuantity(282.74d, VOLTAMPERE),
  0.95
  )

  def simulationStartDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
  def simulationEndDate = TimeUtil.withDefaults.toZonedDateTime("2019-12-31 23:59:00")
  def foreSeenOperationInterval =
  SystemComponent.determineOperationInterval(
  simulationStartDate,
  simulationEndDate,
  loadInput.operationTime
  )
  def testingTolerance = 1e-6 // Equals to 1 W power

  def "A profile load model should be instantiated from valid input correctly"() {
    when:
    def actual = ProfileLoadModel.apply(
        loadInput.copy().loadprofile(profile).build(),
        foreSeenOperationInterval,
        1.0,
        reference)

    then:
    abs((actual.sRated() * actual.cosPhiRated()).subtract(expectedsRated).to(MEGAWATT).value.doubleValue()) < testingTolerance

    where:
    profile | reference                                                          || expectedsRated
    H0      | new ActivePower(Quantities.getQuantity(268.6, WATT))               || Quantities.getQuantity(268.6, WATT)
    H0      | new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR)) || Quantities.getQuantity(805.8089, WATT)
    L0      | new ActivePower(Quantities.getQuantity(268.6, WATT))               || Quantities.getQuantity(268.6, WATT)
    L0      | new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR)) || Quantities.getQuantity(721.2, WATT)
    G0      | new ActivePower(Quantities.getQuantity(268.6, WATT))               || Quantities.getQuantity(268.6, WATT)
    G0      | new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR)) || Quantities.getQuantity(721.2, WATT)
  }

  def "A profile load model should reach the targeted maximum power within a year"() {
    given:
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    def dut = new ProfileLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        profile,
        new ActivePower(Quantities.getQuantity(268.6, WATT))
        )
    def relevantData = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def max = relevantData.stream().mapToDouble({ data ->
      dut.calculateActivePower(ModelState.ConstantState$.MODULE$, data).to(MEGAWATT).value.doubleValue()
    }).max().getAsDouble()

    then:
    abs(max - expectedMax) < testingTolerance

    where:
    profile || expectedMax
    H0      || 268.0029932985852E-6
    L0      || 268.0029932985852E-6
    G0      || 268.0029932985852E-6
  }

  def "A profile load model should account for the (global) scaling factor correctly when scaling to maximum power within a year"() {
    given:
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    def dut = new ProfileLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        globalScaling,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        H0,
        new ActivePower(Quantities.getQuantity(268.6, WATT))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def max = relevantDatas.stream().mapToDouble({ relevantData ->
      dut.calculateActivePower(ModelState.ConstantState$.MODULE$, relevantData).to(MEGAWATT).value.doubleValue()
    }).max().getAsDouble()

    then:
    abs(max - expectedMax) < testingTolerance

    where:
    globalScaling || expectedMax
    0.25          || 67.00074832464630E-6
    0.5           || 134.0014966492930E-6
    0.75          || 201.0022449739390E-6
    1.0           || 268.0029932985852E-6
    1.25          || 335.0037416232310E-6
    1.5           || 402.0044899478780E-6
    1.75          || 469.0052382725240E-6
    2.0           || 536.0059865971700E-6
  }

  def "A profile load model should reach the targeted annual energy consumption"() {
    given:
    /* Test against a permissible deviation of 2 %. As per official documentation of the bdew load profiles
     * [https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf] 1.5 %
     * are officially permissible. But, as we currently do not take (bank) holidays into account, we cannot reach
     * this accuracy. */
    def testingTolerance = 0.02
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    def dut = new ProfileLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        profile,
        new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def annualEnergy = relevantDatas.stream().mapToDouble({ relevantData ->
      ((dut.calculateActivePower(ModelState.ConstantState$.MODULE$, relevantData) * Quantities.getQuantity(15d, MINUTE)) as ComparableQuantity<Energy>).to(KILOWATTHOUR).value.doubleValue()
    }).sum()

    then:
    abs(annualEnergy - expectedEnergy) / expectedEnergy < testingTolerance

    where:
    profile || expectedEnergy
    H0      || 3000d
    L0      || 3000d
    G0      || 3000d
  }

  def "A profile load model should account for the (global) scaling factor correctly when scaling to annual energy consumption"() {
    given:
    /* Test against a permissible deviation of 2 %. As per official documentation of the bdew load profiles
     * [https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf] 1.5 %
     * are officially permissible. But, as we currently do not take (bank) holidays into account, we cannot reach
     * this accuracy. */
    def testingTolerance = 0.02
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    def dut = new ProfileLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        globalScaling,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        H0,
        new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def annualEnergy = relevantDatas.stream().mapToDouble({ relevantData ->
      ((dut.calculateActivePower(ModelState.ConstantState$.MODULE$, relevantData) * Quantities.getQuantity(15d, MINUTE)) as ComparableQuantity<Energy>).to(KILOWATTHOUR).value.doubleValue()
    }).sum()

    then:
    abs(annualEnergy - expectedEnergy) / expectedEnergy < testingTolerance

    where:
    globalScaling || expectedEnergy
    0.25          || 750d
    0.5           || 1500d
    0.75          || 2250d
    1.0           || 3000d
    1.25          || 3750d
    1.5           || 4500d
    1.75          || 5250d
    2.0           || 6000d
  }
}
