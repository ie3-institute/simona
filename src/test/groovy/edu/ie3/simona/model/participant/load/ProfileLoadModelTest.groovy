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
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.util.TimeUtil
import spock.lang.Specification
import squants.energy.KilowattHours$
import squants.energy.Kilowatts$
import squants.energy.Watts$
import edu.ie3.util.scala.quantities.Sq
import squants.time.Minutes$
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
  def wattTolerance = 1 // Equals to 1 W power

  def "A profile load model should be instantiated from valid input correctly"() {
    when:
    def actual = ProfileLoadModel.apply(
        loadInput.copy().loadprofile(profile).build(),
        foreSeenOperationInterval,
        1.0,
        reference)

    then:
    abs((actual.sRated().toWatts() * actual.cosPhiRated()).toDouble() - expectedsRated.doubleValue()) < wattTolerance

    where:
    profile | reference                                                           || expectedsRated
    H0      | new ActivePower(Sq.create(268.6d, Watts$.MODULE$))                  || 268.6d
    H0      | new EnergyConsumption(Sq.create(3000d, KilowattHours$.MODULE$))     || 805.8089d
    L0      | new ActivePower(Sq.create(268.6d, Watts$.MODULE$))                  || 268.6d
    L0      | new EnergyConsumption(Sq.create(3000d, KilowattHours$.MODULE$))     || 721.2d
    G0      | new ActivePower(Sq.create(268.6d, Watts$.MODULE$))                  || 268.6d
    G0      | new EnergyConsumption(Sq.create(3000d, KilowattHours$.MODULE$))     || 721.2d
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
        Sq.create(loadInput.getsRated().to(KILOWATT).getValue().doubleValue(), Kilowatts$.MODULE$),
        loadInput.cosPhiRated,
        profile,
        new ActivePower(Sq.create(268.6d, Watts$.MODULE$))
        )
    def relevantData = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def max = relevantData.stream().mapToDouble({ data ->
      dut.calculateActivePower(data).toMegawatts().doubleValue()
    }).max().getAsDouble()

    then:
    abs(max - expectedMax) < wattTolerance

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
        Sq.create(loadInput.getsRated().to(KILOWATT).getValue().doubleValue(), Kilowatts$.MODULE$),
        loadInput.cosPhiRated,
        H0,
        new ActivePower(Sq.create(268.6d, Watts$.MODULE$))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def max = relevantDatas.stream().mapToDouble({ relevantData ->
      dut.calculateActivePower(relevantData).toMegawatts().doubleValue()
    }).max().getAsDouble()

    then:
    abs(max - expectedMax) < wattTolerance

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
        Sq.create(loadInput.getsRated().to(KILOWATT).getValue().doubleValue(), Kilowatts$.MODULE$),
        loadInput.cosPhiRated,
        profile,
        new EnergyConsumption(Sq.create(3000d, KilowattHours$.MODULE$))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def annualEnergy = relevantDatas.stream().mapToDouble({ relevantData ->
      ((dut.calculateActivePower(relevantData).$times(Sq.create(15d, Minutes$.MODULE$)).toKilowattHours()))
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
        Sq.create(loadInput.getsRated().to(KILOWATT).getValue().doubleValue(), Kilowatts$.MODULE$),
        loadInput.cosPhiRated,
        H0,
        new EnergyConsumption(Sq.create(3000d, KilowattHours$.MODULE$))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new ProfileLoadModel.ProfileRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def annualEnergy = relevantDatas.stream().mapToDouble({ relevantData ->
      ((dut.calculateActivePower(relevantData).$times(Sq.create(15d, Minutes$.MODULE$)).toKilowattHours()))
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
