/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import static edu.ie3.datamodel.models.profile.BdewStandardLoadProfile.H0
import static edu.ie3.simona.model.participant.load.LoadReference.EnergyConsumption
import static edu.ie3.util.quantities.PowerSystemUnits.*
import static org.apache.commons.math3.util.FastMath.abs

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.OperatorInput
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.util.TimeUtil
import squants.energy.*
import spock.lang.Specification

import squants.time.Minutes$
import tech.units.indriya.quantity.Quantities
import edu.ie3.util.scala.quantities.Sq

import java.time.temporal.ChronoUnit
import java.util.stream.Collectors

class RandomLoadModelTest extends Specification {
  def loadInput = new LoadInput(
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

  def "A random load model should approx. reach the targeted annual energy consumption"() {
    given:
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    def dut = new RandomLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        Sq.create(loadInput.sRated.to(KILOWATT).value.doubleValue(), Kilowatts$.MODULE$),
        loadInput.cosPhiRated,
        new EnergyConsumption(Sq.create(3000d, KilowattHours$.MODULE$))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new RandomLoadModel.RandomRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def avgEnergy = (0..10).parallelStream().mapToDouble( { runCnt ->
      relevantDatas.parallelStream().mapToDouble( { relevantData ->
        (dut.calculateActivePower(relevantData).$times(Sq.create(15d, Minutes$.MODULE$))).toKilowattHours()
      }).sum()
    }).average().orElse(0d)

    then:
    abs(avgEnergy - 3000) / 3000 < 0.01
  }
}
