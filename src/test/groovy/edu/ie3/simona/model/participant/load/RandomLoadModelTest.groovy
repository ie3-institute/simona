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
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadParameters
import edu.ie3.util.TimeUtil
import spock.lang.Specification
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.Energy
import java.time.temporal.ChronoUnit
import java.util.stream.Collectors

import static edu.ie3.datamodel.models.profile.BdewStandardLoadProfile.H0
import static edu.ie3.simona.model.participant.load.LoadReference.ActivePower
import static edu.ie3.simona.model.participant.load.LoadReference.EnergyConsumption
import static edu.ie3.util.quantities.PowerSystemUnits.*
import static org.apache.commons.math3.util.FastMath.abs
import static tech.units.indriya.unit.Units.MINUTE
import static tech.units.indriya.unit.Units.WATT

class RandomLoadModelTest extends Specification {
  def loadInput =  new LoadInput(
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

  def "A random load model should be instantiated from valid input correctly"() {
    when:
    def actual = RandomLoadModel.apply(
        loadInput,
        foreSeenOperationInterval,
        1.0,
        reference
        )

    then:
    abs(actual.sRated().subtract(expSRated).getValue().doubleValue()) < testingTolerance

    where:
    reference                                                          | expSRated
    new ActivePower(Quantities.getQuantity(268.6, WATT))               | Quantities.getQuantity(311.0105263157895, VOLTAMPERE)
    new EnergyConsumption(Quantities.getQuantity(2000d, KILOWATTHOUR)) | Quantities.getQuantity(467.156124576697, VOLTAMPERE)
  }

  def "A random load model is able to deliver the correct distribution on request"() {
    given:
    def dut = new RandomLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        new ActivePower(Quantities.getQuantity(268.6, WATT))
        )
    /* Working day, 61th quarter hour */
    def queryDate = TimeUtil.withDefaults.toZonedDateTime('2019-07-19 15:21:00')
    def expectedParams = new RandomLoadParameters(0.405802458524704, 0.0671483352780342, 0.0417016632854939)

    when:
    /* First query leeds to generation of distribution */
    def firstHit = dut.getGevDistribution(queryDate)

    then:
    firstHit.with {
      assert k == expectedParams.k()
      assert mu == expectedParams.my()
      assert sigma == expectedParams.sigma()
    }

    when:
    /* Second query is only look up in storage */
    def secondHit = dut.getGevDistribution(queryDate)

    then:
    secondHit == firstHit
  }

  def "A random load model should approx. reach the targeted annual energy consumption"() {
    given:
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    def dut = new RandomLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        new EnergyConsumption(Quantities.getQuantity(3000d, KILOWATTHOUR))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new RandomLoadModel.RandomRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def avgEnergy = (0..10).parallelStream().mapToDouble( { runCnt ->
      relevantDatas.parallelStream().mapToDouble( { relevantData ->
        (dut.calculateActivePower(relevantData) * Quantities.getQuantity(15d, MINUTE)).asType(Energy).to(KILOWATTHOUR).value.doubleValue()
      }).sum()
    }).average().orElse(0d)

    then:
    abs(avgEnergy - 3000) / 3000 < 0.01
  }

  def "A random load model should approx. reach the targeted maximum power"() {
    given:
    def startDate = TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    def dut = new RandomLoadModel(
        loadInput.uuid,
        loadInput.id,
        foreSeenOperationInterval,
        1.0,
        QControl.apply(loadInput.qCharacteristics),
        loadInput.sRated,
        loadInput.cosPhiRated,
        new ActivePower(Quantities.getQuantity(268.6, WATT))
        )
    def relevantDatas = (0..35040).stream().map({ cnt ->
      new RandomLoadModel.RandomRelevantData(
          startDate.plus(cnt * 15, ChronoUnit.MINUTES))
    }).collect(Collectors.toSet())

    when:
    def powers = (0..10).parallelStream().flatMap({ runCnt ->
      relevantDatas.stream().parallel().map({ data ->
        dut.calculateActivePower(data).to(WATT).getValue().doubleValue()
      })
    }).sorted().toArray() as Double[]
    def quantilePower = get95Quantile(powers)

    then:
    abs(quantilePower - 268.6) / 268.6 < 0.01
  }

  def get95Quantile(Double[] sortedArray) {
    def quantIdx = sortedArray.length * 0.95 as int
    sortedArray[quantIdx]
  }
}