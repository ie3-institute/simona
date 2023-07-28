/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import static edu.ie3.util.quantities.PowerSystemUnits.*

import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiP
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.util.scala.OperationInterval
import spock.lang.Specification
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.Quantity
import javax.measure.quantity.Power



class SystemParticipantTest extends Specification {

  def "Test calculateQ for a load or generation unit with fixed cosphi"() {
    given: "the mocked system participant model with a q_v characteristic"

    def loadMock = new SystemParticipant<CalcRelevantData>(
        UUID.fromString("b69f6675-5284-4e28-add5-b76952ec1ec2"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new CosPhiFixed(varCharacteristicString)),
        Quantities.getQuantity(200, KILOVOLTAMPERE),
        1d) {
          @Override
          ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
            return Quantities.getQuantity(0, MEGAWATT)
          }
        }
    Quantity adjustedVoltage = Quantities.getQuantity(1, PU) // needed for method call but not applicable for cosphi_p

    when: "the reactive power is calculated"
    Quantity power = Quantities.getQuantity(pVal, KILOWATT)
    def qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.subtract(Quantities.getQuantity(qSol, KILOVAR)).value.doubleValue()) < 0.0001

    where:
    varCharacteristicString   | pVal || qSol
    "cosPhiFixed:{(0.0,0.9)}" | 0    || 0
    "cosPhiFixed:{(0.0,0.9)}" | 50   || 24.216105241892627000
    "cosPhiFixed:{(0.0,0.9)}" | 100  || 48.432210483785254000
    "cosPhiFixed:{(0.0,0.9)}" | 200  || 0
    "cosPhiFixed:{(0.0,0.9)}" | -50  || -24.216105241892627000
    "cosPhiFixed:{(0.0,0.9)}" | -100 || -48.432210483785254000
    "cosPhiFixed:{(0.0,0.9)}" | -200 || 0
    "cosPhiFixed:{(0.0,1.0)}" | 100  || 0
  }

  def "Test calculateQ for a load unit with cosphi_p"() {
    given: "the mocked load model"

    def loadMock = new SystemParticipant<CalcRelevantData>(
        UUID.fromString("3d28b9f7-929a-48e3-8696-ad2330a04225"),
        "Load calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new CosPhiP(varCharacteristicString)),
        Quantities.getQuantity(102, KILOWATT),
        1d) {
          @Override
          ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
            return Quantities.getQuantity(0, MEGAWATT)
          }
        }

    Quantity adjustedVoltage = Quantities.getQuantity(1, PU) // needed for method call but not applicable for cosphi_p

    when: "the reactive power is calculated"
    def qCalc = loadMock.calculateReactivePower(Quantities.getQuantity(p, KILOWATT), adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(qSol, KILOVAR).toSystemUnit().getValue().doubleValue()) < 0.0001

    where: // explained below
    varCharacteristicString                                                                                                                                                                                                                     | p     || qSol
    "cosPhiP:{(0,1),(0.05,1),(0.1,1),(0.15,1),(0.2,1),(0.25,1),(0.3,1),(0.35,1),(0.4,1),(0.45,1),(0.5,1),(0.55,0.99),(0.6,0.98),(0.65,0.97),(0.7,0.96),(0.75,0.95),(0.8,0.94),(0.85,0.93),(0.9,0.92),(0.95,0.91),(1,0.9)}"                      | 100.0 || 20.09975124224169
    "cosPhiP:{(0,-1),(0.05,-1),(0.1,-1),(0.15,-1),(0.2,-1),(0.25,-1),(0.3,-1),(0.35,-1),(0.4,-1),(0.45,-1),(0.5,-1),(0.55,-0.99),(0.6,-0.98),(0.65,-0.97),(0.7,-0.96),(0.75,-0.95),(0.8,-0.94),(0.85,-0.93),(0.9,-0.92),(0.95,-0.91),(1,-0.9)}" | 100.0 || -20.09975124224169

    // first line is "with P" -> positive Q (influence on voltage level: decrease) is expected
    // second line is "against P" -> negative Q (influence on voltage level: increase) is expected
  }

  def "Test calculateQ for a generation unit with cosphi_p"() {
    given: "the mocked generation model"

    def loadMock = new SystemParticipant<CalcRelevantData>(
        UUID.fromString("30f84d97-83b4-4b71-9c2d-dbc7ebb1127c"),
        "Generation calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new CosPhiP(varCharacteristicString)),
        Quantities.getQuantity(101, KILOWATT),
        1d) {
          @Override
          ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
            return Quantities.getQuantity(0, MEGAWATT)
          }
        }

    Quantity adjustedVoltage = Quantities.getQuantity(1, PU) // needed for method call but not applicable for cosphi_p

    when: "the reactive power is calculated"
    def qCalc = loadMock.calculateReactivePower(Quantities.getQuantity(p, KILOWATT), adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(qSol, KILOVAR).toSystemUnit().getValue().doubleValue()) < 0.0001

    where: // explained below
    varCharacteristicString                                                                                                                                                                                                                                         | p      || qSol
    "cosPhiP:{(-1,0.9),(-0.95,0.91),(-0.9,0.92),(-0.85,0.93),(-0.8,0.94),(-0.75,0.95),(-0.7,0.96),(-0.65,0.97),(-0.6,0.98),(-0.55,0.99),(-0.5,1),(-0.45,1),(-0.4,1),(-0.35,1),(-0.3,1),(-0.25,1),(-0.2,1),(-0.15,1),(-0.1,1),(-0.05,1),(0,1)}"                      | -100.0 || -14.177446878757818
    "cosPhiP:{(-1,-0.9),(-0.95,-0.91),(-0.9,-0.92),(-0.85,-0.93),(-0.8,-0.94),(-0.75,-0.95),(-0.7,-0.96),(-0.65,-0.97),(-0.6,-0.98),(-0.55,-0.99),(-0.5,-1),(-0.45,-1),(-0.4,-1),(-0.35,-1),(-0.3,-1),(-0.25,-1),(-0.2,-1),(-0.15,-1),(-0.1,-1),(-0.05,-1),(0,-1)}" | -100.0 || 14.177446878757818

    // first line is "with P" -> negative Q (influence on voltage level: increase) is expected
    // second line is "against P" -> positive Q (influence on voltage level: decrease) is expected
  }

  def "Test calculateQ for a standard q_v characteristic"() {
    given: "the mocked system participant model with a q_v characteristic"

    Quantity p = Quantities.getQuantity(42, KILOWATT)

    def loadMock = new SystemParticipant<CalcRelevantData>(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Quantities.getQuantity(200, KILOWATT),
        0.98) {
          @Override
          ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
            return Quantities.getQuantity(0, MEGAWATT)
          }
        }

    when: "the reactive power is calculated"
    Quantity adjustedVoltage = Quantities.getQuantity(adjustedVoltageVal, PU)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(qSoll, KILOVAR).toSystemUnit().getValue().doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSoll
    0.9                || -39.79949748426482
    0.93               || -39.79949748426482
    0.95               ||  -19.89974874213241
    0.97               || 0
    1.00               || 0
    1.03               || 0
    1.05               || 19.89974874213241
    1.07               || 39.79949748426482
    1.1                || 39.79949748426482
  }

  def "Test calculateQ for a standard q_v characteristic if active power is zero and cosPhiRated 1"() {
    given: "the mocked system participant model with a q_v characteristic"

    Quantity p = Quantities.getQuantity(0, KILOWATT)

    def loadMock = new SystemParticipant<CalcRelevantData>(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Quantities.getQuantity(200, KILOWATT),
        1d) {
          @Override
          ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
            return Quantities.getQuantity(0, MEGAWATT)
          }
        }

    when: "the reactive power is calculated"
    Quantity adjustedVoltage = Quantities.getQuantity(adjustedVoltageVal, PU)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(qSoll, KILOVAR).toSystemUnit().getValue().doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSoll
    0.9                || 0
    0.93               || 0
    0.95               || 0
    0.97               || 0
    1.00               || 0
    1.03               || 0
    1.05               || 0
    1.07               || 0
    1.1                || 0
  }

  def "Test calculateQ for a standard q_v characteristic if active power is not zero and cosPhiRated 0.95"() {
    given: "the mocked system participant model with a q_v characteristic"

    Quantity p = Quantities.getQuantity(100d, KILOWATT)

    def loadMock = new SystemParticipant<CalcRelevantData>(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Quantities.getQuantity(200, KILOWATT),
        0.95) {
          @Override
          ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
            return Quantities.getQuantity(0, MEGAWATT)
          }
        }

    when: "the reactive power is calculated"
    Quantity adjustedVoltage = Quantities.getQuantity(adjustedVoltageVal, PU)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(qSoll, KILOVAR).toSystemUnit().getValue().doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSoll
    0.9                || -62.449979983984
    0.93               || -62.449979983984
    0.95               || -31.224989991992
    0.97               || 0
    1.00               || 0
    1.03               || 0
    1.05               || 31.224989991992
    1.07               || 62.449979983984
    1.1                || 62.449979983984
  }

  def "Test calculateQ for a standard q_v characteristic if active power is 195 and cosPhiRated 0.95"() {
    given: "the mocked system participant model with a q_v characteristic"

    Quantity p = Quantities.getQuantity(195d, KILOWATT)

    def loadMock = new SystemParticipant<CalcRelevantData>(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Quantities.getQuantity(200, KILOWATT),
        0.95) {
          @Override
          ComparableQuantity<Power> calculateActivePower(CalcRelevantData data) {
            return Quantities.getQuantity(0, MEGAWATT)
          }
        }

    when: "the reactive power is calculated"
    Quantity adjustedVoltage = Quantities.getQuantity(adjustedVoltageVal, PU)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toSystemUnit().getValue().doubleValue() - Quantities.getQuantity(qSoll, KILOVAR).toSystemUnit().getValue().doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSoll
    0.9                || -44.440972086578
    0.93               || -44.440972086578
    0.95               || -31.224989991992
    0.97               || 0
    1.00               || 0
    1.03               || 0
    1.05               || 31.224989991992
    1.07               || 44.440972086578
    1.1                || 44.440972086578
  }
}
