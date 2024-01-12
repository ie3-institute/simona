/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiP
import edu.ie3.datamodel.models.input.system.characteristic.QV
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.model.MockParticipant
import edu.ie3.util.scala.OperationInterval
import edu.ie3.util.scala.quantities.Sq
import spock.lang.Specification
import squants.*
import squants.energy.*

class SystemParticipantTest extends Specification {

  def "Test calculateQ for a load or generation unit with fixed cosphi"() {
    given: "the mocked system participant model with a q_v characteristic"

    def loadMock = new MockParticipant(
        UUID.fromString("b69f6675-5284-4e28-add5-b76952ec1ec2"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new CosPhiFixed(varCharacteristicString)),
        Sq.create(200, Kilowatts$.MODULE$),
        1d)
    Dimensionless adjustedVoltage = Sq.create(1, Each$.MODULE$) // needed for method call but not applicable for cosphi_p

    when: "the reactive power is calculated"
    Power power = Sq.create(pVal, Kilowatts$.MODULE$)
    def qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toKilovars() - qSol.doubleValue()) < 0.0001

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

    def loadMock = new MockParticipant(
        UUID.fromString("3d28b9f7-929a-48e3-8696-ad2330a04225"),
        "Load calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new CosPhiP(varCharacteristicString)),
        Sq.create(102, Kilowatts$.MODULE$),
        1d)
    Dimensionless adjustedVoltage = Sq.create(1, Each$.MODULE$) // needed for method call but not applicable for cosphi_p

    when: "the reactive power is calculated"
    Power power = Sq.create(p, Kilowatts$.MODULE$)
    def qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toKilovars() - qSol.doubleValue()) < 0.0001

    where: // explained below
    varCharacteristicString                                                                                                                                                                                                                     | p      || qSol
    "cosPhiP:{(0,1),(0.05,1),(0.1,1),(0.15,1),(0.2,1),(0.25,1),(0.3,1),(0.35,1),(0.4,1),(0.45,1),(0.5,1),(0.55,0.99),(0.6,0.98),(0.65,0.97),(0.7,0.96),(0.75,0.95),(0.8,0.94),(0.85,0.93),(0.9,0.92),(0.95,0.91),(1,0.9)}"                      | 100.0d || 20.09975124224169d
    "cosPhiP:{(0,-1),(0.05,-1),(0.1,-1),(0.15,-1),(0.2,-1),(0.25,-1),(0.3,-1),(0.35,-1),(0.4,-1),(0.45,-1),(0.5,-1),(0.55,-0.99),(0.6,-0.98),(0.65,-0.97),(0.7,-0.96),(0.75,-0.95),(0.8,-0.94),(0.85,-0.93),(0.9,-0.92),(0.95,-0.91),(1,-0.9)}" | 100.0d || -20.09975124224169d

    // first line is "with P" -> positive Q (influence on voltage level: decrease) is expected
    // second line is "against P" -> negative Q (influence on voltage level: increase) is expected
  }

  def "Test calculateQ for a generation unit with cosphi_p"() {
    given: "the mocked generation model"

    def loadMock = new MockParticipant(
        UUID.fromString("30f84d97-83b4-4b71-9c2d-dbc7ebb1127c"),
        "Generation calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new CosPhiP(varCharacteristicString)),
        Sq.create(101, Kilowatts$.MODULE$),
        1d)

    Dimensionless adjustedVoltage = Sq.create(1, Each$.MODULE$) // needed for method call but not applicable for cosphi_p

    when: "the reactive power is calculated"
    Power power = Sq.create(p, Kilowatts$.MODULE$)
    def qCalc = loadMock.calculateReactivePower(power, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toKilovars() - qSol.doubleValue()) < 0.0001

    where: // explained below
    varCharacteristicString                                                                                                                                                                                                                                         | p       || qSol
    "cosPhiP:{(-1,0.9),(-0.95,0.91),(-0.9,0.92),(-0.85,0.93),(-0.8,0.94),(-0.75,0.95),(-0.7,0.96),(-0.65,0.97),(-0.6,0.98),(-0.55,0.99),(-0.5,1),(-0.45,1),(-0.4,1),(-0.35,1),(-0.3,1),(-0.25,1),(-0.2,1),(-0.15,1),(-0.1,1),(-0.05,1),(0,1)}"                      | -100.0d || -14.177446878757818d
    "cosPhiP:{(-1,-0.9),(-0.95,-0.91),(-0.9,-0.92),(-0.85,-0.93),(-0.8,-0.94),(-0.75,-0.95),(-0.7,-0.96),(-0.65,-0.97),(-0.6,-0.98),(-0.55,-0.99),(-0.5,-1),(-0.45,-1),(-0.4,-1),(-0.35,-1),(-0.3,-1),(-0.25,-1),(-0.2,-1),(-0.15,-1),(-0.1,-1),(-0.05,-1),(0,-1)}" | -100.0d || 14.177446878757818d

    // first line is "with P" -> negative Q (influence on voltage level: increase) is expected
    // second line is "against P" -> positive Q (influence on voltage level: decrease) is expected
  }

  def "Test calculateQ for a standard q_v characteristic"() {
    given: "the mocked system participant model with a q_v characteristic"

    Power p = Sq.create(42, Kilowatts$.MODULE$)

    def loadMock = new MockParticipant(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Sq.create(200, Kilowatts$.MODULE$),
        0.98)

    when: "the reactive power is calculated"
    Dimensionless adjustedVoltage = Sq.create(adjustedVoltageVal.doubleValue(), Each$.MODULE$)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toKilovars() - qSol.doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSol
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

    Power p = Sq.create(0, Kilowatts$.MODULE$)

    def loadMock = new MockParticipant(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Sq.create(200, Kilowatts$.MODULE$),
        1d)

    when: "the reactive power is calculated"
    Dimensionless adjustedVoltage = Sq.create(adjustedVoltageVal.doubleValue(), Each$.MODULE$)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toKilovars() - qSol.doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSol
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

    Power p = Sq.create(100, Kilowatts$.MODULE$)

    def loadMock = new MockParticipant(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Sq.create(200, Kilowatts$.MODULE$),
        0.95)

    when: "the reactive power is calculated"
    Dimensionless adjustedVoltage = Sq.create(adjustedVoltageVal.doubleValue(), Each$.MODULE$)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toKilovars() - qSol.doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSol
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

    Power p = Sq.create(195, Kilowatts$.MODULE$)

    def loadMock = new MockParticipant(
        UUID.fromString("d8461624-d142-4360-8e02-c21965ec555e"),
        "System participant calculateQ Test",
        OperationInterval.apply(0L, 86400L),
        1d,
        QControl.apply(new QV("qV:{(0.93,-1),(0.97,0),(1,0),(1.03,0),(1.07,1)}")),
        Sq.create(200, Kilowatts$.MODULE$),
        0.95)

    when: "the reactive power is calculated"
    Dimensionless adjustedVoltage = Sq.create(adjustedVoltageVal.doubleValue(), Each$.MODULE$)
    def qCalc = loadMock.calculateReactivePower(p, adjustedVoltage)

    then: "compare the results in watt"
    Math.abs(qCalc.toKilovars() - qSol.doubleValue()) < 0.0001

    where:
    adjustedVoltageVal || qSol
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
