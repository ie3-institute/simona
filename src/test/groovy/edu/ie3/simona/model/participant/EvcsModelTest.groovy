/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.system.type.evcslocation.EvcsLocationType
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.test.common.model.MockEvModel
import edu.ie3.util.scala.OperationInterval
import spock.lang.Shared
import spock.lang.Specification
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import edu.ie3.util.quantities.QuantityUtil

import javax.measure.quantity.Power
import scala.collection.immutable.Set

import static edu.ie3.util.quantities.PowerSystemUnits.*
import static tech.units.indriya.unit.Units.MINUTE

class EvcsModelTest extends Specification {

  static final double TESTING_TOLERANCE = 1e-10

  @Shared
  double scalingFactor = 1.0d
  @Shared
  int chargingPoints = 2

  def getStandardModel(ComparableQuantity<Power> sRated) {
    return new EvcsModel(
        UUID.fromString("06a14909-366e-4e94-a593-1016e1455b30"),
        "Evcs Model Test",
        OperationInterval.apply(0L, 86400L),
        scalingFactor,
        QControl.apply(new CosPhiFixed("cosPhiFixed:{(0.0,1.0)}")),
        sRated,
        1d,
        chargingPoints,
        EvcsLocationType.HOME
        )
  }

  def "Test charge"() {
    given:
    EvcsModel evcsModel = getStandardModel(
        Quantities.getQuantity(evcsSRated, KILOVOLTAMPERE)
        )
    EvModel evModel = new MockEvModel(
        UUID.fromString("73c041c7-68e9-470e-8ca2-21fd7dbd1797"),
        "TestEv",
        Quantities.getQuantity(evSRated, KILOWATT),
        Quantities.getQuantity(evEStorage, KILOWATTHOUR),
        Quantities.getQuantity(evStoredEnergy, KILOWATTHOUR)
        )
    def chargingTime = Quantities.getQuantity(durationMins, MINUTE)

    when:
    def res = evcsModel.charge(evModel, chargingTime)

    then:
    QuantityUtil.isEquivalentAbs(res._1(),
        Quantities.getQuantity(solChargedEnergy, KILOWATTHOUR), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(res._2().storedEnergy,
        Quantities.getQuantity(solStoredEnergy, KILOWATTHOUR), TESTING_TOLERANCE)

    where:
    evcsSRated | evSRated | evEStorage | evStoredEnergy | durationMins || solStoredEnergy | solChargedEnergy
    100d       | 10d      | 20d        | 0d             | 60           || 10d             | 10d // charge a bit
    100d       | 100d     | 20d        | 0d             | 60           || 20d             | 20d // charge to full
    100d       | 100d     | 80d        | 30d            | 30           || 80d             | 50d // charge to full with non-empty start
    100d       | 10d      | 20d        | 20d            | 60           || 20d             | 0d  // already full
  }

  def "Test calcActivePowerAndEvSoc"() {
    given:
    def evsRated = Quantities.getQuantity(100d, KILOVOLTAMPERE)

    EvcsModel evcsModel = getStandardModel(evsRated)
    EvModel ev1Model = new MockEvModel(
        UUID.fromString("73c041c7-68e9-470e-8ca2-21fd7dbd1797"),
        "TestEv1",
        Quantities.getQuantity(ev1SRated, KILOWATT),
        Quantities.getQuantity(50d, KILOWATTHOUR),
        Quantities.getQuantity(ev1StoredEnergy, KILOWATTHOUR)
        )
    EvModel ev2Model = new MockEvModel(
        UUID.fromString("5e86454d-3434-4d92-856e-2f62dd1d0d90"),
        "TestEv2",
        Quantities.getQuantity(ev2SRated, KILOWATT),
        Quantities.getQuantity(50d, KILOWATTHOUR),
        Quantities.getQuantity(ev2StoredEnergy, KILOWATTHOUR)
        )
    Set mySet = new Set.Set2<EvModel>(ev1Model, ev2Model)
    def data = new EvcsModel.EvcsRelevantData(durationTicks, mySet)

    when:
    def res = evcsModel.calculateActivePowerAndEvSoc(data)

    then:
    QuantityUtil.isEquivalentAbs(res._1(), Quantities.getQuantity(solPower, KILOWATT), TESTING_TOLERANCE)
    res._2().size() == 2
    QuantityUtil.isEquivalentAbs(res._2().head().storedEnergy,
        Quantities.getQuantity(solEv1Stored, KILOWATTHOUR), TESTING_TOLERANCE)
    QuantityUtil.isEquivalentAbs(res._2().last().storedEnergy,
        Quantities.getQuantity(solEv2Stored, KILOWATTHOUR), TESTING_TOLERANCE)

    where:
    ev1SRated | ev1StoredEnergy | ev2SRated | ev2StoredEnergy | durationTicks || solPower | solEv1Stored | solEv2Stored
    10d       | 0d              | 10d       | 0d              | 3600L         || 20d      | 10d          | 10d    // well below evcs sRated
    10d       | 0d              | 10d       | 0d              | 900L          || 20d      | 2.5d         | 2.5d
    50d       | 0d              | 50d       | 0d              | 7200L         || 50d      | 50d          | 50d
    50d       | 0d              | 50d       | 0d              | 1800L         || 100d     | 25d          | 25d   // hitting evcs sRated exactly
    100d      | 0d              | 25d       | 0d              | 1800L         || 100d     | 50d          | 0d    // going above evcs sRated
    50d       | 25d             | 50d       | 25d             | 1800L         || 100d     | 50d          | 50d   // with non-zero start
    50d       | 45d             | 50d       | 35d             | 3600L         || 20d      | 50d          | 50d
    200d      | 25d             | 50d       | 50d             | 3600L         || 25d      | 50d          | 50d

  }
}
