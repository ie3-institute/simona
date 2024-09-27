/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import de.lmu.ifi.dbs.elki.math.statistics.distribution.GeneralizedExtremeValueDistribution
import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption,
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.Power
import squants.energy.{KilowattHours, Kilowatts, Watts}
import tech.units.indriya.quantity.Quantities

import java.util.UUID

class RandomLoadModelSpec extends UnitSpec with TableDrivenPropertyChecks {
  implicit val tolerance: Power = Watts(1d)
  "Having a random load model" when {
    val loadInput =
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
          Quantities.getQuantity(1d, PowerSystemUnits.PU),
          false,
          NodeInput.DEFAULT_GEO_POSITION,
          GermanVoltageLevelUtils.LV,
          -1,
        ),
        new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
        null,
        BdewStandardLoadProfile.H0,
        false,
        Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR),
        Quantities.getQuantity(282.74d, PowerSystemUnits.VOLTAMPERE),
        0.95,
      )

    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-12-31T23:59:00Z")
    val foreSeenOperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        loadInput.getOperationTime,
      )

    "instantiating it" should {
      "deliver a proper model" in {

        val testData = Table(
          ("reference", "expectedSRated"),
          (ActivePower(Watts(268.6)), Watts(311.0105263157895d)),
          (EnergyConsumption(KilowattHours(2000d)), Watts(513.871737d)),
        )

        forAll(testData) { (reference, expectedSRated: Power) =>
          val actual = RandomLoadModel(
            loadInput,
            foreSeenOperationInterval,
            1.0,
            reference,
          )

          actual.sRated should approximate(expectedSRated)
        }
      }
    }

    "calculating results" should {
      "deliver the correct distribution on request" in {
        val dut = new RandomLoadModel(
          loadInput.getUuid,
          loadInput.getId,
          foreSeenOperationInterval,
          QControl.apply(loadInput.getqCharacteristics()),
          Kilowatts(
            loadInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue()
          ),
          loadInput.getCosPhiRated,
          ActivePower(Watts(268.6)),
        )
        /* Working day, 61th quarter hour */
        val queryDate =
          TimeUtil.withDefaults.toZonedDateTime("2019-07-19T15:21:00Z")
        val expectedParams = new RandomLoadParameters(
          0.405802458524704,
          0.0671483352780342,
          0.0417016632854939,
        )

        /* First query leeds to generation of distribution */
        val getGevDistribution =
          PrivateMethod[GeneralizedExtremeValueDistribution](
            Symbol("getGevDistribution")
          )

        def firstHit = dut invokePrivate getGevDistribution(queryDate)

        firstHit.getK shouldBe expectedParams.k
        firstHit.getMu shouldBe expectedParams.my
        firstHit.getSigma shouldBe expectedParams.sigma

        /* Second query is only look up in storage */
        def secondHit = dut invokePrivate getGevDistribution(queryDate)

        secondHit shouldBe firstHit
      }
    }
  }
}

object RandomLoadModelSpec {
  def get95Quantile[V](sortedArray: Array[V]): V = sortedArray(
    (sortedArray.length * 0.95).toInt
  )
}
