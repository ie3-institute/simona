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
  EnergyConsumption
}
import edu.ie3.simona.model.participant.load.random.{
  RandomLoadModel,
  RandomLoadParameters
}
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.simona.test.matchers.QuantityMatchers
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID

class RandomLoadModelSpec
    extends UnitSpec
    with TableDrivenPropertyChecks
    with QuantityMatchers {
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
          -1
        ),
        new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
        BdewStandardLoadProfile.H0,
        false,
        Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR),
        Quantities.getQuantity(282.74d, PowerSystemUnits.VOLTAMPERE),
        0.95
      )

    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-12-31 23:59:00")
    val foreSeenOperationInterval =
      SystemComponent.determineOperationInterval(
        simulationStartDate,
        simulationEndDate,
        loadInput.getOperationTime
      )
    val testingTolerance = 1e-6 // Equals to 1 W power

    "instantiating it" should {
      "deliver a proper model" in {
        forAll(
          Table(
            ("reference", "expectedSRated"),
            (
              ActivePower(Quantities.getQuantity(268.6, Units.WATT)),
              Quantities
                .getQuantity(311.0105263157895, PowerSystemUnits.VOLTAMPERE)
            ),
            (
              EnergyConsumption(
                Quantities.getQuantity(2000d, PowerSystemUnits.KILOWATTHOUR)
              ),
              Quantities
                .getQuantity(467.156124576697, PowerSystemUnits.VOLTAMPERE)
            )
          )
        ) { (reference, expectedSRated) =>
          val actual = RandomLoadModel(
            loadInput,
            foreSeenOperationInterval,
            1.0,
            reference
          )

          actual.sRated should equalWithTolerance(
            expectedSRated.to(PowerSystemUnits.MEGAVOLTAMPERE),
            testingTolerance
          )
        }
      }
    }

    "calculating results" should {
      "deliver the correct distribution on request" in {
        val dut = new RandomLoadModel(
          loadInput.getUuid,
          loadInput.getId,
          foreSeenOperationInterval,
          1.0,
          QControl.apply(loadInput.getqCharacteristics()),
          loadInput.getsRated(),
          loadInput.getCosPhiRated,
          new ActivePower(Quantities.getQuantity(268.6, Units.WATT))
        )
        /* Working day, 61th quarter hour */
        val queryDate =
          TimeUtil.withDefaults.toZonedDateTime("2019-07-19 15:21:00")
        val expectedParams = new RandomLoadParameters(
          0.405802458524704,
          0.0671483352780342,
          0.0417016632854939
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