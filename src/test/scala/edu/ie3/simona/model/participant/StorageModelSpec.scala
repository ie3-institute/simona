/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.StorageInput
import edu.ie3.datamodel.models.input.system.`type`.StorageTypeInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.ontology.messages.flex.MinMaxFlexibilityMessage.ProvideMinMaxFlexOptions
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits._
import org.scalatest.matchers.should.Matchers
import squants.Power
import squants.energy.{KilowattHours, Kilowatts}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.quantity.Quantities.getQuantity

import java.util.UUID

class StorageModelSpec extends UnitSpec with Matchers {

  var inputModel: StorageInput = _
  final val TOLERANCE = 1e-10
  final implicit val TOLERANCE2: Power = Kilowatts(1e-10)

  "StorageModel" should {
      val nodeInput = new NodeInput(
        UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
        "NodeInput",
        OperatorInput.NO_OPERATOR_ASSIGNED,
        OperationTime.notLimited(),
        Quantities.getQuantity(1, PowerSystemUnits.PU),
        false,
        NodeInput.DEFAULT_GEO_POSITION,
        GermanVoltageLevelUtils.LV,
        -1
      )

      val typeInput = new StorageTypeInput(
        UUID.fromString("fbee4995-24dd-45e4-9c85-7d986fe99ff3"),
        "Test_StorageTypeInput",
        Quantities.getQuantity(10000d, EURO),
        getQuantity(0.05d, EURO_PER_MEGAWATTHOUR),
        Quantities.getQuantity(100d, KILOWATTHOUR),
        getQuantity(13d, KILOVOLTAMPERE),
        0.997,
        getQuantity(10d, KILOWATT),
        getQuantity(0.03, PU_PER_HOUR),
        getQuantity(0.9, PU),
      )

      inputModel = new StorageInput(
        UUID.randomUUID(),
        "Test_StorageInput",
        new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
        OperationTime.notLimited(),
        nodeInput,
        CosPhiFixed.CONSTANT_CHARACTERISTIC,
        null,
        typeInput
      )
    }
  }

  def buildStorageModel(targetSoc: Option[Double] = Option.empty): StorageModel = {
    StorageModel.apply(
      inputModel,
      1,
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z"),
      0d,
      targetSoc
    )
  }

  "Calculate flex options" should {
    "correctly compute the flexibility options" in {
      val storageModel = buildStorageModel()
      val startTick = 3600L

      val testCases = Table(
        ("lastStored", "lastPower", "timeDelta", "pRef", "pMin", "pMax"),
        // UNCHANGED STATE
        (0.0, 0.0, 1, 0.0, 0.0, 10.0),
        (0.011, 0.0, 1, 0.0, -10.0, 10.0),
        (60.0, 0.0, 1, 0.0, -10.0, 10.0),
        (99.989, 0.0, 1, 0.0, -10.0, 10.0),
        (100.0, 0.0, 1, 0.0, -10.0, 0.0),
        // CHANGED STATE
        (10.0, -9.0, 3600, 0.0, 0.0, 10.0),
        (10.0, -9.0, 3590, 0.0, -10.0, 10.0),
        (41.0, 10.0, 3600, 0.0, -10.0, 10.0),
        (60.0, -9.0, 3600, 0.0, -10.0, 10.0),
        (95.5, 4.98, 3600, 0.0, -10.0, 10.0),
        (95.5, 5.0, 3600, 0.0, -10.0, 0.0)
      )

      forAll(testCases) { (lastStored: Double, lastPower: Double, timeDelta: Int, pRef: Double, pMin: Double, pMax: Double) =>
        val data = StorageModel.StorageRelevantData(startTick + timeDelta)
        val oldState = StorageModel.StorageState(
          KilowattHours(lastStored),
          Kilowatts(lastPower),
          startTick
        )

        val result = storageModel.determineFlexOptions(data, oldState).asInstanceOf[ProvideMinMaxFlexOptions]

        result.ref should approximate(Kilowatts(pRef))
        result.min should approximate(Kilowatts(pMin))
        result.max should approximate(Kilowatts(pMax))
      }
    }
  }
}
