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
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroKW
import org.scalatest.matchers.should.Matchers
import squants.energy.{KilowattHours, Kilowatts}
import squants.{Energy, Power}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.quantity.Quantities.getQuantity

import java.util.UUID

class StorageModelSpec extends UnitSpec with Matchers {

  final val inputModel: StorageInput = createStorageInput()
  implicit val powerTolerance: Power = Kilowatts(1e-10)
  implicit val energyTolerance: Energy = KilowattHours(1e-10)

  def createStorageInput(): StorageInput = {
    val nodeInput = new NodeInput(
      UUID.fromString("ad39d0b9-5ad6-4588-8d92-74c7d7de9ace"),
      "NodeInput",
      OperatorInput.NO_OPERATOR_ASSIGNED,
      OperationTime.notLimited(),
      Quantities.getQuantity(1, PowerSystemUnits.PU),
      false,
      NodeInput.DEFAULT_GEO_POSITION,
      GermanVoltageLevelUtils.LV,
      -1,
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

    new StorageInput(
      UUID.randomUUID(),
      "Test_StorageInput",
      new OperatorInput(UUID.randomUUID(), "NO_OPERATOR"),
      OperationTime.notLimited(),
      nodeInput,
      CosPhiFixed.CONSTANT_CHARACTERISTIC,
      null,
      typeInput,
    )
  }

  def buildStorageModel(
      targetSoc: Option[Double] = Option.empty
  ): StorageModel = {
    StorageModel.apply(
      inputModel,
      1,
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T00:00:00Z"),
      TimeUtil.withDefaults.toZonedDateTime("2020-01-01T01:00:00Z"),
      0d,
      targetSoc,
    )
  }

  "StorageModel" should {
    "Calculate flex options" in {
      val storageModel = buildStorageModel()
      val startTick = 3600L

      val testCases = Table(
        ("lastStored", "lastPower", "timeDelta", "pRef", "pMin", "pMax"),
        // UNCHANGED STATE
        // completely empty
        (0.0, 0.0, 1, 0.0, 0.0, 10.0),
        // at a tiny bit above empty
        (0.011, 0.0, 1, 0.0, -10.0, 10.0),
        // at mid-level charge
        (60.0, 0.0, 1, 0.0, -10.0, 10.0),
        // almost fully charged
        (99.989, 0.0, 1, 0.0, -10.0, 10.0),
        // fully charged
        (100.0, 0.0, 1, 0.0, -10.0, 0.0),
        // CHANGED STATE
        // discharged to empty
        (10.0, -9.0, 3600, 0.0, 0.0, 10.0),
        // almost discharged to lowest allowed charge
        (10.0, -9.0, 3590, 0.0, -10.0, 10.0),
        // charged to mid-level charge
        (41.0, 10.0, 3600, 0.0, -10.0, 10.0),
        // discharged to mid-level charge
        (60.0, -9.0, 3600, 0.0, -10.0, 10.0),
        // almost fully charged
        (95.5, 4.98, 3600, 0.0, -10.0, 10.0),
        // fully charged
        (95.5, 5.0, 3600, 0.0, -10.0, 0.0),
      )

      forAll(testCases) {
        (
            lastStored: Double,
            lastPower: Double,
            timeDelta: Int,
            pRef: Double,
            pMin: Double,
            pMax: Double,
        ) =>
          val data = StorageModel.StorageRelevantData(startTick + timeDelta)
          val oldState = StorageModel.StorageState(
            KilowattHours(lastStored),
            Kilowatts(lastPower),
            startTick,
          )

          val result = storageModel
            .determineFlexOptions(data, oldState)
            .asInstanceOf[ProvideMinMaxFlexOptions]

          result.ref should approximate(Kilowatts(pRef))
          result.min should approximate(Kilowatts(pMin))
          result.max should approximate(Kilowatts(pMax))
      }
    }
    "Calculate flex options with target SOC" in {
      val storageModel = buildStorageModel(Some(0.5d))
      val startTick = 3600L
      val data = StorageModel.StorageRelevantData(startTick + 1)

      val testCases = Table(
        ("lastStored", "pRef", "pMin", "pMax"),
        // completely empty
        (0.0, 10.0, 0.0, 10.0),
        // below margin of ref power target
        (49.9974, 10.0, -10.0, 10.0),
        // within margin below ref power target
        (49.9976, 0.0, -10.0, 10.0),
        // exactly at ref power target
        (50.0, 0.0, -10.0, 10.0),
        // within margin above ref power target
        (50.0030, 0.0, -10.0, 10.0),
        // above margin of ref power target
        (50.0031, -10.0, -10.0, 10.0),
        // at mid-level charge
        (60.0, -10.0, -10.0, 10.0),
        // fully charged
        (100.0, -10.0, -10.0, 0.0),
      )

      forAll(testCases) {
        (lastStored: Double, pRef: Double, pMin: Double, pMax: Double) =>
          val oldState = StorageModel.StorageState(
            KilowattHours(lastStored),
            zeroKW,
            startTick,
          )

          val result = storageModel
            .determineFlexOptions(data, oldState)
            .asInstanceOf[ProvideMinMaxFlexOptions]

          result.ref should approximate(Kilowatts(pRef))
          result.min should approximate(Kilowatts(pMin))
          result.max should approximate(Kilowatts(pMax))
      }
    }

    "Handle controlled power change" in {
      val storageModel = buildStorageModel()
      val startTick = 3600L
      val data = StorageModel.StorageRelevantData(startTick + 1)
      val testCases = Table(
        (
          "lastStored",
          "setPower",
          "expPower",
          "expActiveNext",
          "expScheduled",
          "expDelta",
        ),
        // no power
        (0.0, 0.0, 0.0, false, false, 0.0),
        (50.0, 0.0, 0.0, false, false, 0.0),
        (100.0, 0.0, 0.0, false, false, 0.0),
        // charging on empty
        (0.0, 1.0, 1.0, true, true, 100 * 3600 / 0.9),
        (0.0, 2.5, 2.5, true, true, 40 * 3600 / 0.9),
        (0.0, 5.0, 5.0, true, true, 20 * 3600 / 0.9),
        (0.0, 10.0, 10.0, true, true, 10 * 3600 / 0.9),
        // charging on half full
        (50.0, 5.0, 5.0, false, true, 10 * 3600 / 0.9),
        (50.0, 10.0, 10.0, false, true, 5 * 3600 / 0.9),
        // discharging on half full
        (50.0, -4.5, -4.5, false, true, 10 * 3600.0),
        (50.0, -9.0, -9.0, false, true, 5 * 3600.0),
        // discharging on full
        (100.0, -4.5, -4.5, true, true, 20 * 3600.0),
        (100.0, -9.0, -9.0, true, true, 10 * 3600.0),
      )

      forAll(testCases) {
        (
            lastStored: Double,
            setPower: Double,
            expPower: Double,
            expActiveNext: Boolean,
            expScheduled: Boolean,
            expDelta: Double,
        ) =>
          val oldState = StorageModel.StorageState(
            KilowattHours(lastStored),
            Kilowatts(0d),
            startTick,
          )

          val (newState, flexChangeIndication) =
            storageModel.handleControlledPowerChange(
              data,
              oldState,
              Kilowatts(setPower),
            )

          newState.chargingPower should approximate(Kilowatts(expPower))
          newState.tick shouldBe (startTick + 1)
          newState.storedEnergy should approximate(KilowattHours(lastStored))

          flexChangeIndication.changesAtTick.isDefined shouldBe expScheduled
          flexChangeIndication.changesAtTick.forall(
            _ == (startTick + 1 + expDelta)
          ) shouldBe true
          flexChangeIndication.changesAtNextActivation shouldBe expActiveNext
      }
    }

    "Handle controlled power change with ref target SOC" in {
      val storageModel = buildStorageModel(Some(0.5d))
      val startTick = 3600L
      val data = StorageModel.StorageRelevantData(startTick + 1)

      val testCases = Table(
        (
          "lastStored",
          "setPower",
          "expPower",
          "expActiveNext",
          "expScheduled",
          "expDelta",
        ),
        // no power
        (0.0, 0.0, 0.0, false, false, 0.0),
        (50.0, 0.0, 0.0, false, false, 0.0),
        (100.0, 0.0, 0.0, false, false, 0.0),
        // charging on empty
        (0.0, 1.0, 1.0, true, true, 50 * 3600 / 0.9),
        (0.0, 2.5, 2.5, true, true, 20 * 3600 / 0.9),
        (0.0, 5.0, 5.0, true, true, 10 * 3600 / 0.9),
        (0.0, 10.0, 10.0, true, true, 5 * 3600 / 0.9),
        // charging on target ref
        (50.0, 5.0, 5.0, true, true, 10 * 3600 / 0.9),
        (50.0, 10.0, 10.0, true, true, 5 * 3600 / 0.9),
        // discharging on target ref
        (50.0, -4.5, -4.5, true, true, 10 * 3600.0),
        (50.0, -9.0, -9.0, true, true, 5 * 3600.0),
        // discharging on full
        (100.0, -4.5, -4.5, true, true, 10 * 3600.0),
        (100.0, -9.0, -9.0, true, true, 5 * 3600.0),
      )

      forAll(testCases) {
        (
            lastStored: Double,
            setPower: Double,
            expPower: Double,
            expActiveNext: Boolean,
            expScheduled: Boolean,
            expDelta: Double,
        ) =>
          val oldState = StorageModel.StorageState(
            KilowattHours(lastStored),
            zeroKW,
            startTick,
          )

          val result = storageModel.handleControlledPowerChange(
            data,
            oldState,
            Kilowatts(setPower),
          )

          val (newState, flexChangeIndication) = result

          newState.chargingPower should approximate(Kilowatts(expPower))
          newState.tick shouldBe (startTick + 1)
          newState.storedEnergy should approximate(KilowattHours(lastStored))

          flexChangeIndication.changesAtTick.isDefined shouldBe expScheduled
          flexChangeIndication.changesAtTick.forall(
            _ == (startTick + 1 + expDelta)
          ) shouldBe true
          flexChangeIndication.changesAtNextActivation shouldBe expActiveNext
      }
    }

    "Handle the edge case of discharging in tolerance margins" in {
      val storageModel = buildStorageModel()
      val startTick = 1800L
      val data = StorageModel.StorageRelevantData(startTick + 1)
      // margin is at ~ 0.0030864 kWh
      val oldState = StorageModel.StorageState(
        KilowattHours(0.002d),
        Kilowatts(0d),
        startTick,
      )

      val result = storageModel.handleControlledPowerChange(
        data,
        oldState,
        Kilowatts(-5d),
      )

      result._1.chargingPower should approximate(zeroKW)
      result._1.tick shouldBe (startTick + 1)
      result._1.storedEnergy should approximate(oldState.storedEnergy)

      val flexChangeIndication = result._2
      flexChangeIndication.changesAtTick.isDefined shouldBe false
      flexChangeIndication.changesAtNextActivation shouldBe true
    }

    "Handle the edge case of charging in tolerance margins" in {
      val storageModel = buildStorageModel()
      val startTick = 1800L
      val data = StorageModel.StorageRelevantData(startTick + 1)
      // margin is at ~ 99.9975 kWh
      val oldState = StorageModel.StorageState(
        KilowattHours(99.999d),
        zeroKW,
        startTick,
      )

      val result = storageModel.handleControlledPowerChange(
        data,
        oldState,
        Kilowatts(9d),
      )

      result._1.chargingPower should approximate(zeroKW)
      result._1.tick shouldBe (startTick + 1)
      result._1.storedEnergy should approximate(oldState.storedEnergy)

      val flexChangeIndication = result._2
      flexChangeIndication.changesAtTick.isDefined shouldBe false
      flexChangeIndication.changesAtNextActivation shouldBe true
    }
    "Handle the edge case of discharging in positive target margin" in {
      val storageModel = buildStorageModel(Some(0.3d))
      val startTick = 1800L
      val data = StorageModel.StorageRelevantData(startTick + 1)
      // margin is at ~ 30.0025 kWh
      val oldState = StorageModel.StorageState(
        KilowattHours(30.0024d),
        zeroKW,
        startTick,
      )

      val result = storageModel.handleControlledPowerChange(
        data,
        oldState,
        Kilowatts(-9d),
      )

      result._1.chargingPower should approximate(Kilowatts(-9d))
      result._1.tick shouldBe (startTick + 1)
      result._1.storedEnergy should approximate(oldState.storedEnergy)
      val flexChangeIndication = result._2
      flexChangeIndication.changesAtTick should be(
        Some(startTick + 1L + 10801L)
      )
      flexChangeIndication.changesAtNextActivation should be(true)
    }
    "Handle the edge case of charging in negative target margin" in {
      val storageModel = buildStorageModel(Some(0.4d))
      val startTick = 1800L
      val data = StorageModel.StorageRelevantData(startTick + 1)
      // margin is at ~ 39.9975 kWh
      val oldState = StorageModel.StorageState(
        KilowattHours(39.998d),
        zeroKW,
        startTick,
      )

      val result = storageModel.handleControlledPowerChange(
        data,
        oldState,
        Kilowatts(5d),
      )

      result._1.chargingPower should approximate(Kilowatts(5d))
      result._1.tick shouldBe (startTick + 1)
      result._1.storedEnergy should approximate(oldState.storedEnergy)
      val flexChangeIndication = result._2
      flexChangeIndication.changesAtTick should be(
        Some(startTick + 1L + 48002L)
      )
      flexChangeIndication.changesAtNextActivation should be(true)
    }
  }
}
