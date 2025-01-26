/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import edu.ie3.datamodel.models.OperationTime
import edu.ie3.datamodel.models.input.system.LoadInput
import edu.ie3.datamodel.models.input.system.characteristic.CosPhiFixed
import edu.ie3.datamodel.models.input.{NodeInput, OperatorInput}
import edu.ie3.datamodel.models.profile.BdewStandardLoadProfile
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.simona.model.SystemComponent
import edu.ie3.simona.model.participant.CalcRelevantData.LoadRelevantData
import edu.ie3.simona.model.participant.ModelState.ConstantState
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption,
}
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Watts}
import squants.time.Minutes
import squants.{Dimensionless, Each, Energy, Percent, Power, Quantity}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID

class LoadModelScalingSpec extends UnitSpec with TableDrivenPropertyChecks {

  "Testing correct scaling of load models" when {
    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01T00:00:00Z")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-12-31T23:59:00Z")

    "having a profile load model" should {
      val profileLoadInput =
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
      val operationInterval =
        SystemComponent.determineOperationInterval(
          simulationStartDate,
          simulationEndDate,
          profileLoadInput.getOperationTime,
        )

      val targetEnergyConsumption = KilowattHours(3000d)

      "reach the targeted annual energy consumption" in {
        forAll(
          Table(
            "profile",
            BdewStandardLoadProfile.H0,
            BdewStandardLoadProfile.L0,
            BdewStandardLoadProfile.G0,
          )
        ) { profile =>
          val loadInput = profileLoadInput.copy().loadprofile(profile).build()

          val model = ProfileLoadModel(
            loadInput,
            operationInterval,
            1.0d,
            EnergyConsumption(targetEnergyConsumption),
          )

          /* Test against a permissible deviation of 2 %. As per official documentation of the bdew load profiles
           * [https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf] 1.5 %
           * are officially permissible. But, as we currently do not take (bank) holidays into account, we cannot reach
           * this accuracy. */

          calculateEnergyDiffForYear(
            model,
            simulationStartDate,
            targetEnergyConsumption,
          ) should be < Percent(2d)
        }
      }

      "correctly account for the scaling factor, when targeting a given annual energy consumption" in {
        val scalingFactor = 1.5
        val expectedEnergy = KilowattHours(4500d)

        val model = ProfileLoadModel(
          profileLoadInput,
          operationInterval,
          scalingFactor,
          EnergyConsumption(targetEnergyConsumption),
        )

        calculateEnergyDiffForYear(
          model,
          simulationStartDate,
          expectedEnergy,
        ) should be < Percent(2d)
      }

      val targetMaximumPower = Watts(268.6)

      "approximately reach the maximum power" in {
        forAll(
          Table(
            "profile",
            BdewStandardLoadProfile.H0,
            BdewStandardLoadProfile.L0,
            BdewStandardLoadProfile.G0,
          )
        ) { profile =>
          val loadInput = profileLoadInput.copy().loadprofile(profile).build()

          val model = ProfileLoadModel(
            loadInput,
            operationInterval,
            1.0d,
            ActivePower(targetMaximumPower),
          )

          val maximumPower = calculatePowerForYear(
            model,
            simulationStartDate,
          ).maxOption.value

          implicit val tolerance: Power = Watts(1d)
          maximumPower should approximate(targetMaximumPower)
        }
      }

      "correctly account for the scaling factor when targeting at maximum power" in {
        val scalingFactor = 1.5
        val expectedMaximum = Watts(402.9)

        val model = ProfileLoadModel(
          profileLoadInput,
          operationInterval,
          scalingFactor,
          ActivePower(targetMaximumPower),
        )

        val maximumPower = calculatePowerForYear(
          model,
          simulationStartDate,
        ).maxOption.value

        implicit val tolerance: Power = Watts(1.5d)
        maximumPower should approximate(expectedMaximum)
      }
    }

    "having a random load model" should {
      val randomLoadInput =
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
      val operationInterval =
        SystemComponent.determineOperationInterval(
          simulationStartDate,
          simulationEndDate,
          randomLoadInput.getOperationTime,
        )

      val targetEnergyConsumption = KilowattHours(3000d)

      "reach the targeted annual energy consumption" in {
        val model = RandomLoadModel(
          randomLoadInput,
          operationInterval,
          1.0d,
          EnergyConsumption(targetEnergyConsumption),
        )

        calculateEnergyDiffForYear(
          model,
          simulationStartDate,
          targetEnergyConsumption,
        ) should be < Percent(1d)
      }

      "correctly account for the scaling factor, when targeting a given annual energy consumption" in {
        val scalingFactor = 1.5
        val expectedEnergy = KilowattHours(4500d)

        val model = RandomLoadModel(
          randomLoadInput,
          operationInterval,
          scalingFactor,
          EnergyConsumption(targetEnergyConsumption),
        )

        calculateEnergyDiffForYear(
          model,
          simulationStartDate,
          expectedEnergy,
        ) should be < Percent(2d)
      }

      val targetMaximumPower = Watts(268.6)
      "approximately reach the maximum power" in {
        val model = RandomLoadModel(
          randomLoadInput,
          operationInterval,
          1.0d,
          ActivePower(targetMaximumPower),
        )

        val powers = calculatePowerForYear(
          model,
          simulationStartDate,
        ).toIndexedSeq.sorted.toArray

        val quantile95 = RandomLoadModelSpec.get95Quantile(powers)

        getRelativeDifference(
          quantile95,
          targetMaximumPower,
        ) should be < Percent(2d)
      }

      "correctly account for the scaling factor when targeting at maximum power" in {
        val scalingFactor = 1.5
        val expectedMaximum = targetMaximumPower * scalingFactor

        val model = RandomLoadModel(
          randomLoadInput,
          operationInterval,
          scalingFactor,
          ActivePower(targetMaximumPower),
        )

        val powers = calculatePowerForYear(
          model,
          simulationStartDate,
        ).toIndexedSeq.sorted.toArray

        /* Tolerance is equivalent to 10 W difference between the 95%-percentile of the obtained random results and the
         * target maximum power. Because of the stochastic nature, the maximum power cannot be met perfectly */
        implicit val tolerance: Power = Watts(10d)
        RandomLoadModelSpec.get95Quantile(powers) should
          approximate(expectedMaximum)
      }
    }
  }

  def calculateEnergyDiffForYear[C <: LoadRelevantData](
      model: LoadModel[C],
      simulationStartDate: ZonedDateTime,
      expectedEnergy: Energy,
  ): Dimensionless = {
    val duration = Minutes(15d)

    val avgEnergy = calculatePowerForYear(
      model: LoadModel[C],
      simulationStartDate: ZonedDateTime,
    ).foldLeft(KilowattHours(0)) { case (energySum, power) =>
      energySum + (power * duration)
    }

    getRelativeDifference(
      avgEnergy,
      expectedEnergy,
    )
  }

  def calculatePowerForYear[C <: LoadRelevantData](
      model: LoadModel[C],
      simulationStartDate: ZonedDateTime,
  ): Iterable[Power] = {
    val quarterHoursInYear = 365L * 96L

    (0L until quarterHoursInYear)
      .map { quarterHour =>
        val tick = quarterHour * 15 * 60
        val relevantData = createRelevantData(model)(
          simulationStartDate.plus(quarterHour * 15, ChronoUnit.MINUTES)
        )

        model
          .calculatePower(
            tick,
            Each(0d),
            ConstantState,
            relevantData,
          )
          .p
      }
  }

  def createRelevantData[C <: LoadRelevantData](
      model: LoadModel[C]
  ): ZonedDateTime => C =
    model match {
      case _: RandomLoadModel  => RandomLoadModel.RandomRelevantData
      case _: ProfileLoadModel => ProfileLoadModel.ProfileRelevantData
    }

  def getRelativeDifference[Q <: Quantity[Q]](
      actualResult: Q,
      expectedResult: Q,
  ): Dimensionless =
    Each((expectedResult - actualResult).abs / expectedResult)

}
