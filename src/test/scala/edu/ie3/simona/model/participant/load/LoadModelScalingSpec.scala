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
import edu.ie3.simona.model.participant.control.QControl
import edu.ie3.simona.model.participant.load.LoadReference.{
  ActivePower,
  EnergyConsumption
}
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Kilowatts, Watts}
import squants.time.Minutes
import squants.{Dimensionless, Each, Energy, Percent, Power, Quantity}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID

class LoadModelScalingSpec extends UnitSpec with TableDrivenPropertyChecks {

  "Testing correct scaling of load models" when {
    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-12-31 23:59:00")

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
            -1
          ),
          new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
          BdewStandardLoadProfile.H0,
          false,
          Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR),
          Quantities.getQuantity(282.74d, PowerSystemUnits.VOLTAMPERE),
          0.95
        )
      val foreSeenOperationInterval =
        SystemComponent.determineOperationInterval(
          simulationStartDate,
          simulationEndDate,
          profileLoadInput.getOperationTime
        )

      val targetEnergyConsumption = KilowattHours(3000d)

      "reach the targeted annual energy consumption" in {
        /* Test against a permissible deviation of 2 %. As per official documentation of the bdew load profiles
         * [https://www.bdew.de/media/documents/2000131_Anwendung-repraesentativen_Lastprofile-Step-by-step.pdf] 1.5 %
         * are officially permissible. But, as we currently do not take (bank) holidays into account, we cannot reach
         * this accuracy. */

        forAll(
          Table(
            "profile",
            BdewStandardLoadProfile.H0,
            BdewStandardLoadProfile.L0,
            BdewStandardLoadProfile.G0
          )
        ) { profile =>
          val dut = ProfileLoadModel(
            profileLoadInput.getUuid,
            profileLoadInput.getId,
            foreSeenOperationInterval,
            1.0d,
            QControl.apply(profileLoadInput.getqCharacteristics()),
            Kilowatts(
              profileLoadInput
                .getsRated()
                .to(PowerSystemUnits.KILOWATT)
                .getValue
                .doubleValue
            ),
            profileLoadInput.getCosPhiRated,
            profile,
            EnergyConsumption(targetEnergyConsumption)
          )
          dut.enable()

          calculateEnergyDiffForYear(
            dut,
            simulationStartDate,
            targetEnergyConsumption
          ) should be < Percent(2d)
        }
      }

      "correctly account for the scaling factor, when targeting a given annual energy consumption" in {
        val scalingFactor = 1.5
        val expectedEnergy = KilowattHours(4500d)

        val dut = ProfileLoadModel(
          profileLoadInput.getUuid,
          profileLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(profileLoadInput.getqCharacteristics()),
          Kilowatts(
            profileLoadInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue
          ),
          profileLoadInput.getCosPhiRated,
          BdewStandardLoadProfile.H0,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        calculateEnergyDiffForYear(
          dut,
          simulationStartDate,
          expectedEnergy
        ) should be < Percent(2d)
      }

      val targetMaximumPower = Watts(268.6)

      "approximately reach the maximum power" in {
        implicit val tolerance: Power = Watts(1d)

        forAll(
          Table(
            "profile",
            BdewStandardLoadProfile.H0,
            BdewStandardLoadProfile.L0,
            BdewStandardLoadProfile.G0
          )
        ) { profile =>
          val dut = ProfileLoadModel(
            profileLoadInput.getUuid,
            profileLoadInput.getId,
            foreSeenOperationInterval,
            1.0,
            QControl.apply(profileLoadInput.getqCharacteristics()),
            Kilowatts(
              profileLoadInput
                .getsRated()
                .to(PowerSystemUnits.KILOWATT)
                .getValue
                .doubleValue
            ),
            profileLoadInput.getCosPhiRated,
            profile,
            ActivePower(targetMaximumPower)
          )
          dut.enable()

          val maximumPower = calculatePowerForYear(
            dut,
            simulationStartDate
          ).maxOption.value

          maximumPower should approximate(targetMaximumPower)
        }
      }

      "correctly account for the scaling factor when targeting at maximum power" in {
        val scalingFactor = 1.5
        val expectedMaximum = Watts(402.0044899478780)

        val dut = ProfileLoadModel(
          profileLoadInput.getUuid,
          profileLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(profileLoadInput.getqCharacteristics()),
          Kilowatts(
            profileLoadInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue
          ),
          profileLoadInput.getCosPhiRated,
          BdewStandardLoadProfile.H0,
          ActivePower(targetMaximumPower)
        )
        dut.enable()

        val maximumPower = calculatePowerForYear(
          dut,
          simulationStartDate
        ).maxOption.value

        maximumPower should be < expectedMaximum
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
            -1
          ),
          new CosPhiFixed("cosPhiFixed:{(0.0,0.95)}"),
          BdewStandardLoadProfile.H0,
          false,
          Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR),
          Quantities.getQuantity(282.74d, PowerSystemUnits.VOLTAMPERE),
          0.95
        )
      val foreSeenOperationInterval =
        SystemComponent.determineOperationInterval(
          simulationStartDate,
          simulationEndDate,
          randomLoadInput.getOperationTime
        )

      val targetEnergyConsumption =
        KilowattHours(3000d)
      "reach the targeted annual energy consumption" in {
        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          1.0,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          Kilowatts(
            randomLoadInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue
          ),
          randomLoadInput.getCosPhiRated,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        calculateEnergyDiffForYear(
          dut,
          simulationStartDate,
          targetEnergyConsumption
        ) should be < Percent(1d)
      }

      "correctly account for the scaling factor, when targeting a given annual energy consumption" in {
        val scalingFactor = 1.5
        val expectedEnergy =
          KilowattHours(4500d)

        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          Kilowatts(
            randomLoadInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue
          ),
          randomLoadInput.getCosPhiRated,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        calculateEnergyDiffForYear(
          dut,
          simulationStartDate,
          expectedEnergy
        ) should be < Percent(2d)
      }

      val targetMaximumPower = Watts(268.6)
      "approximately reach the maximum power" in {
        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          1.0,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          Kilowatts(
            randomLoadInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue
          ),
          randomLoadInput.getCosPhiRated,
          ActivePower(targetMaximumPower)
        )
        dut.enable()

        val powers = calculatePowerForYear(
          dut,
          simulationStartDate
        ).toIndexedSeq.sorted.toArray

        val quantile95 = RandomLoadModelSpec.get95Quantile(powers)

        getRelativeDifference(
          quantile95,
          targetMaximumPower
        ) should be < Percent(1d)
      }

      "correctly account for the scaling factor when targeting at maximum power" in {
        val scalingFactor = 1.5
        val expectedMaximum = targetMaximumPower * scalingFactor

        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          Kilowatts(
            randomLoadInput
              .getsRated()
              .to(PowerSystemUnits.KILOWATT)
              .getValue
              .doubleValue
          ),
          randomLoadInput.getCosPhiRated,
          ActivePower(targetMaximumPower)
        )
        dut.enable()
        val powers = calculatePowerForYear(
          dut,
          simulationStartDate
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
      dut: LoadModel[C],
      simulationStartDate: ZonedDateTime,
      expectedEnergy: Energy
  ): Dimensionless = {
    val duration = Minutes(15d)

    val avgEnergy = calculatePowerForYear(
      dut: LoadModel[C],
      simulationStartDate: ZonedDateTime
    ).foldLeft(KilowattHours(0)) { case (energySum, power) =>
      energySum + (power * duration)
    }

    getRelativeDifference(
      avgEnergy,
      expectedEnergy
    )
  }

  def calculatePowerForYear[C <: LoadRelevantData](
      dut: LoadModel[C],
      simulationStartDate: ZonedDateTime
  ): Iterable[Power] = {
    val quarterHoursInYear = 365L * 96L

    (0L until quarterHoursInYear)
      .map { quarterHour =>
        val tick = quarterHour * 15 * 60
        val relevantData = createRelevantData(dut)(
          simulationStartDate.plus(quarterHour * 15, ChronoUnit.MINUTES)
        )

        dut
          .calculatePower(
            tick,
            Each(0d),
            relevantData
          )
          .p
      }
  }

  def createRelevantData[C <: LoadRelevantData](
      dut: LoadModel[C]
  ): ZonedDateTime => C =
    dut match {
      case _: RandomLoadModel  => RandomLoadModel.RandomRelevantData
      case _: ProfileLoadModel => ProfileLoadModel.ProfileRelevantData
    }

  def getRelativeDifference[Q <: Quantity[Q]](
      actualResult: Q,
      expectedResult: Q
  ): Dimensionless =
    Each((expectedResult - actualResult) / expectedResult)

}
