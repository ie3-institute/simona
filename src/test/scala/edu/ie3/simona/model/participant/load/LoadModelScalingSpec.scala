/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant.load

import breeze.numerics.abs
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
import edu.ie3.simona.model.participant.load.profile.ProfileLoadModel.ProfileRelevantData
import edu.ie3.simona.model.participant.load.random.RandomLoadModel
import edu.ie3.simona.model.participant.load.random.RandomLoadModel.RandomRelevantData
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import squants.energy.{KilowattHours, Kilowatts, Watts}
import squants.time.Minutes
import squants.{Dimensionless, Each, Energy, Percent, Power}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID

class LoadModelScalingSpec extends UnitSpec with TableDrivenPropertyChecks {
  implicit val tolerance: Dimensionless = Each(1e-10)

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

      val targetEnergyConsumption =
        KilowattHours(3000d)
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
                .doubleValue()
            ),
            profileLoadInput.getCosPhiRated,
            profile,
            EnergyConsumption(targetEnergyConsumption)
          )
          dut.enable()

          calculateAverageEnergy[ProfileRelevantData](
            dut,
            simulationStartDate,
            targetEnergyConsumption
          ) =~ Percent(2d)
        }
      }

      "correctly account for the scaling factor, when targeting a given annual energy consumption" in {
        val scalingFactor = 1.5
        val expectedEnergy =
          KilowattHours(4500d)

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
              .doubleValue()
          ),
          profileLoadInput.getCosPhiRated,
          BdewStandardLoadProfile.H0,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        calculateAverageEnergy[ProfileRelevantData](
          dut,
          simulationStartDate,
          expectedEnergy
        ) =~ Percent(2d)
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
                .doubleValue()
            ),
            profileLoadInput.getCosPhiRated,
            profile,
            ActivePower(targetMaximumPower)
          )
          dut.enable()

          calculatePowerFromRelevantData[ProfileRelevantData](
            simulationStartDate,
            dut
          ).maxOption match {
            case Some(maximumPower) =>
              maximumPower =~ targetMaximumPower
            case None => fail("Unable to determine maximum power.")
          }
        }
      }

      "correctly account for the scaling factor when targeting at maximum power" in {
        val scalingFactor = 1.5
        val expectedMaximum =
          Watts(402.0044899478780)
        implicit val tolerance: Power = Watts(1d)
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
              .doubleValue()
          ),
          profileLoadInput.getCosPhiRated,
          BdewStandardLoadProfile.H0,
          ActivePower(targetMaximumPower)
        )
        dut.enable()

        calculatePowerFromRelevantData[ProfileRelevantData](
          simulationStartDate,
          dut
        ).maxOption match {
          case Some(maximumPower) =>
            maximumPower =~ expectedMaximum

          case None => fail("Unable to determine maximum power.")
        }
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
              .doubleValue()
          ),
          randomLoadInput.getCosPhiRated,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        calculateAverageEnergy[RandomRelevantData](
          dut,
          simulationStartDate,
          targetEnergyConsumption
        ) =~ Percent(1d)
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
              .doubleValue()
          ),
          randomLoadInput.getCosPhiRated,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        calculateAverageEnergy[RandomRelevantData](
          dut,
          simulationStartDate,
          expectedEnergy
        ) =~ Percent(2d)
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
              .doubleValue()
          ),
          randomLoadInput.getCosPhiRated,
          ActivePower(targetMaximumPower)
        )
        dut.enable()

        val powers =
          calculatePowerFromRelevantData[RandomRelevantData](
            simulationStartDate,
            dut
          ).sorted.toArray

        val quantile95 = RandomLoadModelSpec.get95Quantile(powers)

        getRelativeResult(
          quantile95,
          targetMaximumPower
        ) =~ Percent(1d)

      }

      "correctly account for the scaling factor when targeting at maximum power" in {
        val scalingFactor = 1.5
        val expectedMaximum = targetMaximumPower * scalingFactor
        implicit val tolerance: Power = Watts(1d)
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
              .doubleValue()
          ),
          randomLoadInput.getCosPhiRated,
          ActivePower(targetMaximumPower)
        )
        dut.enable()
        val powers =
          calculatePowerFromRelevantData[RandomRelevantData](
            simulationStartDate,
            dut
          ).sorted.toArray
        /* Tolerance is equivalent to 10 W difference between the 95%-percentile of the obtained random results and the
         * target maximum power. Because of the stochastic nature, the maximum power cannot be met perfectly */
        RandomLoadModelSpec.get95Quantile(powers) =~ expectedMaximum
      }
    }
  }

  def getRelativeResult[Q <: squants.Quantity[Q]](
      avgResult: Q,
      expectedResult: Q
  ): Dimensionless = {
    val result = Percent(100) -
      Percent(abs(avgResult.divide(expectedResult)))
    Percent(abs(result.value.doubleValue()))
  }

  def getRelevantData[
      C <: LoadRelevantData
  ](dut: LoadModel[C], simulationStartDate: ZonedDateTime): Map[Long, C] = {
    dut match {
      case _: RandomLoadModel =>
        (0L until 35040)
          .map(tick =>
            tick -> RandomLoadModel.RandomRelevantData(
              simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
            )
          )
          .toMap[Long, C]
      case _: ProfileLoadModel =>
        (0L until 35040)
          .map(tick =>
            tick -> ProfileLoadModel
              .ProfileRelevantData(
                simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
              )
          )
          .toMap[Long, C]
    }
  }

  def calculateAverageEnergy[C <: LoadRelevantData](
      dut: LoadModel[C],
      simulationStartDate: ZonedDateTime,
      expectedEnergy: Energy
  ): Dimensionless = {

    val relevantData = getRelevantData(dut, simulationStartDate)

    val totalRuns = 10
    val avgEnergy = (0 until totalRuns)
      .map { _ =>
        relevantData
          .map { case (tick, relevantData) =>
            dut
              .calculatePower(
                tick,
                Each(0d),
                relevantData
              )
              .p * Minutes(15d)
          }
          .fold(KilowattHours(0))(
            _ + _
          )
      }
      .fold(KilowattHours(0d))(
        _ + _
      )
      .divide(totalRuns)

    getRelativeResult(
      avgEnergy,
      expectedEnergy
    )

  }

  def calculatePowerFromRelevantData[C <: LoadRelevantData](
      simulationStartDate: ZonedDateTime,
      dut: LoadModel[C]
  ): IndexedSeq[Power] = {

    val relevantData = getRelevantData(dut, simulationStartDate)

    val totalRuns = 10
    (0 until totalRuns)
      .flatMap { _ =>
        relevantData
          .map { case (tick, relevantData) =>
            dut
              .calculatePower(
                tick,
                Each(0d),
                relevantData
              )
              .p
          }
      }
  }
}
