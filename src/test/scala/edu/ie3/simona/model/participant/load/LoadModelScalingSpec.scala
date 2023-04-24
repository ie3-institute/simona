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
import edu.ie3.simona.test.common.TestTags.SnailTest
import edu.ie3.simona.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import org.scalatest.prop.TableDrivenPropertyChecks
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import java.util.UUID
import javax.measure.quantity.{Dimensionless, Energy}

class LoadModelScalingSpec extends UnitSpec with TableDrivenPropertyChecks {
  "Testing correct scaling of load models" when {
    val simulationStartDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-01-01 00:00:00")
    val simulationEndDate =
      TimeUtil.withDefaults.toZonedDateTime("2019-12-31 23:59:00")
    val testingTolerance = 1e-6 // Equals to 1 W power

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
        Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR)
      "reach the targeted annual energy consumption" taggedAs SnailTest in {
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
            1.0,
            QControl.apply(profileLoadInput.getqCharacteristics()),
            profileLoadInput.getsRated(),
            profileLoadInput.getCosPhiRated,
            profile,
            EnergyConsumption(targetEnergyConsumption)
          )
          dut.enable()

          val relevantDatas = (0 until 35040)
            .map(tick =>
              tick -> ProfileRelevantData(
                simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
              )
            )
            .toMap

          val totalRuns = 10
          val avgEnergy = (0 until totalRuns)
            .map { _ =>
              relevantDatas
                .map { case (tick, relevantData) =>
                  dut
                    .calculatePower(
                      tick,
                      Quantities.getQuantity(0d, PowerSystemUnits.PU),
                      relevantData
                    )
                    .p
                    .multiply(Quantities.getQuantity(15d, Units.MINUTE))
                    .asType(classOf[Energy])
                    .to(PowerSystemUnits.KILOWATTHOUR)
                }
                .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
                  _.add(_)
                )
            }
            .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
              _.add(_)
            )
            .divide(totalRuns)

          Quantities
            .getQuantity(100, Units.PERCENT)
            .subtract(
              Quantities.getQuantity(
                abs(
                  avgEnergy
                    .divide(targetEnergyConsumption)
                    .asType(classOf[Dimensionless])
                    .to(Units.PERCENT)
                    .getValue
                    .doubleValue()
                ),
                Units.PERCENT
              )
            ) should beLessThanWithTolerance(
            Quantities.getQuantity(2d, Units.PERCENT),
            1e-1
          )
        }
      }

      "correctly account for the scaling factor, when targeting a given annual energy consumption" taggedAs SnailTest in {
        val scalingFactor = 1.5
        val expectedEnergy =
          Quantities.getQuantity(4500d, PowerSystemUnits.KILOWATTHOUR)

        val dut = ProfileLoadModel(
          profileLoadInput.getUuid,
          profileLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(profileLoadInput.getqCharacteristics()),
          profileLoadInput.getsRated(),
          profileLoadInput.getCosPhiRated,
          BdewStandardLoadProfile.H0,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        calculateAverageEnergy(
          dut.asInstanceOf[LoadModel[LoadRelevantData]],
          simulationStartDate,
          expectedEnergy
        ) should beLessThanWithTolerance(
          Quantities.getQuantity(2d, Units.PERCENT),
          1e-1
        )
      }

      val targetMaximumPower =
        Quantities.getQuantity(268.6, Units.WATT)
      "approximately reach the maximum power" taggedAs SnailTest in {

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
            profileLoadInput.getsRated(),
            profileLoadInput.getCosPhiRated,
            profile,
            ActivePower(targetMaximumPower)
          )
          dut.enable()
          val relevantDatas = (0 until 35040)
            .map(tick =>
              tick -> ProfileRelevantData(
                simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
              )
            )
            .toMap

          val totalRuns = 10
          (0 until totalRuns).flatMap { _ =>
            relevantDatas
              .map { case (tick, relevantData) =>
                dut
                  .calculatePower(
                    tick,
                    Quantities.getQuantity(0d, PowerSystemUnits.PU),
                    relevantData
                  )
                  .p
              }
          }.maxOption match {
            case Some(maximumPower) =>
              maximumPower should equalWithTolerance(
                targetMaximumPower.to(PowerSystemUnits.MEGAWATT),
                testingTolerance
              )
            case None => fail("Unable to determine maximum power.")
          }
        }
      }

      "correctly account for the scaling factor when targeting at maximum power" taggedAs SnailTest in {
        val scalingFactor = 1.5
        val expectedMaximum =
          Quantities.getQuantity(402.0044899478780, Units.WATT)

        val dut = ProfileLoadModel(
          profileLoadInput.getUuid,
          profileLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(profileLoadInput.getqCharacteristics()),
          profileLoadInput.getsRated(),
          profileLoadInput.getCosPhiRated,
          BdewStandardLoadProfile.H0,
          ActivePower(targetMaximumPower)
        )
        dut.enable()
        val relevantDatas = (0 until 35040)
          .map(tick =>
            tick -> ProfileRelevantData(
              simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
            )
          )
          .toMap

        val totalRuns = 10
        (0 until totalRuns).flatMap { _ =>
          relevantDatas
            .map { case (tick, relevantData) =>
              dut
                .calculatePower(
                  tick,
                  Quantities.getQuantity(0d, PowerSystemUnits.PU),
                  relevantData
                )
                .p
            }
        }.maxOption match {
          case Some(maximumPower) =>
            maximumPower should equalWithTolerance(
              expectedMaximum.to(PowerSystemUnits.MEGAWATT),
              testingTolerance
            )
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
        Quantities.getQuantity(3000d, PowerSystemUnits.KILOWATTHOUR)
      "reach the targeted annual energy consumption" taggedAs SnailTest in {
        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          1.0,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          randomLoadInput.getsRated(),
          randomLoadInput.getCosPhiRated,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()

        val relevantDatas = (0 until 35040)
          .map(tick =>
            tick -> RandomLoadModel.RandomRelevantData(
              simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
            )
          )
          .toMap

        val totalRuns = 10
        val avgEnergy = (0 until totalRuns)
          .map { _ =>
            relevantDatas
              .map { case (tick, relevantData) =>
                dut
                  .calculatePower(
                    tick,
                    Quantities.getQuantity(0d, PowerSystemUnits.PU),
                    relevantData
                  )
                  .p
                  .multiply(Quantities.getQuantity(15d, Units.MINUTE))
                  .asType(classOf[Energy])
                  .to(PowerSystemUnits.KILOWATTHOUR)
              }
              .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
                _.add(_)
              )
          }
          .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
            _.add(_)
          )
          .divide(totalRuns)

        Quantities
          .getQuantity(100, Units.PERCENT)
          .subtract(
            Quantities.getQuantity(
              abs(
                avgEnergy
                  .divide(targetEnergyConsumption)
                  .asType(classOf[Dimensionless])
                  .to(Units.PERCENT)
                  .getValue
                  .doubleValue()
              ),
              Units.PERCENT
            )
          ) should beLessThanWithTolerance(
          Quantities.getQuantity(1d, Units.PERCENT),
          1e-1
        )
      }

      "correctly account for the scaling factor, when targeting a given annual energy consumption" taggedAs SnailTest in {
        val scalingFactor = 1.5
        val expectedEnergy =
          Quantities.getQuantity(4500d, PowerSystemUnits.KILOWATTHOUR)

        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          randomLoadInput.getsRated(),
          randomLoadInput.getCosPhiRated,
          EnergyConsumption(targetEnergyConsumption)
        )
        dut.enable()
        val relevantDatas = (0 until 35040)
          .map(tick =>
            tick -> RandomLoadModel.RandomRelevantData(
              simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
            )
          )
          .toMap

        val totalRuns = 10
        val avgEnergy = (0 until totalRuns)
          .map { _ =>
            relevantDatas
              .map { case (tick, relevantData) =>
                dut
                  .calculatePower(
                    tick,
                    Quantities.getQuantity(0d, PowerSystemUnits.PU),
                    relevantData
                  )
                  .p
                  .multiply(Quantities.getQuantity(15d, Units.MINUTE))
                  .asType(classOf[Energy])
                  .to(PowerSystemUnits.KILOWATTHOUR)
              }
              .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
                _.add(_)
              )
          }
          .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
            _.add(_)
          )
          .divide(totalRuns)

        Quantities
          .getQuantity(100, Units.PERCENT)
          .subtract(
            Quantities.getQuantity(
              abs(
                avgEnergy
                  .divide(expectedEnergy)
                  .asType(classOf[Dimensionless])
                  .to(Units.PERCENT)
                  .getValue
                  .doubleValue()
              ),
              Units.PERCENT
            )
          ) should beLessThanWithTolerance(
          Quantities.getQuantity(2d, Units.PERCENT),
          1e-1
        )
      }

      val targetMaximumPower = Quantities.getQuantity(268.6, Units.WATT)
      "approximately reach the maximum power" taggedAs SnailTest in {
        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          1.0,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          randomLoadInput.getsRated(),
          randomLoadInput.getCosPhiRated,
          ActivePower(targetMaximumPower)
        )
        dut.enable()

        val relevantDatas = (0 until 35040)
          .map(tick =>
            tick -> RandomLoadModel.RandomRelevantData(
              simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
            )
          )
          .toMap

        val totalRuns = 10
        val powers = (0 until totalRuns)
          .flatMap { _ =>
            relevantDatas
              .map { case (tick, relevantData) =>
                dut
                  .calculatePower(
                    tick,
                    Quantities.getQuantity(0d, PowerSystemUnits.PU),
                    relevantData
                  )
                  .p
              }
          }
          .sorted
          .toArray

        val quantile95 = RandomLoadModelSpec.get95Quantile(powers)

        Quantities.getQuantity(
          abs(
            100 -
              quantile95
                .divide(targetMaximumPower)
                .asType(classOf[Dimensionless])
                .to(Units.PERCENT)
                .getValue
                .doubleValue()
          ),
          Units.PERCENT
        ) should beLessThanWithTolerance(
          Quantities.getQuantity(1d, Units.PERCENT),
          1e-1
        )
      }

      "correctly account for the scaling factor when targeting at maximum power" taggedAs SnailTest in {
        val scalingFactor = 1.5
        val expectedMaximum = targetMaximumPower.multiply(scalingFactor)

        val dut = RandomLoadModel(
          randomLoadInput.getUuid,
          randomLoadInput.getId,
          foreSeenOperationInterval,
          scalingFactor,
          QControl.apply(randomLoadInput.getqCharacteristics()),
          randomLoadInput.getsRated(),
          randomLoadInput.getCosPhiRated,
          ActivePower(targetMaximumPower)
        )
        dut.enable()
        val relevantDatas = (0 until 35040)
          .map(tick =>
            tick -> RandomLoadModel.RandomRelevantData(
              simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
            )
          )
          .toMap

        val totalRuns = 10
        val powers = (0 until totalRuns)
          .flatMap { _ =>
            relevantDatas
              .map { case (tick, relevantData) =>
                dut
                  .calculatePower(
                    tick,
                    Quantities.getQuantity(1d, PowerSystemUnits.PU),
                    relevantData
                  )
                  .p
              }
          }
          .sorted
          .toArray
        /* Tolerance is equivalent to 10 W difference between the 95%-percentile of the obtained random results and the
         * target maximum power. Because of the stochastic nature, the maximum power cannot be met perfectly */
        RandomLoadModelSpec.get95Quantile(powers) should equalWithTolerance(
          expectedMaximum.to(PowerSystemUnits.MEGAWATT),
          1e-5
        )
      }
    }
  }

  def getRelevantDatas[T <: LoadModel[LoadRelevantData]](
      dut: LoadModel[LoadRelevantData],
      simulationStartDate: ZonedDateTime
  ): Map[Int, LoadRelevantData] = {

    if (dut.isInstanceOf[ProfileLoadModel]) {
      val relevantDatas = (0 until 35040)
        .map(tick =>
          tick -> ProfileLoadModel.ProfileRelevantData(
            simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
          )
        )
        .toMap
      return relevantDatas
    }

    if (dut.isInstanceOf[RandomLoadModel]) {
      val relevantDatas = (0 until 35040)
        .map(tick =>
          tick -> RandomLoadModel.RandomRelevantData(
            simulationStartDate.plus(tick * 15, ChronoUnit.MINUTES)
          )
        )
        .toMap
      relevantDatas
    } else {
      throw new IllegalArgumentException("Unknown load model type")
    }
  }

  def calculateAverageEnergy[T <: LoadModel[ProfileRelevantData]](
      dut: LoadModel[LoadRelevantData],
      simulationStartDate: ZonedDateTime,
      expectedEnergy: ComparableQuantity[Energy]
  ): ComparableQuantity[Dimensionless] = {

    val relevantDatas = getRelevantDatas(dut, simulationStartDate)

    val totalRuns = 10
    val avgEnergy = (0 until totalRuns)
      .map { _ =>
        relevantDatas
          .map { case (tick, relevantData) =>
            dut
              .calculatePower(
                tick,
                Quantities.getQuantity(0d, PowerSystemUnits.PU),
                relevantData
              )
              .p
              .multiply(Quantities.getQuantity(15d, Units.MINUTE))
              .asType(classOf[Energy])
              .to(PowerSystemUnits.KILOWATTHOUR)
          }
          .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
            _.add(_)
          )
      }
      .fold(Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR))(
        _.add(_)
      )
      .divide(totalRuns)

    val result = Quantities
      .getQuantity(100, Units.PERCENT)
      .subtract(
        Quantities.getQuantity(
          abs(
            avgEnergy
              .divide(expectedEnergy)
              .asType(classOf[Dimensionless])
              .to(Units.PERCENT)
              .getValue
              .doubleValue()
          ),
          Units.PERCENT
        )
      )
    result
  }

}
