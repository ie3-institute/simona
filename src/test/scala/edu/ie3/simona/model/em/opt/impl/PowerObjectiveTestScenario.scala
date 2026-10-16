/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt.impl

import edu.ie3.simona.model.em.opt.FlexibilityOptimization.{
  OptimizationParams,
  TimeParams,
}
import edu.ie3.simona.model.em.opt.impl.CommonLossObjectiveFactory.{
  CommonLossVariant,
  LinearizedQuadraticPowerObjectiveFactory,
  MinAbsPowerObjectiveFactory,
  PeakShavingObjectiveFactory,
  PriceObjectiveFactory,
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.AssetEnergyBoundaries
import edu.ie3.simona.service.Data.SecondaryData.SecondarySeriesData
import edu.ie3.simona.test.common.OptimizingTestLike
import edu.ie3.util.interval.ClosedInterval
import edu.ie3.util.scala.quantities.DefaultQuantities.{onePU, zeroKWh}
import optimus.optimization.enums.SolverLib
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.{Hours, Time}

import java.util.UUID
import scala.collection.immutable.SortedMap

trait PowerObjectiveTestScenario extends OptimizingTestLike {

  protected val pvUUID: UUID = UUID.fromString("0-0-0-0-1")
  protected val loadUUID: UUID = UUID.fromString("0-0-0-0-2")
  protected val batUUID: UUID = UUID.fromString("0-0-0-0-3")
  protected val bat2UUID: UUID = UUID.fromString("0-0-0-0-4")

  /* SAMPLE TIME DEFINITIONS */

  protected val halfHour: Time = Hours(0.5)
  protected val halfHourTicks: Long = halfHour.toSeconds.toLong

  protected val hour: Time = Hours(1)
  protected val hourTicks: Long = hour.toSeconds.toLong

  protected val oneHalfHour: TimeParams = TimeParams(
    sampleTime = halfHour,
    predictionHorizon = halfHour,
    currentTick = 0L,
  )

  protected val fourHalfHours: TimeParams = TimeParams(
    sampleTime = halfHour,
    predictionHorizon = halfHour * 4,
    currentTick = 0L,
  )

  protected val twelveHalfHours: TimeParams = TimeParams(
    sampleTime = halfHour,
    predictionHorizon = halfHour * 12,
    currentTick = 0L,
  )

  protected val oneHour: TimeParams = TimeParams(
    sampleTime = hour,
    predictionHorizon = hour,
    currentTick = 0L,
  )

  protected val oneTwoHours: TimeParams = TimeParams(
    sampleTime = hour * 2,
    predictionHorizon = hour * 2,
    currentTick = 0L,
  )

  protected val threeHours: TimeParams = TimeParams(
    sampleTime = hour,
    predictionHorizon = hour * 3,
    currentTick = 0L,
  )

  protected val fourHours: TimeParams = TimeParams(
    sampleTime = hour,
    predictionHorizon = hour * 4,
    currentTick = 0L,
  )

  protected val eightHours: TimeParams = TimeParams(
    sampleTime = hour,
    predictionHorizon = hour * 8,
    currentTick = 0L,
  )

  /* BASIC FUNCTIONALITY TEST */
  // low efficiencies (e.g. 0.8) might not be realistic,
  // but simplify the testing

  private val batteryHalfFull: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(12),
        currentEnergy = KilowattHours(6),
        pMax = Kilowatts(10),
        etaCharge = Each(0.8),
        etaDischarge = Each(0.8),
        currentTick = 0L,
      )
    )

  // power sequence to be balanced out by battery
  // positive values are loads, negative values are feed-ins
  private val fixedLowAddPower: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(5, -10, 10, -2).toPowerMap(fourHalfHours)
      )
    )

  protected val paramsLowAddPower: OptimizationParams = OptimizationParams(
    flexOptionsById = Map(
      loadUUID -> fixedLowAddPower,
      batUUID -> batteryHalfFull,
    ),
    timeParams = fourHalfHours,
    objectiveFactory =
      MinAbsPowerObjectiveFactory(variant = CommonLossVariant.SoftConstraints),
    solverLib = SolverLib.oJSolver,
    tightenBoundaries = true,
  )

  // power sequence to be balanced out by battery
  // positive values are loads, negative values are feed-ins
  private val fixedHighAddPower: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(5, -60, 110, -2).toPowerMap(fourHalfHours)
      )
    )

  protected val paramsHighAddPower: OptimizationParams =
    paramsLowAddPower.copy(
      flexOptionsById = Map(
        loadUUID -> fixedHighAddPower,
        batUUID -> batteryHalfFull,
      )
    )

  // power sequence to be balanced out by battery
  // positive values are loads, negative values are feed-ins
  private val fixedHighAddEnergy: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(-10, -10, 10, 10).toPowerMap(fourHalfHours)
      )
    )

  protected val paramsHighAddEnergy: OptimizationParams =
    paramsLowAddPower.copy(
      flexOptionsById = Map(
        loadUUID -> fixedHighAddEnergy,
        batUUID -> batteryHalfFull,
      )
    )

  // power sequence to be balanced out by battery
  // positive values are loads, negative values are feed-ins
  private val fixedHighAddPowerAndEnergy: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(-10, -50, 20, 30).toPowerMap(fourHalfHours)
      )
    )

  protected val paramsHighAddPowerAndEnergy: OptimizationParams =
    paramsLowAddPower.copy(
      flexOptionsById = Map(
        loadUUID -> fixedHighAddPowerAndEnergy,
        batUUID -> batteryHalfFull,
      )
    )

  // power sequence to be balanced out by battery
  // positive values are loads, negative values are feed-ins
  private val fixedDischargeFirst: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(1, 1, -10, -10).toPowerMap(fourHalfHours)
      )
    )

  val paramsDischargeFirst: OptimizationParams =
    paramsLowAddPower.copy(
      flexOptionsById = Map(
        loadUUID -> fixedDischargeFirst,
        batUUID -> batteryHalfFull,
      )
    )

  /* DEMONSTRATIVE EXAMPLES */

  private val batteryDemoExample1: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(10),
        currentEnergy = KilowattHours(0),
        pMax = Kilowatts(10),
        etaCharge = Each(0.8),
        etaDischarge = Each(0.8),
        currentTick = 0L,
      )
    )

  private val fixedDischargeOneStep: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(-10, 0).toPowerMap(oneTwoHours)
      )
    )

  protected val paramsExcessLossOneStep: OptimizationParams =
    OptimizationParams(
      flexOptionsById = Map(
        loadUUID -> fixedDischargeOneStep,
        batUUID -> batteryDemoExample1,
      ),
      timeParams = oneTwoHours,
      objectiveFactory = PeakShavingObjectiveFactory(variant =
        CommonLossVariant.SoftConstraints
      ),
      solverLib = SolverLib.oJSolver,
      tightenBoundaries = false,
    )

  private val fixedDischargeFourSteps: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(-10, -10, -10, -10).toPowerMap(fourHours)
      )
    )

  protected val paramsExcessLossFourSteps: OptimizationParams =
    OptimizationParams(
      flexOptionsById = Map(
        loadUUID -> fixedDischargeFourSteps,
        batUUID -> batteryDemoExample1,
      ),
      timeParams = fourHours,
      objectiveFactory = PeakShavingObjectiveFactory(variant =
        CommonLossVariant.SoftConstraints
      ),
      solverLib = SolverLib.oJSolver,
      tightenBoundaries = false,
    )

  private val fixedLowDischargeFourSteps: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(-2, -2, -2, -2).toPowerMap(fourHours)
      )
    )

  protected val priceDataNegative: SecondarySeriesData =
    Seq((-0.1d, -0.05d), (-0.08d, -0.04d), (-0.06d, -0.03d), (0.01d, 0.01d))
      .toPriceData(fourHours)

  protected val paramsExcessLossPrices: OptimizationParams = OptimizationParams(
    flexOptionsById = Map(
      loadUUID -> fixedLowDischargeFourSteps,
      batUUID -> batteryDemoExample1,
    ),
    receivedData = Seq(priceDataNegative),
    timeParams = fourHours,
    objectiveFactory =
      PriceObjectiveFactory(variant = CommonLossVariant.SoftConstraints),
    solverLib = SolverLib.oJSolver,
    tightenBoundaries = false,
  )

  private val fixedDischargeStorageActivity: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(-10, -10, -10, -10, -10, -10, -10, -10).toPowerMap(eightHours)
      )
    )

  protected val paramsExcessLossStorageActivity: OptimizationParams =
    OptimizationParams(
      flexOptionsById = Map(
        loadUUID -> fixedDischargeStorageActivity,
        batUUID -> batteryDemoExample1,
        bat2UUID -> batteryDemoExample1,
      ),
      timeParams = eightHours,
      objectiveFactory = PeakShavingObjectiveFactory(variant =
        CommonLossVariant.SoftConstraints
      ),
      solverLib = SolverLib.oJSolver,
      tightenBoundaries = false,
    )

  /* MODEL WITH NO LOSS */

  // no losses, thus efficiency = 1
  private val batteryNoLoss: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(12),
        currentEnergy = KilowattHours(6),
        pMax = Kilowatts(10),
        etaCharge = onePU,
        etaDischarge = onePU,
        currentTick = 0L,
      )
    )

  protected val paramsNoLoss: OptimizationParams =
    paramsLowAddPower.copy(
      flexOptionsById = Map(
        loadUUID -> fixedLowAddPower,
        batUUID -> batteryNoLoss,
      )
    )

  /* TESTING DISCONNECTING ASSET */

  private val batteryAlmostHalfFull: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(12),
        currentEnergy = KilowattHours(5),
        pMax = Kilowatts(10),
        etaCharge = Each(0.8),
        etaDischarge = Each(0.8),
        currentTick = 0L,
      )
    )

  private val evHalfFull: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        currentEnergy = KilowattHours(5d),
        energyLimits = SortedMap(
          // half full in the beginning
          0L -> new ClosedInterval(
            zeroKWh,
            KilowattHours(10d),
          ),
          // we need to be 90% full when disconnecting
          3600L -> new ClosedInterval(
            KilowattHours(9d),
            KilowattHours(10d),
          ),
        ),
        powerLimits = ClosedInterval(Kilowatts(-11d), Kilowatts(11)),
        tickDisconnect = Some(3600L),
      )
    )

  // power sequence to be balanced out by battery
  // positive values are loads, negative values are feed-ins
  private val fixedAlternating: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(-4, -4, 8, -8).toPowerMap(fourHalfHours)
      )
    )

  protected val paramsEvcsDisconnect: OptimizationParams =
    paramsLowAddPower.copy(
      flexOptionsById = Map(
        loadUUID -> fixedAlternating,
        batUUID -> batteryAlmostHalfFull,
        bat2UUID -> evHalfFull,
      )
    )

  /* OBJECTIVE TESTS */

  protected val priceDataScenario1: SecondarySeriesData =
    (Seq.fill(2)((0.1d, 0.3d)) ++
      Seq.fill(6)((-0.02d, 0.2d)) ++
      Seq.fill(4)((0.1d, 0.3d))).toPriceData(twelveHalfHours)

  // 16.5 kWh of feed-in in total, more than battery can store
  protected val pvFlexScenario1: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(0, -6, -8, -7, -12, 0, 0, 0, 0, 0, 0, 0).toPowerMap(twelveHalfHours)
      )
    )

  // 18 kWh of load in total, more than battery can provide
  protected val loadFlexScenario1: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(0, 0, 0, 0, 0, 0, 8, 12, 4, 7, 5, 0).toPowerMap(twelveHalfHours)
      )
    )

  // low efficiency for simplicity of the test
  protected val batFlexScenario1: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(10),
        currentEnergy = KilowattHours(0),
        pMax = Kilowatts(10),
        etaCharge = Each(0.8),
        etaDischarge = Each(0.8),
        currentTick = 0L,
      )
    )

  protected val flexOptionsScenario1: Map[UUID, EnergyBoundariesFlexOptions] =
    Map(
      pvUUID -> pvFlexScenario1,
      loadUUID -> loadFlexScenario1,
      batUUID -> batFlexScenario1,
    )

  protected val paramsMinAbsPowerTest: OptimizationParams =
    OptimizationParams(
      flexOptionsById = flexOptionsScenario1,
      timeParams = twelveHalfHours,
      objectiveFactory = MinAbsPowerObjectiveFactory(variant =
        CommonLossVariant.SoftConstraints
      ),
      solverLib = SolverLib.oJSolver,
      tightenBoundaries = true,
    )

  protected val paramsLinQuadPowerTest: OptimizationParams =
    paramsMinAbsPowerTest.copy(
      objectiveFactory = LinearizedQuadraticPowerObjectiveFactory(
        variant = CommonLossVariant.SoftConstraints,
        // absolute total power is 22 kW,
        // thus pick segment count for 2 kW per segment
        segmentCount = 11,
      )
    )

  protected val paramsPriceObjectiveTest: OptimizationParams =
    paramsMinAbsPowerTest.copy(
      flexOptionsById = flexOptionsScenario1,
      receivedData = Seq(priceDataScenario1),
      objectiveFactory =
        PriceObjectiveFactory(variant = CommonLossVariant.SoftConstraints),
      solverLib = SolverLib.oJSolver,
      tightenBoundaries = true,
    )

  /* SOFT CONSTRAINTS TEST */

  // to produce the wrong results here, we need two things:
  // 1. transformed prices with absolute values below (1 - eta), with adapted eta here: ~0.781
  // 2. a negative price somewhere
  private val priceDataSoftConstraintsTest: SecondarySeriesData =
    Seq((0.1d, 0.21d), (-0.1d, 1d)).toPriceData(oneHalfHour)

  val paramsSoftConstraintsTest = OptimizationParams(
    flexOptionsById = Map(
      batUUID -> batteryHalfFull
    ),
    receivedData = Seq(priceDataSoftConstraintsTest),
    timeParams = oneHalfHour,
    objectiveFactory =
      PriceObjectiveFactory(variant = CommonLossVariant.SoftConstraints),
    solverLib = SolverLib.oJSolver,
    tightenBoundaries = true,
  )

  /* TWO BATTERIES */

  private val fixedForTwoBatteries: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        Seq(4, 4, 4, 14, -1, -4, -1, -14, -9.625, -2, 14, 0).toPowerMap(
          twelveHalfHours
        )
      )
    )

  // high storage capacity, low power
  private val batteryHighCap: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(10),
        currentEnergy = KilowattHours(10),
        pMax = Kilowatts(4),
        etaCharge = Each(0.8),
        etaDischarge = Each(0.8),
        currentTick = 0L,
      )
    )

  // low storage capacity, high power
  private val batteryLowCap: EnergyBoundariesFlexOptions =
    EnergyBoundariesFlexOptions(
      AssetEnergyBoundaries(
        eStorage = KilowattHours(6.25),
        currentEnergy = KilowattHours(6.25),
        pMax = Kilowatts(10),
        etaCharge = Each(0.8),
        etaDischarge = Each(0.8),
        currentTick = 0L,
      )
    )

  private val priceDataTwoBatteries: SecondarySeriesData =
    (Seq.fill(4)((0.1d, 0.2d)) ++
      Seq.fill(6)((-0.2d, -0.1)) ++
      Seq.fill(2)((0.05d, 0.15d))).toPriceData(twelveHalfHours)

  protected val paramsTwoBatteries: OptimizationParams =
    paramsPriceObjectiveTest.copy(
      flexOptionsById = Map(
        loadUUID -> fixedForTwoBatteries,
        batUUID -> batteryHighCap,
        bat2UUID -> batteryLowCap,
      ),
      receivedData = Seq(priceDataTwoBatteries),
    )

}
