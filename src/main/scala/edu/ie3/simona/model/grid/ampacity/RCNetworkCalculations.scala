/*
 * © 2026. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.grid.ampacity

import org.apache.commons.math3.linear.{
  Array2DRowRealMatrix,
  ArrayRealVector,
  LUDecomposition,
  RealMatrix,
  RealVector,
}
import com.typesafe.scalalogging.LazyLogging
import edu.ie3.simona.model.grid.ampacity.LineSegmentThermalModel.LineState
import edu.ie3.simona.model.grid.ampacity.LineThermalModelCalculations.{
  LineTemperatures,
  TWO_PI,
}
import edu.ie3.simona.model.grid.ampacity.SoilTypeExtensions.*
import edu.ie3.simona.model.grid.ampacity.SoilThermalResistanceCalculations.{
  EnvironmentalCell,
  calcCableBedEnvironmentalCells,
  calcCableBedEquivalentRadius,
  calcCableBedLossWeightFactor,
  calcCableBedT4OuterCorrection,
  calcCableBedT4Split,
  calcSoilModelOuterRadius,
  calcThermalResistanceToSoilNonUniform,
  calcThermalResistanceToSoilThreeSingleCoreFlatFormationDistance,
  calcThermalResistanceToSoilThreeSingleCoreFlatFormationTouching,
  calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching,
}
import edu.ie3.util.scala.quantities.QuantityConversionUtils.*
import edu.ie3.util.quantities.electro.{OhmsPerMeter, SpecificResistance}
import edu.ie3.util.quantities.thermal.{
  KelvinMetersPerWatt,
  ThermalCapacityPerLength,
}
import squants.electro.*
import squants.space.{Length, Meters, Millimeters}
import squants.thermal.Celsius
import squants.time.{Frequency, Hertz}
import squants.{ElectricCurrent, Kelvin, Temperature}
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.CELSIUS

import scala.math.*

/** A collection of methods for building and solving the thermoelectric RC
  * network used for short-duration transient temperature calculations.
  */
object RCNetworkCalculations extends LazyLogging {

  /** Determines the Van-Wormer-Coefficient for long-duration transients.
    * @param diameterDielectric
    *   The inner diameter of this layer.
    * @param diameterConductor
    *   The outer diameter of this layer.
    * @return
    *   Van-Wormer coefficient as Double.
    */
  def vanWormerCoefficientLongDurationTransients(
      diameterDielectric: Length,
      diameterConductor: Length,
  ): Double = {
    val diameterRatio = diameterDielectric / diameterConductor
    (1 / (2 * log(diameterRatio))) - (1 / (pow(diameterRatio, 2) - 1))
  }

  /** Determines the Van-Wormer-Coefficient for short-duration transients.
    * @param innerDiameter
    *   The inner diameter of this layer.
    * @param outerDiameter
    *   The outer diameter of this layer.
    * @return
    *   Van-Wormer coefficient as Double.
    */
  def vanWormerCoefficientShortTermDurationTransients(
      innerDiameter: Length,
      outerDiameter: Length,
  ): Double = {
    val diameterRatio = outerDiameter / innerDiameter
    (1 / log(diameterRatio)) - (1 / (diameterRatio - 1))
  }

  /** Splits a given capacitance in case of short-term durations by the method
    * of van-Wormer through its coefficient.
    */
  private def splitCapacitanceByVanWormerShortDuration(
      thermalCapacityPerLength: ThermalCapacityPerLength,
      innerDiameter: Length,
      outerDiameter: Length,
  ) = {
    val diameterMid = innerDiameter + (outerDiameter - innerDiameter) / 2
    val vanWormerJackFirstHalf =
      vanWormerCoefficientShortTermDurationTransients(
        innerDiameter,
        diameterMid,
      )
    val capacitanceC11 = thermalCapacityPerLength * vanWormerJackFirstHalf
    val capacitanceC12 = thermalCapacityPerLength * (1 - vanWormerJackFirstHalf)
    val vanWormerJackSecondHalf =
      vanWormerCoefficientShortTermDurationTransients(
        diameterMid,
        outerDiameter,
      )
    val capacitanceC21 = thermalCapacityPerLength * vanWormerJackSecondHalf
    val capacitanceC22 =
      thermalCapacityPerLength * (1 - vanWormerJackSecondHalf)
    (capacitanceC11, capacitanceC12, capacitanceC21, capacitanceC22)
  }

  def createAndCalcRCNetworkMvCableShortDuration(
      tick: Long,
      state: LineState,
      lineCurrent: ElectricCurrent,
      groundTemperature: Temperature,
  ): LineTemperatures = {
    val currentLineModel = state.currentLineSegmentThermalModel
    val cableSetup = state.cableSetup

    val temperatureDifference = Quantities.getQuantity(
      (state.lineTemperatures.currentLineTemp4 - groundTemperature).toCelsiusDegrees,
      CELSIUS,
    )

    val effectiveSoilResistivity =
      currentLineModel.soilType
        .currentThermalResistivity(temperatureDifference)
        .toSquants

    val (bedResistivity, cableBed) = currentLineModel.cableBed match {
      case Some(bed) =>
        val bedRes =
          bed.soilType
            .currentThermalResistivity(temperatureDifference)
            .toSquants
        (Some(bedRes), Some(bed))
      case None => (None, None)
    }

    val baseSoilResistivity =
      bedResistivity.getOrElse(effectiveSoilResistivity)

    // TODO DF check this

    val soilCapacitance = currentLineModel.thermalCapacityCe

    val acResistance = CableLossCalculations.calcAcResistance(
      cableSetup.specificElectricResistance,
      cableSetup.conductor.material.getElectricalResistivityTemperatureCoefficient,
      cableSetup,
      currentLineModel.distanceCables,
    )

    val conductorLosses =
      CableLossCalculations.calcLossesConductor(acResistance, lineCurrent)

    val phaseToGroundVoltage = state.currentVoltage / sqrt(3)

    val dielectricLosses = CableLossCalculations.calcDielectricLosses(
      phaseToGroundVoltage,
      cableSetup.frequency,
      cableSetup.tanDelta,
      cableSetup.electricCapacitance,
    )

    val (acResistanceSheath, sheatAverageDiameter) =
      cableSetup.screenLayer match {
        case Some(layer) =>
          val resistance = CableLossCalculations.calcAcResistanceSheath(
            cableSetup.screenLayer,
            cableSetup.limitTemperature,
            state.currentLineSegmentThermalModel.thermalResistanceT1,
            conductorLosses,
            dielectricLosses,
          )
          val diameter = (layer.innerDiameter + layer.outerDiameter) / 2
          (resistance, diameter)

        case None => (OhmsPerMeter(0d), Millimeters(0d))
      }

    val sheathLossesLeadingPhase = CableLossCalculations.calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      currentLineModel.distanceCables,
      sheatAverageDiameter,
      "leading",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )
    val sheathLossesMiddlePhase = CableLossCalculations.calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      currentLineModel.distanceCables,
      sheatAverageDiameter,
      "middle",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )
    val sheathLossesLaggingPhase = CableLossCalculations.calcLossesSheath(
      cableSetup.layoutFormation,
      acResistance,
      acResistanceSheath,
      currentLineModel.distanceCables,
      sheatAverageDiameter,
      "lagging",
      cableSetup.eddyCurrentsLossFactorScreen,
      conductorLosses,
    )

    val thermalTotalLossesLeadingCable =
      conductorLosses + dielectricLosses + sheathLossesLeadingPhase
    val thermalTotalLossesMiddleCable =
      conductorLosses + dielectricLosses + sheathLossesMiddlePhase
    val thermalTotalLossesLaggingCable =
      conductorLosses + dielectricLosses + sheathLossesLaggingPhase

    /* Update thermoelectric equivalent circuit */
    // No changes for T1-T3
    val t1 = currentLineModel.thermalResistanceT1
    val t2 = currentLineModel.thermalResistanceT2
    val t3 = currentLineModel.thermalResistanceT3

    // Sanity Check t2 should be zero, else some simplifications further down are not possible.
    if t2 != KelvinMetersPerWatt(0) then
      throw new IllegalStateException(
        s"Unexpected value for thermal-electric resistance T2, which should be zero."
      )

    // T4 changes since it depends on losses from neighbouring cables
    val t4Base = cableSetup.layoutFormation match {
      case LayoutFormation.FlatDistance =>
        calcThermalResistanceToSoilThreeSingleCoreFlatFormationDistance(
          baseSoilResistivity,
          currentLineModel.depthCables,
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
          currentLineModel.distanceCables,
          thermalTotalLossesLeadingCable,
          thermalTotalLossesMiddleCable,
          thermalTotalLossesLaggingCable,
        )
      case LayoutFormation.FlatTouching =>
        calcThermalResistanceToSoilThreeSingleCoreFlatFormationTouching(
          baseSoilResistivity,
          currentLineModel.depthCables,
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
        ) // FIXME DF add tests!
      case LayoutFormation.TrefoilNotTouching => // FIXME DF Check for Trefoil
        throw new IllegalArgumentException(
          s"Trefoil not touching layout formation is currently not supported"
        )
      case LayoutFormation.TrefoilTouching =>
        calcThermalResistanceToSoilThreeSingleCoreTrefoilTouching(
          baseSoilResistivity,
          currentLineModel.depthCables,
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
        )
    }

    val t4 = cableBed match {
      case Some(bed) =>
        val bedRes =
          bed.soilType
            .currentThermalResistivity(temperatureDifference)
            .toSquants
        val bedRadius = calcCableBedEquivalentRadius(bed.width, bed.height)
        val bedCenterDepth = Meters(-currentLineModel.depthCables.toMeters)
        val lossWeightFactor = calcCableBedLossWeightFactor(
          cableSetup.layoutFormation,
          thermalTotalLossesLeadingCable,
          thermalTotalLossesMiddleCable,
          thermalTotalLossesLaggingCable,
        )
        val correction = calcCableBedT4OuterCorrection(
          effectiveSoilResistivity,
          bedRes,
          bedCenterDepth,
          bedRadius,
          lossWeightFactor,
        )
        t4Base + correction
      case None =>
        calcThermalResistanceToSoilNonUniform(
          cableSetup.layoutFormation,
          currentLineModel.depthCables,
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected but not found for thermal resistance to soil calculation"
              )
            ),
          currentLineModel.distanceCables,
          thermalTotalLossesLeadingCable,
          thermalTotalLossesMiddleCable,
          thermalTotalLossesLaggingCable,
          currentLineModel.soilLayers,
          temperatureDifference,
        ).getOrElse(t4Base)
    }

    /* Split Capacitance with van-Wormer Coefficient */
    val (
      dielectricThermCapacitanceC11,
      dielectricThermCapacitanceC12,
      dielectricThermCapacitanceC21,
      dielectricThermCapacitanceC22,
    ) =
      splitCapacitanceByVanWormerShortDuration(
        currentLineModel.thermalCapacityCd,
        cableSetup.layersIsolationElements
          .map(_.innerDiameter)
          .minOption
          .getOrElse(
            throw new IllegalArgumentException("No isolation layers found")
          ),
        cableSetup.layersIsolationElements
          .map(_.outerDiameter)
          .maxOption
          .getOrElse(
            throw new IllegalArgumentException("No isolation layers found")
          ),
      )

    val (
      jackThermCapacitanceC11,
      jackThermCapacitanceC12,
      jackThermCapacitanceC21,
      jackThermCapacitanceC22,
    ) =
      splitCapacitanceByVanWormerShortDuration(
        currentLineModel.thermalCapacityCj,
        cableSetup.layersJackElements
          .map(_.innerDiameter)
          .minOption
          .getOrElse(
            throw new IllegalArgumentException(
              "No jack layers found for van-Wormer capacitance splitting"
            )
          ),
        cableSetup.layersJackElements
          .map(_.outerDiameter)
          .maxOption
          .getOrElse(
            throw new IllegalArgumentException(
              "No jack layers found for van-Wormer capacitance splitting"
            )
          ),
      )

    /* Build the short-duration RC network. */
    // Van-Wormer splitting replaces each cable-shell resistance with two half-resistances.
    val g1 = 2d / t1.toKelvinMetersPerWatt
    val g2 = 2d / t1.toKelvinMetersPerWatt
    val g3 = 2d / t3.toKelvinMetersPerWatt
    val g4 = 2d / t3.toKelvinMetersPerWatt

    // the RC-Network can be simplified since all parallel capacitance can be merged
    // Conductor capacitance and first part of the first half of the dielectric
    val c1 =
      (currentLineModel.thermalCapacityCc + dielectricThermCapacitanceC11).toJoulesPerMeterKelvin // FIXME DF check conversion factor !
        // Capacitance of the second part of first half of the dielectric + first part of the second half of the dielectric
    val c2 =
      (dielectricThermCapacitanceC12 + dielectricThermCapacitanceC21).toJoulesPerMeterKelvin
    // Capacitance of the second part of second half of the dielectric + the sheath + the first part of the first half of the jack
    val c3 =
      (dielectricThermCapacitanceC22 + currentLineModel.thermalCapacityCs + jackThermCapacitanceC11).toJoulesPerMeterKelvin
    // Capacitance of the second part of first half of the jack + first part of the second half of the jack
    val c4 =
      (jackThermCapacitanceC12 + jackThermCapacitanceC21).toJoulesPerMeterKelvin

    val (
      nodeCount,
      matrixAData,
      vectorBData,
    ) = cableBed match {
      case None =>
        // 5-node network: cable nodes 1-4 and node 5 bundling the cable outer
        // surface (jack) with the outer soil. T4 connects node 5 to ground.
        val g5 = 1d / t4.toKelvinMetersPerWatt
        // Capacitance of the second part of second half of the jack + the capacitance of the soil
        val c5 =
          (jackThermCapacitanceC22 + soilCapacitance).toJoulesPerMeterKelvin
        (
          5,
          Array(
            Array(-g1 / c1, g1 / c1, 0.0, 0.0, 0.0),
            Array(g1 / c2, (-g1 - g2) / c2, g2 / c2, 0.0, 0.0),
            Array(0.0, g2 / c3, (-g2 - g3) / c3, g3 / c3, 0.0),
            Array(0.0, 0.0, g3 / c4, (-g3 - g4) / c4, g4 / c4),
            Array(0.0, 0.0, 0.0, g4 / c5, (-g4 - g5) / c5),
          ),
          Array(
            (conductorLosses.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c1,
            0d,
            (sheathLossesMiddlePhase.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c3,
            0d,
            (g5 * groundTemperature.toCelsiusScale) / c5,
          ),
        )
      case Some(bed) =>
        // Ten-node network: five cable nodes, two bed cells, and three outer-soil cells.
        val bedRes =
          bed.soilType
            .currentThermalResistivity(temperatureDifference)
            .toSquants
        val bedRadius = calcCableBedEquivalentRadius(bed.width, bed.height)
        val bedCenterDepth = Meters(-currentLineModel.depthCables.toMeters)
        if bedCenterDepth <= bedRadius then {
          val message =
            s"Cable bed center depth $bedCenterDepth must exceed its equivalent radius $bedRadius."
          logger.error(message)
          throw new IllegalArgumentException(message)
        }
        val lossWeightFactor = calcCableBedLossWeightFactor(
          cableSetup.layoutFormation,
          thermalTotalLossesLeadingCable,
          thermalTotalLossesMiddleCable,
          thermalTotalLossesLaggingCable,
        )
        val (bedResistance, soilResistance) = calcCableBedT4Split(
          t4Base,
          bedRes,
          effectiveSoilResistivity,
          bedCenterDepth,
          bedRadius,
          lossWeightFactor,
        )
        val cableOuterRadius =
          cableSetup.layersJackElements
            .map(_.outerDiameter)
            .maxOption
            .getOrElse(
              throw new IllegalArgumentException(
                "Jack layer expected for environmental resistance calculation"
              )
            ) / 2d
        val soilOuterRadius =
          calcSoilModelOuterRadius(bedCenterDepth, bedRadius)
        val cells = calcCableBedEnvironmentalCells(
          bedResistance,
          soilResistance,
          cableOuterRadius,
          bedRadius,
          soilOuterRadius,
          bed.soilType.getVolumetricHeatCapacity.toSquantsKWhPerCubicMeterKelvin, // TODO DF check units!
          currentLineModel.soilType.getVolumetricHeatCapacity.toSquantsKWhPerCubicMeterKelvin, // TODO DF check units!
        )
        val cellResistances = cells.map(_.resistance.toKelvinMetersPerWatt)
        val cellConductances = Array.tabulate(cells.size) { index =>
          if index == cells.size - 1 then 2d / cellResistances(index)
          else 2d / (cellResistances(index) + cellResistances(index + 1))
        }
        val cableToEnvironmentConductance = 2d / cellResistances.head
        val capacities = Array(
          c1,
          c2,
          c3,
          c4,
          jackThermCapacitanceC22.toJoulesPerMeterKelvin,
        ) ++ cells.map(_.thermalCapacity.toJoulesPerMeterKelvin)
        val matrix = Array.tabulate(10, 10)((_, _) => 0d)
        val cableConductances =
          Array(g1, g2, g3, g4, cableToEnvironmentConductance)
        for row <- 0 until 5 do {
          val leftConductance =
            if row == 0 then 0d else cableConductances(row - 1)
          val rightConductance = cableConductances(row)
          matrix(row)(row) =
            -(leftConductance + rightConductance) / capacities(row)
          if row > 0 then
            matrix(row)(row - 1) = leftConductance / capacities(row)
          matrix(row)(row + 1) = rightConductance / capacities(row)
        }
        for index <- cells.indices do {
          val row = index + 5
          val leftConductance =
            if index == 0 then cableToEnvironmentConductance
            else cellConductances(index - 1)
          val rightConductance = cellConductances(index)
          matrix(row)(row) =
            -(leftConductance + rightConductance) / capacities(row)
          matrix(row)(row - 1) = leftConductance / capacities(row)
          if index < cells.size - 1 then
            matrix(row)(row + 1) = rightConductance / capacities(row)
        }
        val vector = Array.fill(10)(0d)
        vector(0) =
          (conductorLosses.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c1
        vector(2) =
          (sheathLossesMiddlePhase.toWattsPerMeter + dielectricLosses.toWattsPerMeter / 2) / c3
        vector(9) =
          cellConductances.last * groundTemperature.toCelsiusScale / capacities.last
        (10, matrix, vector)
    }

    val matrixA: RealMatrix = new Array2DRowRealMatrix(matrixAData)
    val vectorB: RealVector = new ArrayRealVector(vectorBData)

    val (eigenvalues, eigenvectors) =
      LineThermalModelNetworkSolver.determineEigenvaluesAndVectors(matrixA)

    val stateTemps = state.lineTemperatures
    val vStartEntries =
      if nodeCount == 5 then
        Array(
          stateTemps.currentLineTemp1.toCelsiusScale,
          stateTemps.currentLineTemp2.toCelsiusScale,
          stateTemps.currentLineTemp3.toCelsiusScale,
          stateTemps.currentLineTemp4.toCelsiusScale,
          stateTemps.currentLineTemp5.toCelsiusScale,
        )
      else
        Array(
          stateTemps.currentLineTemp1.toCelsiusScale,
          stateTemps.currentLineTemp2.toCelsiusScale,
          stateTemps.currentLineTemp3.toCelsiusScale,
          stateTemps.currentLineTemp4.toCelsiusScale,
          stateTemps.currentLineTemp5.toCelsiusScale,
          stateTemps.currentLineTemp6.toCelsiusScale,
          stateTemps.currentLineTemp7.toCelsiusScale,
          stateTemps.currentLineTemp8.toCelsiusScale,
          stateTemps.currentLineTemp9.toCelsiusScale,
          stateTemps.currentLineTemp10.toCelsiusScale,
        )

    val vStart: RealVector = new ArrayRealVector(vStartEntries)

    val luSolver = new LUDecomposition(matrixA).getSolver
    val vp: RealVector = luSolver.solve(vectorB.mapMultiply(-1.0))

    val vStartMinusVp: RealVector = vStart.subtract(vp)
    val evSolver = new LUDecomposition(eigenvectors).getSolver
    val c: RealVector = evSolver.solve(vStartMinusVp)

    def getNodeTemperature(t: Long, node: Int): Temperature = {
      var temperature =
        vp.getEntry(node)
      for i <- 0 until nodeCount do {
        temperature +=
          c.getEntry(i) * eigenvectors.getEntry(node, i) * math.exp(
            eigenvalues(i) * t
          )
      }
      Celsius(temperature)
    }

    val duration = tick - state.tick
    if duration > 3600 then
      logger.warn(
        s"RC-Network for short durations has been used. However, the duration of $duration ticks might be of type long duration. Currently used method might be inaccurate but should estimate on the safe side."
      )

    if nodeCount == 5 then
      LineTemperatures(
        getNodeTemperature(duration, 0),
        getNodeTemperature(duration, 1),
        getNodeTemperature(duration, 2),
        getNodeTemperature(duration, 3),
        getNodeTemperature(duration, 4),
        Celsius(0d),
        Celsius(0d),
        Celsius(0d),
        Celsius(0d),
        Celsius(0d),
      )
    else
      LineTemperatures(
        getNodeTemperature(duration, 0),
        getNodeTemperature(duration, 1),
        getNodeTemperature(duration, 2),
        getNodeTemperature(duration, 3),
        getNodeTemperature(duration, 4),
        getNodeTemperature(duration, 5),
        getNodeTemperature(duration, 6),
        getNodeTemperature(duration, 7),
        getNodeTemperature(duration, 8),
        getNodeTemperature(duration, 9),
      )
  }
}
