/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em

import edu.ie3.util.interval.ClosedInterval
import edu.ie3.simona.model.em.opt.OptimizedFlexStrat
import edu.ie3.simona.model.em.opt.PowerObjectiveFactory.{
  LinearizedQuadraticPowerObjectiveFactory,
  MinAbsPowerObjectiveFactory,
}
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions
import edu.ie3.simona.ontology.messages.flex.EnergyBoundariesFlexOptions.ParticipantEnergyBoundaries
import edu.ie3.simona.test.common.{MathFlexTestLike, UnitSpec}
import optimus.optimization.MPModel
import optimus.optimization.enums.{SolutionStatus, SolverLib}
import squants.Each
import squants.energy.{KilowattHours, Kilowatts}
import squants.time.Hours

import java.util.UUID
import scala.collection.immutable.SortedMap

class OptimizedFlexStratIT extends UnitSpec with MathFlexTestLike {

  // Testing tolerances
  given Double = 1e-6
  val constraintTolerance = 1e-3

  "An optimized flex strat" when {
    "provided with battery and constant constraints" should {

      given ticks: Seq[Long] = Range.Long.inclusive(0, 12 * 3600, 3600)
      val sampleTime = Hours(1)

      // 33 kWh of feed-in in total, more than battery can store
      val pvUUID = UUID.fromString("0-0-0-0-1")
      val pvFlex = EnergyBoundariesFlexOptions(
        ParticipantEnergyBoundaries(
          Seq(0, -6, -8, -7, -12, 0, 0, 0, 0, 0, 0, 0).toPowerMap,
          sampleTime,
        )
      )

      // 31 kWh of load in total, more than battery can provide
      val loadUUID = UUID.fromString("0-0-0-0-2")
      val loadFlex = EnergyBoundariesFlexOptions(
        ParticipantEnergyBoundaries(
          Seq(0, 0, 0, 0, 0, 0, 5, 10, 3, 7, 6, 0).toPowerMap,
          sampleTime,
        )
      )

      val batUUID = UUID.fromString("0-0-0-0-3")
      val batFlex = EnergyBoundariesFlexOptions(
        ParticipantEnergyBoundaries(
          energyLimits = SortedMap(
            0L -> ClosedInterval(KilowattHours(0), KilowattHours(20))
          ),
          powerLimits = ClosedInterval(Kilowatts(-10), Kilowatts(10)),
          etaCharge = Each(0.8),
          etaDischarge = Each(0.8),
        )
      )

      val flexOptions = Map(
        pvUUID -> pvFlex,
        loadUUID -> loadFlex,
        batUUID -> batFlex,
      )

      // since energy values have been adapted, we need this
      // factor to convert back to "real" values
      given EnergyConversionFactor =
        EnergyConversionFactor(
          Each(0.8),
          OptimizedFlexStrat
            .adaptEnergyBoundaries(batFlex.energyBoundaries.headOption.value)
            .etaCharge,
        )

      "compensate the load and feed-in with battery flexibility" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          Kilowatts(0),
          Hours(1),
          MinAbsPowerObjectiveFactory,
        )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          Since excess power costs the same at all points in time and
          at all magnitudes, there are many optimal solutions.
          Thus, we only test for things that are true for every optimal
          solution: We know when the battery should be definitely
          full/empty and how much energy was charged/discharged.
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          batRes should have size 12

          batRes.slice(0, 4).foreach {
            _.energyVal should (be >= 0d and be < 20d)
          }

          batRes.slice(4, 6).foreach {
            _.energyVal should approximate(20d)
          }

          batRes.slice(6, 10).foreach {
            _.energyVal should (be >= 0d and be < 20d)
          }

          batRes.slice(10, 12).foreach {
            _.energyVal should approximate(0d)
          }

          // we should've charged 20 kWh plus 5 kWh losses
          val inputCharged = batRes.slice(0, 6).map(_.pVal).sum
          inputCharged should approximate(25)

          // we should've discharged 20 kWh minus 4 kWh losses
          val outputDischarged =
            batRes.slice(6, 12).map(_.pVal).sum
          outputDischarged should approximate(-16d)

        } withClue buildDebugString(batVars)

        model.release()

      }

      "minimize peaks" in {

        given model: MPModel = MPModel(SolverLib.oJSolver)

        val assetVars =
          OptimizedFlexStrat.addAssetConstraints(flexOptions, sampleTime, ticks)

        val objectiveContainer = OptimizedFlexStrat.buildObjective(
          assetVars,
          Kilowatts(0),
          Hours(1),
          LinearizedQuadraticPowerObjectiveFactory(
            segmentCount = 10,
            lastSegment = 10d, // 10 kW
          ),
        )

        model.minimize(objectiveContainer.objective)
        model.start(timeLimit = 10000)

        model.getStatus shouldBe SolutionStatus.OPTIMAL

        /*
          EXPECTED RESULTS
          When using the (linearized) quadratic objective, the
          test is designed in a way such that only one optimal
          solution exists: During the first phase (charging),
          battery power is utilized so that 2 kW remains at every
          time step. During the second phase (discharging), there's
          exactly enough energy available to go down to 3 kW at
          every time step.
         */

        val batVars = assetVars
          .find(_.assetUuid == batUUID)
          .getOrElse(fail(s"No asset variables for battery ($batUUID) found."))

        val batRes = batVars.results.headOption
          .getOrElse(fail(s"Empty results for battery ($batUUID)."))

        {
          objectiveContainer.softConstraints.foreach { constraint =>
            withClue(constraint.getWarningMessage) {
              constraint.getError should be < constraintTolerance
            }
          }

          // 0 kW to compensate
          batRes(0).pVal should approximate(0d)
          batRes(0).energyVal should approximate(0d)

          // 6 kW of feed-in to compensate, 2 kW remains
          batRes(1).pVal should approximate(4d)
          batRes(1).energyVal should approximate(3.2d)

          // 8 kW of feed-in to compensate, 2 kW remains
          batRes(2).pVal should approximate(6d)
          batRes(2).energyVal should approximate(8d)

          // 7 kW of feed-in to compensate, 2 kW remains
          batRes(3).pVal should approximate(5d)
          batRes(3).energyVal should approximate(12d)

          // 12 kW of feed-in to compensate, 2 kW remains
          batRes(4).pVal should approximate(10d)
          batRes(4).energyVal should approximate(20d)

          // 0 kW to compensate
          batRes(5).pVal should approximate(0d)
          batRes(5).energyVal should approximate(20d)

          // 5 kW of load to compensate, 3 kW remains
          batRes(6).pVal should approximate(-2d)
          batRes(6).energyVal should approximate(17.5d)

          // 10 kW of load to compensate, 3 kW remains
          batRes(7).pVal should approximate(-7d)
          batRes(7).energyVal should approximate(8.75d)

          // 3 kW of load to compensate, 3 kW remains
          batRes(8).pVal should approximate(0d)
          batRes(8).energyVal should approximate(8.75d)

          // 7 kW of load to compensate, 3 kW remains
          batRes(9).pVal should approximate(-4d)
          batRes(9).energyVal should approximate(3.75d)

          // 6 kW of load to compensate, 3 kW remains
          batRes(10).pVal should approximate(-3d)
          batRes(10).energyVal should approximate(0d)

          // 0 kW to compensate
          batRes(11).pVal should approximate(0d)
          batRes(11).energyVal should approximate(0d)

          // we should've charged 20 kWh plus 5 kWh losses
          val inputCharged = batRes.slice(0, 6).map(_.pVal).sum
          inputCharged should approximate(25)

          // we should've discharged 20 kWh minus 4 kWh losses
          val outputDischarged =
            batRes.slice(6, 12).map(_.pVal).sum
          outputDischarged should approximate(-16d)

        } withClue buildDebugString(batVars)

        model.release()

      }
    }
  }

}
