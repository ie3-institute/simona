/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import optimus.algebra.{Double2Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.{MPFloatVar, MPVar}
import squants.Power

/** Trait for factories of power objectives. An objective is created for the sum
  * of power of a single time step.
  */
trait PowerObjectiveFactory {

  /** Creates an objective expression involving the sum of power for a single
    * time step.
    *
    * @param totalPower
    *   The sum of power of all assets for a time step.
    * @param model
    *   The optimization model to add variables and constraints to.
    * @return
    *   The objective as an expression.
    */
  def build(totalPower: Expression, target: Power)(using
      model: MPModel
  ): Expression
}

object PowerObjectiveFactory {

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  object MinAbsPowerObjectiveFactory extends PowerObjectiveFactory {

    override def build(
        totalPower: Expression,
        target: Power,
    )(using model: MPModel): Expression = {
      val difference = totalPower - target.toKilowatts

      absoluteValue(difference, "differenceAbs")
    }

  }

  /** Creates an objective that uses a piecewise-linear (over-)approximation of
    * the quadratic function on the sum of power. The convex epigraph is used to
    * derive a linear constraint. Effectively, higher power values are punished
    * more than lower ones.
    *
    * The piecewise approximation is created with a fixed number of segments
    * (secant lines) up until given last segment.
    *
    * @param segmentCount
    *   The number of segments (secant lines) to create. Increasing the number
    *   of segments improves the accuracy of the approximation, but might impact
    *   efficiency.
    * @param lastSegment
    *   The value of the last segment boundary. This should be set close to the
    *   maximum value that is to be expected, otherwise the approximation
    *   becomes inaccurate beyond this value.
    */
  class LinearizedQuadraticPowerObjectiveFactory(
      segmentCount: Int,
      lastSegment: Double,
  ) extends PowerObjectiveFactory {

    override def build(
        totalPower: Expression,
        target: Power,
    )(using model: MPModel): Expression = {
      val difference = totalPower - target.toKilowatts

      val differenceAbs = absoluteValue(difference, "differenceAbs")

      val segmentSize = lastSegment / segmentCount

      val t = MPFloatVar.positive("t")

      Range.inclusive(0, segmentCount).map(_ * segmentSize).sliding(2).foreach {
        case Seq(uCurrent, uNext) =>
          val m = uCurrent + uNext
          val b = -uCurrent * uNext

          model.add(t >:= m * differenceAbs + b)
      }

      // normalize the final value so that it maximizes
      // somewhat close to the absolute value
      val normalizationFactor = 1 / lastSegment

      t * normalizationFactor
    }

  }

  private def absoluteValue(value: Expression, name: String)(using
      model: MPModel
  ): MPVar = {
    val valueAbs = MPFloatVar.positive(name)
    model.add(valueAbs >:= value)
    model.add(valueAbs >:= -value)
    valueAbs
  }

}
