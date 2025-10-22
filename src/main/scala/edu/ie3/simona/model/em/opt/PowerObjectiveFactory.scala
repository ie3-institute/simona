/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import optimus.algebra.{Double2Const, Expression}
import optimus.optimization.MPModel
import optimus.optimization.model.MPFloatVar

/** Trait for factories of power objectives. An objective is created for the sum
  * of power of a single time step.
  */
trait PowerObjectiveFactory {

  /** Creates an objective for a single time step involving the sum of power.
    *
    * @param totalPower
    *   The sum of power of all assets for a time step.
    * @param model
    *   The optimization model to use.
    * @return
    *   The objective as an expression.
    */
  def build(totalPower: Expression)(using model: MPModel): Expression
}

object PowerObjectiveFactory {

  /** Creates an objective that simply minimizes the absolute value of the sum
    * of power by using an epigraph constraint.
    */
  object MinAbsPowerObjectiveFactory extends PowerObjectiveFactory {

    override def build(
        totalPower: Expression
    )(using model: MPModel): Expression = {
      val d = MPFloatVar.positive("d")
      model.add(d >:= totalPower)
      model.add(d >:= -totalPower)

      d
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
    *   The number of segments (secant lines) to create. A high number of
    *   segments might impact efficiency.
    * @param lastSegment
    *   The value of the last segment boundary. This should be set close to the
    *   maximum value that is to be expected, otherwise the approximation
    *   becomes inaccurate.
    */
  class LinearizedQuadraticPowerObjectiveFactory(
      segmentCount: Int,
      lastSegment: Double,
  ) extends PowerObjectiveFactory {

    override def build(
        totalPower: Expression
    )(using model: MPModel): Expression = {

      val powerAbs = MPFloatVar.positive("powerAbs")
      model.add(powerAbs >:= totalPower)
      model.add(powerAbs >:= -totalPower)

      val segmentSize = lastSegment / segmentCount

      val t = MPFloatVar.positive("t")

      Range.inclusive(0, segmentCount).map(_ * segmentSize).sliding(2).foreach {
        case Seq(uCurrent, uNext) =>
          val m = uCurrent + uNext
          val b = -uCurrent * uNext

          model.add(t >:= m * powerAbs + b)
      }

      // normalize the final value so that it maximizes
      // somewhat close to the absolute value
      val normalizationFactor = 1 / lastSegment

      t * normalizationFactor
    }

  }
}
