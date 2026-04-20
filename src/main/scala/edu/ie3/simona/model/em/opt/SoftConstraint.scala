/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

import edu.ie3.simona.exceptions.CriticalFailureException
import optimus.algebra.{Double2Const, Expression}
import optimus.optimization.model.MPFloatVar
import squants.Dimensionless

/** Trait to be extended by classes detailing a soft constraint as part of the
  * optimization objective, including possible error handling.
  */
trait SoftConstraint {

  /** The soft constraint expression to be included in the objective to be
    * minimized.
    *
    * @return
    *   The soft constraint expression.
    */
  def getExpression: Expression

  /** Returns the amount of error stemming from the soft constraint. Only call
    * this if you're sure that a solution has been determined! A
    * [[edu.ie3.simona.exceptions.CriticalFailureException]] will be thrown
    * otherwise.
    *
    * @return
    *   The amount of error.
    */
  def getError: Double

  /** A warning message explaining what was expected and what happened instead.
    * The message only makes sense if the error is actually larger than
    * expected.
    *
    * @return
    *   The warning message.
    */
  def getWarningMessage: String

}

object SoftConstraint {

  /** Small number to add to the constraint penalty, in order for the penalty to
    * be slightly larger than the absolute value.
    */
  val epsilon: Double = 1e-6

  /** Soft constraint for an absolute value of a free variable.
    *
    * @param variable
    *   The variable that can be assigned a positive or negative number.
    * @param absoluteVariable
    *   The variable that is supposed to be set to the absolute value of the
    *   [[variable]].
    * @param penalty
    *   The penalty factor to be multiplied with the absolute value for the soft
    *   constraint expression.
    */
  final case class AbsValueSoftConstraint(
      variable: MPFloatVar,
      absoluteVariable: MPFloatVar,
      penalty: Double,
  ) extends SoftConstraint {

    override def getExpression: Expression =
      absoluteVariable * penalty

    override def getError: Double = {
      val (value, absoluteValue) = getVals
      math.abs(math.abs(value) - absoluteValue)
    }

    override def getWarningMessage: String = {
      val (value, absoluteValue) = getVals
      s"Soft constraint for storage: Approximated absolute value $absoluteValue " +
        s"and actual absolute value ${math.abs(value)} are $getError apart."
    }

    private def getVals: (Double, Double) =
      variable.value
        .zip(absoluteVariable.value)
        .getOrElse(
          throw new CriticalFailureException(
            "Solution are expected to be determined at this point!"
          )
        )

  }

  object AbsValueSoftConstraint {

    /** Creates a soft constraint for the absolute value of a power value, used
      * during energy optimization.
      *
      * @param p
      *   The power value variable that can be positive or negative.
      * @param pAbs
      *   The variable that is supposed to hold the absolute value of the power.
      * @param eta
      *   The charging/discharging efficiency.
      * @return
      *   The soft constraint.
      */
    def apply(
        p: MPFloatVar,
        pAbs: MPFloatVar,
        eta: Dimensionless,
    ): AbsValueSoftConstraint = {
      // Total penalty is slightly larger than the model losses.
      // Thus, the value of pAbs should be pushed down to the
      // absolute of p.
      val penalty = 1 - eta.toEach + epsilon

      AbsValueSoftConstraint(p, pAbs, penalty)
    }
  }

}
