/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.OperationVars
import optimus.algebra.Expression
import optimus.optimization.MPModel
import squants.{Power, Time}

/** Flex options used for mathematical programming, e.g. for optimization.
  *
  * @tparam SV
  *   State variables
  * @tparam OV
  *   Operation variables
  */
trait MathFlexOptions[SV, OV <: OperationVars] extends FlexOptions {

  def addInitialState(tick: Long)(using model: MPModel): SV

  def addOperationConstraints(state: SV)(using model: MPModel): OV

  def addNewStateConstraints(formerState: SV, op: OV, tick: Long)(using
      model: MPModel
  ): SV

}

object MathFlexOptions {

  trait OperationVars {

    def getPowerExpression: Expression

    def getPowerSolution: Option[Power]

    /** @param duration
      *   The duration that the system participant was operating at this
      *   operating point, i.e. the time step size.
      */
    def getSoftConstraints(duration: Time): Option[SoftConstraint]

  }

  /** Trait to be extended by classes detailing a soft constraint and possible
    * error handling.
    */
  trait SoftConstraint {

    /** The soft constraint expression to be included in the objective to be
      * minimized.
      * @return
      *   The soft constraint expression.
      */
    def getExpression: Expression

    /** Returns the amount of error stemming from the soft constraint. Only call
      * this if you're sure that a solution has been determined! A
      * [[edu.ie3.simona.exceptions.CriticalFailureException]] will be thrown
      * otherwise.
      * @return
      *   The amount of error.
      */
    def getError: Double

    /** A warning message explaining what was expected and what happened
      * instead.
      * @return
      *   The warning message.
      */
    def getWarningMessage: String

  }

}
