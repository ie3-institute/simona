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
  * Methods for creating a chain of states and operating points need to be
  * provided.
  *
  * @tparam SV
  *   The type of state variables.
  * @tparam OV
  *   The type of operation variables.
  */
trait MathFlexOptions[SV, OV <: OperationVars] extends FlexOptions {

  /** Adds and returns the initial state.
    *
    * @param tick
    *   The current tick, which is also the tick of the initial state.
    * @param model
    *   The model to use.
    * @return
    *   The initial state variable.
    */
  def addInitialState(tick: Long)(using model: MPModel): SV

  /** Adds and returns operation variables for given state.
    *
    * @param state
    *   The state to add operation variables for.
    * @param model
    *   The model to use.
    * @return
    *   The operation variables.
    */
  def addOperationConstraints(state: SV)(using model: MPModel): OV

  /** Adds and returns state variables for the state that results from the
    * former state and given operation constraints.
    *
    * @param formerState
    *   The former state.
    * @param op
    *   The operation variables and constraints.
    * @param tick
    *   The tick to create the new state for.
    * @param model
    *   The model to use.
    * @return
    *   The state variable.
    */
  def addNewStateConstraints(formerState: SV, op: OV, tick: Long)(using
      model: MPModel
  ): SV

}

object MathFlexOptions {

  /** Trait that needs to be extended by all types of operation variables.
    */
  trait OperationVars {

    /** The final power expression to use within the objective of optimization.
      *
      * @return
      *   The power expression.
      */
    def getPowerExpression: Expression

    /** The solution found for the power expression. Only available once
      * optimization has run and succeeded. Will return [[None]] otherwise.
      *
      * @return
      *   The solution to the power expression, if applicable.
      */
    def getPowerSolution: Option[Power]

    /** Returns the soft constraint for the operation variables, if applicable.
      *
      * @param duration
      *   The duration that the system participant was operating at this
      *   operating point, i.e. the sample time.
      */
    def getSoftConstraint(duration: Time): Option[SoftConstraint]

  }

  /** Trait to be extended by classes detailing a soft constraint as part of the
    * optimization objective and possible error handling.
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

    /** A warning message explaining what was expected and what happened
      * instead. Only makes sense if the error is larger than expected.
      *
      * @return
      *   The warning message.
      */
    def getWarningMessage: String

  }

}
