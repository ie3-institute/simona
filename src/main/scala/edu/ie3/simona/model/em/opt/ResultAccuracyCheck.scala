/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.em.opt

/** Trait to be extended by classes detailing a result accuracy check, including
  * error message formulation.
  */
trait ResultAccuracyCheck {

  /** Returns the amount of error of resulting power in kW compared to a
    * physically exact result. Only call this if you're sure that a solution has
    * been determined! A [[edu.ie3.simona.exceptions.CriticalFailureException]]
    * will be thrown otherwise.
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
