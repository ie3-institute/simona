/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.event.listener

/** Test data used in multiple tests of [[RuntimeEventListener]]
  */
trait RuntimeTestData {

  protected val startDateTimeString: String = "2011-01-01T00:00:00Z"
  protected val endTick: Long = 3600

  /** Duration in milliseconds that is used in some of
    * [[edu.ie3.simona.event.RuntimeEvent]]
    */
  protected val duration: Long = 10805000
  protected val errMsg: String = "testing error msg"

  protected val currentTick: Long = 0

}
