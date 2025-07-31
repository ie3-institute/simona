/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages.flex

import optimus.optimization.MPModel
import squants.Time

/** @tparam SV
  *   State variables
  * @tparam OV
  *   Operation variables
  */
trait MathProgrammingFlexOptions[SV, OV] extends FlexOptions {

  def addInitialState(using model: MPModel): SV

  def addOperationConstraints(state: SV)(using model: MPModel): OV

  def addNewStateConstraints(formerState: SV, op: OV, timeSpan: Time)(using
      model: MPModel
  ): SV

}
