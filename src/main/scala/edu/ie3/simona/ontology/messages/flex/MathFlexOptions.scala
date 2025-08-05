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

  def addInitialState(using model: MPModel): SV

  def addOperationConstraints(state: SV)(using model: MPModel): OV

  def addNewStateConstraints(formerState: SV, op: OV, timeSpan: Time)(using
      model: MPModel
  ): SV

}

object MathFlexOptions {

  trait OperationVars {

    def getPowerExpression: Expression

    def getPowerSolution: Option[Power]

    def getSoftConstraints: Option[Expression]

  }

}
