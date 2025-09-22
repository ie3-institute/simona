/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.participant

import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.participant.PowerSeriesMathFlexOptions.*
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions
import edu.ie3.simona.ontology.messages.flex.MathFlexOptions.{
  OperationVars,
  SoftConstraint,
}
import optimus.algebra.{Const, Expression}
import optimus.optimization.MPModel
import squants.{Power, Time}
import squants.energy.Kilowatts

import scala.collection.immutable.SortedMap

/** Flex options for participants that follow a fixed trajectory of power
  * values.
  *
  * @param powers
  *   The power values as a [[SortedMap]], thus powers in between keys can be
  *   extract with `maxBefore`.
  */
final case class PowerSeriesMathFlexOptions(powers: SortedMap[Long, Power])
    extends MathFlexOptions[PowerStateVars, PowerOperationVars] {

  override def addInitialState(tick: Long)(using
      model: MPModel
  ): PowerStateVars = PowerStateVars(tick)

  override def addOperationConstraints(
      state: PowerStateVars
  )(using model: MPModel): PowerOperationVars = {
    val (_, power) = powers
      .maxBefore(state.tick + 1)
      .getOrElse(
        throw new CriticalFailureException(
          s"No power found for tick ${state.tick} in provided power set $powers"
        )
      )
    PowerOperationVars(Const(power.toKilowatts))
  }

  override def addNewStateConstraints(
      formerState: PowerStateVars,
      op: PowerOperationVars,
      tick: Long,
  )(using model: MPModel): PowerStateVars = PowerStateVars(tick)

}

object PowerSeriesMathFlexOptions {

  /** Only stores the tick, no actual state variables required.
    *
    * @param tick
    *   The tick of the state.
    */
  final case class PowerStateVars(tick: Long)

  /** Stores the power as a constant.
    *
    * @param power
    *   The power value in kW.
    */
  final case class PowerOperationVars(power: Const) extends OperationVars {

    override def getPowerExpression: Expression = power

    override def getPowerSolution: Option[Power] = Some(Kilowatts(power.value))

    override def getSoftConstraint(duration: Time): Option[SoftConstraint] =
      None

  }

}
