/*
 * © 2025. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid.congestion

import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.agent.grid.TransformerTappingSupport.getTappingOptions
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.exceptions.CriticalFailureException
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{TransformerTapping, VoltageLimits}
import edu.ie3.util.scala.quantities.DefaultQuantities.zeroPU
import edu.ie3.util.scala.quantities.QuantityConversionUtils.toSquants
import org.apache.pekko.actor.typed.ActorRef
import squants.{Dimensionless, Each}

import java.util.UUID

/** Object that contains information about possible voltage changes. <p> If the
  * delta plus is negative -> upper voltage violation <p> If the delta minus is
  * positive -> lower voltage violation <p> If both above cases happen at the
  * same time the suggestion is set to the delta plus, because having a too high
  * voltage is more severe.
  *
  * @param deltaPlus
  *   Maximal possible voltage increase.
  * @param deltaMinus
  *   Maximal possible voltage decrease.
  * @param suggestion
  *   For voltage change.
  */
final case class VoltageRange(
    deltaPlus: Dimensionless,
    deltaMinus: Dimensionless,
    suggestion: Dimensionless,
) {

  /** Method to update this voltage range with voltage delta.
    *
    * @param deltaV
    *   The voltage difference to consider for the range limits.
    * @return
    *   A new [[VoltageRange]].
    */
  private[congestion] def updateWithVoltageDelta(
      deltaV: Dimensionless
  ): VoltageRange = {

    if deltaV == zeroPU then {
      this
    } else if deltaV < zeroPU then {
      // we have limit the maximal decrease
      val minus = deltaMinus.max(deltaV)

      VoltageRange(deltaPlus, minus)

    } else {
      // we have to increase the voltage by at least the specified delta
      val minus = deltaV.max(deltaMinus).min(deltaPlus)

      VoltageRange(deltaPlus, minus)
    }
  }

  /** Method to update this voltage range with inferior voltage ranges.
    *
    * @param inferiorData
    *   Map: inferior grid to [[VoltageRange]] and [[TransformerTappingModel]].
    * @return
    *   A new [[VoltageRange]].
    */
  def updateWithInferiorRanges(
      inferiorData: Map[ActorRef[
        GridAgent.Message
      ], (VoltageRange, Set[TransformerTapping])]
  ): VoltageRange = {
    inferiorData.foldLeft(this) {
      case (range, (_, (inferiorRange, tappings))) =>
        // get tapping options
        val (possiblePlus, possibleMinus) = getTappingOptions(tappings)

        val increase =
          range.deltaPlus + possibleMinus <= inferiorRange.deltaPlus
        val decrease =
          range.deltaMinus + possiblePlus >= inferiorRange.deltaMinus

        (increase, decrease) match {
          case (true, true) =>
            VoltageRange(range.deltaPlus, range.deltaMinus)
          case (true, false) =>
            VoltageRange(
              range.deltaPlus,
              inferiorRange.deltaMinus - possiblePlus,
            )
          case (false, true) =>
            VoltageRange(
              inferiorRange.deltaPlus - possibleMinus,
              range.deltaMinus,
            )
          case (false, false) =>
            VoltageRange(inferiorRange.deltaPlus, inferiorRange.deltaMinus)
        }
    }
  }
}

object VoltageRange {

  private given Dimensionless = Each(1e-3)

  def apply(
      deltaPlus: Dimensionless,
      deltaMinus: Dimensionless,
  ): VoltageRange = {
    val plus = deltaPlus.toEach
    val minus = deltaMinus.toEach

    val value = if plus > minus then {
      // we could have a voltage violation of one limit
      (plus + minus) / 2
    } else {
      (plus > 0, minus < 0) match {
        case (true, false) =>
          // we have a voltage violation of the lower limit
          // since the upper limit is fine, we can increase the voltage a bit
          plus
        case (false, true) =>
          // we have a voltage violation of the upper limit
          // since the lower limit is fine, we can decrease the voltage a bit
          minus
        case _ =>
          // we have a voltage violation of both limits, we can't fix this
          0
      }
    }

    val factor = 1e3

    val suggestion = if value < 0 then {
      (value * factor).floor / factor
    } else {
      (value * factor).ceil / factor
    }

    // check if tapping is required
    if plus < 0 || minus > 0 then {
      VoltageRange(
        deltaPlus,
        deltaMinus,
        Each(suggestion),
      )
    } else {
      // the voltage in this range is fine, set the suggested voltage change to zero
      VoltageRange(
        deltaPlus,
        deltaMinus,
        zeroPU,
      )
    }
  }

  /** Method to calculate the possible range of voltage changes.
    *
    * @param powerFlowResultEvent
    *   Results from simulating the grid.
    * @param voltageLimits
    *   Voltage limits.
    * @param gridComponents
    *   All components of the grid.
    * @param inferiorData
    *   Map: inferior grid to [[VoltageRange]] and [[TransformerTappingModel]].
    * @return
    *   A [[VoltageRange]].
    */
  def apply(
      powerFlowResultEvent: PowerFlowResultEvent,
      voltageLimits: VoltageLimits,
      gridComponents: GridComponents,
      inferiorData: Map[ActorRef[
        GridAgent.Message
      ], (VoltageRange, Set[TransformerTapping])],
      subnetNo: Int,
  ): VoltageRange = {
    // filter nodes in subnet
    val nodesInSubnet =
      gridComponents.nodes.filter(_.subnet == subnetNo).map(_.uuid)

    // calculate voltage range
    val nodeResMap = powerFlowResultEvent.nodeResults
      .filter(res => nodesInSubnet.contains(res.getInputModel))
      .map(res => res.getInputModel -> res.getvMag.toSquants)
      .toMap
    val minVoltage = nodeResMap
      .minByOption(_._2.toEach)
      .getOrElse(throw new CriticalFailureException(s"No node result found!"))
    val maxVoltage = nodeResMap
      .maxByOption(_._2.toEach)
      .getOrElse(throw new CriticalFailureException(s"No node result found!"))

    // build initial range
    val range = VoltageRange(
      voltageLimits.vMax.toSquants - maxVoltage._2,
      voltageLimits.vMin.toSquants - minVoltage._2,
    )

    // updating the voltage range prevent or heal line congestions
    val deltaV = calculateVoltageDeltaFromLineCurrent(
      nodeResMap,
      powerFlowResultEvent.lineResults,
      gridComponents,
    )
    val updatedRange = range.updateWithVoltageDelta(deltaV)

    if inferiorData.isEmpty then {
      // if there are no inferior grids, return the voltage range
      updatedRange
    } else {
      // if there are inferior grids, update the voltage range
      updatedRange.updateWithInferiorRanges(inferiorData)
    }
  }

  /** Method to calculate a voltage delta from given line currents. <p> - If
    * there is a line congestion, increasing the voltage by the returned delta
    * should mitigate them. <p> - If there is no line congestion, the returned
    * voltage shows the possible voltage decrease. <p> - Formula: V * I = (V +
    * deltaV) * (I + deltaI).
    *
    * @param nodeResults
    *   Node voltages.
    * @param lineResults
    *   Line currents.
    * @param gridComponents
    *   Information of components.
    * @return
    *   A voltage delta.
    */
  def calculateVoltageDeltaFromLineCurrent(
      nodeResults: Map[UUID, Dimensionless],
      lineResults: Iterable[LineResult],
      gridComponents: GridComponents,
  ): Dimensionless = {
    val lineMap = gridComponents.lines.map(line => line.uuid -> line).toMap

    // calculate the voltage change that ensures there is no line congestion
    val voltageChanges =
      lineResults.map(res => res.getInputModel -> res).map { case (uuid, res) =>
        val line = lineMap(uuid)

        // calculate the voltage and the current change at the end of the line that sees the highest current
        val (current, node) =
          if res.getiAMag().isGreaterThan(res.getiBMag()) then {
            (res.getiAMag(), line.nodeAUuid)
          } else {
            (res.getiBMag(), line.nodeBUuid)
          }

        val voltage = nodeResults(node)
        val deltaI = line.iNom - current.toSquants

        // calculate the voltage change
        val currentInfluence = deltaI / line.iNom
        voltage * -1 * currentInfluence
      }

    // determine the actual possible voltage change
    val change = voltageChanges
      .maxByOption(_.value)
      .getOrElse(
        throw new CriticalFailureException(s"No line result found!")
      )

    // change < 0 => tapping down possible
    // change > 0 => tapping up is necessary
    change
  }

  /** Combines the given [[VoltageRange]]s and adds the given offset.
    * @param ranges
    *   Given ranges.
    * @param offset
    *   To use.
    * @return
    *   A new [[VoltageRange]].
    */
  def combineAndUpdate(
      ranges: Iterable[VoltageRange],
      offset: Dimensionless,
  ): VoltageRange = {
    // finds the minimal voltage increase
    val minPlus = ranges.minByOption(_.deltaPlus).map(_.deltaPlus)

    // finds the maximal voltage decrease
    val maxMinus = ranges.maxByOption(_.deltaMinus).map(_.deltaMinus)

    (minPlus, maxMinus) match {
      case (Some(plus), Some(minus)) if offset ~= zeroPU =>
        VoltageRange(plus, minus)
      case (Some(plus), Some(minus)) =>
        // multiply -1 to get the needed compensation
        val offsetCompensation = offset * -1

        VoltageRange(
          plus - offset,
          minus - offset,
          offsetCompensation,
        )
      case _ =>
        VoltageRange(zeroPU, zeroPU)
    }
  }
}
