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
import edu.ie3.simona.model.grid.{
  TransformerTapping,
  TransformerTappingModel,
  VoltageLimits,
}
import edu.ie3.util.quantities.QuantityUtils.asPu
import edu.ie3.util.scala.quantities.QuantityConversionUtils.{
  CurrentToSimona,
  DimensionlessToSimona,
}
import org.apache.pekko.actor.typed.ActorRef
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Dimensionless

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
    deltaPlus: ComparableQuantity[Dimensionless],
    deltaMinus: ComparableQuantity[Dimensionless],
    suggestion: ComparableQuantity[Dimensionless],
) {

  /** Method to update this voltage range with line voltage delta.
    *
    * @param deltaV
    *   To consider.
    * @return
    *   A new [[VoltageRange]].
    */
  def updateWithLineDelta(
      deltaV: ComparableQuantity[Dimensionless]
  ): VoltageRange = {
    val (plus, minus) = (
      deltaV.isGreaterThan(deltaPlus),
      deltaV.isGreaterThan(deltaMinus),
    ) match {
      case (true, true) =>
        (deltaPlus, deltaPlus)
      case (false, true) =>
        (deltaPlus, deltaV)
      case (true, false) =>
        (deltaPlus, deltaPlus)
      case (false, false) =>
        (deltaPlus, deltaMinus)
    }

    VoltageRange(plus, minus)
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
        GridAgent.Request
      ], (VoltageRange, Set[TransformerTapping])]
  ): VoltageRange = {
    inferiorData.foldLeft(this) { case (range, (_, (infRange, tappings))) =>
      // get tapping options
      val (possiblePlus, possibleMinus) = getTappingOptions(tappings)

      val increase = range.deltaPlus
        .add(possibleMinus)
        .isLessThanOrEqualTo(infRange.deltaPlus)
      val decrease = range.deltaMinus
        .add(possiblePlus)
        .isGreaterThanOrEqualTo(infRange.deltaMinus)

      (increase, decrease) match {
        case (true, true) =>
          VoltageRange(range.deltaPlus, range.deltaMinus)
        case (true, false) =>
          VoltageRange(
            range.deltaPlus,
            infRange.deltaMinus.subtract(possiblePlus),
          )
        case (false, true) =>
          VoltageRange(
            infRange.deltaPlus.subtract(possibleMinus),
            range.deltaMinus,
          )
        case (false, false) =>
          VoltageRange(infRange.deltaPlus, infRange.deltaMinus)
      }
    }
  }
}

object VoltageRange {

  def apply(
      deltaPlus: ComparableQuantity[Dimensionless],
      deltaMinus: ComparableQuantity[Dimensionless],
  ): VoltageRange = {

    val plus = deltaPlus.getValue.doubleValue()
    val minus = deltaMinus.getValue.doubleValue()

    val value = if (plus > minus) {
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

    val suggestion = if (value < 0) {
      (value * factor).floor / factor
    } else {
      (value * factor).ceil / factor
    }

    // check if tapping is required
    if (plus < 0 || minus > 0) {
      VoltageRange(
        deltaPlus,
        deltaMinus,
        suggestion.asPu,
      )
    } else {
      // the voltage in this range is fine, set the suggested voltage change to zero
      VoltageRange(
        deltaPlus,
        deltaMinus,
        0.asPu,
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
        GridAgent.Request
      ], (VoltageRange, Set[TransformerTapping])],
      subnetNo: Int,
  ): VoltageRange = {
    // filter nodes in subnet
    val nodesInSubnet =
      gridComponents.nodes.filter(_.subnet == subnetNo).map(_.uuid)

    // calculate voltage range
    val nodeResMap = powerFlowResultEvent.nodeResults
      .filter(res => nodesInSubnet.contains(res.getInputModel))
      .map(res => res.getInputModel -> res.getvMag())
      .toMap
    val minVoltage = nodeResMap
      .minByOption(_._2)
      .getOrElse(throw new CriticalFailureException(s"No node result found!"))
    val maxVoltage = nodeResMap
      .maxByOption(_._2)
      .getOrElse(throw new CriticalFailureException(s"No node result found!"))

    // build initial range
    val range = VoltageRange(
      voltageLimits.vMax.subtract(maxVoltage._2),
      voltageLimits.vMin.subtract(minVoltage._2),
    )

    // updating the voltage range prevent or cure line congestions
    val deltaV = calculatePossibleVoltageDeltaForLines(
      nodeResMap,
      powerFlowResultEvent.lineResults,
      gridComponents,
    )
    val updatedRange = range.updateWithLineDelta(deltaV)

    if (inferiorData.isEmpty) {
      // if there are no inferior grids, return the voltage range
      updatedRange
    } else {
      // if there are inferior grids, update the voltage range
      updatedRange.updateWithInferiorRanges(inferiorData)
    }
  }

  /** Method to calculate a voltage delta for the given line currents. <p> - If
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
  def calculatePossibleVoltageDeltaForLines(
      nodeResults: Map[UUID, ComparableQuantity[Dimensionless]],
      lineResults: Iterable[LineResult],
      gridComponents: GridComponents,
  ): ComparableQuantity[Dimensionless] = {
    val lineMap = gridComponents.lines.map(line => line.uuid -> line).toMap

    // calculate the voltage change that ensures there is no line congestion
    val voltageChanges =
      lineResults.map(res => res.getInputModel -> res).map { case (uuid, res) =>
        val line = lineMap(uuid)

        // calculate the voltage and the current change at the end of the line that sees the highest current
        val (voltage, deltaI) =
          if (res.getiAMag().isGreaterThan(res.getiBMag())) {
            (
              nodeResults(line.nodeAUuid).toSquants,
              line.iNom - res.getiAMag().toSquants,
            )
          } else {
            (
              nodeResults(line.nodeBUuid).toSquants,
              line.iNom - res.getiBMag().toSquants,
            )
          }

        // calculate the voltage change
        (voltage * deltaI) / line.iNom * -1
      }

    // determine the actual possible voltage change
    val change = voltageChanges.maxOption.getOrElse(
      throw new CriticalFailureException(s"No line result found!")
    )

    // change < 0 => tapping down possible
    // change > 0 => tapping up is necessary
    change.asPu
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
      offset: ComparableQuantity[Dimensionless],
  ): VoltageRange = {
    // finds the minimal increase
    val minPlus = ranges.minByOption(_.deltaPlus).map(_.deltaPlus)

    // finds the maximal decrease
    val maxMinus = ranges.maxByOption(_.deltaMinus).map(_.deltaMinus)

    (minPlus, maxMinus) match {
      case (Some(plus), Some(minus)) if offset.isEquivalentTo(0.asPu) =>
        VoltageRange(plus, minus)
      case (Some(plus), Some(minus)) =>
        VoltageRange(
          plus.subtract(offset),
          minus.subtract(offset),
          offset.multiply(-1),
        )
      case _ =>
        VoltageRange(0.asPu, 0.asPu)
    }
  }
}
