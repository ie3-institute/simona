/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.CongestionManagementSupport.VoltageRange
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.exceptions.ResultException
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.{
  TransformerTapping,
  TransformerTappingModel,
  VoltageLimits,
}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.typed.ActorRef
import squants.electro.Amperes
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.quantity.Dimensionless

/** Support and helper methods for calculations done during the congestion
  * management.
  */
trait CongestionManagementSupport {

  def calculateTapAndVoltage(
      suggestion: ComparableQuantity[Dimensionless],
      delta: ComparableQuantity[Dimensionless],
      currentTap: Int,
      tapMax: Int,
      tapMin: Int,
  ): (Int, ComparableQuantity[Dimensionless]) = {
    val tapNeeded = suggestion.divide(delta).multiply(-1).getValue.doubleValue()

    val increase = tapMax - currentTap
    val decrease = tapMin - currentTap

    val taps = if (tapNeeded >= increase) {
      increase
    } else if (tapNeeded <= decrease) {
      decrease
    } else {
      tapNeeded.round.toInt
    }

    (taps, delta.multiply(taps * -1))
  }

  /** Method to calculate the range of possible voltage changes.
    *
    * @param powerFlowResultEvent
    *   results from simulating the grid
    * @param voltageLimits
    *   voltage limits
    * @param gridComponents
    *   all components of the grid
    * @param inferiorData
    *   map: inferior grid to [[VoltageRange]] and [[TransformerTappingModel]]
    * @return
    */
  def calculateVoltageOptions(
      powerFlowResultEvent: PowerFlowResultEvent,
      voltageLimits: VoltageLimits,
      gridComponents: GridComponents,
      inferiorData: Map[ActorRef[
        GridAgent.Request
      ], (VoltageRange, TransformerTapping)],
  ): VoltageRange = {
    // calculate voltage range
    val nodeResMap = powerFlowResultEvent.nodeResults
      .map(res => res.getInputModel -> res.getvMag())
      .toMap
    val minVoltage = nodeResMap
      .minByOption(_._2)
      .getOrElse(throw new ResultException(s"No node result found!"))
    val maxVoltage = nodeResMap
      .maxByOption(_._2)
      .getOrElse(throw new ResultException(s"No node result found!"))

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
    * deltaV) * (I + deltaI)
    *
    * @param nodeResults
    *   node voltages
    * @param lineResults
    *   line currents
    * @param gridComponents
    *   information of components
    * @return
    *   a voltage delta
    */
  def calculatePossibleVoltageDeltaForLines(
      nodeResults: Map[UUID, ComparableQuantity[Dimensionless]],
      lineResults: Iterable[LineResult],
      gridComponents: GridComponents,
  ): ComparableQuantity[Dimensionless] = {
    val lineResMap = lineResults.map(res => res.getInputModel -> res).toMap

    val lineMap = gridComponents.lines.map(line => line.uuid -> line).toMap

    // calculates the
    val lineUtilisation = lineResMap.map { case (uuid, res) =>
      val iNom = lineMap(uuid).iNom
      val diffA = Amperes(res.getiAMag().getValue.doubleValue()) / iNom
      val diffB = Amperes(res.getiBMag().getValue.doubleValue()) / iNom

      uuid -> Math.max(diffA, diffB)
    }

    val maxUtilisation = lineUtilisation
      .maxByOption(_._2)
      .getOrElse(throw new ResultException(s"No line result found!"))
      ._1

    val line = lineMap(maxUtilisation)
    val res = lineResMap(maxUtilisation)
    val resA = res.getiAMag()
    val resB = res.getiBMag()

    val deltaV = if (resA.isGreaterThan(resB)) {
      val nodeRes = nodeResults(line.nodeAUuid).getValue.doubleValue()
      val current = resA.getValue.doubleValue()
      val deltaI = line.iNom.value - current
      (nodeRes * deltaI) / (current + deltaI) * -1
    } else {
      val nodeRes = nodeResults(line.nodeBUuid).getValue.doubleValue()
      val current = resB.getValue.doubleValue()
      val deltaI = line.iNom.value - current
      (nodeRes * current) / (current + deltaI) * -1
    }

    // deltaV < 0 => tapping down possible
    // deltaV > 0 => tapping up is necessary
    deltaV.asPu
  }

}

object CongestionManagementSupport {

  case class VoltageRange(
      deltaPlus: ComparableQuantity[Dimensionless],
      deltaMinus: ComparableQuantity[Dimensionless],
      suggestion: ComparableQuantity[Dimensionless],
  ) {

    /** Method to update this voltage range with line voltage delta.
      * @param deltaV
      *   to consider
      * @return
      *   a new [[VoltageRange]]
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

    /** Method to update this voltage range with inferior voltage ranges
      * @param inferiorData
      *   map: inferior grid to [[VoltageRange]] and [[TransformerTappingModel]]
      * @return
      *   a new [[VoltageRange]]
      */
    def updateWithInferiorRanges(
        inferiorData: Map[ActorRef[
          GridAgent.Request
        ], (VoltageRange, TransformerTapping)]
    ): VoltageRange = {

      inferiorData.foldLeft(this) { case (range, (_, (infRange, tapping))) =>
        if (tapping.hasAutoTap) {
          val currentPos = tapping.currentTapPos
          val deltaV = tapping.deltaV
          val possiblePlus = deltaV.multiply(tapping.tapMax - currentPos)
          val possibleMinus = deltaV.multiply(tapping.tapMin - currentPos)

          (
            range.deltaPlus
              .add(possibleMinus)
              .isLessThanOrEqualTo(infRange.deltaPlus),
            range.deltaMinus
              .add(possiblePlus)
              .isGreaterThanOrEqualTo(infRange.deltaMinus),
          ) match {
            case (true, true) =>
              range
            case (true, false) =>
              range.copy(deltaMinus =
                infRange.deltaMinus.subtract(possiblePlus)
              )
            case (false, true) =>
              range.copy(deltaPlus = infRange.deltaPlus.subtract(possibleMinus))
            case (false, false) =>
              infRange
          }
        } else {
          // no tapping possible, just update the range

          (
            range.deltaPlus.isGreaterThanOrEqualTo(infRange.deltaPlus),
            range.deltaMinus.isLessThanOrEqualTo(infRange.deltaMinus),
          ) match {
            case (true, true) =>
              infRange
            case (true, false) =>
              range.copy(deltaPlus = infRange.deltaPlus)
            case (false, true) =>
              range.copy(deltaMinus = infRange.deltaMinus)
            case (false, false) =>
              range
          }
        }
      }
    }
  }

  object VoltageRange {
    def apply(
        deltaPlus: ComparableQuantity[Dimensionless],
        deltaMinus: ComparableQuantity[Dimensionless],
    ): VoltageRange = {

      val suggestion = (
        deltaPlus.isGreaterThanOrEqualTo(0.asPu),
        deltaMinus.isLessThanOrEqualTo(0.asPu),
      ) match {
        case (true, true) =>
          // calculate an equal distance to 1 pu
          deltaPlus.add(deltaMinus)
        case (false, true) =>
          // violation of the upper voltage limit
          if (deltaPlus.isGreaterThan(deltaMinus)) {
            // if deltaPlus > deltaMinus, we can decrease the voltage by deltaPlus
            deltaPlus
          } else deltaMinus
        case (true, false) =>
          // violation of the upper voltage limit

          if (deltaMinus.isLessThan(deltaPlus)) {
            // if deltaMinus < deltaPlus, we can increase the voltage by deltaMinus
            deltaMinus
          } else deltaPlus
        case (false, false) =>
          // violation of both limit
          deltaPlus
      }

      VoltageRange(
        deltaPlus,
        deltaMinus,
        suggestion,
      )
    }

    def combineSuggestions(
        ranges: Set[VoltageRange]
    ): ComparableQuantity[Dimensionless] = {
      ranges.headOption match {
        case Some(value) =>
          if (ranges.size == 1) {
            value.suggestion
          } else {
            ranges
              .foldLeft(value) { case (combined, current) =>
                (
                  combined.deltaPlus.isGreaterThanOrEqualTo(current.deltaPlus),
                  combined.deltaMinus.isLessThanOrEqualTo(current.deltaMinus),
                ) match {
                  case (true, true) =>
                    current
                  case (true, false) =>
                    combined.copy(deltaPlus = current.deltaPlus)
                  case (false, true) =>
                    combined.copy(deltaMinus = current.deltaMinus)
                  case (false, false) =>
                    combined
                }
              }
              .suggestion
          }
        case None =>
          // no suggestion found => no tapping suggestion
          0.asPu
      }
    }
  }

  case class Congestions(
      voltageCongestions: Boolean,
      lineCongestions: Boolean,
      transformerCongestions: Boolean,
  ) {

    def any: Boolean =
      voltageCongestions || lineCongestions || transformerCongestions

    def assetCongestion: Boolean = lineCongestions || transformerCongestions

    def combine(options: Iterable[Congestions]): Congestions =
      Congestions(
        voltageCongestions || options.exists(_.voltageCongestions),
        lineCongestions || options.exists(_.lineCongestions),
        transformerCongestions || options.exists(_.transformerCongestions),
      )
  }

  object CongestionManagementSteps extends Enumeration {
    val TransformerTapping, TopologyChanges, UsingFlexibilities = Value
  }

}
