/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.GridAgent.getTappingOptions
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.exceptions.ResultException
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid._
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.typed.ActorRef
import squants.electro.ElectricPotential
import tech.units.indriya.ComparableQuantity

import java.lang.Math.sqrt
import java.util.UUID
import javax.measure.quantity.Dimensionless

/** Support and helper methods for calculations done during the congestion
  * management.
  */
trait CongestionManagementSupport {

  /** Method to get the tapping options.
    *
    * @param tappings
    *   all [[TransformerTapping]] models
    * @return
    *   the possible voltage increase and decrease
    */
  def getTappingOptions(tappings: Set[TransformerTapping]): (
      ComparableQuantity[Dimensionless],
      ComparableQuantity[Dimensionless],
  ) = {
    // allow tapping only if all transformers support tapping
    if (tappings.forall(_.hasAutoTap)) {

      val tappingRanges = tappings.map { tapping =>
        val currentPos = tapping.currentTapPos
        val deltaV = tapping.deltaV.divide(-100)
        val increase = deltaV.multiply(tapping.tapMin - currentPos)
        val decrease = deltaV.multiply(tapping.tapMax - currentPos)

        (increase, decrease)
      }.toSeq

      if (tappings.size == 1) {
        tappingRanges(0)
      } else {
        // check for possible increase and decrease that can be applied to all transformers
        (
          tappingRanges.map(_._1).minOption.getOrElse(0.asPu),
          tappingRanges.map(_._2).maxOption.getOrElse(0.asPu),
        )
      }
    } else {
      // no tapping possible
      (0.asPu, 0.asPu)
    }
  }
}

object CongestionManagementSupport {

  /** Case class that contains information about congestions in a subgrid.
    *
    * @param voltageCongestions
    *   true if the lower or upper voltage limit is violated
    * @param lineCongestions
    *   true if there is a line congestion
    * @param transformerCongestions
    *   true if there is a transformer congestion
    */
  final case class Congestions(
      voltageCongestions: Boolean,
      lineCongestions: Boolean,
      transformerCongestions: Boolean,
  ) {

    /** Returns true if any congestion occurred.
      */
    def any: Boolean =
      voltageCongestions || lineCongestions || transformerCongestions

    /** Returns true if there is either a line or transformer congestion
      */
    def assetCongestion: Boolean = lineCongestions || transformerCongestions

    /** Method for combining multiple [[Congestions]].
      * @param options
      *   that should be combined with the own options
      * @return
      *   a new [[Congestions]]
      */
    def combine(options: Iterable[Congestions]): Congestions =
      Congestions(
        voltageCongestions || options.exists(_.voltageCongestions),
        lineCongestions || options.exists(_.lineCongestions),
        transformerCongestions || options.exists(_.transformerCongestions),
      )
  }

  object Congestions {

    /** Method for finding congestions within the given power flow results.
      * @param powerFlowResults
      *   of the last simulation
      * @param gridComponents
      *   all [[GridComponents]]
      * @param voltageLimits
      *   the voltage limits of the grid
      * @param vNom
      *   the nominal voltage to calculate the transformer power
      * @param subnetNo
      *   number of the subgrid
      * @return
      *   a new [[Congestions]]
      */
    def apply(
        powerFlowResults: PowerFlowResultEvent,
        gridComponents: GridComponents,
        voltageLimits: VoltageLimits,
        vNom: ElectricPotential,
        subnetNo: Int,
    ): Congestions = {
      val nodeRes =
        powerFlowResults.nodeResults.map(res => res.getInputModel -> res).toMap

      // filter nodes in subnet
      val nodesInSubnet =
        gridComponents.nodes.filter(_.subnet == subnetNo).map(_.uuid)

      // checking for voltage congestions
      val voltageCongestion = nodeRes.values
        .filter(res => nodesInSubnet.contains(res.getInputModel))
        .exists { res =>
          !voltageLimits.isInLimits(res.getvMag())
        }

      // checking for line congestions
      val linesLimits = gridComponents.lines.map { line =>
        line.uuid -> line
      }.toMap
      val lineCongestion = powerFlowResults.lineResults.exists { res =>
        val iA = res.getiAMag().getValue.doubleValue()
        val iB = res.getiBMag().getValue.doubleValue()
        val iNom = linesLimits(res.getInputModel).iNom.value

        iA > iNom || iB > iNom
      }

      // checking for transformer congestions
      val transformer2w = gridComponents.transformers.map { transformer =>
        transformer.uuid -> transformer
      }.toMap
      val transformer2wCongestion =
        powerFlowResults.transformer2wResults.exists { res =>
          val transformer = transformer2w(res.getInputModel)

          val vMag = nodeRes(
            transformer.lvNodeUuid
          ).getvMag().getValue.doubleValue() * vNom.toKilovolts

          sqrt(3.0) * res
            .getiBMag()
            .getValue
            .doubleValue() * vMag > transformer.sRated.toKilowatts
        }

      val transformer3w = gridComponents.transformers3w.map { transformer =>
        transformer.uuid -> transformer
      }.toMap
      val transformer3wCongestion =
        powerFlowResults.transformer3wResults.exists { res =>
          val transformer = transformer3w(res.input)

          val vMag = nodeRes(
            transformer.lvNodeUuid
          ).getvMag().getValue.doubleValue() * vNom.toKilovolts

          sqrt(
            3.0
          ) * res.currentMagnitude.value * vMag > transformer.sRated.toKilowatts
        }

      Congestions(
        voltageCongestion,
        lineCongestion,
        transformer2wCongestion || transformer3wCongestion,
      )
    }
  }

  /** Object that contains information about possible voltage changes. <p> If
    * the delta plus is negative -> upper voltage violation <p> If the delta
    * minus is positive -> lower voltage violation <p> If both above cases
    * happen at the same time the the suggestion is set to the delta plus,
    * because having a too high voltage is more severe
    *
    * @param deltaPlus
    *   maximale possible voltage increase
    * @param deltaMinus
    *   maximale possible voltage decrease
    * @param suggestion
    *   for voltage change
    */
  final case class VoltageRange(
      deltaPlus: ComparableQuantity[Dimensionless],
      deltaMinus: ComparableQuantity[Dimensionless],
      suggestion: ComparableQuantity[Dimensionless],
  ) {

    /** Method to update this voltage range with line voltage delta.
      *
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
      *
      * @param inferiorData
      *   map: inferior grid to [[VoltageRange]] and [[TransformerTappingModel]]
      * @return
      *   a new [[VoltageRange]]
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
      *   results from simulating the grid
      * @param voltageLimits
      *   voltage limits
      * @param gridComponents
      *   all components of the grid
      * @param inferiorData
      *   map: inferior grid to [[VoltageRange]] and [[TransformerTappingModel]]
      * @return
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
        .getOrElse(throw new ResultException(s"No node result found!"))
      val maxVoltage = nodeResMap
        .maxByOption(_._2)
        .getOrElse(throw new ResultException(s"No node result found!"))

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

    /** Method to calculate a voltage delta for the given line currents. <p> -
      * If there is a line congestion, increasing the voltage by the returned
      * delta should mitigate them. <p> - If there is no line congestion, the
      * returned voltage shows the possible voltage decrease. <p> - Formula: V *
      * I = (V + deltaV) * (I + deltaI)
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
      val lineMap = gridComponents.lines.map(line => line.uuid -> line).toMap

      // calculate the voltage change that ensures there is no line congestion
      val voltageChanges =
        lineResults.map(res => res.getInputModel -> res).map {
          case (uuid, res) =>
            val line = lineMap(uuid)

            // calculate the voltage and the current change at the end of the line that sees the highest current
            val (voltage, deltaI) =
              if (res.getiAMag().isGreaterThan(res.getiBMag())) {
                (
                  nodeResults(line.nodeAUuid).getValue.doubleValue(),
                  line.iNom.value - res.getiAMag().getValue.doubleValue(),
                )
              } else {
                (
                  nodeResults(line.nodeBUuid).getValue.doubleValue(),
                  line.iNom.value - res.getiBMag().getValue.doubleValue(),
                )
              }

            // calculate the voltage change
            (voltage * deltaI) / line.iNom.value * -1
        }

      // determine the actual possible voltage change
      val change = voltageChanges.maxOption.getOrElse(
        throw new ResultException(s"No line result found!")
      )

      // change < 0 => tapping down possible
      // change > 0 => tapping up is necessary
      change.asPu
    }

    /** Combines the given [[VoltageRange]]s and adds the given offset.
      * @param ranges
      *   given ranges
      * @param offset
      *   to use
      * @return
      *   a new [[VoltageRange]]
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

  /** Enumeration with all congestion management steps.
    */
  object CongestionManagementSteps extends Enumeration {
    val TransformerTapping, TopologyChanges, UsingFlexibilities = Value
  }

}
