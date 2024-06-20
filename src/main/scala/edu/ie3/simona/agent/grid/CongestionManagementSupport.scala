/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.grid

import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.datamodel.models.result.connector.LineResult
import edu.ie3.simona.agent.grid.CongestionManagementSupport.{
  TappingGroup,
  VoltageRange,
}
import edu.ie3.simona.event.ResultEvent.PowerFlowResultEvent
import edu.ie3.simona.exceptions.{GridInconsistencyException, ResultException}
import edu.ie3.simona.model.grid.GridModel.GridComponents
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.PowerFlowCaseA
import edu.ie3.simona.model.grid._
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

  /** Method for grouping transformers with their [[ActorRef]]s. A group consist
    * of all transformers connecting another grid with this grid and the
    * [[ActorRef]] of the other grid.
    *
    * <p> If the other grid is connected by a port of [[Transformer3wModel]],
    * only the model with [[PowerFlowCaseA]] is inside the returned map, due to
    * the way the tapping works. Because the tapping also effects the other port
    * of the [[Transformer3wModel]], the [[ActorRef]] of that grid needs to be
    * in the same group and also all of its other connecting transformers,
    *
    * <p> Examples: <p> - grid 0 -> grid 1: [[TransformerModel]] <p> - grid 0 ->
    * grid 1: [[Transformer3wModel]] port B <p> - grid 0 -> grid 2:
    * [[Transformer3wModel]] port C <p> - grid 0 -> grid 3: [[TransformerModel]]
    * <p> - grid 0 -> grid 4: two [[TransformerModel]]
    *
    * <p> Result: <p> - Group 1: one [[TransformerModel]] and one
    * [[Transformer3wModel]] to [[ActorRef]]s of grid 1 and 2 <p> - Group 2: one
    * [[TransformerModel]] to [[ActorRef]] of grid 3 <p> - Group 3: two
    * [[TransformerModel]] to [[ActorRef]] of grid 4
    *
    * @param receivedData
    *   map: actor ref to connecting transformers
    * @param transformer3ws
    *   set of [[Transformer3wModel]] with [[PowerFlowCaseA]]
    * @return
    *   a set of [[TappingGroup]]s
    */
  def groupTappingModels(
      receivedData: Map[ActorRef[GridAgent.Request], Set[TransformerTapping]],
      transformer3ws: Set[Transformer3wModel],
  ): Set[TappingGroup] = {
    val transformer3wMap = transformer3ws.map(t => t.uuid -> t).toMap

    // builds all groups
    receivedData
      .foldLeft(
        Map.empty[Set[TransformerTapping], Set[ActorRef[GridAgent.Request]]]
      ) { case (combined, (ref, tappings)) =>
        // get all transformer models
        val updated: Set[TransformerTapping] = tappings.map {
          case transformerModel: TransformerModel =>
            transformerModel
          case transformer3wModel: Transformer3wModel =>
            // in case of a three winding transformer, we need the model of the port A
            transformer3wMap.getOrElse(
              transformer3wModel.uuid,
              throw new GridInconsistencyException(
                s"No three winding transformer found."
              ),
            )
          case unsupported =>
            throw new IllegalArgumentException(
              s"The transformer type ${unsupported.getClass} is not supported."
            )
        }.toSet

        // find a group that already contains one of the given transformer models
        val keyOption = combined.keySet.find { keys =>
          updated.exists(key => keys.contains(key))
        }

        // if a key is found, add the current transformer models and the ref to that group
        // else add a new group
        keyOption
          .map { key =>
            val refs = combined(key)
            val updatedMap = combined.removed(key)

            val newKey = key ++ updated
            val newValue = refs ++ Set(ref)

            updatedMap ++ Map(newKey -> newValue)
          }
          .getOrElse {
            combined ++ Map(updated -> Set(ref))
          }
      }
      .map { case (tappingModels, refs) =>
        TappingGroup(refs, tappingModels)
      }
      .toSet
  }

  /** Method for calculating the tap pos changes for all given transformers and
    * the voltage delta.
    *
    * @param suggestion
    *   given delta suggestion
    * @param tappings
    *   a set of all transformers
    * @return
    *   a map: model to tap pos change and resulting voltage delta
    */
  def calculateTapAndVoltage(
      suggestion: ComparableQuantity[Dimensionless],
      tappings: Seq[TransformerTapping],
  ): (Map[TransformerTapping, Int], ComparableQuantity[Dimensionless]) = {
    val noTapping = (tappings.map(t => t -> 0).toMap, 0.asPu)

    if (suggestion.isEquivalentTo(0.asPu)) {
      return noTapping
    }

    // calculate a tap option for each transformer
    if (tappings.forall(_.hasAutoTap)) {
      val possibleDeltas = tappings.map(_.possibleDeltas(ConnectorPort.B))

      if (possibleDeltas.exists(_.size < 2)) {
        // there is a transformer that cannot be taped
        noTapping
      } else {

        // reduce all possible deltas
        val reducedOptions = possibleDeltas.map { deltas =>
          if (deltas.exists(_.isEquivalentTo(suggestion))) {
            List(suggestion)
          } else {
            val minOption = deltas.filter(_.isLessThan(suggestion)).lastOption
            val maxOption = deltas.find(_.isGreaterThan(suggestion))

            // check possible deltas
            (minOption, maxOption) match {
              case (Some(min), Some(max)) => List(min, max)
              case (Some(min), _)         => List(min)
              case (_, Some(max))         => List(max)
              case _                      => List()
            }
          }
        }

        // filter the possible options
        val filteredOptions = reducedOptions.flatten
          .groupBy(identity)
          .filter(_._2.size == reducedOptions.size)
          .flatMap(_._2.headOption)

        // find the best suitable delta
        val deltaOption = if (suggestion.isGreaterThan(0.asPu)) {
          filteredOptions.minByOption(_.getValue.doubleValue())
        } else {
          filteredOptions.maxByOption(_.getValue.doubleValue())
        }

        // the actual delta that can be used for all transformers
        val delta = deltaOption.getOrElse(0.asPu)

        val deltas = tappings
          .map(model => model -> model.computeDeltas(delta, ConnectorPort.B))
          .toMap

        val taps = deltas.map { case (tapping, (tap, _)) => tapping -> tap }
        val actualDelta = deltas.map(_._2._2).toSeq(0)

        (taps, actualDelta)
      }
    } else {
      // return no tappings if there is at least one transformer that cannot be taped
      noTapping
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
  def calculatePossibleVoltageRange(
      powerFlowResultEvent: PowerFlowResultEvent,
      voltageLimits: VoltageLimits,
      gridComponents: GridComponents,
      inferiorData: Map[ActorRef[
        GridAgent.Request
      ], (VoltageRange, Set[TransformerTapping])],
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

    // calculates the utilisation of each line
    val lineUtilisation = lineResMap.map { case (uuid, res) =>
      val iNom = lineMap(uuid).iNom
      val diffA = Amperes(res.getiAMag().getValue.doubleValue()) / iNom
      val diffB = Amperes(res.getiBMag().getValue.doubleValue()) / iNom

      uuid -> Math.max(diffA, diffB)
    }

    // find the maximale utilisation
    val maxUtilisation = lineUtilisation
      .maxByOption(_._2)
      .getOrElse(throw new ResultException(s"No line result found!"))
      ._1

    val line = lineMap(maxUtilisation)
    val res = lineResMap(maxUtilisation)
    val resA = res.getiAMag()
    val resB = res.getiBMag()

    // calculate the voltage change limits
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

  /** A group of [[TransformerTapping]] with all associated [[ActorRef]]s.
    * @param refs
    *   a set of [[ActorRef]]s
    * @param tappingModels
    *   a set of [[TransformerTapping]]
    */
  final case class TappingGroup(
      refs: Set[ActorRef[GridAgent.Request]],
      tappingModels: Set[TransformerTapping],
  )

  /** Object that contains information about possible voltage changes. <p> If
    * the delta plus is negative -> upper voltage violation <p> If the delta
    * minus is positive -> lower voltage violation <p> If both above cases
    * happen at the same time the the suggestion is set to the delta plus,
    * because having a too high voltage is more severe
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

    def isInRange(
        delta: ComparableQuantity[Dimensionless]
    ): Boolean =
      delta.isGreaterThanOrEqualTo(deltaMinus) && delta.isLessThanOrEqualTo(
        deltaPlus
      )

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
        ], (VoltageRange, Set[TransformerTapping])]
    ): VoltageRange = {

      inferiorData.foldLeft(this) { case (range, (_, (infRange, tappings))) =>
        // allow tapping only if all transformers support tapping
        val (plus, minus) = if (tappings.forall(_.hasAutoTap)) {

          // TODO: Enhance tests, to tests these changes
          val tappingRanges = tappings.map { tapping =>
            val currentPos = tapping.currentTapPos
            val deltaV = tapping.deltaV
            val increase = deltaV.multiply(tapping.tapMax - currentPos)
            val decrease = deltaV.multiply(tapping.tapMin - currentPos)

            (increase, decrease)
          }.toSeq

          val (possiblePlus, possibleMinus) = if (tappings.size == 1) {
            tappingRanges(0)
          } else {
            // check for possible increase and decrease that can be applied to all transformers
            (
              tappingRanges.map(_._1).minOption.getOrElse(0.asPu),
              tappingRanges.map(_._2).maxOption.getOrElse(0.asPu),
            )
          }

          (
            range.deltaPlus
              .add(possibleMinus)
              .isLessThanOrEqualTo(infRange.deltaPlus),
            range.deltaMinus
              .add(possiblePlus)
              .isGreaterThanOrEqualTo(infRange.deltaMinus),
          ) match {
            case (true, true) =>
              (range.deltaPlus, range.deltaMinus)
            case (true, false) =>
              (range.deltaPlus, infRange.deltaMinus.subtract(possiblePlus))
            case (false, true) =>
              (infRange.deltaPlus.subtract(possibleMinus), range.deltaMinus)
            case (false, false) =>
              (infRange.deltaPlus, infRange.deltaMinus)
          }
        } else {
          // no tapping possible, just update the range

          (
            range.deltaPlus.isGreaterThanOrEqualTo(infRange.deltaPlus),
            range.deltaMinus.isLessThanOrEqualTo(infRange.deltaMinus),
          ) match {
            case (true, true) =>
              (infRange.deltaPlus, infRange.deltaMinus)
            case (true, false) =>
              (infRange.deltaPlus, range.deltaMinus)
            case (false, true) =>
              (range.deltaPlus, infRange.deltaMinus)
            case (false, false) =>
              (range.deltaPlus, range.deltaMinus)
          }
        }

        VoltageRange(plus, minus)
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
      } else if (plus > 0 && minus > 0) {
        // we have a voltage violation of the lower limit
        // since the upper limit is fine, we can increase the voltage a bit
        plus
      } else if (plus < 0 && minus < 0) {
        // we have a voltage violation of the upper limit
        // since the lower limit is fine, we can decrease the voltage a bit
        minus
      } else 0 // we have a voltage violation of both limits, we can't fix this

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

    def combineSuggestions(
        ranges: Iterable[VoltageRange]
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

  final case class Congestions(
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
