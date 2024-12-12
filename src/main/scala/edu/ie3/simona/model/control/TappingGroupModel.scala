/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.model.control

import edu.ie3.datamodel.models.input.connector.ConnectorPort
import edu.ie3.simona.agent.grid.CongestionManagementSupport.VoltageRange
import edu.ie3.simona.agent.grid.GridAgent
import edu.ie3.simona.exceptions.GridInconsistencyException
import edu.ie3.simona.model.grid.Transformer3wPowerFlowCase.PowerFlowCaseA
import edu.ie3.simona.model.grid.{
  Transformer3wModel,
  TransformerModel,
  TransformerTapping,
}
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.apache.pekko.actor.typed.ActorRef
import org.slf4j.Logger
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Dimensionless

/** A group of [[TransformerTapping]] with all associated [[ActorRef]]s.
  * @param refs
  *   a set of [[ActorRef]]s
  * @param tappings
  *   a set of [[TransformerTapping]]s
  */
final case class TappingGroupModel(
    tappings: Set[TransformerTapping],
    refs: Set[ActorRef[GridAgent.Request]],
    hasAutoTap: Boolean,
) {

  def updateTapPositions(
      delta: ComparableQuantity[Dimensionless],
      refMap: Map[ActorRef[GridAgent.Request], VoltageRange],
      log: Logger,
  ): ComparableQuantity[Dimensionless] = if (hasAutoTap) {
    // get all possible voltage ranges of the inferior grids
    val inferiorRanges = refs.map(refMap)

    val suggestion =
      VoltageRange.combineAndUpdate(inferiorRanges, delta)

    // calculating the tap changes for all transformers and the resulting voltage delta
    val (tapChange, deltaV) = calculateTapAndVoltage(suggestion)

    // change the tap pos of all transformers
    tapChange.foreach { case (tapping, tapChange) =>
      tapChange compare 0 match {
        case 1 =>
          // change > 0 -> increase
          tapping.incrTapPos(tapChange)
        case -1 =>
          // change < 0 -> decrease
          tapping.decrTapPos(Math.abs(tapChange))
        case 0 =>
        // no change, do nothing
      }
    }

    log.debug(
      s"For inferior grids $refs, suggestion: $suggestion, delta: $deltaV"
    )

    deltaV
  } else 0.asPu

  /** Method for calculating the tap pos changes for all given transformers and
    * the voltage delta.
    *
    * @param range
    *   given voltage range
    * @return
    *   a map: model to tap pos change and resulting voltage delta
    */
  def calculateTapAndVoltage(
      range: VoltageRange
  ): (Map[TransformerTapping, Int], ComparableQuantity[Dimensionless]) = {
    val noTapping = (tappings.map(t => t -> 0).toMap, 0.asPu)
    val suggestion = range.suggestion

    if (suggestion.isEquivalentTo(0.asPu)) {
      return noTapping
    }

    // calculate a tap option for each transformer
    if (tappings.forall(_.hasAutoTap)) {

      // get all possible deltas
      val possibleDeltas = tappings
        .map(
          _.getPossibleVoltageChanges(
            range.deltaPlus,
            range.deltaMinus,
            ConnectorPort.B,
          )
        )

      // calculates a voltage change option
      val deltaOption = if (possibleDeltas.exists(_.isEmpty)) {
        // there is a transformer that cannot be tapped
        None
      } else {
        // the actual delta that can be used for all transformers
        Some(findCommonDelta(suggestion, possibleDeltas))
      }

      deltaOption match {
        case Some(delta) =>
          // calculate the voltage and tap position change for all models
          val deltas = tappings
            .map(model => model -> model.computeDeltas(delta, ConnectorPort.B))
            .toMap

          // mapping the data
          val taps = deltas.map { case (tapping, (tap, _)) => tapping -> tap }
          val actualDelta =
            deltas.map { case (_, (_, delta)) => delta }.toSeq(0)

          (taps, actualDelta)
        case None =>
          noTapping
      }
    } else {
      // return no tappings if there is at least one transformer that cannot be taped
      noTapping
    }
  }

  /** Method for finding a common delta that can be applied to all transformers.
    * @param suggestion
    *   the given suggestion
    * @param possibleDeltas
    *   the possible deltas for each transformer
    * @return
    *   either a common delta or zero
    */
  def findCommonDelta(
      suggestion: ComparableQuantity[Dimensionless],
      possibleDeltas: Set[List[ComparableQuantity[Dimensionless]]],
  ): ComparableQuantity[Dimensionless] = {
    val expectedSize = possibleDeltas.size
    // reduce and filter the possible options
    val filteredOptions: Set[ComparableQuantity[Dimensionless]] =
      getReducedOptions(suggestion, possibleDeltas)
        .groupBy(identity)
        .filter { case (_, seq) => seq.size == expectedSize }
        .keySet

    // find the best suitable delta
    filteredOptions.size match {
      case 0 => 0.asPu
      case 1 => filteredOptions.headOption.getOrElse(0.asPu)
      case _ if filteredOptions.exists(_.isEquivalentTo(suggestion)) =>
        suggestion
      case _ => // get the minimal and maximal option
        val minOption =
          filteredOptions.filter(_.isLessThan(suggestion)).lastOption
        val maxOption = filteredOptions.find(_.isGreaterThan(suggestion))

        (minOption, maxOption) match {
          case (Some(min), Some(max)) =>
            val minDiff =
              Math.abs(suggestion.subtract(min).getValue.doubleValue())
            val maxDiff =
              Math.abs(suggestion.subtract(max).getValue.doubleValue())

            // find the difference that is smaller
            if (minDiff < maxDiff) {
              min
            } else max

          case (Some(min), _) => min // only minimal option found
          case (_, Some(max)) => max // only maximal option found
          case _ => 0.asPu // no option found -> therefore no change
        }
    }
  }

  /** Method to reduce the possible deltas.
    * @param suggestion
    *   to compare to
    * @param possibleDeltas
    *   the possible deltas
    * @return
    *   a reduced sequence
    */
  private def getReducedOptions(
      suggestion: ComparableQuantity[Dimensionless],
      possibleDeltas: Set[List[ComparableQuantity[Dimensionless]]],
  ): Seq[ComparableQuantity[Dimensionless]] = possibleDeltas.toSeq.flatMap {
    deltas =>
      if (deltas.exists(_.isEquivalentTo(suggestion))) {
        List(suggestion)
      } else {
        val minOption =
          deltas.filter(_.isLessThan(suggestion)).sorted.lastOption
        val maxOption = deltas.sorted.find(_.isGreaterThan(suggestion))

        // check possible deltas
        (minOption, maxOption) match {
          case (Some(min), Some(max)) => List(min, max)
          case (Some(min), _)         => List(min)
          case (_, Some(max))         => List(max)
          case _                      => List()
        }
      }
  }

}

object TappingGroupModel {

  def apply(
      tappings: Set[TransformerTapping],
      refs: Set[ActorRef[GridAgent.Request]],
  ): TappingGroupModel =
    TappingGroupModel(
      tappings,
      refs,
      tappings.forall(_.hasAutoTap),
    )

  /** Method for building [[TappingGroupModel]]s by grouping transformers with
    * their [[ActorRef]]s. A group consist of all transformers connecting
    * another grid with this grid and the [[ActorRef]] of the other grid.
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
    *   a set of [[TappingGroupModel]]s
    */
  def buildModels(
      receivedData: Map[ActorRef[GridAgent.Request], Set[TransformerTapping]],
      transformer3ws: Set[Transformer3wModel],
  ): Set[TappingGroupModel] = {
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
        // building tapping groups
        TappingGroupModel(tappingModels, refs)
      }
      .toSet
  }

}
