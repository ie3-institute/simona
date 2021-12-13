/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower

import java.util.UUID
import javax.measure.quantity.{Dimensionless, Power}
import tech.units.indriya.ComparableQuantity

sealed trait PowerMessage

/** Messages that should be send and received/processed from AssetAgents
  */
object PowerMessage {

  sealed trait PowerRequestMessage extends PowerMessage

  sealed trait PowerResponseMessage extends PowerMessage

  sealed trait ProvidePowerMessage extends PowerResponseMessage {
    def p: ComparableQuantity[Power]

    def q: ComparableQuantity[Power]
  }

  /** Request the power values for the requested tick from an AssetAgent
    *
    * @param currentTick
    */
  final case class RequestAssetPowerMessage(
      currentTick: Long,
      eInPu: ComparableQuantity[Dimensionless],
      fInPu: ComparableQuantity[Dimensionless]
  ) extends PowerRequestMessage

  /** Provide power values as a reply on an [[RequestAssetPowerMessage]]
    *
    * @param p
    * @param q
    */
  final case class AssetPowerChangedMessage(
      override val p: ComparableQuantity[Power],
      override val q: ComparableQuantity[Power]
  ) extends ProvidePowerMessage

  /** Provide values as a reply on a [[RequestAssetPowerMessage]]. In contrast
    * to [[AssetPowerChangedMessage]], this message indicates that the same
    * values for [[p]] and [[q]] has been send again as in the previous request
    *
    * @param p
    *   active power from the previous request
    * @param q
    *   reactive power from the previous request
    */
  final case class AssetPowerUnchangedMessage(
      override val p: ComparableQuantity[Power],
      override val q: ComparableQuantity[Power]
  ) extends ProvidePowerMessage

  final case class RequestGridPowerMessage(
      currentSweepNo: Int,
      nodeUuids: Vector[UUID]
  ) extends PowerRequestMessage

  final case class ProvideGridPowerMessage(
      nodalResidualPower: Vector[ExchangePower]
  ) extends PowerResponseMessage
  object ProvideGridPowerMessage {

    /** Defining the exchanged power at one interconnection point
      *
      * @param nodeUuid
      *   Unique identifier of the node, at which this residual power did appear
      * @param p
      *   active power from the previous request
      * @param q
      *   reactive power from the previous request
      */
    final case class ExchangePower(
        nodeUuid: UUID,
        override val p: ComparableQuantity[Power],
        override val q: ComparableQuantity[Power]
    ) extends ProvidePowerMessage
  }

  final case object FailedPowerFlow extends PowerResponseMessage

}
