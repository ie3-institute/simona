/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.ontology.messages

import edu.ie3.simona.ontology.messages.PowerMessage.ProvideGridPowerMessage.ExchangePower
import edu.ie3.util.scala.quantities.ReactivePower
import squants.{Dimensionless, Power}

import java.util.UUID

sealed trait PowerMessage

/** Messages that should be send and received/processed from AssetAgents
  */
object PowerMessage {

  sealed trait PowerRequestMessage extends PowerMessage

  sealed trait PowerResponseMessage extends PowerMessage

  sealed trait ProvidePowerMessage extends PowerResponseMessage {
    def p: Power

    def q: ReactivePower
  }

  /** Request the power values for the requested tick from an AssetAgent and
    * provide the latest nodal voltage
    *
    * @param currentTick
    *   The tick that power values are requested for
    * @param eInPu
    *   Real part of the complex, dimensionless nodal voltage
    * @param fInPu
    *   Imaginary part of the complex, dimensionless nodal voltage
    */
  final case class RequestAssetPowerMessage(
      currentTick: Long,
      eInPu: Dimensionless,
      fInPu: Dimensionless,
  ) extends PowerRequestMessage

  /** Provide power values as a reply to a [[RequestAssetPowerMessage]]
    *
    * @param p
    *   Unchanged active power
    * @param q
    *   Unchanged reactive power
    */
  final case class AssetPowerChangedMessage(
      override val p: Power,
      override val q: ReactivePower,
  ) extends ProvidePowerMessage

  /** Provide values as a reply to a [[RequestAssetPowerMessage]]. In contrast
    * to [[AssetPowerChangedMessage]], this message indicates that the same
    * values for [[p]] and [[q]] has been send again as in the previous request
    *
    * @param p
    *   Active power from the previous request
    * @param q
    *   Reactive power from the previous request
    */
  final case class AssetPowerUnchangedMessage(
      override val p: Power,
      override val q: ReactivePower,
  ) extends ProvidePowerMessage

  /** Request complex power at the nodes that the inferior sub grid shares with
    * the sender's sub grid
    * @param currentSweepNo
    *   The current sweep
    * @param nodeUuids
    *   The UUIDs of the nodes that are bordering the sender's grid
    */
  final case class RequestGridPowerMessage(
      currentSweepNo: Int,
      nodeUuids: Seq[UUID],
  ) extends PowerRequestMessage

  /** Provide complex power at the nodes that the sender's sub grid shares with
    * the superior sub grid, as a reply to a [[RequestGridPowerMessage]].
    * @param nodalResidualPower
    *   The complex powers of the shared nodes
    */
  final case class ProvideGridPowerMessage(
      nodalResidualPower: Seq[ExchangePower]
  ) extends PowerResponseMessage

  object ProvideGridPowerMessage {

    /** Defining the exchanged power at one interconnection point
      *
      * @param nodeUuid
      *   Unique identifier of the node, at which this residual power did appear
      * @param p
      *   Active power from the previous request
      * @param q
      *   Reactive power from the previous request
      */
    final case class ExchangePower(
        nodeUuid: UUID,
        override val p: Power,
        override val q: ReactivePower,
    ) extends ProvidePowerMessage
  }

  /** Indicate that the power flow calculation failed, as a reply to a
    * [[RequestGridPowerMessage]].
    */
  final case object FailedPowerFlow extends PowerResponseMessage

}
