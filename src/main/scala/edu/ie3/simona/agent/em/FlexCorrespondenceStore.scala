/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.simona.agent.em.FlexCorrespondenceStore.{
  FlexCorrespondence,
  WithTime,
}
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage.{
  IssueFlexControl,
  ProvideFlexOptions,
}

import java.time.ZonedDateTime
import java.util.UUID

/** Data structure that supports storing flex correspondences, i.e. flex
  * messages that have been sent by various flexibility providers and received
  * by an [[EmAgent]]. This correspondence store only stores the last received
  * message of each type per flex provider.
  *
  * @param store
  *   Map that stores a flex correspondence per flex provider model UUID
  * @param startDate
  *   The start date of the simulation used for calculations involving ticks
  */
final case class FlexCorrespondenceStore(
    store: Map[UUID, FlexCorrespondence] = Map.empty
)(implicit val startDate: ZonedDateTime) {

  /** Updates the latest flex options for the flex provider, overwriting the
    * former flex options, if applicable
    *
    * @param flexOptions
    *   The new flex options
    * @param tick
    *   The tick that the flex options were received at
    * @return
    *   The updated flex options store
    */
  def updateFlexOptions(
      flexOptions: ProvideFlexOptions,
      tick: Long,
  ): FlexCorrespondenceStore =
    updateCorrespondence(
      flexOptions.modelUuid,
      _.copy(receivedFlexOptions = Some(WithTime(flexOptions, tick))),
    )

  /** Updates the latest flex control for the flex provider, overwriting the
    * former flex control, if applicable
    *
    * @param modelUuid
    *   The UUID of the flex provider model
    * @param flexControl
    *   The new flex control message sent
    * @param tick
    *   The tick that the flex control message was sent at
    * @return
    *   The updated flex options store
    */
  def updateFlexControl(
      modelUuid: UUID,
      flexControl: IssueFlexControl,
      tick: Long,
  ): FlexCorrespondenceStore =
    updateCorrespondence(
      modelUuid,
      _.copy(issuedCtrlMsg = Some(WithTime(flexControl, tick))),
    )

  /** Updates the latest result for the flex provider, overwriting the former
    * result, if applicable
    *
    * @param modelUuid
    *   The UUID of the flex provider model
    * @param result
    *   The new result
    * @param tick
    *   The tick that the result was received at
    * @return
    *   The updated flex options store
    */
  def updateResult(
      modelUuid: UUID,
      result: ApparentPower,
      tick: Long,
  ): FlexCorrespondenceStore =
    updateCorrespondence(
      modelUuid,
      _.copy(receivedResult = Some(WithTime(result, tick))),
    )

  private def updateCorrespondence(
      modelUuid: UUID,
      update: FlexCorrespondence => FlexCorrespondence,
  ): FlexCorrespondenceStore = {
    val correspondence = store.getOrElse(
      modelUuid,
      FlexCorrespondence(),
    )
    copy(store = store.updated(modelUuid, update(correspondence)))
  }

}

object FlexCorrespondenceStore {

  /** Class that holds flex messages for one flex provider. Only the latest
    * messages of a type are stored with the tick that they were received.
    *
    * @param receivedFlexOptions
    *   The latest flex options that have been received by the EmAgent
    * @param issuedCtrlMsg
    *   The latest flex control message that has been sent to the flex provider
    * @param receivedResult
    *   The latest result that has been received by the EmAgent
    */
  final case class FlexCorrespondence(
      receivedFlexOptions: Option[WithTime[ProvideFlexOptions]] = None,
      issuedCtrlMsg: Option[WithTime[IssueFlexControl]] = None,
      receivedResult: Option[WithTime[ApparentPower]] = None,
  )

  /** Wrapper that allows storing a tick with an object
    *
    * @param obj
    *   The object
    * @param tick
    *   The tick
    * @tparam T
    *   The type of the object
    */
  final case class WithTime[T](private val obj: T, tick: Long) {
    def get: T = obj
  }

}
