/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore.{
  FlexCorrespondence,
  WithTime
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueFlexControl,
  ProvideFlexOptions
}

import java.time.ZonedDateTime
import java.util.UUID

/** TODO use own type of exceptions
  */
final case class FlexCorrespondenceStore(
    store: Map[UUID, FlexCorrespondence] = Map.empty
)(implicit val startDate: ZonedDateTime) {

  def updateFlexOptions(
      flexOptions: ProvideFlexOptions,
      tick: Long
  ): FlexCorrespondenceStore = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(receivedFlexOptions =
        Some(WithTime(flexOptions, tick))
      )

    updateCorrespondence(flexOptions.model, update)
  }

  def updateFlexControl(
      model: UUID,
      flexControl: IssueFlexControl,
      tick: Long
  ): FlexCorrespondenceStore = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(issuedCtrlMsg = Some(WithTime(flexControl, tick)))

    updateCorrespondence(model, update)
  }

  def updateResult(
      model: UUID,
      result: ApparentPower,
      tick: Long
  ): FlexCorrespondenceStore = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(receivedResult = Some(WithTime(result, tick)))

    updateCorrespondence(model, update)
  }

  private def updateCorrespondence(
      model: UUID,
      update: FlexCorrespondence => FlexCorrespondence
  ): FlexCorrespondenceStore =
    copy(store = store.updated(model, update(get(model))))

  private def get(model: UUID): FlexCorrespondence =
    store
      .getOrElse(
        model,
        throw new RuntimeException(
          s"No flex correspondences found for $model"
        )
      )
}

object FlexCorrespondenceStore {

  final case class FlexCorrespondence(
      receivedFlexOptions: Option[WithTime[ProvideFlexOptions]] = None,
      issuedCtrlMsg: Option[WithTime[IssueFlexControl]] = None,
      receivedResult: Option[WithTime[ApparentPower]] = None
  )

  final case class WithTime[T](private val obj: T, tick: Long) {
    def get: T = obj
  }

  def apply(
      allParticipants: Set[UUID]
  )(implicit simulationStartDate: ZonedDateTime): FlexCorrespondenceStore = {
    val store = allParticipants.map { participant =>
      participant -> FlexCorrespondence()
    }.toMap

    FlexCorrespondenceStore(store)
  }

}
