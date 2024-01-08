/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore2.{
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
final case class FlexCorrespondenceStore2(
    store: Map[UUID, FlexCorrespondence] = Map.empty
)(implicit val startDate: ZonedDateTime) {

  def updateFlexOptions(
      flexOptions: ProvideFlexOptions,
      tick: Long
  ): FlexCorrespondenceStore2 = {
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
  ): FlexCorrespondenceStore2 = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(issuedCtrlMsg = Some(WithTime(flexControl, tick)))

    updateCorrespondence(model, update)
  }

  def updateResult(
      model: UUID,
      result: ApparentPower,
      tick: Long
  ): FlexCorrespondenceStore2 = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(receivedResult = Some(WithTime(result, tick)))

    updateCorrespondence(model, update)
  }

  def correspondences: Iterable[FlexCorrespondence] =
    store.values

  def latestFlexData(
      participantInput: Map[UUID, SystemParticipantInput]
  ): Iterable[(SystemParticipantInput, FlexCorrespondence)] = {
    store.map { case (model, correspondence) =>
      val spi = participantInput.getOrElse(
        model,
        throw new RuntimeException(
          s"There's no participant input model for $model whatsoever"
        )
      )

      (spi, correspondence)
    }
  }

  private def updateCorrespondence(
      model: UUID,
      update: FlexCorrespondence => FlexCorrespondence
  ): FlexCorrespondenceStore2 =
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

object FlexCorrespondenceStore2 {

  object ExpectedDataTypes extends Enumeration {
    type ExpectedDataType = Value

    val FlexOptions, Results, Nothing = Value
  }

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
  )(implicit simulationStartDate: ZonedDateTime): FlexCorrespondenceStore2 = {
    val store = allParticipants.map { participant =>
      participant -> FlexCorrespondence()
    }.toMap

    FlexCorrespondenceStore2(store)
  }

}
