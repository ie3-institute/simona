/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import edu.ie3.simona.agent.participant.em.EmAgentTyped.Actor
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore2.{
  FlexCorrespondence,
  WithTime
}
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  FlexRequest,
  IssueFlexControl,
  ProvideFlexOptions
}
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK

import java.time.ZonedDateTime

/** TODO use own type of exceptions
  */
final case class FlexCorrespondenceStore2(
    store: Map[Actor, FlexCorrespondence] = Map.empty
)(implicit val startDate: ZonedDateTime) {

  def updateFlexOptions(
      flexOptions: ProvideFlexOptions,
      tick: Long
  ): FlexCorrespondenceStore2 = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(receivedFlexOptions =
        Some(WithTime(flexOptions, tick))
      )

    updateCorrespondence(flexOptions.participant, update)
  }

  def updateFlexControl(
      participant: Actor,
      flexControl: IssueFlexControl,
      tick: Long
  ): FlexCorrespondenceStore2 = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(issuedCtrlMsg = Some(WithTime(flexControl, tick)))

    updateCorrespondence(participant, update)
  }

  def updateResult(
      participant: Actor,
      result: ApparentPower,
      tick: Long
  ): FlexCorrespondenceStore2 = {
    val update: FlexCorrespondence => FlexCorrespondence = correspondence =>
      correspondence.copy(receivedResult = Some(WithTime(result, tick)))

    updateCorrespondence(participant, update)
  }

  def correspondences: Iterable[FlexCorrespondence] =
    store.values

  def latestFlexData(
      participantInput: Map[Actor, SystemParticipantInput]
  ): Iterable[(SystemParticipantInput, FlexCorrespondence)] = {
    store.map { case (participant, correspondence) =>
      val spi = participantInput.getOrElse(
        participant,
        throw new RuntimeException(
          s"There's no participant input model for $participant whatsoever"
        )
      )

      (spi, correspondence)
    }
  }

  private def updateCorrespondence(
      participant: Actor,
      update: FlexCorrespondence => FlexCorrespondence
  ): FlexCorrespondenceStore2 =
    copy(store = store.updated(participant, update(get(participant))))

  private def get(participant: Actor): FlexCorrespondence =
    store
      .getOrElse(
        participant,
        throw new RuntimeException(
          s"No flex correspondences found for $participant"
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
      allParticipants: Set[Actor]
  )(implicit simulationStartDate: ZonedDateTime): FlexCorrespondenceStore2 = {
    val store = allParticipants.map { participant =>
      participant -> FlexCorrespondence()
    }.toMap

    FlexCorrespondenceStore2(store)
  }

}
