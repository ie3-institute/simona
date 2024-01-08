/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.datamodel.models.input.system.SystemParticipantInput
import edu.ie3.datamodel.models.result.system.SystemParticipantResult
import edu.ie3.simona.agent.ValueStore
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore.{
  FlexCorrespondence,
  ExpectedDataTypes
}
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore.ExpectedDataTypes.ExpectedDataType
import edu.ie3.simona.ontology.messages.FlexibilityMessage.{
  IssueFlexControl,
  ProvideFlexOptions
}
import edu.ie3.simona.util.TickUtil.RichZonedDateTime

import java.time.ZonedDateTime
import java.util.UUID

/** TODO probably we do not need the value stores here
  */
final case class FlexCorrespondenceStore(
    private val expectedParticipants: Set[UUID],
    expectedDataType: ExpectedDataType,
    private val correspondences: Map[UUID, ValueStore[FlexCorrespondence]],
    implicit val startDate: ZonedDateTime
) {
  def isComplete: Boolean = expectedParticipants.isEmpty

  def setExpectingFlexOptions(
      expectedParticipants: Set[UUID],
      tick: Long
  ): FlexCorrespondenceStore = {

    val updatedCorrespondences =
      expectedParticipants.foldLeft(correspondences) { case (flexCorr, uuid) =>
        val participantValueStore = flexCorr.getOrElse(
          uuid,
          throw new RuntimeException(s"ValueStore for UUID $uuid not found")
        )

        val updatedStore = ValueStore.updateValueStore(
          participantValueStore,
          tick,
          FlexCorrespondence()
        )

        flexCorr.updated(uuid, updatedStore)
      }

    copy(
      expectedParticipants = expectedParticipants,
      expectedDataType = ExpectedDataTypes.FlexOptions,
      correspondences = updatedCorrespondences
    )
  }

  def setExpectingResults(
      expectedParticipants: Set[UUID]
  ): FlexCorrespondenceStore =
    copy(
      expectedParticipants = expectedParticipants,
      expectedDataType = ExpectedDataTypes.Results
    )

  def setReceiveComplete(): FlexCorrespondenceStore =
    copy(expectedDataType = ExpectedDataTypes.Nothing)

  def addReceivedFlexOptions(
      tick: Long,
      flexOptions: ProvideFlexOptions
  ): FlexCorrespondenceStore = {

    // safety check
    if (expectedDataType != ExpectedDataTypes.FlexOptions)
      throw new RuntimeException(
        s"Received flex options $flexOptions at tick $tick, but did not expect any. " +
          s"Expected data type: $expectedDataType"
      )

    val addToCorrespondence: FlexCorrespondence => FlexCorrespondence = _ =>
      FlexCorrespondence(flexOptions)

    addData(flexOptions.model, tick, addToCorrespondence).copy(
      expectedParticipants = expectedParticipants.excl(flexOptions.model)
    )
  }

  def addIssuedFlexControl(
      participant: UUID,
      tick: Long,
      flexControl: IssueFlexControl
  ): FlexCorrespondenceStore = {

    val addToCorrespondence: FlexCorrespondence => FlexCorrespondence =
      _.copy(issuedCtrlMsg = Some(flexControl), participantResult = None)

    addData(participant, tick, addToCorrespondence)
  }

  def addReceivedResult(
      result: SystemParticipantResult
  ): FlexCorrespondenceStore = {
    val participant = result.getInputModel
    val resultTick = result.getTime.toTick

    if (expectedDataType != ExpectedDataTypes.Results)
      throw new RuntimeException(
        s"Received result $result for tick $resultTick, but did not expect any. " +
          s"Expected data type: $expectedDataType"
      )

    val addToCorrespondence: FlexCorrespondence => FlexCorrespondence =
      _.copy(participantResult = Some(result))

    addData(participant, resultTick, addToCorrespondence).copy(
      expectedParticipants = expectedParticipants.excl(participant)
    )
  }

  private def addData(
      participant: UUID,
      tick: Long,
      addToCorrespondence: FlexCorrespondence => FlexCorrespondence
  ): FlexCorrespondenceStore = {
    val participantValueStore =
      correspondences.getOrElse(
        participant,
        throw new RuntimeException(
          s"No flex correspondence store found for $participant"
        )
      )

    val (_, flexCorrespondence) = participantValueStore
      .last(tick)
      .getOrElse(
        throw new RuntimeException(
          s"No flex correspondence found for model $participant and tick $tick"
        )
      )

    val updatedValueStore = ValueStore.updateValueStore(
      participantValueStore,
      tick,
      addToCorrespondence(flexCorrespondence)
    )

    val updatedCorrespondences =
      correspondences.updated(
        participant,
        updatedValueStore
      )

    copy(correspondences = updatedCorrespondences)
  }

  def latestCorrespondences: Iterable[FlexCorrespondence] =
    correspondences.map { case (uuid, store) =>
      store
        .last()
        .map { case (_, correspondence) =>
          correspondence
        }
        .getOrElse(
          throw new RuntimeException(
            s"There's no expected flex options for $uuid whatsoever"
          )
        )
    }

  def latestFlexData(
      participantInput: Map[UUID, SystemParticipantInput]
  ): Iterable[(SystemParticipantInput, FlexCorrespondence, Long)] =
    correspondences.map { case (uuid, store) =>
      val spi = participantInput.getOrElse(
        uuid,
        throw new RuntimeException(
          s"There's no participant input model for $uuid whatsoever"
        )
      )
      val (dataTick, correspondence) = store
        .last()
        .getOrElse(
          throw new RuntimeException(
            s"There's no expected flex options for $spi whatsoever"
          )
        )

      (spi, correspondence, dataTick)
    }
}

object FlexCorrespondenceStore {

  object ExpectedDataTypes extends Enumeration {
    type ExpectedDataType = Value

    val FlexOptions, Results, Nothing = Value
  }

  final case class FlexCorrespondence(
      receivedFlexOptions: Option[ProvideFlexOptions] = None,
      issuedCtrlMsg: Option[IssueFlexControl] = None,
      participantResult: Option[SystemParticipantResult] = None
  ) {
    def hasResults: Boolean =
      participantResult.nonEmpty
  }

  object FlexCorrespondence {
    def apply(flexOptions: ProvideFlexOptions): FlexCorrespondence =
      FlexCorrespondence(
        Some(flexOptions),
        None
      )
  }

  def apply(
      allParticipants: Set[UUID],
      maxTickSpan: Long,
      simulationStartDate: ZonedDateTime
  ): FlexCorrespondenceStore = {
    val correspondences = allParticipants.map { participant =>
      participant -> ValueStore(maxTickSpan)
    }.toMap

    FlexCorrespondenceStore(
      Set.empty,
      ExpectedDataTypes.Nothing,
      correspondences,
      simulationStartDate
    )
  }

}
