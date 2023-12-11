/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.participant.em

import edu.ie3.simona.agent.participant.em.EmAgent.Actor
import edu.ie3.simona.agent.participant.em.FlexCorrespondenceStore2.WithTime
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet
import squants.Power
import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower

import java.time.ZonedDateTime

/** Data related to participant scheduling and flex correspondences
  */
object EmDataCore {

  def create(implicit startDate: ZonedDateTime): Inactive =
    Inactive(
      PriorityMultiBiSet.empty,
      Set.empty,
      FlexCorrespondenceStore2(),
      None
    )

  /** @param activationQueue
    * @param flexWithNext
    *   to be activated with next activation, whatever tick that is going to be
    * @param lastActiveTick
    */
  final case class Inactive private (
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
      private val flexWithNext: Set[Actor],
      private val correspondenceStore: FlexCorrespondenceStore2,
      private val lastActiveTick: Option[Long]
  ) {
    def checkActivation(newTick: Long): Boolean =
      activationQueue.headKeyOption.contains(newTick)

    def activate(): AwaitingFlexOptions = {
      val newActiveTick = activationQueue.headKeyOption.getOrElse(
        throw new RuntimeException("Nothing scheduled, cannot activate.")
      )
      val updatedQueue = flexWithNext.foldLeft(activationQueue) {
        case (currentQueue, participant) =>
          currentQueue.set(newActiveTick, participant)
          currentQueue
      }
      AwaitingFlexOptions(
        updatedQueue,
        correspondenceStore,
        activeTick = newActiveTick
      )
    }

    def checkSchedule(newTick: Long): Boolean =
      lastActiveTick.forall(newTick >= _ + 1)

    def handleSchedule(
        participant: Actor,
        newTick: Long
    ): (Option[Long], Inactive) = {
      val oldEarliestTick = activationQueue.headKeyOption

      activationQueue.set(newTick, participant)
      val newEarliestTick = activationQueue.headKeyOption

      val maybeScheduleTick =
        Option.when(newEarliestTick != oldEarliestTick)(newEarliestTick).flatten

      (maybeScheduleTick, this)
    }

  }

  /** @param activationQueue
    * @param activeActors
    * @param activeTick
    */
  final case class AwaitingFlexOptions(
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
      private val correspondenceStore: FlexCorrespondenceStore2,
      private val awaitedFlexOptions: Set[Actor] = Set.empty,
      activeTick: Long
  ) {
    def checkSchedule( // TODO needed here?
        participant: Actor,
        newTick: Long
    ): Boolean =
      newTick >= activeTick

    def handleSchedule(
        participant: Actor,
        newTick: Long
    ): AwaitingFlexOptions = {
      activationQueue.set(newTick, participant)
      this
    }

    def takeNewActivations(): (Iterable[Actor], AwaitingFlexOptions) = {
      val toActivate = activationQueue.getAndRemoveSet(activeTick)
      val newActiveCore =
        copy(awaitedFlexOptions = awaitedFlexOptions.concat(toActivate))
      (toActivate, newActiveCore)
    }

    def handleFlexOptions(
        flexOptions: ProvideFlexOptions
    ): AwaitingFlexOptions =
      copy(correspondenceStore =
        correspondenceStore.updateFlexOptions(flexOptions, activeTick)
      )

    def isComplete: Boolean = awaitedFlexOptions.isEmpty

    def getFlexOptions: Iterable[(Actor, ProvideFlexOptions)] =
      correspondenceStore.store.flatMap { case (actor, correspondence) =>
        correspondence.receivedFlexOptions.map(actor -> _.get)
      }

    def handleFlexCtrl(
        ctrlMsgs: Iterable[(Actor, Power)]
    ): AwaitingFlexOptions = {
      val updatedStore = ctrlMsgs.foldLeft(correspondenceStore) {
        case (store, (actor, power)) =>
          val ctrlMsg = IssuePowerCtrl(activeTick, power)
          store.updateFlexControl(actor, ctrlMsg, activeTick)
      }
      copy(correspondenceStore = updatedStore)
    }

    def fillInMissingIssueCtrl(): AwaitingFlexOptions = {
      val updatedStore = correspondenceStore.store
        .filter { case (_, correspondence) =>
          // let's get those correspondences that have not received a ctrl msg at this tick
          correspondence.issuedCtrlMsg.forall(_.tick < activeTick)
        }
        .flatMap { case (participant, correspondence) =>
          // We still create a no-control-msg in its place, if...

          // ... a response is expected for this tick, since we've
          // requested flex options at this tick
          val currentlyRequested =
            correspondence.receivedFlexOptions.forall(_.tick == activeTick)

          // ... flex control has been issued for this participant
          // at an earlier tick
          val flexControlCancelled = correspondence.issuedCtrlMsg match {
            case Some(WithTime(_: IssuePowerCtrl, tick)) if tick < activeTick =>
              true
            case _ => false
          }

          Option.when(currentlyRequested || flexControlCancelled)(
            participant -> IssueNoCtrl(activeTick)
          )
        }
        .foldLeft(correspondenceStore) {
          case (updatedStore, (participant, flexCtrl)) =>
            updatedStore.updateFlexControl(participant, flexCtrl, activeTick)
        }
      copy(correspondenceStore = updatedStore)
    }

    def complete(): (Iterable[(Actor, IssueFlexControl)], AwaitingResults) = {

      val currentCtrlMessages = correspondenceStore.store.flatMap {
        case (participant, correspondence) =>
          correspondence.issuedCtrlMsg.flatMap {
            case WithTime(issueCtrl, tick) if tick == activeTick =>
              Some(participant -> issueCtrl)
            case _ => None
          }
      }

      (
        currentCtrlMessages,
        AwaitingResults(
          activationQueue = activationQueue,
          correspondenceStore = correspondenceStore,
          awaitedResults = currentCtrlMessages.map { case (participant, _) =>
            participant
          }.toSet,
          activeTick = activeTick
        )
      )
    }

  }

  /** @param activationQueue
    * @param flexWithNext
    *   to be asked for flex options with the following active tick, whatever
    *   that tick is going to be (not with the currently active tick though!)
    * @param lastActiveTick
    */
  final case class AwaitingResults(
      private val activationQueue: PriorityMultiBiSet[Long, Actor],
      private val flexWithNext: Set[Actor] = Set.empty,
      private val correspondenceStore: FlexCorrespondenceStore2,
      private val awaitedResults: Set[Actor],
      activeTick: Long
  ) {

    def checkCompletion(participant: Actor): Boolean =
      awaitedResults.contains(participant)

    def handleCompletion(completion: FlexCtrlCompletion): AwaitingResults = {

      // mutable queue
      completion.requestAtTick
        .foreach { activationQueue.set(_, completion.participant) }

      val updatedCorrespondence =
        correspondenceStore.updateResult(
          completion.participant,
          completion.result,
          activeTick
        )

      val updatedFlexWithNext =
        if (completion.requestAtNextActivation)
          flexWithNext.incl(completion.participant)
        else flexWithNext

      copy(
        correspondenceStore = updatedCorrespondence,
        flexWithNext = updatedFlexWithNext,
        awaitedResults = awaitedResults.excl(completion.participant)
      )
    }

    def maybeComplete(): Option[(Option[Long], Inactive)] =
      Option.when(
        awaitedResults.isEmpty &&
          !activationQueue.headKeyOption.contains(activeTick)
      ) {
        (
          activationQueue.headKeyOption,
          Inactive(
            activationQueue,
            flexWithNext,
            correspondenceStore,
            Some(activeTick)
          )
        )
      }

    def getResults: Iterable[ApparentPower] = {
      correspondenceStore.store.values.flatMap(_.receivedResult.map(_.get))
    }

  }

}
