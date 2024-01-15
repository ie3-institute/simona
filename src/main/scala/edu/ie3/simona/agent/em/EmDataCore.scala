/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import EmAgent.Actor
import FlexCorrespondenceStore.WithTime
import edu.ie3.simona.ontology.messages.FlexibilityMessage._
import edu.ie3.util.scala.collection.mutable.PriorityMultiBiSet
import squants.Power

import java.time.ZonedDateTime
import java.util.UUID

/** Data related to participant scheduling and flex correspondences
  */
object EmDataCore {

  def create(implicit startDate: ZonedDateTime): Inactive =
    Inactive(
      Map.empty,
      PriorityMultiBiSet.empty,
      Set.empty,
      FlexCorrespondenceStore(),
      None
    )

  /** @param activationQueue
    * @param flexWithNext
    *   to be activated with next activation, whatever tick that is going to be
    * @param lastActiveTick
    */
  final case class Inactive private (
      private val modelToActor: Map[UUID, Actor],
      private val activationQueue: PriorityMultiBiSet[Long, UUID],
      private val flexWithNext: Set[UUID],
      private val correspondenceStore: FlexCorrespondenceStore,
      private val lastActiveTick: Option[Long]
  ) {
    def addParticipant(actor: Actor, model: UUID): Inactive =
      copy(
        modelToActor = modelToActor.updated(model, actor)
      )

    def checkActivation(newTick: Long): Boolean =
      activationQueue.headKeyOption.contains(newTick)

    def activate(): AwaitingFlexOptions = {
      val newActiveTick = activationQueue.headKeyOption.getOrElse(
        throw new RuntimeException("Nothing scheduled, cannot activate.")
      )
      val updatedQueue = flexWithNext.foldLeft(activationQueue) {
        case (currentQueue, model) =>
          currentQueue.set(newActiveTick, model)
          currentQueue
      }
      AwaitingFlexOptions(
        modelToActor,
        updatedQueue,
        correspondenceStore,
        activeTick = newActiveTick
      )
    }

    def checkSchedule(newTick: Long): Boolean =
      lastActiveTick.forall(newTick >= _ + 1)

    def handleSchedule(
        model: UUID,
        newTick: Long
    ): (Option[Long], Inactive) = {
      val oldEarliestTick = activationQueue.headKeyOption

      activationQueue.set(newTick, model)
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
      private val modelToActor: Map[UUID, Actor],
      private val activationQueue: PriorityMultiBiSet[Long, UUID],
      private val correspondenceStore: FlexCorrespondenceStore,
      private val awaitedFlexOptions: Set[UUID] = Set.empty,
      activeTick: Long
  ) {
    def checkSchedule( // TODO needed here?
        participant: UUID,
        newTick: Long
    ): Boolean =
      newTick >= activeTick

    def handleSchedule(
        model: UUID,
        newTick: Long
    ): AwaitingFlexOptions = {
      activationQueue.set(newTick, model)
      this
    }

    def takeNewActivations(): (Iterable[Actor], AwaitingFlexOptions) = {
      val toActivate = activationQueue
        .getAndRemoveSet(activeTick)
      val newFlexOptionsCore =
        copy(awaitedFlexOptions = awaitedFlexOptions.concat(toActivate))
      val participants = toActivate.map(
        modelToActor.getOrElse(_, throw new RuntimeException(""))
      )
      (participants, newFlexOptionsCore)
    }

    def handleFlexOptions(
        flexOptions: ProvideFlexOptions
    ): AwaitingFlexOptions =
      copy(
        correspondenceStore =
          correspondenceStore.updateFlexOptions(flexOptions, activeTick),
        awaitedFlexOptions = awaitedFlexOptions.excl(flexOptions.modelUuid)
      )

    def isComplete: Boolean = awaitedFlexOptions.isEmpty

    def getFlexOptions: Iterable[(UUID, ProvideFlexOptions)] =
      correspondenceStore.store.flatMap { case (model, correspondence) =>
        correspondence.receivedFlexOptions.map(model -> _.get)
      }

    def handleFlexCtrl(
        ctrlMsgs: Iterable[(UUID, Power)]
    ): AwaitingFlexOptions = {
      val updatedStore = ctrlMsgs.foldLeft(correspondenceStore) {
        case (store, (model, power)) =>
          val ctrlMsg = IssuePowerCtrl(activeTick, power)
          store.updateFlexControl(model, ctrlMsg, activeTick)
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

    def complete()
        : (Iterable[(Actor, IssueFlexControl)], AwaitingCompletions) = {

      val currentCtrlMessages = correspondenceStore.store.flatMap {
        case (modelUuid, correspondence) =>
          correspondence.issuedCtrlMsg.flatMap {
            case WithTime(issueCtrl, tick) if tick == activeTick =>
              Some(modelUuid -> issueCtrl)
            case _ => None
          }
      }

      (
        currentCtrlMessages.map { case (model, issueCtrl) =>
          (
            modelToActor.getOrElse(model, throw new RuntimeException("")),
            issueCtrl
          )
        },
        AwaitingCompletions(
          modelToActor,
          activationQueue = activationQueue,
          correspondenceStore = correspondenceStore,
          awaitedCompletions =
            currentCtrlMessages.map { case (participant, _) =>
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
    * @param activeTick
    */
  final case class AwaitingCompletions(
      private val modelToActor: Map[UUID, Actor],
      private val activationQueue: PriorityMultiBiSet[Long, UUID],
      private val flexWithNext: Set[UUID] = Set.empty,
      private val correspondenceStore: FlexCorrespondenceStore,
      private val awaitedCompletions: Set[UUID],
      activeTick: Long
  ) {

    def checkCompletion(modelUuid: UUID): Boolean =
      awaitedCompletions.contains(modelUuid)

    def handleCompletion(
        completion: FlexCtrlCompletion
    ): AwaitingCompletions = {

      // mutable queue
      completion.requestAtTick
        .foreach { activationQueue.set(_, completion.modelUuid) }

      val updatedCorrespondence =
        correspondenceStore.updateResult(
          completion.modelUuid,
          completion.result,
          activeTick
        )

      val updatedFlexWithNext =
        if (completion.requestAtNextActivation)
          flexWithNext.incl(completion.modelUuid)
        else flexWithNext

      copy(
        correspondenceStore = updatedCorrespondence,
        flexWithNext = updatedFlexWithNext,
        awaitedCompletions = awaitedCompletions.excl(completion.modelUuid)
      )
    }

    def maybeComplete(): Option[(Option[Long], Inactive)] =
      Option.when(
        awaitedCompletions.isEmpty &&
          !activationQueue.headKeyOption.contains(activeTick)
      ) {
        (
          activationQueue.headKeyOption,
          Inactive(
            modelToActor,
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
