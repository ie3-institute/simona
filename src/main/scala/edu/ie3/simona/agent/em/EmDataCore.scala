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

/** Data related to participant scheduling and flex correspondences within an
  * [[EmAgent]]. Depending on the state of the EmAgent, different data is stored
  * and retrieved.
  */
object EmDataCore {

  /** Creates a new instance of an (inactive) EmAgent data core.
    * @param startDate
    *   The start date of the simulation
    */
  def create(implicit startDate: ZonedDateTime): Inactive =
    Inactive(
      Map.empty,
      PriorityMultiBiSet.empty,
      Set.empty,
      FlexCorrespondenceStore(),
      None
    )

  /** @param modelToActor
    *   Map of model uuid to corresponding model actor
    * @param activationQueue
    *   Queue of flex request activations per tick
    * @param flexWithNext
    *   UUIDs of agents to be activated with next activation, whatever tick that
    *   is going to be (the next tick can be changed when agents are
    *   (re-)scheduled)
    * @param correspondences
    *   The data structure storing received and sent flex messages with the
    *   corresponding tick
    * @param lastActiveTick
    *   The last active tick, if applicable
    */
  final case class Inactive private (
      private val modelToActor: Map[UUID, Actor],
      private val activationQueue: PriorityMultiBiSet[Long, UUID],
      private val flexWithNext: Set[UUID],
      private val correspondences: FlexCorrespondenceStore,
      private val lastActiveTick: Option[Long]
  ) {
    def addParticipant(actor: Actor, model: UUID): Inactive =
      copy(
        modelToActor = modelToActor.updated(model, actor)
      )

    def checkActivation(newTick: Long): Boolean =
      activationQueue.headKeyOption.contains(newTick)

    def activate(): Either[String, AwaitingFlexOptions] =
      activationQueue.headKeyOption
        .toRight("Nothing scheduled, cannot activate.")
        .map { newActiveTick =>
          val updatedQueue = flexWithNext.foldLeft(activationQueue) {
            case (currentQueue, model) =>
              currentQueue.set(newActiveTick, model)
              currentQueue
          }
          AwaitingFlexOptions(
            modelToActor,
            updatedQueue,
            correspondences,
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

  /** @param modelToActor
    *   Map of model uuid to corresponding model actor
    * @param activationQueue
    *   Queue of flex request activations per tick
    * @param correspondences
    *   The data structure storing received and sent flex messages with the
    *   corresponding tick
    * @param awaitedFlexOptions
    *   The set of model uuids, from which flex options are still expected
    * @param activeTick
    *   The currently active tick
    */
  final case class AwaitingFlexOptions(
      private val modelToActor: Map[UUID, Actor],
      private val activationQueue: PriorityMultiBiSet[Long, UUID],
      private val correspondences: FlexCorrespondenceStore,
      private val awaitedFlexOptions: Set[UUID] = Set.empty,
      activeTick: Long
  ) {

    def takeNewActivations()
        : Either[String, (Iterable[Actor], AwaitingFlexOptions)] = {
      val toActivate = activationQueue.getAndRemoveSet(activeTick)
      val newFlexOptionsCore =
        copy(awaitedFlexOptions = awaitedFlexOptions.concat(toActivate))
      val (missingActors, participants) = toActivate.toSeq
        .partitionMap { modelUuid =>
          modelToActor
            .get(modelUuid)
            .toRight(modelUuid)
        }

      Either.cond(
        missingActors.isEmpty,
        (participants, newFlexOptionsCore),
        s"Could not find actor(s) for model uuid(s) ${missingActors.mkString(",")}"
      )
    }

    def handleFlexOptions(
        flexOptions: ProvideFlexOptions
    ): AwaitingFlexOptions =
      copy(
        correspondences =
          correspondences.updateFlexOptions(flexOptions, activeTick),
        awaitedFlexOptions = awaitedFlexOptions.excl(flexOptions.modelUuid)
      )

    def isComplete: Boolean = awaitedFlexOptions.isEmpty

    def getFlexOptions: Iterable[(UUID, ProvideFlexOptions)] =
      correspondences.store.flatMap { case (model, correspondence) =>
        correspondence.receivedFlexOptions.map(model -> _.get)
      }

    def handleFlexCtrl(
        ctrlMsgs: Iterable[(UUID, Power)]
    ): AwaitingFlexOptions = {
      val updatedStore = ctrlMsgs.foldLeft(correspondences) {
        case (store, (model, power)) =>
          val ctrlMsg = IssuePowerCtrl(activeTick, power)
          store.updateFlexControl(model, ctrlMsg, activeTick)
      }
      copy(correspondences = updatedStore)
    }

    def fillInMissingIssueCtrl(): AwaitingFlexOptions = {
      val updatedStore = correspondences.store
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
        .foldLeft(correspondences) {
          case (updatedStore, (participant, flexCtrl)) =>
            updatedStore.updateFlexControl(participant, flexCtrl, activeTick)
        }
      copy(correspondences = updatedStore)
    }

    def complete(): Either[
      String,
      (Iterable[(Actor, IssueFlexControl)], AwaitingCompletions)
    ] = {

      val modelUuidToMsg = correspondences.store.flatMap {
        case (modelUuid, correspondence) =>
          correspondence.issuedCtrlMsg.flatMap {
            case WithTime(issueCtrl, tick) if tick == activeTick =>
              Some(modelUuid -> issueCtrl)
            case _ => None
          }
      }

      val (missingActors, actorToMsg) = modelUuidToMsg.toSeq
        .partitionMap { case (modelUuid, issueCtrl) =>
          modelToActor
            .get(modelUuid)
            .map((_, issueCtrl))
            .toRight(modelUuid)
        }

      Either.cond(
        missingActors.isEmpty,
        (
          actorToMsg,
          AwaitingCompletions(
            modelToActor,
            activationQueue = activationQueue,
            correspondences = correspondences,
            awaitedCompletions = modelUuidToMsg.map { case (participant, _) =>
              participant
            }.toSet,
            activeTick = activeTick
          )
        ),
        s"Could not find actor(s) for model uuid(s) ${missingActors.mkString(",")}"
      )
    }

  }

  /** @param modelToActor
    *   Map of model uuid to corresponding model actor
    * @param activationQueue
    *   Queue of flex request activations per tick
    * @param flexWithNext
    *   to be asked for flex options with the following active tick, whatever
    *   that tick is going to be (not with the currently active tick though!)
    * @param correspondences
    *   The data structure storing received and sent flex messages with the
    *   corresponding tick
    * @param awaitedCompletions
    *   The set of model uuids, from which flex completions are still expected
    * @param activeTick
    *   The currently active tick
    */
  final case class AwaitingCompletions(
      private val modelToActor: Map[UUID, Actor],
      private val activationQueue: PriorityMultiBiSet[Long, UUID],
      private val flexWithNext: Set[UUID] = Set.empty,
      private val correspondences: FlexCorrespondenceStore,
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
        correspondences.updateResult(
          completion.modelUuid,
          completion.result,
          activeTick
        )

      val updatedFlexWithNext =
        if (completion.requestAtNextActivation)
          flexWithNext.incl(completion.modelUuid)
        else flexWithNext

      copy(
        correspondences = updatedCorrespondence,
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
            correspondences,
            Some(activeTick)
          )
        )
      }

    def getResults: Iterable[ApparentPower] = {
      correspondences.store.values.flatMap(_.receivedResult.map(_.get))
    }

  }

}
