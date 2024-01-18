/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.agent.em

import edu.ie3.simona.agent.participant.data.Data.PrimaryData.ApparentPower
import EmAgent.Actor
import FlexCorrespondenceStore.WithTime
import edu.ie3.simona.ontology.messages.flex.FlexibilityMessage._
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

  /** Data structure holding relevant data and providing methods that handle
    * interactions with an inactive [[EmAgent]]
    *
    * @param modelToActor
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

    /** Adds a connected agent, given its model UUID and actor reference
      * @param actor
      *   The agent's [[org.apache.pekko.actor.typed.ActorRef]]
      * @param model
      *   The agent's model UUID
      * @return
      *   The adapted [[Inactive]] core
      */
    def addParticipant(actor: Actor, model: UUID): Inactive =
      copy(
        modelToActor = modelToActor.updated(model, actor)
      )

    /** Tries to handle an activation of the EmAgent for given tick. If the
      * activation for the tick is not valid, an error message is returned. If
      * successful, an [[AwaitingFlexOptions]] data core is returned with the
      * active tick set to the earliest tick scheduled.
      *
      * @param newTick
      *   The tick that the scheduler is to be activated with
      * @return
      *   The changed [[AwaitingFlexOptions]] that should be used for the
      *   activated EM agent in a [[Right]] if successful, otherwise an error
      *   message in a [[Left]]
      */
    def tryActivate(newTick: Long): Either[String, AwaitingFlexOptions] =
      activationQueue.headKeyOption
        .map { nextScheduledTick =>
          Either.cond(
            newTick <= nextScheduledTick,
            (),
            s"Cannot activate with new tick $newTick because the next scheduled tick $nextScheduledTick needs to be activated first."
          )
        }
        .getOrElse(Right(()))
        .map { _ =>
          // schedule flex requests for those participants which
          // want to be asked at the next active tick, whatever
          // that tick is going to be
          val updatedQueue = flexWithNext.foldLeft(activationQueue) {
            case (currentQueue, model) =>
              currentQueue.set(newTick, model)
              currentQueue
          }

          AwaitingFlexOptions(
            modelToActor,
            updatedQueue,
            correspondences,
            activeTick = newTick
          )
        }

    /** Tries to handle the scheduling a flex request for a connected agent for
      * given tick. If scheduling for the tick is not valid, an error message is
      * returned. If, on the other hand, the flex request scheduling is
      * successful and makes a separate scheduling of the current [[EmAgent]]
      * with its parent necessary, the tick that the EM agent needs to be
      * scheduled for is returned.
      *
      * @param model
      *   The model UUID of the agent to be scheduled
      * @param newTick
      *   The tick that the agent is scheduled for
      * @return
      *   A tuple of the optional tick that the current EM agent should be
      *   scheduled for with its parent, and the changed [[Inactive]] core in a
      *   [[Right]] if successful, otherwise an error message in a [[Left]]
      */
    def tryHandleSchedule(
        model: UUID,
        newTick: Long
    ): Either[String, (Option[Long], Inactive)] = Either.cond(
      lastActiveTick.forall(newTick >= _ + 1), {
        val oldEarliestTick = activationQueue.headKeyOption

        activationQueue.set(newTick, model)
        val newEarliestTick = activationQueue.headKeyOption

        val maybeScheduleTick =
          Option
            .when(newEarliestTick != oldEarliestTick)(newEarliestTick)
            .flatten

        (maybeScheduleTick, this)
      },
      s"Cannot schedule a flex request for $model at tick $newTick because the last active tick was $lastActiveTick"
    )

    def hasFlexWithNext: Boolean = flexWithNext.nonEmpty

    /** Returns the tick that will be activated next (if applicable) at the
      * current state.
      */
    def nextActiveTick: Option[Long] =
      activationQueue.headKeyOption

    /** Returns relevant results for all connected agents.
      */
    def getResults: Iterable[ApparentPower] =
      correspondences.store.values.flatMap(_.receivedResult.map(_.get))

  }

  /** Data structure holding relevant data and providing methods that handle
    * interactions with an [[EmAgent]] that is waiting to receive all relevant
    * flex options and subsequently calculate flex control
    *
    * @param modelToActor
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

    /** Removes and returns flex requests scheduled for the current tick, which
      * can be sent out at the current moment.
      *
      * @return
      *   A tuple of a collection of agents scheduled for the current tick, and
      *   the updated [[AwaitingFlexOptions]] core
      */
    def tryTakeNewFlexRequests()
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

    /** Handles the retrieval of flex options sent by some connected agent for
      * the currently active tick.
      *
      * @param flexOptions
      *   The received flex options
      * @return
      *   The updated [[AwaitingFlexOptions]] core
      */
    def handleFlexOptions(
        flexOptions: ProvideFlexOptions
    ): AwaitingFlexOptions =
      copy(
        correspondences =
          correspondences.updateFlexOptions(flexOptions, activeTick),
        awaitedFlexOptions = awaitedFlexOptions.excl(flexOptions.modelUuid)
      )

    /** Checks whether all awaited flex options have been received and we can
      * continue by calculating flex control. This method does not change the
      * state of the [[AwaitingFlexOptions]] data core.
      * @return
      *   true if all awaited flex options have been received
      */
    def isComplete: Boolean = awaitedFlexOptions.isEmpty

    /** Returns all flex options that are currently relevant, which can include
      * flex options received at an earlier tick
      * @return
      *   all relevant flex options
      */
    def getFlexOptions: Iterable[(UUID, ProvideFlexOptions)] =
      correspondences.store.flatMap { case (model, correspondence) =>
        correspondence.receivedFlexOptions.map(model -> _.get)
      }

    /** Handles and stores the control messages created by this [[EmAgent]]
      *
      * @param ctrlMsgs
      *   The control messages created by this EM agent
      * @return
      *   The updated [[AwaitingFlexOptions]] core
      */
    def handleFlexCtrl(
        ctrlMsgs: Iterable[(UUID, Power)]
    ): AwaitingFlexOptions = {
      val updatedStore = ctrlMsgs.foldLeft(correspondences) {
        case (store, (model, power)) =>
          val ctrlMsg = IssuePowerControl(activeTick, power)
          store.updateFlexControl(model, ctrlMsg, activeTick)
      }
      copy(correspondences = updatedStore)
    }

    /** The model strategy might miss control messages when creating them in
      * bulk. This method creates the missing messages, in particular for those
      * agents that have been issued a flex request for the current tick and
      * those that have received a control messages at an earlier tick.
      * @return
      *   The updated [[AwaitingFlexOptions]] core
      */
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

          // ... or flex control has been issued for this participant
          // at an earlier tick
          val flexControlCancelled = correspondence.issuedCtrlMsg match {
            case Some(WithTime(_: IssuePowerControl, tick))
                if tick < activeTick =>
              true
            case _ => false
          }

          Option.when(currentlyRequested || flexControlCancelled)(
            participant -> IssueNoControl(activeTick)
          )
        }
        .foldLeft(correspondences) {
          case (updatedStore, (participant, flexCtrl)) =>
            updatedStore.updateFlexControl(participant, flexCtrl, activeTick)
        }
      copy(correspondences = updatedStore)
    }

    /** Completes the current state by collecting and returning the control
      * messages for the current tick if possible, and otherwise returning an
      * error String
      *
      * @return
      *   A [[Left]] with an error messages on error, otherwise a [[Right]] with
      *   a collection of agent-and-message pairs and an updated
      *   [[AwaitingCompletions]] core
      */
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

  /** Data structure holding relevant data and providing methods that handle
    * interactions with an [[EmAgent]] that is waiting to receive flex
    * completions from all active connected agents (those that received flex
    * control in this tick)
    *
    * @param modelToActor
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

    /** Tries to handle the completion of some connected agent for the currently
      * active tick. If completion is not valid, an error message is returned.
      *
      * @param completion
      *   The completion message that has been received
      * @return
      *   The updated [[AwaitingCompletions]] core in a [[Right]] if successful,
      *   otherwise an error message in a [[Left]]
      */
    def tryHandleCompletion(
        completion: FlexCtrlCompletion
    ): Either[String, AwaitingCompletions] = {
      Either.cond(
        awaitedCompletions.contains(completion.modelUuid),
        handleCompletion(completion),
        s"Participant ${completion.modelUuid} is not part of the expected completing participants"
      )
    }

    private def handleCompletion(
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

    /** Checks whether the current activation of the [[EmAgent]] can be
      * completed, which is usually the case when all activated connected agents
      * have completed and there are no new flex requests that can be sent out
      * for the current tick.
      *
      * @return
      *   If the current activation of the EM agent can be completed, the
      *   [[Inactive]] data core is returned that should be used in the
      *   following inactive state.
      */
    def maybeComplete(): Option[Inactive] =
      Option.when(
        awaitedCompletions.isEmpty &&
          !activationQueue.headKeyOption.contains(activeTick)
      ) {
        Inactive(
          modelToActor,
          activationQueue,
          flexWithNext,
          correspondences,
          Some(activeTick)
        )
      }

  }

}
