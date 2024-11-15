/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.listener.{DelayedStopHelper, RuntimeEventListener}
import edu.ie3.simona.main.RunSimona.SimonaEnded
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import edu.ie3.util.scala.Scope
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, PostStop, Terminated}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

/** Main entrance point to a simona simulation as the guardian actor. This actor
  * starts the initialization of all actors and waits for the simulation to end.
  * This actor does NOT report back any simulation status except of if the
  * overall simulation has been successful or not. For specific status
  * information, the user needs to pass in and subscribe to the corresponding
  * listener e.g. [[edu.ie3.simona.event.listener.RuntimeEventListener]] for
  * simulation status or [[edu.ie3.simona.event.listener.ResultEventListener]]
  * for result events
  *
  * @since 01.07.20
  */
object SimonaSim {

  sealed trait Request

  /** Starts the initialization and then the simulation itself */
  final case class Start(
      starter: ActorRef[SimonaEnded]
  ) extends Request

  /** Indicates that the simulation has ended successfully */
  case object SimulationEnded extends Request

  /** Creates a new [[SimonaSim]] behavior
    *
    * @param simonaSetup
    *   The setup instructions
    */
  def apply(simonaSetup: SimonaSetup): Behavior[Request] =
    Behaviors.receivePartial { case (ctx, startMsg @ Start(starter)) =>
      // We redirect to initializing behavior so that starter ref
      // is available in case of a sudden termination of this actor
      ctx.self ! startMsg
      initializing(simonaSetup, starter)
    }

  /** Initializing behavior that is separated from [[apply]] above only because
    * in case of a sudden stop of this actor itself (PostStop signal), the
    * ActorRef of the starter also needs to be available for sending
    * [[SimonaEnded]]
    *
    * @param starter
    *   The starter ref that needs to be notified once simulation has ended
    */
  private def initializing(
      simonaSetup: SimonaSetup,
      starter: ActorRef[SimonaEnded],
  ): Behavior[Request] =
    Behaviors
      .receivePartial[Request] { case (ctx, Start(_)) =>
        val runtimeEventListener = simonaSetup.runtimeEventListener(ctx)

        val timeAdvancer =
          simonaSetup.timeAdvancer(ctx, ctx.self, runtimeEventListener)
        val scheduler = simonaSetup.scheduler(ctx, timeAdvancer)

        // External simulations have to be scheduled for initialization first,
        // so that the phase switch permanently activates them first
        val extSimulationData: ExtSimSetupData =
          simonaSetup.extSimulations(ctx, scheduler)

        /* start services */
        // primary service proxy
        val primaryServiceProxy =
          simonaSetup.primaryServiceProxy(ctx, scheduler, extSimulationData)

        // weather service
        val weatherService =
          simonaSetup.weatherService(ctx, scheduler)

        val environmentRefs = EnvironmentRefs(
          scheduler,
          runtimeEventListener.toClassic,
          primaryServiceProxy,
          weatherService,
          extSimulationData.evDataService,
          extSimulationData.extEmDataService,
        )

        val resultEventListeners =
          simonaSetup.resultEventListener(ctx, extSimulationData)

        /* start grid agents  */
        val gridAgents = simonaSetup.gridAgents(
          ctx,
          environmentRefs,
          resultEventListeners,
        )

        val otherActors = Iterable[ActorRef[_]](
          timeAdvancer,
          scheduler,
          primaryServiceProxy.toTyped,
          weatherService.toTyped,
        ) ++
          gridAgents ++
          extSimulationData.extDataServices.values.map(_.toTyped)

        /* watch all actors */
        resultEventListeners.foreach(ctx.watch)
        ctx.watch(runtimeEventListener)
        extSimulationData.extResultDataService.foreach(ref => ctx.watch(ref))
        extSimulationData.extSimAdapters.foreach(extSimAdapter =>
          ctx.watch(extSimAdapter.toTyped)
        )
        otherActors.foreach(ctx.watch)

        // Start simulation
        timeAdvancer ! TimeAdvancer.Start()

        val delayedActors = resultEventListeners.appended(runtimeEventListener)

        extSimulationData.extResultDataService.foreach(ref =>
          delayedActors.appended(ref)
        )

        idle(
          ActorData(
            starter,
            extSimulationData.extSimAdapters,
            runtimeEventListener,
            delayedActors,
            otherActors,
          )
        )
      }
      .receiveSignal { case (_, PostStop) =>
        // Something must have gone wrong during initialization above
        // (probably an unexpected exception thrown), there's nothing
        // much we can do here besides notifying starter
        starter ! SimonaEnded(successful = false)

        Behaviors.stopped
      }

  /** Behavior that is active while the simulation is running
    */
  private def idle(actorData: ActorData): Behavior[Request] = Behaviors
    .receivePartial[Request] { case (ctx, SimulationEnded) =>
      ctx.log.info(
        "Simulation terminated successfully. Stopping children and waiting for results to flush out..."
      )

      stopChildren(ctx, actorData, simulationSuccessful = true)
    }
    .receiveSignal { case (ctx, Terminated(actor)) =>
      Scope(actorData)
        .map(data =>
          data.copy(
            delayedStoppingActors =
              data.delayedStoppingActors.toSeq.filterNot(_ == actor)
          )
        )
        .map { data =>
          ctx.log.error(
            "An actor ({}) unexpectedly terminated. " +
              "Stopping all children and reporting simulation failure. " +
              "See logs and possibly stacktrace for details.",
            actor,
          )

          // also notify RuntimeEventListener that error happened
          data.runtimeEventListener ! RuntimeEvent.Error(
            "Simulation stopped with error."
          )

          stopChildren(ctx, data, simulationSuccessful = false)
        }
        .get
    }

  /** Stops all children that can be stopped right away, and requests children
    * with delayed stops to stop
    */
  private def stopChildren(
      ctx: ActorContext[_],
      actorData: ActorData,
      simulationSuccessful: Boolean,
  ): Behavior[Request] = {
    actorData.otherActors.foreach { ref =>
      ctx.unwatch(ref)
      ctx.stop(ref)
    }

    actorData.extSimAdapters.foreach(extSimAdapter => {
      ctx.unwatch(extSimAdapter)
      extSimAdapter ! ExtSimAdapter.Stop(simulationSuccessful)
    })

    // if the simulation is successful, we're waiting for the delayed
    // stopping listeners to terminate and thus do not unwatch them here
    actorData.delayedStoppingActors.foreach(
      _ ! DelayedStopHelper.FlushAndStop
    )

    maybeStop(
      ctx,
      actorData.starter,
      actorData.delayedStoppingActors,
      simulationSuccessful,
    )
  }

  /** Behavior that waits for all remaining actors with delayed stops to stop
    */
  private def waitingForListener(
      starter: ActorRef[SimonaEnded],
      remainingListeners: Seq[ActorRef[_]],
      simulationSuccessful: Boolean,
  ): Behavior[Request] = Behaviors.receiveSignal[Request] {
    case (ctx, Terminated(actor)) if remainingListeners.contains(actor) =>
      val updatedRemainingListeners = remainingListeners.filterNot(_ == actor)

      maybeStop(ctx, starter, updatedRemainingListeners, simulationSuccessful)
  }

  /** Stopping this actor and notifying starter if all actors with delayed stops
    * have stopped
    */
  private def maybeStop(
      ctx: ActorContext[_],
      starter: ActorRef[SimonaEnded],
      remainingListeners: Seq[ActorRef[_]],
      simulationSuccessful: Boolean,
  ): Behavior[Request] = {
    if (remainingListeners.isEmpty) {
      ctx.log.debug(
        "All actors with delayed stops have terminated. Ending simulation."
      )

      starter ! SimonaEnded(simulationSuccessful)

      Behaviors.stopped
    } else {
      waitingForListener(
        starter,
        remainingListeners,
        simulationSuccessful,
      )
    }
  }

  /** Data object that mostly holds information about children of this actor
    *
    * @param starter
    *   The ActorRef that started the simulation and should be notified about
    *   its end
    * @param extSimAdapters
    *   [[ExtSimAdapter]]s need to receive a [[ExtSimAdapter.Stop]] message
    * @param runtimeEventListener
    *   The [[RuntimeEventListener]] that possibly receives an error event
    * @param delayedStoppingActors
    *   The actors that are stopped with delay
    * @param otherActors
    *   All remaining children (excluding ExtSimAdapters, ResultListeners and
    *   RuntimeEventListener)
    */
  private final case class ActorData(
      starter: ActorRef[SimonaEnded],
      extSimAdapters: Iterable[ClassicRef],
      runtimeEventListener: ActorRef[RuntimeEventListener.Request],
      delayedStoppingActors: Seq[ActorRef[DelayedStopHelper.StoppingMsg]],
      otherActors: Iterable[ActorRef[_]],
  )
}
