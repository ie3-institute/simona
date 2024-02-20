/*
 * © 2020. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.sim

import edu.ie3.simona.agent.EnvironmentRefs
import edu.ie3.simona.api.ExtSimAdapter
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.event.listener.{ResultEventListener, RuntimeEventListener}
import edu.ie3.simona.main.RunSimona.SimonaEnded
import edu.ie3.simona.scheduler.TimeAdvancer
import edu.ie3.simona.sim.setup.{ExtSimSetupData, SimonaSetup}
import org.apache.pekko.actor.typed.scaladsl.adapter._
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior, PostStop, Terminated}
import org.apache.pekko.actor.{ActorRef => ClassicRef}

import scala.language.postfixOps

/** Main entrance point to a simona simulation as top level actor. This actors
  * allows to control the simulation in the sense that messages for
  * initialization and time steps can be send to this actor and will be handled
  * accordingly. This actor does NOT report back any simulation status except of
  * if the overall simulation has been successful or not. For specific status
  * information, the user needs to pass in and subscribe to the corresponding
  * listener e.g. [[edu.ie3.simona.event.listener.RuntimeEventListener]] for
  * simulation status or [[edu.ie3.simona.event.listener.ResultEventListener]]
  * for result events
  *
  * @version 0.1
  * @since 01.07.20
  */
object SimonaSim {

  sealed trait Incoming

  /** Starts simulation by activating the next (or first) tick */
  final case class Start(
      starter: ActorRef[SimonaEnded]
  ) extends Incoming

  /** Indicate that the simulation has ended successfully */
  case object SimulationEnded extends Incoming

  def apply(simonaSetup: SimonaSetup): Behavior[Incoming] =
    Behaviors.receivePartial { case (ctx, startMsg @ Start(starter)) =>
      // We redirect to initializing behavior so that starter ref
      // is available in case of a sudden termination of this actor
      ctx.self ! startMsg
      initializing(simonaSetup, starter)
    }

  /** Initializing behavior that is separated from [[apply]] above only because
    * in case of a sudden stop of this actor itself (PostStop signal), the
    * [[SimonaEnded]] message is sent to the starter
    *
    * @param starter
    *   The starter ref that needs to be notified once simulation has ended
    */
  private def initializing(
      simonaSetup: SimonaSetup,
      starter: ActorRef[SimonaEnded],
  ): Behavior[Incoming] =
    Behaviors
      .receivePartial[Incoming] { case (ctx, Start(_)) =>
        val resultEventListeners =
          simonaSetup.resultEventListener(ctx)

        val runtimeEventListener = simonaSetup.runtimeEventListener(ctx)

        val timeAdvancer =
          simonaSetup.timeAdvancer(ctx, ctx.self, runtimeEventListener)
        val scheduler = simonaSetup.scheduler(ctx, timeAdvancer)

        /* start services */
        // primary service proxy
        val primaryServiceProxy =
          simonaSetup.primaryServiceProxy(ctx, scheduler)

        // weather service
        val weatherService =
          simonaSetup.weatherService(ctx, scheduler)

        val extSimulationData: ExtSimSetupData =
          simonaSetup.extSimulations(ctx, scheduler)

        val environmentRefs = EnvironmentRefs(
          scheduler,
          runtimeEventListener.toClassic,
          primaryServiceProxy,
          weatherService,
          extSimulationData.evDataService,
        )

        /* start grid agents  */
        val gridAgents = simonaSetup.gridAgents(
          ctx,
          environmentRefs,
          resultEventListeners,
        )

        /* watch all actors */
        resultEventListeners.foreach(ctx.watch)
        ctx.watch(runtimeEventListener)
        ctx.watch(timeAdvancer)
        ctx.watch(scheduler)
        ctx.watch(primaryServiceProxy.toTyped)
        ctx.watch(weatherService.toTyped)
        gridAgents.foreach(ref => ctx.watch(ref.toTyped))

        // Start simulation
        timeAdvancer ! TimeAdvancer.Start()

        val watchedActors = Iterable(
          timeAdvancer,
          scheduler,
          primaryServiceProxy.toTyped,
          weatherService.toTyped,
        ) ++ gridAgents.map(_.toTyped)

        idle(
          ActorData(
            starter,
            watchedActors,
            extSimulationData.extSimAdapters,
            runtimeEventListener,
            resultEventListeners,
          )
        )
      }
      .receiveSignal { case (_, PostStop) =>
        // Something must have gone wrong during initialization above
        // (could have been an exception thrown), there's nothing much
        // we can do here besides notifying starter
        starter ! SimonaEnded(successful = false)

        Behaviors.stopped
      }

  private def idle(actorData: ActorData): Behavior[Incoming] = Behaviors
    .receivePartial[Incoming] { case (ctx, SimulationEnded) =>
      // stop all children and wait for result listeners
      stop(ctx, actorData, simulationSuccessful = true)
    }
    .receiveSignal { case (ctx, Terminated(actor)) =>
      ctx.log.error(
        "An actor ({}) unexpectedly terminated. Shut down all children gracefully and report simulation " +
          "failure. See logs and possible stacktrace for details.",
        actor,
      )

      // stop all children and end
      stop(ctx, actorData, simulationSuccessful = false)
    }

  private def stop(
      ctx: ActorContext[_],
      actorData: ActorData,
      simulationSuccessful: Boolean,
  ): Behavior[Incoming] =
    if (simulationSuccessful) {
      // if the simulation is successful, we're waiting for the result listeners
      // to terminate and thus do not unwatch them here
      ctx.log.info(
        "Simulation terminated successfully. Stopping children and waiting for results to flush out..."
      )

      actorData.resultListener.foreach(_ ! ResultEventListener.FlushAndStop)

      stopChildren(ctx, actorData, simulationSuccessful)

      waitingForListener(
        actorData.starter,
        remainingListeners = actorData.resultListener.toSeq,
      )
    } else {
      ctx.log.error(
        "An error occurred during the simulation. See stacktrace for details."
      )

      // also notify RuntimeEventListener that error happened
      actorData.runtimeEventListener ! RuntimeEvent.Error(
        "Simulation stopped with error."
      )

      actorData.starter ! SimonaEnded(successful = false)

      stopChildren(ctx, actorData, simulationSuccessful)

      Behaviors.stopped
    }

  private def stopChildren(
      ctx: ActorContext[_],
      actorData: ActorData,
      simulationSuccessful: Boolean,
  ): Unit = {
    actorData.watchedActors.foreach { ref =>
      ctx.unwatch(ref)
      ctx.stop(ref)
    }

    actorData.extSimAdapters.foreach { ref =>
      ctx.unwatch(ref)
      ref ! ExtSimAdapter.Stop(simulationSuccessful)
    }

    if (!simulationSuccessful)
      // if an error happened, we do not care for all results to be flushed out
      // and just end ResultEventListeners right away
      actorData.resultListener.foreach { ref =>
        ctx.unwatch(ref)
        ctx.stop(ref)
      }

    ctx.unwatch(actorData.runtimeEventListener)
    // we stop RuntimeEventListener by message so that RuntimeEvents
    // (Error in particular) in queue can still be processed before
    actorData.runtimeEventListener ! RuntimeEventListener.Stop

  }

  private def waitingForListener(
      starter: ActorRef[SimonaEnded],
      remainingListeners: Seq[ActorRef[_]],
  ): Behavior[Incoming] = Behaviors.receiveSignal[Incoming] {
    case (ctx, Terminated(actor)) if remainingListeners.contains(actor) =>
      val updatedRemainingListeners = remainingListeners.filterNot(_ == actor)

      if (updatedRemainingListeners.isEmpty) {
        ctx.log.info(
          "All result listeners have terminated. Ending simulation successfully."
        )

        starter ! SimonaEnded(successful = true)

        Behaviors.stopped
      } else {
        waitingForListener(
          starter,
          updatedRemainingListeners,
        )
      }

  }

  /** TODO scaladoc
    * @param starter
    * @param watchedActors
    *   excluding ExtSimAdapters, ResultListeners, RuntimeEventListener
    * @param extSimAdapters
    * @param runtimeEventListener
    * @param resultListener
    */
  private final case class ActorData(
      starter: ActorRef[SimonaEnded],
      watchedActors: Iterable[ActorRef[_]],
      extSimAdapters: Iterable[ClassicRef],
      runtimeEventListener: ActorRef[RuntimeEventListener.Incoming],
      resultListener: Iterable[ActorRef[ResultEventListener.Incoming]],
  )
}
