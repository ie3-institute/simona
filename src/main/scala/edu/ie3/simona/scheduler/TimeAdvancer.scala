/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.scheduler

import edu.ie3.simona.actor.ActorUtil.stopOnError
import edu.ie3.simona.event.RuntimeEvent
import edu.ie3.simona.ontology.messages.Activation
import edu.ie3.simona.ontology.messages.SchedulerMessage.{
  Completion,
  ScheduleActivation,
}
import edu.ie3.simona.sim.SimonaSim
import edu.ie3.simona.util.SimonaConstants.INIT_SIM_TICK
import org.apache.pekko.actor.typed.scaladsl.{ActorContext, Behaviors}
import org.apache.pekko.actor.typed.{ActorRef, Behavior}

/** Unit that is in control of time advancement within the simulation.
  * Represents the root entity of any scheduler hierarchy.
  */
object TimeAdvancer {

  trait Request

  /** Starts simulation by activating the next (or first) tick
    *
    * @param pauseTick
    *   Last tick that can be activated or completed before the simulation is
    *   paused
    */
  final case class Start(
      pauseTick: Option[Long] = None
  ) extends Request

  /** @param simulation
    *   The root actor of the simulation
    * @param eventListener
    *   listener that receives runtime events
    * @param checkWindow
    *   interval in which check window messages are sent
    * @param endTick
    *   last tick of the simulation
    */
  def apply(
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      eventListener: Option[ActorRef[RuntimeEvent]],
      checkWindow: Option[Int],
      endTick: Long,
  ): Behavior[Request] = Behaviors.receivePartial {
    case (_, ScheduleActivation(actor, tick, _)) =>
      inactive(
        TimeAdvancerData(simulation, actor, endTick),
        eventListener.map(RuntimeNotifier(_, checkWindow)),
        tick,
        tick,
      )
  }

  /** TimeAdvancer is inactive and waiting for a StartScheduleMessage to start
    * or continue
    * @param data
    *   constant time advancer data
    * @param notifier
    *   notifier for runtime events
    * @param startingTick
    *   tick that the simulation continues with at the beginning/after the last
    *   pause
    * @param nextActiveTick
    *   tick that the schedulee wants to be activated for next
    */
  private def inactive(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier],
      startingTick: Long,
      nextActiveTick: Long,
  ): Behavior[Request] = Behaviors.receivePartial {
    case (_, Start(pauseTick)) =>
      val updatedNotifier = notifier.map {
        _.starting(
          startingTick,
          pauseTick,
          data.endTick,
        )
      }

      data.schedulee ! Activation(nextActiveTick)

      active(
        data,
        updatedNotifier,
        nextActiveTick,
        pauseTick,
      )
  }

  /** TimeAdvancer is active and waiting for the current activation of the
    * schedulee to complete
    *
    * @param data
    *   constant time advancer data
    * @param notifier
    *   notifier for runtime events
    * @param activeTick
    *   tick that is currently active
    * @param pauseTick
    *   the tick that we should pause at (if applicable)
    */
  private def active(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier],
      activeTick: Long,
      pauseTick: Option[Long],
  ): Behavior[Request] = Behaviors.receivePartial {
    case (ctx, Completion(_, maybeNewTick)) =>
      checkCompletion(activeTick, maybeNewTick)
        .map(endWithFailure(ctx, notifier, activeTick, _))
        .getOrElse {

          (maybeNewTick, pauseTick) match {
            case (Some(newTick), _) if newTick > data.endTick =>
              // next tick is after endTick, finish simulation
              endSuccessfully(data, notifier)

            case (Some(newTick), Some(pauseTick)) if newTick > pauseTick =>
              // next tick is after pause tick, pause sim
              val updatedNotifier = notifier.map {
                _.completing(
                  math.min(newTick - 1, pauseTick)
                )
                  .pausing(pauseTick)
              }

              // pause, inactivate
              inactive(
                data,
                updatedNotifier,
                pauseTick + 1,
                newTick,
              )

            case (Some(newTick), _) =>
              // next tick is ok, continue
              val updatedNotifier = notifier.map { notifier =>
                val notifierCompleted =
                  notifier.completing(newTick - 1)

                if (activeTick == INIT_SIM_TICK)
                  notifierCompleted.starting(
                    newTick,
                    pauseTick,
                    data.endTick,
                  )
                else
                  notifierCompleted
              }

              // activate next
              data.schedulee ! Activation(newTick)
              active(
                data,
                updatedNotifier,
                newTick,
                pauseTick,
              )

            case (None, _) =>
              // there is no next tick, finish
              ctx.log.info("No next tick supplied, stopping simulation.")
              endSuccessfully(data, notifier)

          }
        }

  }

  private def endSuccessfully(
      data: TimeAdvancerData,
      notifier: Option[RuntimeNotifier],
  ): Behavior[Request] = {
    data.simulation ! SimonaSim.SimulationEnded

    notifier.foreach {
      // we do not want a check window message for the endTick
      _.completing(data.endTick - 1)
        .finishing(data.endTick)
    }

    // we do not stop here, but wait until we are terminated
    Behaviors.empty
  }

  private def endWithFailure(
      ctx: ActorContext[Request],
      notifier: Option[RuntimeNotifier],
      tick: Long,
      errorMsg: String,
  ): Behavior[Request] = {
    notifier.foreach(_.error(tick, errorMsg))

    stopOnError(ctx, errorMsg)
  }

  private def checkCompletion(
      activeTick: Long,
      maybeNewTick: Option[Long],
  ): Option[String] =
    maybeNewTick.filter(_ <= activeTick).map { newTick =>
      s"The next trigger has tick $newTick, although current active tick was $activeTick."
    }

  /** This data container stores objects that are not supposed to change for a
    * [[TimeAdvancer]] during simulation
    *
    * @param simulation
    *   The root actor of the simulation
    * @param schedulee
    *   scheduler or other actor whose time advancement is controlled
    * @param endTick
    *   the last tick of the simulation
    */
  private final case class TimeAdvancerData(
      simulation: ActorRef[SimonaSim.SimulationEnded.type],
      schedulee: ActorRef[Activation],
      endTick: Long,
  )
}
